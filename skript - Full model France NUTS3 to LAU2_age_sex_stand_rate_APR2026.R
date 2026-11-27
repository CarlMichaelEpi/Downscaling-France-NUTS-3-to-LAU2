rm(list = ls()); gc()

library(sf)
library(giscoR)
library(dplyr)
library(tidyr)
library(readxl)
library(haven)
library(writexl)
library(stringr)
library(Matrix)
library(xgboost)
library(purrr)
library(ggplot2)
library(scales)

setwd("F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/")

# -------------------------------------------------------------------
# Helpers
# -------------------------------------------------------------------

to_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", ".", as.character(x))))
}

collapse_age <- function(x) {
  dplyr::case_when(
    x %in% c("85-94", "95+") ~ "85+",
    TRUE ~ as.character(x)
  )
}

age_levels <- c(
  "0-24", "25-34", "35-44", "45-54",
  "55-64", "65-74", "75-84", "85+"
)

read_indicator_file <- function(path, code_col, name_col, value_name) {
  df <- read_excel(path)
  
  excluded <- c(
    code_col,
    name_col,
    grep("DEP_code", names(df), value = TRUE),
    grep("^EU LAU CODE$", names(df), value = TRUE)
  )
  
  value_candidates <- setdiff(names(df), excluded)
  value_col <- tail(value_candidates, 1)
  
  if (length(value_col) == 0 || is.na(value_col)) {
    stop("Fant ikke verdi-kolonne i filen: ", path)
  }
  
  out <- tibble(
    geo_id = as.character(df[[code_col]]),
    geo_name = as.character(df[[name_col]]),
    value = to_num(df[[value_col]])
  )
  
  names(out)[3] <- value_name
  out
}

# -------------------------------------------------------------------
# Standard age weights
# -------------------------------------------------------------------

std_age <- tibble::tribble(
  ~age_group, ~age_weight,
  "0-24", 27500,
  "25-34", 12500,
  "35-44", 14000,
  "45-54", 14000,
  "55-64", 12500,
  "65-74", 10500,
  "75-84", 6500,
  "85+", 2500
  )

stopifnot(sum(std_age$age_weight) == 100000)

# -------------------------------------------------------------------
# Spatial coordinates for XGBoost predictors
# Use GISCO directly
# -------------------------------------------------------------------

nuts_all_3035 <- gisco_get_nuts(
  year = "2021",
  resolution = "20",
  epsg = "3035"
)

fr_nuts3_geom_3035 <- nuts_all_3035 |>
  filter(CNTR_CODE == "FR", LEVL_CODE == 3) |>
  st_make_valid() |>
  select(NUTS_ID, geometry)

nuts3_cent <- st_centroid(fr_nuts3_geom_3035)

nuts3_xy <- bind_cols(
  st_drop_geometry(nuts3_cent),
  as.data.frame(st_coordinates(nuts3_cent))
) |>
  transmute(
    NUTS3_ID = as.character(NUTS_ID),
    x_coord = X,
    y_coord = Y
  )

lau_all_3035 <- gisco_get_lau(
  year = "2020",
  epsg = "3035"
)

fr_lau_geom_3035 <- lau_all_3035 |>
  filter(CNTR_CODE == "FR") |>
  st_make_valid() |>
  select(LAU_ID, geometry)

lau_cent <- st_centroid(fr_lau_geom_3035)

lau2_xy <- bind_cols(
  st_drop_geometry(lau_cent),
  as.data.frame(st_coordinates(lau_cent))
) |>
  transmute(
    LAU_ID = str_pad(as.character(LAU_ID), width = 5, side = "left", pad = "0"),
    x_coord = X,
    y_coord = Y
  )

# -------------------------------------------------------------------
# Read LAU2 population by age and sex
# -------------------------------------------------------------------

pop_cols <- c(
  "MALE_AGED0024", "MALE_AGED2534", "MALE_AGED3544", "MALE_AGED4554",
  "MALE_AGED5564", "MALE_AGED6574", "MALE_AGED7584", "MALE_AGED8500",
  "FEMALE_AGED0024", "FEMALE_AGED2534", "FEMALE_AGED3544", "FEMALE_AGED4554",
  "FEMALE_AGED5564", "FEMALE_AGED6574", "FEMALE_AGED7584", "FEMALE_AGED8500"
)

df_pop_raw <- read_excel("data/pop_eight_ages.xlsx")

nuts3_lookup <- df_pop_raw |>
  transmute(
    dep_code = str_pad(as.character(NUTS3_DEP_code_2021), width = 2, side = "left", pad = "0"),
    NUTS3_ID = as.character(NUTS3_code_Eurostat_2021),
    NUTS3_name = as.character(NUTS3_name_Eurostat_2021)
  ) |>
  distinct()

df_lau2_cells <- df_pop_raw |>
  mutate(
    across(all_of(pop_cols), to_num),
    LAU2_code_Eurostat_2021 = str_pad(as.character(LAU2_code_Eurostat_2021), width = 5, side = "left", pad = "0"),
    NUTS3_code_Eurostat_2021 = as.character(NUTS3_code_Eurostat_2021),
    NUTS3_name_Eurostat_2021 = as.character(NUTS3_name_Eurostat_2021),
    LAU2_name_Eurostat_2021 = as.character(LAU2_name_Eurostat_2021)
  ) |>
  select(
    LAU2_code_Eurostat_2021,
    LAU2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021,
    NUTS3_name_Eurostat_2021,
    all_of(pop_cols)
  ) |>
  pivot_longer(
    cols = all_of(pop_cols),
    names_to = c("sex_code", "age_code"),
    names_pattern = "(MALE|FEMALE)_(AGED[0-9]{4})",
    values_to = "POP_LAU2"
  ) |>
  mutate(
    sex = recode(sex_code, MALE = "male", FEMALE = "female"),
    age_group = recode(
      age_code,
      AGED0024 = "0-24",
      AGED2534 = "25-34",
      AGED3544 = "35-44",
      AGED4554 = "45-54",
      AGED5564 = "55-64",
      AGED6574 = "65-74",
      AGED7584 = "75-84",
      AGED8500 = "85+"
      )
    ) |>
  transmute(
    LAU_ID = LAU2_code_Eurostat_2021,
    LAU2_name = LAU2_name_Eurostat_2021,
    NUTS3_ID = NUTS3_code_Eurostat_2021,
    NUTS3_name = NUTS3_name_Eurostat_2021,
    sex,
    age_group,
    POP_LAU2 = as.numeric(POP_LAU2)
    ) |>
  filter(
    sex %in% c("female", "male"),
    age_group %in% age_levels,
    !is.na(POP_LAU2),
    POP_LAU2 > 0
    ) |>
  group_by(LAU_ID, LAU2_name, NUTS3_ID, NUTS3_name, sex, age_group) |>
  summarise(
    POP_LAU2 = sum(POP_LAU2, na.rm = TRUE),
    .groups = "drop"
    )

# -------------------------------------------------------------------
# National sex weights from the study population
# -------------------------------------------------------------------

std_sex <- df_lau2_cells |>
  group_by(sex) |>
  summarise(
    pop = sum(POP_LAU2, na.rm = TRUE),
    .groups = "drop"
    ) |>
  mutate(
    sex_weight = pop / sum(pop)
    )

std_age_sex <- tidyr::crossing(std_age, std_sex) |>
  mutate(std_weight = age_weight * sex_weight) |>
  select(age_group, sex, std_weight)

stopifnot(abs(sum(std_age_sex$std_weight) - 100000) < 1e-8)

# -------------------------------------------------------------------
# Read observed NUTS3 YLL by age and sex from .dta provided by Carl
# -------------------------------------------------------------------

df_nuts3_raw <- read_dta("data/YLL_Carl/France_ylls_2021_by_age_group_3sexgroups.dta")

df_nuts3_cells <- df_nuts3_raw |>
  transmute(
    dep_code = str_pad(as.character(local_id), width = 2, side = "left", pad = "0"),
    sex = as.character(sex),
    age_group = collapse_age(agegroup),
    yll = as.numeric(yll),
    pop = as.numeric(pop)
    ) |>
  filter(
    sex %in% c("female", "male"),
    age_group %in% age_levels,
    !is.na(yll),
    !is.na(pop),
    pop > 0
    ) |>
  left_join(nuts3_lookup, by = "dep_code") |>
  group_by(NUTS3_ID, NUTS3_name, sex, age_group) |>
  summarise(
    YLL_NUTS3 = sum(yll, na.rm = TRUE),
    POP_NUTS3 = sum(pop, na.rm = TRUE),
    .groups = "drop"
    ) |>
  mutate(
    YLL_rate_NUTS3 = YLL_NUTS3 / POP_NUTS3 * 100000
    )

if (any(is.na(df_nuts3_cells$NUTS3_ID))) {
  stop("Noen NUTS3 i .dta kunne ikke kobles til NUTS3-koder via dep_code.")
}

# -------------------------------------------------------------------
# Read covariates from results/
# Adjust file names here if needed
# -------------------------------------------------------------------

cov_nuts3 <- list(
  read_indicator_file("results/GP_NUTS3.xlsx", "NUTS3_code_Eurostat_2021", "NUTS3_name_Eurostat_2021", "GP"),
  read_indicator_file("results/CULTURE_EXP_NUTS3.xlsx", "NUTS3_code_Eurostat_2021", "NUTS3_name_Eurostat_2021", "CULTURE_EXP"),
  read_indicator_file("results/SOCIAL_AID_NUTS3.xlsx", "NUTS3_code_Eurostat_2021", "NUTS3_name_Eurostat_2021", "SOCIAL_AID"),
  read_indicator_file("results/AREA_NUTS3.xlsx", "NUTS3_code_Eurostat_2021", "NUTS3_name_Eurostat_2021", "AREA")
  ) |>
  purrr::reduce(full_join, by = "geo_id") |>
  transmute(
    NUTS3_ID = as.character(geo_id),
    GP,
    CULTURE_EXP,
    SOCIAL_AID,
    AREA
    ) |>
  distinct() |>
  left_join(nuts3_xy, by = "NUTS3_ID")

cov_lau2 <- list(
  read_indicator_file("results/GP_LAU2.xlsx", "LAU2_code_Eurostat_2021", "LAU2_name_Eurostat_2021", "GP"),
  read_indicator_file("results/CULTURE_EXP_LAU2.xlsx", "LAU2_code_Eurostat_2021", "LAU2_name_Eurostat_2021", "CULTURE_EXP"),
  read_indicator_file("results/SOCIAL_AID_LAU2.xlsx", "LAU2_code_Eurostat_2021", "LAU2_name_Eurostat_2021", "SOCIAL_AID"),
  read_indicator_file("results/AREA_LAU2.xlsx", "LAU2_code_Eurostat_2021", "LAU2_name_Eurostat_2021", "AREA")
  ) |>
  purrr::reduce(full_join, by = "geo_id") |>
  transmute(
    LAU_ID = str_pad(as.character(geo_id), width = 5, side = "left", pad = "0"),
    GP,
    CULTURE_EXP,
    SOCIAL_AID,
    AREA
    ) |>
  distinct() |>
  left_join(lau2_xy, by = "LAU_ID")

# -------------------------------------------------------------------
# Reference standardized NUTS3 rates
# -------------------------------------------------------------------

df_nuts3_standardized <- df_nuts3_cells |>
  left_join(std_age_sex, by = c("age_group", "sex")) |>
  group_by(NUTS3_ID, NUTS3_name) |>
  summarise(
    YLL_total = sum(YLL_NUTS3, na.rm = TRUE),
    POP_total = sum(POP_NUTS3, na.rm = TRUE),
    crude_YLL_rate = YLL_total / POP_total * 100000,
    ASYLL_rate_NUTS3 = sum(YLL_rate_NUTS3 * std_weight, na.rm = TRUE) / 100000,
    .groups = "drop"
    )

# -------------------------------------------------------------------
# Build training data
# -------------------------------------------------------------------

train_df <- df_nuts3_cells |>
  left_join(cov_nuts3, by = "NUTS3_ID") |>
  mutate(
    age_group = factor(age_group, levels = age_levels),
    sex = factor(sex, levels = c("female", "male")),
    log_pop_cell = log(POP_NUTS3),
    target_log_rate = log1p(YLL_rate_NUTS3)
    )

# -------------------------------------------------------------------
# Build prediction data
# -------------------------------------------------------------------

pred_df <- df_lau2_cells |>
  left_join(cov_lau2, by = "LAU_ID") |>
  mutate(
    age_group = factor(age_group, levels = age_levels),
    sex = factor(sex, levels = c("female", "male")),
    log_pop_cell = log(POP_LAU2)
    )

# -------------------------------------------------------------------
# Impute missing numeric covariates using training medians
# -------------------------------------------------------------------

num_vars <- c("GP", "CULTURE_EXP", "SOCIAL_AID", "AREA", "x_coord", "y_coord", "log_pop_cell")

train_medians <- sapply(train_df[num_vars], function(x) median(x, na.rm = TRUE))

for (v in num_vars) {
  train_df[[v]][is.na(train_df[[v]])] <- train_medians[[v]]
  pred_df[[v]][is.na(pred_df[[v]])]  <- train_medians[[v]]
}

print(colSums(is.na(train_df[num_vars])))
print(colSums(is.na(pred_df[num_vars])))

# -------------------------------------------------------------------
# Design matrices
# -------------------------------------------------------------------

feature_formula <- ~ age_group + sex + age_group:sex +
  log_pop_cell + GP + CULTURE_EXP + SOCIAL_AID + AREA +
  x_coord + y_coord - 1

X_train <- sparse.model.matrix(feature_formula, data = train_df)
train_feature_names <- colnames(X_train)

X_pred_raw <- sparse.model.matrix(feature_formula, data = pred_df)

missing_in_pred <- setdiff(train_feature_names, colnames(X_pred_raw))
if (length(missing_in_pred) > 0) {
  zero_block <- Matrix::Matrix(
    0,
    nrow = nrow(X_pred_raw),
    ncol = length(missing_in_pred),
    sparse = TRUE
  )
  colnames(zero_block) <- missing_in_pred
  X_pred_raw <- cbind(X_pred_raw, zero_block)
}

extra_in_pred <- setdiff(colnames(X_pred_raw), train_feature_names)
if (length(extra_in_pred) > 0) {
  X_pred_raw <- X_pred_raw[, setdiff(colnames(X_pred_raw), extra_in_pred), drop = FALSE]
}

X_pred <- X_pred_raw[, train_feature_names, drop = FALSE]

cat("Rows in train_df:", nrow(train_df), "\n")
cat("Rows in pred_df:", nrow(pred_df), "\n")
cat("Dim X_train:", dim(X_train), "\n")
cat("Dim X_pred:", dim(X_pred), "\n")

dtrain <- xgb.DMatrix(data = X_train, label = train_df$target_log_rate, missing = NA_real_)
dpred <- xgb.DMatrix(data = X_pred, missing = NA_real_)

# -------------------------------------------------------------------
# Train XGBoost
# -------------------------------------------------------------------

set.seed(123)

nuts3_groups <- unique(as.character(train_df$NUTS3_ID))
fold_assignment <- sample(rep(1:5, length.out = length(nuts3_groups)))

folds <- lapply(
  1:5,
  function(k) which(as.character(train_df$NUTS3_ID) %in% nuts3_groups[fold_assignment == k])
  )

params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse",
  eta = 0.05,
  max_depth = 4,
  min_child_weight = 5,
  subsample = 0.8,
  colsample_bytree = 0.8
  )

cv_fit <- xgb.cv(
  params = params,
  data = dtrain,
  folds = folds,
  nrounds = 2000,
  early_stopping_rounds = 50,
  verbose = 1
  )

best_nrounds <- cv_fit$best_iteration

xgb_fit <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = best_nrounds,
  verbose = 1
  )

importance_df <- xgb.importance(model = xgb_fit) |>
  as.data.frame()

# -------------------------------------------------------------------
# Predict
# -------------------------------------------------------------------

pred_vec <- predict(xgb_fit, newdata = dpred)

stopifnot(length(pred_vec) == nrow(pred_df))

pred_df <- pred_df |>
  mutate(
    pred_log_rate_raw = as.numeric(pred_vec),
    PredYLL_rate_LAU2_age_sex_raw = pmax(expm1(pred_log_rate_raw), 0),
    PredYLL_raw = PredYLL_rate_LAU2_age_sex_raw / 100000 * POP_LAU2
    ) |>
  left_join(
    df_nuts3_cells |>
      select(NUTS3_ID, sex, age_group, YLL_NUTS3),
    by = c("NUTS3_ID", "sex", "age_group")
    )

if (any(is.na(pred_df$YLL_NUTS3))) {
  stop("Noen LAU2-celler mangler kobling til YLL_NUTS3 etter join.")
}

# -------------------------------------------------------------------
# Rescale within each NUTS3 x sex x age_group
# -------------------------------------------------------------------

df_lau2_pred <- pred_df |>
  group_by(NUTS3_ID, sex, age_group) |>
  mutate(
    sum_raw = sum(PredYLL_raw, na.rm = TRUE),
    PredYLL_LAU2 = case_when(
      first(YLL_NUTS3) == 0 ~ 0,
      sum_raw > 0 ~ PredYLL_raw * first(YLL_NUTS3) / sum_raw,
      TRUE ~ (POP_LAU2 / sum(POP_LAU2, na.rm = TRUE)) * first(YLL_NUTS3)
      ),
    YLL_rate_LAU2_age_sex = PredYLL_LAU2 / POP_LAU2 * 100000
    ) |>
  ungroup()

# -------------------------------------------------------------------
# Check coherence
# -------------------------------------------------------------------

check_yll <- df_lau2_pred |>
  group_by(NUTS3_ID, sex, age_group) |>
  summarise(
    sum_pred_lau2 = sum(PredYLL_LAU2, na.rm = TRUE),
    YLL_NUTS3 = first(YLL_NUTS3),
    diff = sum_pred_lau2 - YLL_NUTS3,
    .groups = "drop"
    )

print(summary(check_yll$diff))
#Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#-4.547e-13  0.000e+00  0.000e+00 -7.679e-16  0.000e+00  2.274e-13 

print(head(check_yll, 20))
## A tibble: 20 × 6
#NUTS3_ID sex    age_group sum_pred_lau2 YLL_NUTS3      diff
#<chr>    <chr>  <chr>             <dbl>     <dbl>     <dbl>
#1 FR101    female 0-24                0         0    0       
#2 FR101    female 25-34               0         0    0       
#3 FR101    female 35-44             104.      104.   0       
#4 FR101    female 45-54             168.      168.   0       
#5 FR101    female 55-64             457.      457.   5.68e-14
#6 FR101    female 65-74             544.      544.   0       
#7 FR101    female 75-84             770.      770.   0       
#8 FR101    female 85+              1030.     1030.   0       
#9 FR101    male   0-24                0         0    0       
#10 FR101    male   25-34               0         0    0       
#11 FR101    male   35-44             259.      259.   0       
#12 FR101    male   45-54            1008.     1008.   0       
#13 FR101    male   55-64            1763.     1763.  -2.27e-13
#14 FR101    male   65-74            2246.     2246.   0       
#15 FR101    male   75-84            1633.     1633.   0       
#16 FR101    male   85+              1129.     1129.   0       
#17 FR102    female 0-24                0         0    0       
#18 FR102    female 25-34              61.6      61.6  0       
#19 FR102    female 35-44             104.      104.   0       
#20 FR102    female 45-54               0         0    0  

# -------------------------------------------------------------------
# Age- and sex-standardized YLL rate per LAU2
# -------------------------------------------------------------------

df_lau2_standardized <- df_lau2_pred |>
  left_join(std_age_sex, by = c("age_group", "sex")) |>
  group_by(LAU_ID, LAU2_name, NUTS3_ID, NUTS3_name) |>
  summarise(
    PredYLL_total = sum(PredYLL_LAU2, na.rm = TRUE),
    POP_total = sum(POP_LAU2, na.rm = TRUE),
    crude_YLL_rate = PredYLL_total / POP_total * 100000,
    ASYLL_rate_LAU2 = sum(YLL_rate_LAU2_age_sex * std_weight, na.rm = TRUE) / 100000,
    .groups = "drop"
    ) |>
  arrange(NUTS3_ID, LAU_ID)

print(head(df_lau2_standardized))
## A tibble: 6 × 8
#LAU_ID LAU2_name                    NUTS3_ID NUTS3_name     PredYLL_total POP_total crude_YLL_rate ASYLL_rate_LAU2
#<chr>  <chr>                        <chr>    <chr>                  <dbl>     <dbl>          <dbl>           <dbl>
#1 75056  "Paris"                      FR101    Paris               11111.    2133111            521.            611.
#2 77001  "Ach\u00e8res-la-For\u00eat" FR102    Seine-et-Marne          7.65     1154            663.            617.
#3 77002  "Amillis"                    FR102    Seine-et-Marne          6.81      811.           840.            675.
#4 77003  "Amponville"                 FR102    Seine-et-Marne          2.24      346.           647.            616.
#5 77004  "Andrezel"                   FR102    Seine-et-Marne          1.69      311.           544.            543.
#6 77005  "Annet-sur-Marne"            FR102    Seine-et-Marne         18.4      3317            556.            682.

# -------------------------------------------------------------------
# Save outputs
# -------------------------------------------------------------------

write_xlsx(
  list(
    NUTS3_standardized = df_nuts3_standardized,
    LAU2_age_sex_predictions = df_lau2_pred,
    LAU2_standardized = df_lau2_standardized,
    NUTS3_LAU2_check = check_yll,
    XGBoost_importance = importance_df
    ),
  path = "results/YLL_ASYLL_xgboost_outputs.xlsx"
  )

# -------------------------------------------------------------------
# Map data
# -------------------------------------------------------------------

nuts_all_4326 <- gisco_get_nuts(
  year = "2021",
  resolution = "20",
  epsg = "4326"
  )

fr_nuts3_map <- nuts_all_4326 |>
  filter(CNTR_CODE == "FR", LEVL_CODE == 3)

lau_all_4326 <- gisco_get_lau(
  year = "2020",
  epsg = "4326"
  )

fr_lau_map <- lau_all_4326 |>
  filter(CNTR_CODE == "FR")

bbox_fr <- st_bbox(
  c(xmin = -6, xmax = 10, ymin = 41, ymax = 52),
  crs = st_crs(fr_lau_map)
  )

fr_lau_map <- st_crop(fr_lau_map, bbox_fr)
fr_nuts3_map <- st_crop(fr_nuts3_map, bbox_fr)

fr_lau_map <- fr_lau_map |>
  left_join(df_lau2_standardized, by = "LAU_ID")

# -------------------------------------------------------------------
# Map: LAU2 age- and sex-standardized YLL rate
# -------------------------------------------------------------------

p_lau2_asyll <- ggplot() +
  geom_sf(
    data = fr_lau_map,
    aes(fill = ASYLL_rate_LAU2),
    color = NA
    ) +
  #geom_sf(
  #  data = fr_nuts3_map,
  #  fill = NA,
  #  color = "white",
  #  linewidth = 0.12
  #  ) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey90",
    labels = scales::label_number(accuracy = 1, decimal.mark = ",")
    ) +
  coord_sf(
    xlim = c(-6, 10),
    ylim = c(41, 52),
    expand = FALSE
    ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
    ) +
  labs(
    title = "Predicted age- and sex-standardized YLL rate at LAU2",
    fill = "YLL per 100,000"
    )

print(p_lau2_asyll)

ggsave(
  filename = "figures/YLL_ASYLL_LAU2_xgboost.png",
  plot = p_lau2_asyll,
  width = 8,
  height = 8,
  dpi = 300
  )
