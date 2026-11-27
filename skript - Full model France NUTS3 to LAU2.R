# -------------------------------------------------------------------
# France: Predict YLL at LAU2 using saved XGBoost model
# Final model: age + sex + PopDensity
# -------------------------------------------------------------------

rm(list = ls()); gc()
Sys.setlocale("LC_ALL", "nb-NO.UTF-8")

setwd("F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/")

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(writexl)
})

if (requireNamespace("conflicted", quietly = TRUE)) {
  conflicted::conflicts_prefer(base::as.matrix)
  conflicted::conflicts_prefer(dplyr::filter)
  conflicted::conflicts_prefer(dplyr::lag)
  conflicted::conflicts_prefer(dplyr::mutate)
  conflicted::conflicts_prefer(dplyr::summarise)
  conflicted::conflicts_prefer(dplyr::select)
}

# -------------------------------------------------------------------
# Helper
# -------------------------------------------------------------------

to_num <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(gsub(",", ".", x)))
}

# -------------------------------------------------------------------
# Paths
# -------------------------------------------------------------------

results_path <- "results"
models_path <- "models"

# -------------------------------------------------------------------
# Load saved model
# -------------------------------------------------------------------

final_model <- readRDS(file.path(models_path, "xgboost_final_model_share_based.rds"))

# -------------------------------------------------------------------
# Read LAU2 input files
# -------------------------------------------------------------------

pop_lau2  <- read_excel(file.path(results_path, "POP_LAU2.xlsx"))
area_lau2 <- read_excel(file.path(results_path, "AREA_LAU2.xlsx"))
yll_nuts3 <- read_excel(file.path(results_path, "YLL_NUTS3.xlsx"))

# -------------------------------------------------------------------
# Clean column names
# -------------------------------------------------------------------

names(pop_lau2)  <- make.names(names(pop_lau2), unique = TRUE)
names(area_lau2) <- make.names(names(area_lau2), unique = TRUE)
names(yll_nuts3) <- make.names(names(yll_nuts3), unique = TRUE)

# -------------------------------------------------------------------
# Convert numeric columns where needed
# -------------------------------------------------------------------

pop_lau2 <- pop_lau2 |>
  mutate(across(where(is.character), ~ .x)) |>
  mutate(
    TOTAL_POP = to_num(TOTAL_POP),
    MALE_AGED0019 = to_num(MALE_AGED0019),
    MALE_AGED2064 = to_num(MALE_AGED2064),
    MALE_AGED6500 = to_num(MALE_AGED6500),
    FEMALE_AGED0019 = to_num(FEMALE_AGED0019),
    FEMALE_AGED2064 = to_num(FEMALE_AGED2064),
    FEMALE_AGED6500 = to_num(FEMALE_AGED6500)
  )

area_lau2 <- area_lau2 |>
  mutate(Total_area.m2._Eurostat_2021 = to_num(Total_area.m2._Eurostat_2021))

yll_nuts3 <- yll_nuts3 |>
  mutate(YLL = to_num(YLL))

# -------------------------------------------------------------------
# Keep only needed columns
# -------------------------------------------------------------------

pop_lau2 <- pop_lau2 |>
  select(
    NUTS1_code_Eurostat_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021,
    NUTS3_name_Eurostat_2021,
    LAU2_code_Eurostat_2021,
    LAU2_name_Eurostat_2021,
    TOTAL_POP,
    MALE_AGED0019,
    MALE_AGED2064,
    MALE_AGED6500,
    FEMALE_AGED0019,
    FEMALE_AGED2064,
    FEMALE_AGED6500
  )

area_lau2 <- area_lau2 |>
  select(
    NUTS3_code_Eurostat_2021,
    LAU2_code_Eurostat_2021,
    Total_area.m2._Eurostat_2021
  )

yll_nuts3 <- yll_nuts3 |>
  select(
    NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021,
    NUTS3_name_Eurostat_2021,
    YLL
  ) |>
  rename(YLL_NUTS3 = YLL)

# -------------------------------------------------------------------
# Merge to one LAU2 dataset
# -------------------------------------------------------------------

lau2_df <- pop_lau2 |>
  left_join(
    area_lau2,
    by = c("NUTS3_code_Eurostat_2021", "LAU2_code_Eurostat_2021")
  ) |>
  left_join(
    yll_nuts3,
    by = c(
      "NUTS2_code_Eurostat_2021",
      "NUTS2_name_Eurostat_2021",
      "NUTS3_code_Eurostat_2021",
      "NUTS3_name_Eurostat_2021"
    )
  )

# -------------------------------------------------------------------
# Create predictors exactly as in training model
# -------------------------------------------------------------------

lau2_df <- lau2_df |>
  mutate(
    females_fracpop0019 = FEMALE_AGED0019 / TOTAL_POP,
    females_fracpop2064 = FEMALE_AGED2064 / TOTAL_POP,
    females_fracpop6500 = FEMALE_AGED6500 / TOTAL_POP,
    males_fracpop0019 = MALE_AGED0019 / TOTAL_POP,
    males_fracpop2064 = MALE_AGED2064 / TOTAL_POP,
    males_fracpop6500 = MALE_AGED6500 / TOTAL_POP,
    PopDensity = TOTAL_POP / (Total_area.m2._Eurostat_2021 / 1000000)
  )

# -------------------------------------------------------------------
# Predictor list: must match saved model
# -------------------------------------------------------------------

predictors <- c(
  "females_fracpop0019",
  "females_fracpop2064",
  "females_fracpop6500",
  "males_fracpop0019",
  "males_fracpop2064",
  "males_fracpop6500",
  "PopDensity"
)

# -------------------------------------------------------------------
# Keep rows with complete predictor data
# -------------------------------------------------------------------

all_needed <- c(
  "NUTS1_code_Eurostat_2021",
  "NUTS1_name_Eurostat_2021",
  "NUTS2_code_Eurostat_2021",
  "NUTS2_name_Eurostat_2021",
  "NUTS3_code_Eurostat_2021",
  "NUTS3_name_Eurostat_2021",
  "LAU2_code_Eurostat_2021",
  "LAU2_name_Eurostat_2021",
  "TOTAL_POP",
  "YLL_NUTS3",
  "Total_area.m2._Eurostat_2021",
  predictors
)

lau2_df <- lau2_df |>
  select(all_of(all_needed)) |>
  mutate(across(c(TOTAL_POP, YLL_NUTS3, Total_area.m2._Eurostat_2021, all_of(predictors)), as.numeric)) |>
  drop_na()

cat("Number of LAU2 units used:", nrow(lau2_df), "\n")
#Number of LAU2 units used in all models: 34830 

# -------------------------------------------------------------------
# Predict raw scores
# -------------------------------------------------------------------

X_lau2 <- lau2_df |>
  dplyr::select(all_of(predictors)) |>
  base::as.matrix()

pred_log_share <- predict(final_model, X_lau2)

# -------------------------------------------------------------------
# Convert raw predictions to shares and scale within NUTS3
# -------------------------------------------------------------------

lau2_df <- lau2_df |>
  mutate(
    Pred_share_raw = exp(pred_log_share) - 1e-9,
    Pred_share_raw = pmax(Pred_share_raw, 1e-12)
  ) |>
  group_by(NUTS3_code_Eurostat_2021) |>
  mutate(
    Pred_share = Pred_share_raw / sum(Pred_share_raw, na.rm = TRUE),
    PredYLL_LAU2 = Pred_share * first(YLL_NUTS3),
    PredYLL_rate_LAU2 = PredYLL_LAU2 / TOTAL_POP * 100000
  ) |>
  ungroup()

# -------------------------------------------------------------------
# Check that totals match within each NUTS3
# -------------------------------------------------------------------

check_totals <- lau2_df |>
  group_by(NUTS3_code_Eurostat_2021, NUTS3_name_Eurostat_2021) |>
  summarise(
    YLL_NUTS3 = first(YLL_NUTS3),
    PredYLL_sum_LAU2 = sum(PredYLL_LAU2, na.rm = TRUE),
    diff = PredYLL_sum_LAU2 - YLL_NUTS3,
    .groups = "drop"
  )

print(summary(check_totals$diff))

# -------------------------------------------------------------------
# Save outputs
# -------------------------------------------------------------------

write_xlsx(
  list(
    predictions_LAU2 = lau2_df,
    check_totals = check_totals
  ),
  path = file.path(results_path, "YLL_predictions_LAU2_share_based.xlsx")
)
