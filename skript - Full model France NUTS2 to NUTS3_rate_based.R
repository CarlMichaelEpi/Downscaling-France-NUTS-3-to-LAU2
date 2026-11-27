# -------------------------------------------------------------------
# France: XGBoost baseline age+sex + one additional predictor at a time
# Uses aggregated files in /results
# Goal: use NUTS2 YLL total to distribute/predict YLL at NUTS3,
# then compare predicted vs observed YLL at NUTS3
# -------------------------------------------------------------------

rm(list = ls()); gc()
Sys.setlocale("LC_ALL", "nb-NO.UTF-8")

setwd("F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/")

suppressPackageStartupMessages({
  library(tidyverse)
  library(readxl)
  library(xgboost)
  library(caret)
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
# Helper functions
# -------------------------------------------------------------------

to_num <- function(x) {
  if (is.numeric(x)) return(x)
  as.numeric(gsub(",", ".", x))
}

count_metrics <- function(obs_count, pred_count) {
  obs <- as.numeric(obs_count)
  pred <- as.numeric(pred_count)
  
  tibble(
    MAE = mean(abs(obs - pred), na.rm = TRUE),
    RMSE = sqrt(mean((obs - pred)^2, na.rm = TRUE)),
    MAPE = mean(abs(obs - pred) / pmax(obs, 1e-9), na.rm = TRUE) * 100,
    MaxAbsPct = max(abs(obs - pred) / pmax(obs, 1e-9), na.rm = TRUE) * 100,
    R2 = suppressWarnings(cor(obs, pred, use = "complete.obs")^2)
  )
}

# -------------------------------------------------------------------
# File paths
# -------------------------------------------------------------------

results_path <- "results"

# -------------------------------------------------------------------
# Read aggregated files
# -------------------------------------------------------------------

yll_nuts2 <- read_excel(file.path(results_path, "YLL_NUTS2.xlsx"))
yll_nuts3 <- read_excel(file.path(results_path, "YLL_NUTS3.xlsx"))
pop_nuts3 <- read_excel(file.path(results_path, "POP_NUTS3.xlsx"))
area_nuts3 <- read_excel(file.path(results_path, "AREA_NUTS3.xlsx"))
social_nuts3 <- read_excel(file.path(results_path, "SOCIAL_AID_NUTS3.xlsx"))
culture_nuts3 <- read_excel(file.path(results_path, "CULTURE_EXP_NUTS3.xlsx"))
gp_nuts3 <- read_excel(file.path(results_path, "GP_NUTS3.xlsx"))

names(yll_nuts2) <- make.names(names(yll_nuts2), unique = TRUE)
names(yll_nuts3) <- make.names(names(yll_nuts3), unique = TRUE)
names(pop_nuts3) <- make.names(names(pop_nuts3), unique = TRUE)
names(area_nuts3) <- make.names(names(area_nuts3), unique = TRUE)
names(social_nuts3) <- make.names(names(social_nuts3), unique = TRUE)
names(culture_nuts3) <- make.names(names(culture_nuts3), unique = TRUE)
names(gp_nuts3) <- make.names(names(gp_nuts3), unique = TRUE)

# -------------------------------------------------------------------
# Convert numeric columns
# -------------------------------------------------------------------

yll_nuts2 <- yll_nuts2 |>
  mutate(across(everything(), to_num)) |>
  mutate(
    NUTS1_code_Eurostat_2021 = read_excel(file.path(results_path, "YLL_NUTS2.xlsx"))$NUTS1_code_Eurostat_2021,
    NUTS1_name_Eurostat_2021 = read_excel(file.path(results_path, "YLL_NUTS2.xlsx"))$NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021 = read_excel(file.path(results_path, "YLL_NUTS2.xlsx"))$NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021 = read_excel(file.path(results_path, "YLL_NUTS2.xlsx"))$NUTS2_name_Eurostat_2021
  )

yll_nuts3 <- yll_nuts3 |>
  mutate(across(everything(), to_num)) |>
  mutate(
    NUTS1_code_Eurostat_2021 = read_excel(file.path(results_path, "YLL_NUTS3.xlsx"))$NUTS1_code_Eurostat_2021,
    NUTS1_name_Eurostat_2021 = read_excel(file.path(results_path, "YLL_NUTS3.xlsx"))$NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021 = read_excel(file.path(results_path, "YLL_NUTS3.xlsx"))$NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021 = read_excel(file.path(results_path, "YLL_NUTS3.xlsx"))$NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021 = read_excel(file.path(results_path, "YLL_NUTS3.xlsx"))$NUTS3_code_Eurostat_2021,
    NUTS3_name_Eurostat_2021 = read_excel(file.path(results_path, "YLL_NUTS3.xlsx"))$NUTS3_name_Eurostat_2021
  )

pop_nuts3 <- pop_nuts3 |>
  mutate(across(everything(), to_num)) |>
  mutate(
    NUTS1_code_Eurostat_2021 = read_excel(file.path(results_path, "POP_NUTS3.xlsx"))$NUTS1_code_Eurostat_2021,
    NUTS1_name_Eurostat_2021 = read_excel(file.path(results_path, "POP_NUTS3.xlsx"))$NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021 = read_excel(file.path(results_path, "POP_NUTS3.xlsx"))$NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021 = read_excel(file.path(results_path, "POP_NUTS3.xlsx"))$NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021 = read_excel(file.path(results_path, "POP_NUTS3.xlsx"))$NUTS3_code_Eurostat_2021,
    NUTS3_name_Eurostat_2021 = read_excel(file.path(results_path, "POP_NUTS3.xlsx"))$NUTS3_name_Eurostat_2021
  )

area_nuts3 |>
  select(
    NUTS2_code_Eurostat_2021,
    NUTS3_code_Eurostat_2021,
    Total_area.m2._Eurostat_2021,
    n_LAU2_Large.urban.centers,
    n_LAU2_Intermediate.urban.centers,
    n_LAU2_Rural.towns,
    n_LAU2_Rural.with.dispersed.housing,
    n_LAU2_Rural.with.very.scattered.housing,
    n_LAU2_Small.towns,
    n_LAU2_Urban.belts
  )

social_nuts3 <- social_nuts3 |>
  mutate(across(everything(), to_num)) |>
  mutate(
    NUTS2_code_Eurostat_2021 = read_excel(file.path(results_path, "SOCIAL_AID_NUTS3.xlsx"))$NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021 = read_excel(file.path(results_path, "SOCIAL_AID_NUTS3.xlsx"))$NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021 = read_excel(file.path(results_path, "SOCIAL_AID_NUTS3.xlsx"))$NUTS3_code_Eurostat_2021,
    NUTS3_name_Eurostat_2021 = read_excel(file.path(results_path, "SOCIAL_AID_NUTS3.xlsx"))$NUTS3_name_Eurostat_2021
  )

culture_nuts3 <- culture_nuts3 |>
  mutate(across(everything(), to_num)) |>
  mutate(
    NUTS2_code_Eurostat_2021 = read_excel(file.path(results_path, "CULTURE_EXP_NUTS3.xlsx"))$NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021 = read_excel(file.path(results_path, "CULTURE_EXP_NUTS3.xlsx"))$NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021 = read_excel(file.path(results_path, "CULTURE_EXP_NUTS3.xlsx"))$NUTS3_code_Eurostat_2021,
    NUTS3_name_Eurostat_2021 = read_excel(file.path(results_path, "CULTURE_EXP_NUTS3.xlsx"))$NUTS3_name_Eurostat_2021
  )

gp_nuts3 <- gp_nuts3 |>
  mutate(across(everything(), to_num)) |>
  mutate(
    NUTS2_code_Eurostat_2021 = read_excel(file.path(results_path, "GP_NUTS3.xlsx"))$NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021 = read_excel(file.path(results_path, "GP_NUTS3.xlsx"))$NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021 = read_excel(file.path(results_path, "GP_NUTS3.xlsx"))$NUTS3_code_Eurostat_2021,
    NUTS3_name_Eurostat_2021 = read_excel(file.path(results_path, "GP_NUTS3.xlsx"))$NUTS3_name_Eurostat_2021
  )

# -------------------------------------------------------------------
# Keep only needed columns and rename YLL variables
# -------------------------------------------------------------------

yll_nuts2 <- yll_nuts2 |>
  select(
    NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021,
    YLL
  ) |>
  rename(YLL_NUTS2 = YLL)

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
# Create baseline predictors from population data
# Same logic as in Norway: age*sex as share of total population
# -------------------------------------------------------------------

pop_nuts3 <- pop_nuts3 |>
  mutate(
    females_fracpop0019 = FEMALE_AGED0019 / TOTAL_POP,
    females_fracpop2064 = FEMALE_AGED2064 / TOTAL_POP,
    females_fracpop6500 = FEMALE_AGED6500 / TOTAL_POP,
    males_fracpop0019   = MALE_AGED0019 / TOTAL_POP,
    males_fracpop2064   = MALE_AGED2064 / TOTAL_POP,
    males_fracpop6500   = MALE_AGED6500 / TOTAL_POP
  )

# -------------------------------------------------------------------
# Merge all NUTS3-level predictors and outcomes
# -------------------------------------------------------------------

model_df <- pop_nuts3 |>
  left_join(
    area_nuts3 |>
      select(
        NUTS2_code_Eurostat_2021,
        NUTS3_code_Eurostat_2021,
        Total_area.m2._Eurostat_2021,
        n_LAU2_Large.urban.centers,
        n_LAU2_Intermediate.urban.centers,
        n_LAU2_Rural.towns,
        n_LAU2_Rural.with.dispersed.housing,
        n_LAU2_Rural.with.very.scattered.housing,
        n_LAU2_Small.towns,
        n_LAU2_Urban.belts
      ),
    by = c("NUTS2_code_Eurostat_2021", "NUTS3_code_Eurostat_2021")
  ) |>
  left_join(
    social_nuts3 |>
      select(
        NUTS2_code_Eurostat_2021,
        NUTS3_code_Eurostat_2021,
        Exp_social_aid_before_tax_euro_per_inhabitant_2021
      ),
    by = c("NUTS2_code_Eurostat_2021", "NUTS3_code_Eurostat_2021")
  ) |>
  left_join(
    culture_nuts3 |>
      select(
        NUTS2_code_Eurostat_2021,
        NUTS3_code_Eurostat_2021,
        Avg_total_exp_culture_services_euro_per_inhabitants_2023_NUTS3
      ),
    by = c("NUTS2_code_Eurostat_2021", "NUTS3_code_Eurostat_2021")
  ) |>
  left_join(
    gp_nuts3 |>
      select(
        NUTS2_code_Eurostat_2021,
        NUTS3_code_Eurostat_2021,
        Avg_GP_consultations_number_2019
      ),
    by = c("NUTS2_code_Eurostat_2021", "NUTS3_code_Eurostat_2021")
  ) |>
  left_join(
    yll_nuts3,
    by = c(
      "NUTS2_code_Eurostat_2021",
      "NUTS2_name_Eurostat_2021",
      "NUTS3_code_Eurostat_2021",
      "NUTS3_name_Eurostat_2021"
    )
  ) |>
  left_join(
    yll_nuts2,
    by = c(
      "NUTS2_code_Eurostat_2021",
      "NUTS2_name_Eurostat_2021"
    )
  ) |>
  mutate(
    PopDensity = TOTAL_POP / (Total_area.m2._Eurostat_2021 / 1000000)
  )

# -------------------------------------------------------------------
# Baseline predictors: age + sex
# -------------------------------------------------------------------

baseline_predictors <- c(
  "females_fracpop0019",
  "females_fracpop2064",
  "females_fracpop6500",
  "males_fracpop0019",
  "males_fracpop2064",
  "males_fracpop6500"
)

# -------------------------------------------------------------------
# Candidate predictors to test one by one
# -------------------------------------------------------------------

candidate_predictors <- c(
  "PopDensity",
  "Exp_social_aid_before_tax_euro_per_inhabitant_2021",
  "Avg_total_exp_culture_services_euro_per_inhabitants_2023_NUTS3",
  "Avg_GP_consultations_number_2019",
  "n_LAU2_Large.urban.centers",
  "n_LAU2_Intermediate.urban.centers",
  "n_LAU2_Rural.towns",
  "n_LAU2_Urban.belts"
)

# -------------------------------------------------------------------
# Use same rows for all models
# -------------------------------------------------------------------

all_needed <- c(
  "NUTS2_code_Eurostat_2021",
  "NUTS2_name_Eurostat_2021",
  "NUTS3_code_Eurostat_2021",
  "NUTS3_name_Eurostat_2021",
  "TOTAL_POP",
  "YLL_NUTS2",
  "YLL_NUTS3",
  baseline_predictors,
  candidate_predictors
)

model_df <- model_df |>
  select(all_of(all_needed)) |>
  mutate(across(
    .cols = c(TOTAL_POP, YLL_NUTS2, YLL_NUTS3, all_of(baseline_predictors), all_of(candidate_predictors)),
    .fns = ~ suppressWarnings(as.numeric(.x))
  )) |>
  drop_na()

cat("Number of NUTS3 units used in all models:", nrow(model_df), "\n")
#Number of NUTS3 units used in all models: 96 

# -------------------------------------------------------------------
# Outcome for modelling:
# use observed NUTS3 rate of YLL per population
# -------------------------------------------------------------------

model_df <- model_df |>
  mutate(
    yll_rate = YLL_NUTS3 / TOTAL_POP,
    log_yll_rate = log(yll_rate + 1e-9)
  )

# -------------------------------------------------------------------
# Common CV setup
# Group folds by NUTS2 to avoid leakage
# -------------------------------------------------------------------

set.seed(1984)

nuts2_ids <- unique(model_df$NUTS2_code_Eurostat_2021)
fold_index_nuts2 <- createFolds(nuts2_ids, k = 5, returnTrain = TRUE)

fold_index <- lapply(fold_index_nuts2, function(idx) {
  train_nuts2 <- nuts2_ids[idx]
  which(model_df$NUTS2_code_Eurostat_2021 %in% train_nuts2)
})

train_control <- trainControl(
  method = "cv",
  number = 5,
  index = fold_index,
  verboseIter = FALSE,
  returnResamp = "final"
)

tune_grid <- expand.grid(
  nrounds = c(50, 100, 200),
  max_depth = c(3, 5, 7),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 1),
  colsample_bytree = c(0.7, 1),
  min_child_weight = c(1, 5),
  subsample = c(0.5, 0.9)
)

# -------------------------------------------------------------------
# Helper function: fit one model and return metrics
# -------------------------------------------------------------------

fit_one_model <- function(df, predictors, model_name) {
  
  X <- df |>
    select(all_of(predictors)) |>
    base::as.matrix()
  
  Y <- df$log_yll_rate
  weights <- sqrt(df$TOTAL_POP)
  
  set.seed(1984)
  model_fit <- caret::train(
    x = X,
    y = Y,
    method = "xgbTree",
    trControl = train_control,
    tuneGrid = tune_grid,
    weights = weights,
    metric = "MAE",
    maximize = FALSE,
    verbosity = 0
  )
  
  predicted_log_rate <- predict(model_fit, X)
  
  comparison <- tibble(
    NUTS2_code_Eurostat_2021 = df$NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021 = df$NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021 = df$NUTS3_code_Eurostat_2021,
    NUTS3_name_Eurostat_2021 = df$NUTS3_name_Eurostat_2021,
    Total_pop = df$TOTAL_POP,
    ObsYLL_NUTS2 = df$YLL_NUTS2,
    ObsYLL_NUTS3 = df$YLL_NUTS3,
    Obs_rate = df$yll_rate,
    Pred_rate_raw = exp(predicted_log_rate) - 1e-9
  )
  
  comparison <- comparison |>
    mutate(
      Pred_rate_raw = pmax(Pred_rate_raw, 1e-12)
    ) |>
    group_by(NUTS2_code_Eurostat_2021) |>
    mutate(
      Pred_rate = Pred_rate_raw,
      PredYLL_NUTS3 = Pred_rate * first(ObsYLL_NUTS2)
    ) |>
    ungroup()
  
  best_row <- model_fit$results[which.min(model_fit$results$MAE), ]
  
  metrics_count <- count_metrics(
    obs_count = comparison$ObsYLL_NUTS3,
    pred_count = comparison$PredYLL_NUTS3
  ) |>
    mutate(
      model = model_name,
      n_predictors = length(predictors),
      predictors = paste(predictors, collapse = ", "),
      cv_MAE = best_row$MAE,
      cv_RMSE = best_row$RMSE,
      cv_R2 = best_row$Rsquared,
      .before = 1
    )
  
  list(
    model_fit = model_fit,
    comparison = comparison,
    metrics = metrics_count
  )
}

# -------------------------------------------------------------------
# Run baseline model
# -------------------------------------------------------------------

results_all <- list()
metrics_all <- list()

baseline_name <- "Model_1_age_sex"

res_baseline <- fit_one_model(
  df = model_df,
  predictors = baseline_predictors,
  model_name = baseline_name
)

results_all[[baseline_name]] <- res_baseline
metrics_all[[baseline_name]] <- res_baseline$metrics

# -------------------------------------------------------------------
# Run age+sex + one extra predictor at a time
# -------------------------------------------------------------------

for (var in candidate_predictors) {
  model_name <- paste0("Model_2_age_sex_plus_", var)
  
  res <- fit_one_model(
    df = model_df,
    predictors = c(baseline_predictors, var),
    model_name = model_name
  )
  
  results_all[[model_name]] <- res
  metrics_all[[model_name]] <- res$metrics
}

# -------------------------------------------------------------------
# Combine metrics and compare against baseline
# -------------------------------------------------------------------

metrics_table <- bind_rows(metrics_all) |>
  mutate(
    delta_MAE_vs_baseline = MAE - MAE[model == baseline_name][1],
    delta_RMSE_vs_baseline = RMSE - RMSE[model == baseline_name][1],
    delta_MAPE_vs_baseline = MAPE - MAPE[model == baseline_name][1],
    delta_MaxAbsPct_vs_baseline = MaxAbsPct - MaxAbsPct[model == baseline_name][1]
  ) |>
  arrange(MAE)

print(metrics_table)
## A tibble: 9 × 15
#model                      n_predictors predictors cv_MAE cv_RMSE cv_R2   MAE  RMSE  MAPE MaxAbsPct     R2 delta_MAE_vs_baseline delta_RMSE_vs_baseline
#<chr>                             <int> <chr>       <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl>     <dbl>  <dbl>                 <dbl>                  <dbl>
#1 Model_2_age_sex_plus_PopD…            7 females_f…  0.121   0.143 0.614 5024. 5995.  94.1      98.7 0.0811                -7.06                  -6.17 
#2 Model_2_age_sex_plus_n_LA…            7 females_f…  0.116   0.140 0.636 5025. 5995.  94.2      98.7 0.0881                -6.26                  -6.00 
#3 Model_2_age_sex_plus_n_LA…            7 females_f…  0.114   0.136 0.646 5025. 5996.  94.1      98.7 0.0768                -6.22                  -4.85 
#4 Model_2_age_sex_plus_Avg_…            7 females_f…  0.106   0.134 0.663 5030. 6000.  94.3      98.7 0.0919                -0.459                 -0.953
#5 Model_2_age_sex_plus_n_LA…            7 females_f…  0.110   0.132 0.664 5030. 6001.  94.3      98.7 0.0846                -0.446                 -0.445
#6 Model_2_age_sex_plus_n_LA…            7 females_f…  0.115   0.138 0.636 5031. 6001.  94.3      98.7 0.0853                -0.210                 -0.265
#7 Model_2_age_sex_plus_Avg_…            7 females_f…  0.119   0.147 0.608 5031. 6001.  94.3      98.7 0.0861                -0.156                 -0.228
#8 Model_2_age_sex_plus_Exp_…            7 females_f…  0.109   0.133 0.656 5031. 6001.  94.3      98.7 0.0900                -0.102                 -0.510
#9 Model_1_age_sex                       6 females_f…  0.115   0.138 0.619 5031. 6001.  94.3      98.7 0.0848                 0                      0    
## ℹ 2 more variables: delta_MAPE_vs_baseline <dbl>, delta_MaxAbsPct_vs_baseline <dbl>

# -------------------------------------------------------------------
# Save outputs
# -------------------------------------------------------------------

write_xlsx(
  list(
    metrics = metrics_table
  ),
  path = "tables/XGBoost_France_baseline_plus_one_predictor_metrics.xlsx"
)

comparison_export <- lapply(results_all, function(x) x$comparison)

write_xlsx(
  comparison_export,
  path = "data/XGBoost_France_baseline_plus_one_predictor_comparisons_rate_based.xlsx"
)

# -------------------------------------------------------------------
# Plot ranking by MAE
# -------------------------------------------------------------------

metrics_plot <- metrics_table |>
  mutate(model = factor(model, levels = model[order(MAE)]))

p <- ggplot(metrics_plot, aes(x = model, y = MAE)) +
  geom_col(fill = "grey40") +
  coord_flip() +
  theme_classic(base_size = 11) +
  labs(
    x = NULL,
    y = "MAE",
    title = "France: XGBoost baseline age+sex plus one predictor at a time"
  )

ggsave(
  filename = "figures/XGBoost_France_baseline_plus_one_predictor_MAE_rate_based.png",
  plot = p,
  width = 10,
  height = 7,
  dpi = 300
)

# Final model selection and saving
res_final <- results_all[["Model_2_age_sex_plus_PopDensity"]]
final_model <- res_final$model_fit
saveRDS(final_model, file = "models/xgboost_final_model_rate_based.rds")