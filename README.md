# Downscaling-France-NUTS-3-to-LAU2
Municipal YLLs downscaled from NUTS3 (département) level using age and sex structure and population. No socioeconomic indicators are used.

Downscaling of YLL Estimates from NUTS-3 to LAU-2 for France 

Summary 

For France, YLL rates at the municipal level (LAU-2) were unavailable from routine statistics. We therefore downscaled NUTS-3 departmental YLL estimates to LAU-2 municipalities using a two-stage supervised prediction and constrained redistribution procedure. The method preserved observed NUTS-3 totals exactly, ensuring consistency with the original mortality data while generating plausible LAU-2 distributions. 

Input data and age-sex stratification 

Observed mortality data for France were available at NUTS-3 level (96 departments) stratified by sex (male, female, both) and eight age groups: 0–24, 25–34, 35–44, 45–54, 55–64, 65–74, 75–84, and 85+ years. The 85+ category collapsed combined the original 85–94 and 95+ categories. Observed NUTS-3 YLL counts were calculated as deaths × RLE (see Section S1 above). LAU-2 population counts were available from French national statistics (INSEE) for the same sex × age strata. 

Prediction model 

The downscaling model used XGBoost (eXtreme Gradient Boosting), a tree-based machine learning algorithm that captures non-linear relationships and interactions among predictors. The outcome variable was the log-transformed NUTS-3 YLL rate (per 100,000). Predictors included: 

Demographic: Age group (categorical, 8 levels), sex (categorical, 2 levels), and their interaction 

Population: Log-transformed population size (to capture denominator effects) 

Spatial: Centroid coordinates (x and y) of NUTS-3 departments (for training) and LAU-2 municipalities (for prediction), projected in EPSG:3035 (https://epsg.io/3035) 

Area-level covariates: Four municipal-level indicators (GP, CULTURE_EXP, SOCIAL_AID, and AREA) available at both NUTS-3 and LAU-2 levels where GP stand for General Practitioners, CULTURE_EXP for Cultural Expenditure. 

Missing predictor values at LAU-2 level were imputed using medians calculated from the NUTS-3 training data. 
