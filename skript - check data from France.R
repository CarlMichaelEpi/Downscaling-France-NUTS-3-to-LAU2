rm(list = ls()); gc()

library(readxl)

setwd("F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/")

# ------------------------------------------------------------------------------------------------------------------------------------------------
# Check the files
excel_sheets("data/FR_LAU2_level_indicators (1).xlsx")
#[1] "Variables 2021"                  "Heating_FR_LAU2"                 "Unemployement_FR_LAU2"           "SingleParent_Households_FR_LAU2"
#[5] "Pop_change_FR_LAU2"              "Education_FR_LAU2" 

excel_sheets("data/FR_Expenditure_social_aid_2021_NUTS3.xlsx")
#[1] "Variables"                 "Expenditure on social aid" "Documentation"            

excel_sheets("data/FR_Density_grid_LAU2.xlsx")
#[1] "Variables and definitions " "Density grid"               "Documentation"             

excel_sheets("data/FR_Additional_LAU2_variables.xlsx")
#[1] "Variables"              "Proportions Pop data"   "Population density"     "Nb of GP consultations"

excel_sheets("data/FR_Additional_variables_culture_0326.xlsx")
#[1] "Variables"                    "Table mapping NUTS3 - LAU2"   "Av_expenditure_culture_LAU2"  "Av_expenditure_culture_NUTS3" "Pop_density_NUTS3_2018"      

# ------------------------------------------------------------------------------------------------------------------------------------------------


# ------------------------------------------------------------------------------------------------------------------------------------------------
# Read the variables

# MDI: 
#lau2_heating <- read_excel("data/FR_LAU2_level_indicators (1).xlsx", sheet = "Heating_FR_LAU2", skip = 4)
#lau2_heating <- lau2_heating |>
#  dplyr::select(-starts_with("..."))

#lau2_unemp <- read_excel("data/FR_LAU2_level_indicators (1).xlsx", sheet = "Unemployement_FR_LAU2", skip = 4)
#lau2_unemp <- lau2_unemp |>
#  dplyr::select(-starts_with("..."))

#lau2_singleparent <- read_excel("data/FR_LAU2_level_indicators (1).xlsx", sheet = "SingleParent_Households_FR_LAU2", skip = 4)
#lau2_singleparent <- lau2_singleparent |>
#  dplyr::select(-starts_with("..."))

#lau2_popchange <- read_excel("data/FR_LAU2_level_indicators (1).xlsx", sheet = "Pop_change_FR_LAU2", skip = 4)
#lau2_popchange <- lau2_popchange |>
#  dplyr::select(-starts_with("..."))

#lau2_education <- read_excel("data/FR_LAU2_level_indicators (1).xlsx", sheet = "Education_FR_LAU2", skip = 4)
#lau2_education <- lau2_education |>
#  dplyr::select(-starts_with("..."))


# Predictors for the downscaling:
lau2_culture_exp <- read_excel("data/FR_Additional_variables_culture_0326.xlsx", sheet = "Av_expenditure_culture_LAU2")

NUTS3_culture_exp <- read_excel("data/FR_Additional_variables_culture_0326.xlsx", sheet = "Av_expenditure_culture_NUTS3")

NUTS3_density <- read_excel("data/FR_Additional_variables_culture_0326.xlsx", sheet = "Pop_density_NUTS3_2018")
NUTS3_density <- NUTS3_density |>
  dplyr::select(-starts_with("..."))

lau2_density <- read_excel("data/FR_Density_grid_LAU2.xlsx", sheet = "Density grid", skip = 4)
lau2_density <- lau2_density |>
  dplyr::select(-starts_with("..."))

lau2_prop <- read_excel("data/FR_Additional_LAU2_variables.xlsx", sheet = "Proportions Pop data", skip = 4)
lau2_prop <- lau2_prop |>
  dplyr::select(-starts_with("..."))

lau2_popdens <- read_excel("data/FR_Additional_LAU2_variables.xlsx", sheet = "Population density", skip = 3)
lau2_popdens <- lau2_popdens |>
  dplyr::select(-starts_with("..."))

lau2_gp <- read_excel("data/FR_Additional_LAU2_variables.xlsx", sheet = "Nb of GP consultations", skip = 1)
lau2_gp <- lau2_gp |>
  dplyr::select(
    -starts_with("..."),
    -starts_with("Interpretation")
  )

# ------------------------------------------------------------------------------------------------------------------------------------------------