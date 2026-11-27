rm(list = ls()); gc()

library(readxl)
library(dplyr)
library(writexl)

infile <- "F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/data/mapping.xlsx"
outdir <- "F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/results"

if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

POP <- read_excel(infile, sheet = "POP") |>
  filter(!if_all(everything(), is.na)) |>
  transmute(
    LAU2_code_Eurostat_2021,
    TOTAL_POP = as.numeric(gsub(",", ".", as.character(TOTAL_POP)))
  )

POP <- read_excel(infile, sheet = "POP") |>
  filter(!if_all(everything(), is.na)) |>
  transmute(
    LAU2_code_Eurostat_2021,
    TOTAL_POP = as.numeric(gsub(",", ".", as.character(TOTAL_POP)))
  )

CULTURE_EXP <- read_excel(infile, sheet = "CULTURE_EXP") |>
  filter(!if_all(everything(), is.na)) |>
  transmute(
    NUTS1_code_Eurostat_2021,
    NUTS1_DEP_code_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_DEP_code_2021,
    NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021,
    NUTS3_DEP_code_2021,
    NUTS3_name_Eurostat_2021,
    LAU2_code_Eurostat_2021,
    `EU LAU CODE`,
    LAU2_name_Eurostat_2021,
    Avg_total_exp_culture_services_euro_per_inhabitants_2023_NUTS3 =
      as.numeric(gsub(",", ".", as.character(Avg_total_exp_culture_services_euro_per_inhabitants_2023_NUTS3)))
  ) |>
  left_join(POP, by = "LAU2_code_Eurostat_2021")

# LAU2
CULTURE_EXP_LAU2 <- CULTURE_EXP |>
  select(
    NUTS1_code_Eurostat_2021,
    NUTS1_DEP_code_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_DEP_code_2021,
    NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021,
    NUTS3_DEP_code_2021,
    NUTS3_name_Eurostat_2021,
    LAU2_code_Eurostat_2021,
    `EU LAU CODE`,
    LAU2_name_Eurostat_2021,
    Avg_total_exp_culture_services_euro_per_inhabitants_2023_NUTS3
  )

# NUTS3
CULTURE_EXP_NUTS3 <- CULTURE_EXP |>
  distinct(
    NUTS1_code_Eurostat_2021,
    NUTS1_DEP_code_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_DEP_code_2021,
    NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021,
    NUTS3_DEP_code_2021,
    NUTS3_name_Eurostat_2021,
    Avg_total_exp_culture_services_euro_per_inhabitants_2023_NUTS3
  ) |>
  mutate(
    LAU2_code_Eurostat_2021 = NA_character_,
    `EU LAU CODE` = NA_character_,
    LAU2_name_Eurostat_2021 = NA_character_
  ) |>
  select(
    NUTS1_code_Eurostat_2021,
    NUTS1_DEP_code_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_DEP_code_2021,
    NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021,
    NUTS3_DEP_code_2021,
    NUTS3_name_Eurostat_2021,
    LAU2_code_Eurostat_2021,
    `EU LAU CODE`,
    LAU2_name_Eurostat_2021,
    Avg_total_exp_culture_services_euro_per_inhabitants_2023_NUTS3
  )

# NUTS2
CULTURE_EXP_NUTS2 <- CULTURE_EXP |>
  group_by(
    NUTS1_code_Eurostat_2021,
    NUTS1_DEP_code_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_DEP_code_2021,
    NUTS2_name_Eurostat_2021
  ) |>
  summarise(
    Avg_total_exp_culture_services_euro_per_inhabitants_2023_NUTS3 =
      sum(Avg_total_exp_culture_services_euro_per_inhabitants_2023_NUTS3 * TOTAL_POP, na.rm = TRUE) /
      sum(TOTAL_POP, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    NUTS3_code_Eurostat_2021 = NA_character_,
    NUTS3_DEP_code_2021 = NA_character_,
    NUTS3_name_Eurostat_2021 = NA_character_,
    LAU2_code_Eurostat_2021 = NA_character_,
    `EU LAU CODE` = NA_character_,
    LAU2_name_Eurostat_2021 = NA_character_
  ) |>
  select(
    NUTS1_code_Eurostat_2021,
    NUTS1_DEP_code_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_DEP_code_2021,
    NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021,
    NUTS3_DEP_code_2021,
    NUTS3_name_Eurostat_2021,
    LAU2_code_Eurostat_2021,
    `EU LAU CODE`,
    LAU2_name_Eurostat_2021,
    Avg_total_exp_culture_services_euro_per_inhabitants_2023_NUTS3
  )

# NUTS1
CULTURE_EXP_NUTS1 <- CULTURE_EXP |>
  group_by(
    NUTS1_code_Eurostat_2021,
    NUTS1_DEP_code_2021,
    NUTS1_name_Eurostat_2021
  ) |>
  summarise(
    Avg_total_exp_culture_services_euro_per_inhabitants_2023_NUTS3 =
      sum(Avg_total_exp_culture_services_euro_per_inhabitants_2023_NUTS3 * TOTAL_POP, na.rm = TRUE) /
      sum(TOTAL_POP, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    NUTS2_code_Eurostat_2021 = NA_character_,
    NUTS2_DEP_code_2021 = NA_character_,
    NUTS2_name_Eurostat_2021 = NA_character_,
    NUTS3_code_Eurostat_2021 = NA_character_,
    NUTS3_DEP_code_2021 = NA_character_,
    NUTS3_name_Eurostat_2021 = NA_character_,
    LAU2_code_Eurostat_2021 = NA_character_,
    `EU LAU CODE` = NA_character_,
    LAU2_name_Eurostat_2021 = NA_character_
  ) |>
  select(
    NUTS1_code_Eurostat_2021,
    NUTS1_DEP_code_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_DEP_code_2021,
    NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021,
    NUTS3_DEP_code_2021,
    NUTS3_name_Eurostat_2021,
    LAU2_code_Eurostat_2021,
    `EU LAU CODE`,
    LAU2_name_Eurostat_2021,
    Avg_total_exp_culture_services_euro_per_inhabitants_2023_NUTS3
  )

write_xlsx(CULTURE_EXP_LAU2,  file.path(outdir, "CULTURE_EXP_LAU2.xlsx"))
write_xlsx(CULTURE_EXP_NUTS3, file.path(outdir, "CULTURE_EXP_NUTS3.xlsx"))
write_xlsx(CULTURE_EXP_NUTS2, file.path(outdir, "CULTURE_EXP_NUTS2.xlsx"))
write_xlsx(CULTURE_EXP_NUTS1, file.path(outdir, "CULTURE_EXP_NUTS1.xlsx"))