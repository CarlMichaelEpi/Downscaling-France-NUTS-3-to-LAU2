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

GP <- read_excel(infile, sheet = "GP_consulatations") |>
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
    Avg_GP_consultations_number_2019 =
      as.numeric(gsub(",", ".", as.character(Avg_GP_consultations_number_2019))),
    Est_GP_consultations_number_2019 =
      as.numeric(gsub(",", ".", as.character(Est_GP_consultations_number_2019)))
  ) |>
  left_join(POP, by = "LAU2_code_Eurostat_2021")

# LAU2
GP_LAU2 <- GP |>
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
    Avg_GP_consultations_number_2019,
    Est_GP_consultations_number_2019
  )

# NUTS3
GP_NUTS3 <- GP |>
  group_by(
    NUTS1_code_Eurostat_2021,
    NUTS1_DEP_code_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_DEP_code_2021,
    NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021,
    NUTS3_DEP_code_2021,
    NUTS3_name_Eurostat_2021
  ) |>
  summarise(
    Avg_GP_consultations_number_2019 =
      sum(Avg_GP_consultations_number_2019 * TOTAL_POP, na.rm = TRUE) /
      sum(TOTAL_POP, na.rm = TRUE),
    Est_GP_consultations_number_2019 =
      sum(Est_GP_consultations_number_2019, na.rm = TRUE),
    .groups = "drop"
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
    Avg_GP_consultations_number_2019,
    Est_GP_consultations_number_2019
  )

# NUTS2
GP_NUTS2 <- GP |>
  group_by(
    NUTS1_code_Eurostat_2021,
    NUTS1_DEP_code_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_DEP_code_2021,
    NUTS2_name_Eurostat_2021
  ) |>
  summarise(
    Avg_GP_consultations_number_2019 =
      sum(Avg_GP_consultations_number_2019 * TOTAL_POP, na.rm = TRUE) /
      sum(TOTAL_POP, na.rm = TRUE),
    Est_GP_consultations_number_2019 =
      sum(Est_GP_consultations_number_2019, na.rm = TRUE),
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
    Avg_GP_consultations_number_2019,
    Est_GP_consultations_number_2019
  )

# NUTS1
GP_NUTS1 <- GP |>
  group_by(
    NUTS1_code_Eurostat_2021,
    NUTS1_DEP_code_2021,
    NUTS1_name_Eurostat_2021
  ) |>
  summarise(
    Avg_GP_consultations_number_2019 =
      sum(Avg_GP_consultations_number_2019 * TOTAL_POP, na.rm = TRUE) /
      sum(TOTAL_POP, na.rm = TRUE),
    Est_GP_consultations_number_2019 =
      sum(Est_GP_consultations_number_2019, na.rm = TRUE),
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
    Avg_GP_consultations_number_2019,
    Est_GP_consultations_number_2019
  )

write_xlsx(GP_LAU2,  file.path(outdir, "GP_LAU2.xlsx"))
write_xlsx(GP_NUTS3, file.path(outdir, "GP_NUTS3.xlsx"))
write_xlsx(GP_NUTS2, file.path(outdir, "GP_NUTS2.xlsx"))
write_xlsx(GP_NUTS1, file.path(outdir, "GP_NUTS1.xlsx"))