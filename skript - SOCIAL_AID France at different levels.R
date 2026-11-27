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

SOCIAL_AID <- read_excel(infile, sheet = "SOCIAL_AID") |>
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
    Exp_social_aid_before_tax_euro_per_inhabitant_2021 =
      as.numeric(gsub(",", ".", as.character(Exp_social_aid_before_tax_euro_per_inhabitant_2021)))
  ) |>
  left_join(POP, by = "LAU2_code_Eurostat_2021")

# LAU2
SOCIAL_AID_LAU2 <- SOCIAL_AID |>
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
    Exp_social_aid_before_tax_euro_per_inhabitant_2021
  )

# NUTS3: én verdi per NUTS3, så distinct er OK
SOCIAL_AID_NUTS3 <- SOCIAL_AID |>
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
    Exp_social_aid_before_tax_euro_per_inhabitant_2021
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
    Exp_social_aid_before_tax_euro_per_inhabitant_2021
  )

# NUTS2: må vektes med befolkning
SOCIAL_AID_NUTS2 <- SOCIAL_AID |>
  group_by(
    NUTS1_code_Eurostat_2021,
    NUTS1_DEP_code_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_DEP_code_2021,
    NUTS2_name_Eurostat_2021
  ) |>
  summarise(
    Exp_social_aid_before_tax_euro_per_inhabitant_2021 =
      sum(Exp_social_aid_before_tax_euro_per_inhabitant_2021 * TOTAL_POP, na.rm = TRUE) /
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
    Exp_social_aid_before_tax_euro_per_inhabitant_2021
  )

# NUTS1: må vektes med befolkning
SOCIAL_AID_NUTS1 <- SOCIAL_AID |>
  group_by(
    NUTS1_code_Eurostat_2021,
    NUTS1_DEP_code_2021,
    NUTS1_name_Eurostat_2021
  ) |>
  summarise(
    Exp_social_aid_before_tax_euro_per_inhabitant_2021 =
      sum(Exp_social_aid_before_tax_euro_per_inhabitant_2021 * TOTAL_POP, na.rm = TRUE) /
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
    Exp_social_aid_before_tax_euro_per_inhabitant_2021
  )

write_xlsx(SOCIAL_AID_LAU2,  file.path(outdir, "SOCIAL_AID_LAU2.xlsx"))
write_xlsx(SOCIAL_AID_NUTS3, file.path(outdir, "SOCIAL_AID_NUTS3.xlsx"))
write_xlsx(SOCIAL_AID_NUTS2, file.path(outdir, "SOCIAL_AID_NUTS2.xlsx"))
write_xlsx(SOCIAL_AID_NUTS1, file.path(outdir, "SOCIAL_AID_NUTS1.xlsx"))