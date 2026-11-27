rm(list = ls()); gc()

library(readxl)
library(dplyr)
library(writexl)

infile <- "F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/data/mapping.xlsx"
outdir <- "F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/results"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

POP <- read_excel(infile, sheet = "POP")
POP <- POP |>
  filter(!if_all(everything(), is.na))

pop_vars <- c(
  "TOTAL_POP", "MALE_TOTAL", "MALE_AGED0019", "MALE_AGED2064", "MALE_AGED6500",
  "FEMALE_TOTAL", "FEMALE_AGED0019", "FEMALE_AGED2064", "FEMALE_AGED6500"
    )

POP <- POP |>
  mutate(across(all_of(pop_vars), ~ as.numeric(gsub(",", ".", as.character(.)))))

add_perc_cols <- function(df) {
  df |>
    mutate(
      MALE_AGED0019_perc   = if_else(TOTAL_POP > 0, 100 * MALE_AGED0019   / TOTAL_POP, NA_real_),
      MALE_AGED2064_perc   = if_else(TOTAL_POP > 0, 100 * MALE_AGED2064   / TOTAL_POP, NA_real_),
      MALE_AGED6500_perc   = if_else(TOTAL_POP > 0, 100 * MALE_AGED6500   / TOTAL_POP, NA_real_),
      FEMALE_AGED0019_perc = if_else(TOTAL_POP > 0, 100 * FEMALE_AGED0019 / TOTAL_POP, NA_real_),
      FEMALE_AGED2064_perc = if_else(TOTAL_POP > 0, 100 * FEMALE_AGED2064 / TOTAL_POP, NA_real_),
      FEMALE_AGED6500_perc = if_else(TOTAL_POP > 0, 100 * FEMALE_AGED6500 / TOTAL_POP, NA_real_)
    )
  }

# LAU2
POP_LAU2 <- POP |>
  add_perc_cols()

# NUTS3
POP_NUTS3 <- POP |>
  group_by(
    NUTS1_code_Eurostat_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021,
    NUTS3_name_Eurostat_2021
  ) |>
  summarise(across(all_of(pop_vars), sum, na.rm = TRUE), .groups = "drop") |>
  add_perc_cols()

# NUTS2
POP_NUTS2 <- POP |>
  group_by(
    NUTS1_code_Eurostat_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021
  ) |>
  summarise(across(all_of(pop_vars), sum, na.rm = TRUE), .groups = "drop") |>
  add_perc_cols()

# NUTS1
POP_NUTS1 <- POP |>
  group_by(
    NUTS1_code_Eurostat_2021,
    NUTS1_name_Eurostat_2021
  ) |>
  summarise(across(all_of(pop_vars), sum, na.rm = TRUE), .groups = "drop") |>
  add_perc_cols()

write_xlsx(POP_LAU2, file.path(outdir, "POP_LAU2.xlsx"))
write_xlsx(POP_NUTS3, file.path(outdir, "POP_NUTS3.xlsx"))
write_xlsx(POP_NUTS2, file.path(outdir, "POP_NUTS2.xlsx"))
write_xlsx(POP_NUTS1, file.path(outdir, "POP_NUTS1.xlsx"))
