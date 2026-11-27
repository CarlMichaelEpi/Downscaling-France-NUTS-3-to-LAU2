rm(list = ls()); gc()

library(readxl)
library(dplyr)
library(writexl)

infile <- "F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/data/mapping.xlsx"
outdir <- "F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/results"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

# =========================================================
# YLL
# =========================================================

YLL <- read_excel(infile, sheet = "YLL") |>
  filter(!if_all(everything(), is.na)) |>
  filter(SEX == "both") |>
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
    YLL = as.numeric(gsub(",", ".", as.character(YLL)))
  )

# NUTS3
YLL_NUTS3 <- YLL |>
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
    YLL = sum(YLL, na.rm = TRUE),
    .groups = "drop"
  )

# NUTS2
YLL_NUTS2 <- YLL |>
  group_by(
    NUTS1_code_Eurostat_2021,
    NUTS1_DEP_code_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_DEP_code_2021,
    NUTS2_name_Eurostat_2021
  ) |>
  summarise(
    YLL = sum(YLL, na.rm = TRUE),
    .groups = "drop"
  )

# NUTS1
YLL_NUTS1 <- YLL |>
  group_by(
    NUTS1_code_Eurostat_2021,
    NUTS1_DEP_code_2021,
    NUTS1_name_Eurostat_2021
  ) |>
  summarise(
    YLL = sum(YLL, na.rm = TRUE),
    .groups = "drop"
  )

write_xlsx(YLL_NUTS3, file.path(outdir, "YLL_NUTS3.xlsx"))
write_xlsx(YLL_NUTS2, file.path(outdir, "YLL_NUTS2.xlsx"))
write_xlsx(YLL_NUTS1, file.path(outdir, "YLL_NUTS1.xlsx"))
