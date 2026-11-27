rm(list = ls()); gc()

library(readxl)
library(dplyr)
library(tidyr)
library(writexl)

infile <- "F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/data/mapping.xlsx"
outdir <- "F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/results"
if (!dir.exists(outdir)) dir.create(outdir, recursive = TRUE)

AREA <- read_excel(infile, sheet = "AREA")
AREA <- AREA |>
  filter(!if_all(everything(), is.na))

AREA <- AREA |>
  mutate(
    `Total_area(m2)_Eurostat_2021` = as.numeric(gsub(",", ".", as.character(`Total_area(m2)_Eurostat_2021`))),
    Density_type_value = as.numeric(gsub(",", ".", as.character(Density_type_value))),
    Density_type_class = as.character(Density_type_class)
    )

# LAU2
AREA_LAU2 <- AREA

# NUTS3
AREA_NUTS3 <- AREA |>
  group_by(
    NUTS1_code_Eurostat_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021,
    NUTS3_name_Eurostat_2021
    ) |>
  summarise(`Total_area(m2)_Eurostat_2021` = sum(`Total_area(m2)_Eurostat_2021`, na.rm = TRUE), .groups = "drop")

NUTS3_density_counts <- AREA |>
  count(
    NUTS1_code_Eurostat_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021,
    NUTS3_name_Eurostat_2021,
    Density_type_class,
    name = "n_LAU2_class"
    ) |>
  tidyr::pivot_wider(
    names_from = Density_type_class,
    values_from = n_LAU2_class,
    values_fill = list(n_LAU2_class = 0),
    names_prefix = "n_LAU2_"
    )

AREA_NUTS3 <- AREA_NUTS3 |>
  left_join(
    NUTS3_density_counts,
    by = c(
      "NUTS1_code_Eurostat_2021",
      "NUTS1_name_Eurostat_2021",
      "NUTS2_code_Eurostat_2021",
      "NUTS2_name_Eurostat_2021",
      "NUTS3_code_Eurostat_2021",
      "NUTS3_name_Eurostat_2021"
      )
    )

# NUTS2
AREA_NUTS2 <- AREA |>
  group_by(
    NUTS1_code_Eurostat_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021
    ) |>
  summarise(`Total_area(m2)_Eurostat_2021` = sum(`Total_area(m2)_Eurostat_2021`, na.rm = TRUE), .groups = "drop")

NUTS2_density_counts <- AREA |>
  count(
    NUTS1_code_Eurostat_2021,
    NUTS1_name_Eurostat_2021,
    NUTS2_code_Eurostat_2021,
    NUTS2_name_Eurostat_2021,
    Density_type_class,
    name = "n_LAU2_class"
    ) |>
  tidyr::pivot_wider(
    names_from = Density_type_class,
    values_from = n_LAU2_class,
    values_fill = list(n_LAU2_class = 0),
    names_prefix = "n_LAU2_"
  )

AREA_NUTS2 <- AREA_NUTS2 |>
  left_join(
    NUTS2_density_counts,
    by = c(
      "NUTS1_code_Eurostat_2021",
      "NUTS1_name_Eurostat_2021",
      "NUTS2_code_Eurostat_2021",
      "NUTS2_name_Eurostat_2021"
      )
    )

# NUTS1
AREA_NUTS1 <- AREA |>
  group_by(
    NUTS1_code_Eurostat_2021,
    NUTS1_name_Eurostat_2021
    ) |>
  summarise(`Total_area(m2)_Eurostat_2021` = sum(`Total_area(m2)_Eurostat_2021`, na.rm = TRUE), .groups = "drop")

NUTS1_density_counts <- AREA |>
  count(
    NUTS1_code_Eurostat_2021,
    NUTS1_name_Eurostat_2021,
    Density_type_class,
    name = "n_LAU2_class"
    )  |>
  tidyr::pivot_wider(
    names_from = Density_type_class,
    values_from = n_LAU2_class,
    values_fill = list(n_LAU2_class = 0),
    names_prefix = "n_LAU2_"
    )

AREA_NUTS1 <- AREA_NUTS1 |>
  left_join(
    NUTS1_density_counts,
    by = c(
      "NUTS1_code_Eurostat_2021",
      "NUTS1_name_Eurostat_2021"
      )
    )

write_xlsx(AREA_LAU2, file.path(outdir, "AREA_LAU2.xlsx"))
write_xlsx(AREA_NUTS3, file.path(outdir, "AREA_NUTS3.xlsx"))
write_xlsx(AREA_NUTS2, file.path(outdir, "AREA_NUTS2.xlsx"))
write_xlsx(AREA_NUTS1, file.path(outdir, "AREA_NUTS1.xlsx"))
