
rm(list = ls()); gc()

suppressPackageStartupMessages({
  library(INLA)
  library(sf)
  library(spdep)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(scales)
  library(stringr)
  library(readr)
  library(purrr)
  library(tibble)
})

setwd("F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Spatio-temporal Estonia/")

# ------------------------------------------------------------------------------
# Shape + graph (IMPORTANT: ID order must match graph)
# ------------------------------------------------------------------------------
shape <- st_read("data/omavalitsus_shp/omavalitsus.shp", quiet = TRUE) |>
  st_transform(4326) |>
  rename(municipalityID = OKOOD) |>
  mutate(
    municipalityID = dplyr::recode(
      as.character(municipalityID),
      .default = as.character(municipalityID)
    )
  ) |>
  arrange(municipalityID) |>
  mutate(ID = row_number())

graph_path <- "data/municipality.graph"
nb <- poly2nb(shape)
nb2INLA(file = graph_path, nb = nb)

g <- inla.read.graph(graph_path)
stopifnot(nrow(shape) == g$n)

# ------------------------------------------------------------------------------
# Read data
# ------------------------------------------------------------------------------

data_long <- read_delim(
  "data/data_long.csv",
  delim = ";",
  locale = locale(decimal_mark = ","),
  show_col_types = FALSE
) |>
  rename(
    year             = Year,
    YLL              = YLL,
    population       = `Mean Annual Population`,
    countyID         = `Location ID county`,
    municipalityID   = `Location ID municipality`,
    countyNAME       = County,
    municipalityNAME = Municipality,
    age              = `Age group`,
    sex              = Sex
  ) |>
  mutate(
    year = as.integer(year),
    YLL  = as.numeric(YLL),
    population = suppressWarnings(as.numeric(population)),
    municipalityID = dplyr::recode(
      as.character(municipalityID),
      "0250" = "0251",
      "0638" = "0637",
      "0809" = "0806",
      .default = as.character(municipalityID)
    ),
    sex = tolower(as.character(sex)),
    age = as.character(age)
  ) |>
  filter(!is.na(year),
         municipalityID != "-",
         !is.na(population), population > 0,
         is.finite(YLL))

sex_vals <- sort(unique(data_long$sex))
message("Sex values in file: ", paste(sex_vals, collapse = ", "))

# Use total if present
if ("total" %in% sex_vals) {
  data_long <- data_long |> filter(sex == "total")
} else {
  data_long <- data_long |> filter(sex %in% c("male", "female"))
}

Estonia_MuniPop <- data_long |>
  filter(year == 2023) |>
  select(
    year, countyID, countyNAME,
    municipalityID, municipalityNAME,
    Vald, sex, population
  ) |>
  group_by(municipalityID) |>
  summarise(
    year             = first(year),
    countyNAME       = first(countyNAME),
    municipalityID   = first(municipalityID),
    municipalityNAME = first(municipalityNAME),
    Vald             = first(Vald),
    sex              = first(sex),
    population       = sum(population, na.rm = TRUE),
    .groups = "drop"
  )


setwd("F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/data/Estonia/Fw_ correct file/")

equal <- read_csv("data_equal_allcountries.csv")

Estonia_MuniPop2 <- equal |>
  filter(
    country.x == "Estonia",
    local_level.x == "Municipality"
  ) |>
  mutate(
    municipalityID = stringr::str_pad(as.character(local_id), width = 4, pad = "0"),
    municipalityID = dplyr::recode(
      municipalityID,
      "0250" = "0251",
      "0638" = "0637",
      "0809" = "0806",
      .default = municipalityID
    )
  )

Estonia_MuniPop2 <- Estonia_MuniPop2 |>
  transmute(
    year             = year_m,
    countyID         = NA_character_,
    countyNAME       = NA_character_,
    municipalityID   = as.character(municipalityID),
    municipalityNAME = as.character(local.y),
    Vald             = as.character(local.y),
    sex              = NA_character_,
    population       = pop
  )



common_pop <- Estonia_MuniPop |>
  select(year, municipalityID, population) |>
  rename(population_1 = population) |>
  inner_join(
    Estonia_MuniPop2 |>
      select(year, municipalityID, population) |>
      rename(population_2 = population),
    by = c("year", "municipalityID")
  )

nrow(common_pop)
#[1] 79
summary(common_pop$population_1)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#90    5186    7783   17350   13452  455722
summary(common_pop$population_2)
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#89    5188    7731   17288   13448  453864 

cor(common_pop$population_1, common_pop$population_2, use = "complete.obs", method = "pearson")
#[1] 0.9999976
cor(common_pop$population_1, common_pop$population_2, use = "complete.obs", method = "spearman")
#[1] 0.9996288






library(ggplot2)
library(scales)
library(patchwork)

map_theme <- theme_minimal() +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )

p1 <- ggplot(map_2023) +
  geom_sf(aes(fill = population), colour = "grey60", linewidth = 0.1) +
  scale_fill_viridis_c(
    trans = "log10",
    breaks = c(100, 1000, 10000, 100000),
    labels = label_number(big.mark = " "),
    na.value = "grey90"
  ) +
  labs(title = "a) Population 2023", fill = NULL) +
  map_theme

p2 <- ggplot(map_2023) +
  geom_sf(aes(fill = mortality_rate), colour = "grey60", linewidth = 0.1) +
  scale_fill_viridis_c(
    na.value = "grey90",
    labels = label_number(big.mark = " ")
  ) +
  labs(title = "b) Mortality rate per 100 000", fill = NULL) +
  map_theme

p3 <- ggplot(map_2023) +
  geom_sf(aes(fill = YLL_count), colour = "grey60", linewidth = 0.1) +
  scale_fill_viridis_b(
    breaks = c(0, 50, 100, 250, 500, 1000, 2500, 8000),
    labels = label_number(big.mark = " "),
    na.value = "grey90"
  ) +
  labs(title = "c) YLL count", fill = NULL) +
  map_theme

p4 <- ggplot(map_2023) +
  geom_sf(aes(fill = YLL_rate), colour = "grey60", linewidth = 0.1) +
  scale_fill_viridis_c(
    na.value = "grey90",
    labels = label_number(big.mark = " ")
  ) +
  labs(title = "d) YLL rate per 100 000", fill = NULL) +
  map_theme

(p1 | p2) / (p3 | p4)




