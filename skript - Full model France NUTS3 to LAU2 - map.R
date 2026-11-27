rm(list = ls()); gc()

library(sf)
library(giscoR)
library(dplyr)
library(ggplot2)
library(readxl)
library(scales)

setwd("F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/")

# -------------------------------------------------------------------
# Helper
# -------------------------------------------------------------------

to_num <- function(x) {
  suppressWarnings(as.numeric(gsub(",", ".", x)))
}

# -------------------------------------------------------------------
# Read map data
# -------------------------------------------------------------------

nuts_all <- gisco_get_nuts(year = "2021", resolution = "20", epsg = "4326")

fr_nuts3 <- nuts_all |>
  filter(CNTR_CODE == "FR", LEVL_CODE == 3)

lau_all <- gisco_get_lau(year = "2020", epsg = "4326")

fr_lau <- lau_all |>
  filter(CNTR_CODE == "FR")

bbox_fr <- st_bbox(
  c(xmin = -6, xmax = 10, ymin = 41, ymax = 52),
  crs = st_crs(fr_lau)
)

fr_lau <- st_crop(fr_lau, bbox_fr)

# -------------------------------------------------------------------
# Read observed NUTS3 YLL
# -------------------------------------------------------------------

df_nuts3_yll <- read_excel("results/YLL_NUTS3.xlsx") |>
  mutate(
    YLL = to_num(YLL)
  ) |>
  select(
    NUTS3_code_Eurostat_2021,
    NUTS3_name_Eurostat_2021,
    YLL
  ) |>
  rename(
    NUTS_ID = NUTS3_code_Eurostat_2021,
    NUTS3_name = NUTS3_name_Eurostat_2021,
    YLL_NUTS3 = YLL
  )

# -------------------------------------------------------------------
# Read NUTS3 population and calculate observed NUTS3 rate per 100,000
# -------------------------------------------------------------------

df_nuts3_pop <- read_excel("results/POP_NUTS3.xlsx") |>
  mutate(
    TOTAL_POP = to_num(TOTAL_POP)
  ) |>
  select(
    NUTS3_code_Eurostat_2021,
    TOTAL_POP
  ) |>
  rename(
    NUTS_ID = NUTS3_code_Eurostat_2021,
    TOTAL_POP_NUTS3 = TOTAL_POP
  )

df_nuts3 <- df_nuts3_yll |>
  left_join(df_nuts3_pop, by = "NUTS_ID") |>
  mutate(
    YLL_rate_NUTS3 = YLL_NUTS3 / TOTAL_POP_NUTS3 * 100000
  )

# -------------------------------------------------------------------
# Read predicted LAU2 YLL
# -------------------------------------------------------------------

df_lau2 <- read_excel(
  "results/YLL_predictions_LAU2_share_based.xlsx",
  sheet = "predictions_LAU2"
) |>
  mutate(
    PredYLL_LAU2 = to_num(PredYLL_LAU2)
  ) |>
  select(
    LAU2_code_Eurostat_2021,
    LAU2_name_Eurostat_2021,
    NUTS3_code_Eurostat_2021,
    NUTS3_name_Eurostat_2021,
    PredYLL_LAU2
  ) |>
  rename(
    LAU_ID = LAU2_code_Eurostat_2021,
    LAU2_name = LAU2_name_Eurostat_2021,
    NUTS3_ID = NUTS3_code_Eurostat_2021,
    NUTS3_name = NUTS3_name_Eurostat_2021
  )

# -------------------------------------------------------------------
# Read LAU2 population and calculate predicted LAU2 rate per 100,000
# -------------------------------------------------------------------

df_lau2_pop <- read_excel("results/POP_LAU2.xlsx") |>
  mutate(
    TOTAL_POP = to_num(TOTAL_POP)
  ) |>
  select(
    LAU2_code_Eurostat_2021,
    TOTAL_POP
  ) |>
  rename(
    LAU_ID = LAU2_code_Eurostat_2021,
    TOTAL_POP_LAU2 = TOTAL_POP
  )

df_lau2 <- df_lau2 |>
  left_join(df_lau2_pop, by = "LAU_ID") |>
  mutate(
    PredYLL_rate_LAU2 = PredYLL_LAU2 / TOTAL_POP_LAU2 * 100000
  )

# -------------------------------------------------------------------
# Optional check: LAU2 sums should match NUTS3 totals
# -------------------------------------------------------------------

check <- df_lau2 |>
  group_by(NUTS3_ID) |>
  summarise(
    sum_pred_lau2 = sum(PredYLL_LAU2, na.rm = TRUE),
    .groups = "drop"
  ) |>
  left_join(
    df_nuts3 |>
      select(NUTS_ID, YLL_NUTS3),
    by = c("NUTS3_ID" = "NUTS_ID")
  ) |>
  mutate(
    diff = sum_pred_lau2 - YLL_NUTS3
  )

print(head(check))
## A tibble: 6 × 4
#NUTS3_ID sum_pred_lau2 YLL_NUTS3      diff
#<chr>            <dbl>     <dbl>     <dbl>
#1 FR101           11661.    11661.  0       
#2 FR102            7146.     7146.  0       
#3 FR103            8387.     8387.  0       
#4 FR104            8243.     8243.  0       
#5 FR105            7864.     7864. -9.09e-13
#6 FR106            8031.     8031. -1.82e-12

# -------------------------------------------------------------------
# Join to map data
# -------------------------------------------------------------------

fr_nuts3_map <- fr_nuts3 |>
  left_join(df_nuts3, by = "NUTS_ID")

fr_lau_map <- fr_lau |>
  left_join(df_lau2, by = "LAU_ID")

# -------------------------------------------------------------------
# Map 1: observed YLL rate at NUTS3
# -------------------------------------------------------------------

p1 <- ggplot() +
  geom_sf(
    data = fr_nuts3_map,
    aes(fill = YLL_rate_NUTS3),
    color = "white",
    linewidth = 0.15
  ) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey90",
    labels = scales::label_number(accuracy = 1, big.mark = " ", decimal.mark = ",")
  ) +
  coord_sf(
    xlim = c(-6, 10),
    ylim = c(41, 52),
    expand = FALSE
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Observed YLL rate at NUTS3",
    fill = "YLL per 100,000"
  )

p1

ggsave(
  "figures/YLL_rate_observed_NUTS3_share_based.png",
  plot = p1,
  width = 8,
  height = 8,
  dpi = 300
)

# -------------------------------------------------------------------
# Map 2: predicted YLL rate at LAU2
# -------------------------------------------------------------------

p2 <- ggplot() +
  geom_sf(
    data = fr_lau_map,
    aes(fill = log1p(PredYLL_rate_LAU2)),
    color = NA
  ) +
  geom_sf(
    data = fr_nuts3,
    fill = NA,
    color = "white",
    linewidth = 0.12
  ) +
  scale_fill_viridis_c(
    option = "magma",
    na.value = "grey90",
    labels = scales::label_number(accuracy = 0.1, decimal.mark = ",")
  ) +
  coord_sf(
    xlim = c(-6, 10),
    ylim = c(41, 52),
    expand = FALSE
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    title = "Predicted and scaled log YLL rate at LAU2",
    fill = "log(rate + 1)"
  )

p2

ggsave(
  "figures/YLL_rate_predicted_LAU2_share_based.png",
  plot = p2,
  width = 8,
  height = 8,
  dpi = 300
)