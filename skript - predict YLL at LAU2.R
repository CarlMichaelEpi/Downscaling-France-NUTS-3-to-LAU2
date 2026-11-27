rm(list = ls()); gc()

library(sf)
library(giscoR)
library(dplyr)
library(ggplot2)
library(readr)

setwd("F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/")

nuts_all <- gisco_get_nuts(year = "2021", resolution = "20", epsg = "4326")

fr_nuts3 <- nuts_all |>
  filter(CNTR_CODE == "FR", LEVL_CODE == 3)

lau_all <- gisco_get_lau(year = "2020", epsg = "4326")

fr_lau <- lau_all |>
  filter(CNTR_CODE == "FR")

bbox_fr <- st_bbox(c(xmin = -6, xmax = 10,
                     ymin = 41, ymax = 52),
                   crs = st_crs(fr_lau))

fr_lau <- st_crop(fr_lau, bbox_fr)


# Eksempel: NUTS3-data (må finnes lokalt)
# Kolonner:
# NUTS_ID, YLL, population, prop_0_19, prop_20_64, prop_65plus, prop_male

df_nuts3 <- read_csv("data/nuts3_yll_demography.csv")

# LAU2-data
# Kolonner:
# LAU_ID, NUTS3_ID, population, prop_0_19, prop_20_64, prop_65plus, prop_male

df_lau2 <- read_csv("data/lau2_demography.csv")

df_nuts3 <- df_nuts3 |>
  mutate(
    yll_rate = YLL / population,
    log_yll_rate = log(yll_rate)
  )

model_n3 <- lm(
  log_yll_rate ~ prop_0_19 +
    prop_20_64 +
    prop_65plus +
    prop_male,
  data = df_nuts3
)

summary(model_n3)

df_lau2 <- df_lau2 |>
  mutate(
    pred_log_rate = predict(model_n3, newdata = df_lau2),
    pred_rate     = exp(pred_log_rate),
    pred_yll_raw  = pred_rate * population
  )

df_lau2 <- df_lau2 |>
  left_join(df_nuts3 |> select(NUTS_ID, YLL),
            by = c("NUTS3_ID" = "NUTS_ID")) |>
  group_by(NUTS3_ID) |>
  mutate(
    YLL_lau2_scaled =
      pred_yll_raw / sum(pred_yll_raw) * unique(YLL)
  ) |>
  ungroup()

# Kontroll
check <- df_lau2 |>
  group_by(NUTS3_ID) |>
  summarise(
    sum_scaled = sum(YLL_lau2_scaled),
    nuts3_total = unique(YLL)
  )

print(head(check))


# FIGURE | - YLL - NUTS3
fr_nuts3_map <- fr_nuts3 |>
  left_join(df_nuts3, by = "NUTS_ID")

p1 <- ggplot() +
  geom_sf(data = fr_nuts3_map,
          aes(fill = YLL),
          color = "white",
          linewidth = 0.2) +
  scale_fill_viridis_c(option = "magma") +
  coord_sf(xlim = c(-6, 10),
           ylim = c(41, 52),
           expand = FALSE) +
  theme_void() +
  ggtitle("Observed YLL – NUTS3")

p1

ggsave("figures/YLL_observed_NUTS3.png",
       plot = p1,
       width = 8, height = 8, dpi = 300)

# FIGURE 2 - YLL - LAU2
fr_lau_map <- fr_lau |>
  left_join(df_lau2, by = "LAU_ID")

p2 <- ggplot() +
  geom_sf(data = fr_lau_map,
          aes(fill = YLL_lau2_scaled),
          color = NA) +
  geom_sf(data = fr_nuts3,
          fill = NA,
          color = "black",
          linewidth = 0.4) +
  scale_fill_viridis_c(option = "magma") +
  coord_sf(xlim = c(-6, 10),
           ylim = c(41, 52),
           expand = FALSE) +
  theme_void() +
  ggtitle("Predicted & Scaled YLL – LAU2")

p2

ggsave("figures/YLL_predicted_LAU2.png",
       plot = p2,
       width = 8, height = 8, dpi = 300)
