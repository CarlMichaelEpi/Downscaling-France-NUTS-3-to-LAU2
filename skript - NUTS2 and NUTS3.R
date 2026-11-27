rm(list=ls()); gc()

library(sf)
library(giscoR)
library(dplyr)
library(ggplot2)

setwd("F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/")

nuts_all <- gisco_get_nuts(year = "2021", resolution = "20", epsg = "4326")

fr_nuts2 <- nuts_all |>
  filter(CNTR_CODE == "FR", LEVL_CODE == 2)

fr_nuts3 <- nuts_all |>
  filter(CNTR_CODE == "FR", LEVL_CODE == 3)

countries <- gisco_get_countries(resolution = "20", epsg = "4326")

fr_country <- countries |>
  filter(ISO3_CODE == "FRA")

touch_matrix <- st_touches(countries, fr_country, sparse = FALSE)
neighbors <- countries[as.vector(touch_matrix), ] |>
  filter(ISO3_CODE != "FRA")

fr_nuts2_cent <- st_point_on_surface(fr_nuts2)
fr_nuts3_cent <- st_point_on_surface(fr_nuts3)

p <- ggplot() +
  theme_void() +
  geom_sf(data = neighbors, fill = "#ECECEC", color = "grey70", linewidth = 0.2) +
  geom_sf(data = fr_nuts3, fill = "#F2E8D5", color = "grey50", linewidth = 0.2) +
  geom_sf(data = fr_nuts2, fill = NA, color = "grey20", linewidth = 1.0) +
  geom_sf_text(
    data = fr_nuts3_cent,
    aes(label = NUTS_ID),
    size = 1.8,
    color = "grey35",
    alpha = 0.85
  ) +
  geom_sf_text(
    data = fr_nuts2_cent,
    aes(label = NUTS_ID),
    size = 4,
    fontface = "bold",
    color = "black"
  ) +
  coord_sf(xlim = c(-6, 10), ylim = c(41, 52), expand = FALSE) +
  theme(
    panel.background = element_rect(fill = "#E9EEF2", color = NA),
    plot.background  = element_rect(fill = "#E9EEF2", color = NA)
  )

ggsave(
  filename = "figures/france_nuts2_nuts3_map.png",
  plot = p,
  width = 8,
  height = 8,
  dpi = 300
)
