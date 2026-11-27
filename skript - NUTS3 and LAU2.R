rm(list=ls()); gc()

library(sf)
library(giscoR)
library(dplyr)
library(ggplot2)

setwd("F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/")

# --- Hent NUTS ---
nuts_all <- gisco_get_nuts(year = "2021", resolution = "20", epsg = "4326")

fr_nuts2 <- nuts_all |>
  filter(CNTR_CODE == "FR", LEVL_CODE == 2)

fr_nuts3 <- nuts_all |>
  filter(CNTR_CODE == "FR", LEVL_CODE == 3)

# --- Hent LAU ---
lau_all <- gisco_get_lau(year = "2020", epsg = "4326")

fr_lau <- lau_all |>
  filter(CNTR_CODE == "FR")

# Klipp til metropolitan France (fjerner oversjøiske territorier)
bbox_fr <- st_bbox(c(xmin = -6, xmax = 10,
                     ymin = 41, ymax = 52),
                   crs = st_crs(fr_lau))

fr_lau <- st_crop(fr_lau, bbox_fr)

# Forenkle geometri for ytelse
fr_lau <- st_simplify(fr_lau, dTolerance = 0.001)

# --- Land ---
countries <- gisco_get_countries(resolution = "20", epsg = "4326")

fr_country <- countries |>
  filter(ISO3_CODE == "FRA")

touch_matrix <- st_touches(countries, fr_country, sparse = FALSE)
neighbors <- countries[as.vector(touch_matrix), ] |>
  filter(ISO3_CODE != "FRA")

# --- Plot ---
p <- ggplot() +
  theme_void() +
  
  # Naboland
  geom_sf(data = neighbors,
          fill = "#ECECEC",
          color = "grey70",
          linewidth = 0.2) +
  
  # LAU (communes)
  geom_sf(data = fr_lau,
          fill = "#F2E8D5",
          color = "grey70",
          linewidth = 0.05) +
  
  # NUTS2 grenser
  geom_sf(data = fr_nuts3,
          fill = NA,
          color = "black",
          linewidth = 0.8) +
  
  coord_sf(xlim = c(-6, 10),
           ylim = c(41, 52),
           expand = FALSE) +
  
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )
p

ggsave(
  filename = "figures/france_lau2_nuts3_map.png",
  plot = p,
  width = 8,
  height = 8,
  dpi = 300
)
