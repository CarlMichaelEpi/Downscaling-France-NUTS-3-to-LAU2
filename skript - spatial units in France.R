rm(list = ls()); gc()

library(sf)
library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
library(giscoR)

setwd("F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/")

# ----------------------------
# NUTS
# ----------------------------
nuts_all <- gisco_get_nuts(
  year = "2021",
  resolution = "20",
  epsg = "4326"
)

fr_nuts3 <- nuts_all |>
  filter(CNTR_CODE == "FR", LEVL_CODE == 3)

# ----------------------------
# LAU (communes)
# ----------------------------
lau_all <- gisco_get_lau(
  year = "2020",
  epsg = "4326"
)

fr_lau <- lau_all |>
  filter(CNTR_CODE == "FR")

bbox_fr <- st_bbox(
  c(xmin = -6, xmax = 10,
    ymin = 41, ymax = 52),
  crs = st_crs(4326)
)

fr_lau <- st_crop(fr_lau, bbox_fr)

# remove empty geometries
fr_lau <- fr_lau[!st_is_empty(fr_lau), ]

# fix invalid geometries
fr_lau <- st_make_valid(fr_lau)

# simplify (in projected CRS)
fr_lau <- fr_lau |>
  st_transform(3035) |>
  st_simplify(dTolerance = 100) |>
  st_transform(4326)

# ----------------------------
# Countries
# ----------------------------
countries <- gisco_get_countries(
  resolution = "20",
  epsg = "4326"
)

fr_country <- countries |>
  filter(ISO3_CODE == "FRA")

touch_matrix <- st_touches(countries, fr_country, sparse = FALSE)

neighbors <- countries[as.vector(touch_matrix), ] |>
  filter(ISO3_CODE != "FRA")

# ----------------------------
# Plot
# ----------------------------
p <- ggplot() +
  theme_void() +
  
  geom_sf(
    data = neighbors,
    fill = "#ECECEC",
    color = "grey70",
    linewidth = 0.2
  ) +
  
  geom_sf(
    data = fr_lau,
    fill = "#F2E8D5",
    color = "grey70",
    linewidth = 0.05
  ) +
  
  geom_sf(
    data = fr_nuts3,
    fill = NA,
    color = "black",
    linewidth = 0.8
  ) +
  
  coord_sf(
    xlim = c(-6, 10),
    ylim = c(41, 52),
    expand = FALSE
  )

p

#ggsave(
#  "figures/france_lau_nuts3_map.png",
#  p,
#  width = 8,
#  height = 8,
#  dpi = 300
#)

# SPATIAL MAPPING
idx <- st_nearest_feature(fr_lau, fr_nuts3)

lau_nuts3_key <- fr_lau |>
  mutate(NUTS3 = fr_nuts3$NUTS_ID[idx]) |>
  st_drop_geometry() |>
  select(LAU_ID, NUTS3)

lau_map <- read_excel("data/mapping_NUTS3_LAU2.xlsx") |>
  mutate(CODGEO = sprintf("%05s", CODGEO))

code_fix <- tibble(
  CODGEO_old = c("14666","85165","85212","60694"),
  CODGEO_new = c("14654","85197","85155","60689")
  )

lau_map <- lau_map |>
  left_join(code_fix, by = c("CODGEO" = "CODGEO_old")) |>
  mutate(CODGEO = coalesce(CODGEO_new, CODGEO)) |>
  select(-CODGEO_new)

lau_map_full <- lau_map |>
  left_join(lau_nuts3_key,
            by = c("CODGEO" = "LAU_ID"))

missing <- lau_map_full |> 
  filter(is.na(NUTS3))

if (nrow(missing) > 0) {
  
  missing_sf <- fr_lau |> 
    filter(LAU_ID %in% missing$CODGEO)
  
  idx_missing <- st_nearest_feature(missing_sf, fr_nuts3)
  
  fix <- tibble(
    CODGEO = missing_sf$LAU_ID,
    NUTS3  = fr_nuts3$NUTS_ID[idx_missing]
    )
  
  lau_map_full <- lau_map_full |>
    left_join(fix, by = "CODGEO", suffix = c("", "_fix")) |>
    mutate(NUTS3 = coalesce(NUTS3, NUTS3_fix)) |>
    select(-NUTS3_fix)
  }

lau_map_full <- lau_map_full |>
  mutate(
    NUTS3 = ifelse(CODGEO == "33103", "FRI12", NUTS3)
  )

lau_map_full |> filter(is.na(NUTS3))
setdiff(lau_map$CODGEO, fr_lau$LAU_ID)












missing_codes <- setdiff(lau_map$CODGEO, fr_lau$LAU_ID)

missing_rows <- lau_map |>
  filter(CODGEO %in% missing_codes)

lau_lookup <- fr_lau |>
  st_drop_geometry() |>
  mutate(
    DEP = substr(LAU_ID, 1, 2),
    NAME = LAU_NAME
  )

fix_codes <- missing_rows |>
  inner_join(
    lau_lookup,
    by = c("DEP" = "DEP", "LIBGEO" = "NAME")
  ) |>
  select(CODGEO_old = CODGEO, CODGEO_new = LAU_ID)

lau_map <- lau_map |>
  left_join(fix_codes, by = c("CODGEO" = "CODGEO_old")) |>
  mutate(CODGEO = coalesce(CODGEO_new, CODGEO)) |>
  select(-CODGEO_new)

lau_map_full <- lau_map |>
  left_join(lau_nuts3_key,
            by = c("CODGEO" = "LAU_ID"))

lau_map_full |> filter(is.na(NUTS3))
setdiff(lau_map$CODGEO, fr_lau$LAU_ID)


write_xlsx(lau_map_full, "data/lau_map_full.xlsx")




ggplot(nuts3_fr) +
  geom_sf(aes(fill = area_km2), color = NA) +
  coord_sf(xlim = c(-6, 10), ylim = c(41, 52)) +
  scale_fill_viridis_c() +
  theme_minimal()




