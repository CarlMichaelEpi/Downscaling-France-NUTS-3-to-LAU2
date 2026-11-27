rm(list = ls()); gc()

library(sf)
library(giscoR)
library(dplyr)
library(ggplot2)
library(readr)

set.seed(1234)

setwd("F:/Forskningsprosjekter/PDB 1306 - Norsk sykdomsbyrdepr_/Forskningsfiler/CHMA/Downscaling France/")

dir.create("data", showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
dir.create("figures", showWarnings = FALSE)


# GEOMETRY
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

fr_lau <- st_join(fr_lau, fr_nuts3["NUTS_ID"], join = st_intersects)

fr_lau <- fr_lau |>
  group_by(LAU_ID) |>
  slice(1) |>
  ungroup()

valid_nuts3 <- fr_nuts3$NUTS_ID
fr_lau$NUTS_ID[is.na(fr_lau$NUTS_ID)] <-
  sample(valid_nuts3,
         sum(is.na(fr_lau$NUTS_ID)),
         replace = TRUE)

stopifnot(sum(is.na(fr_lau$NUTS_ID)) == 0)


# SIMULATION OF LAU2 DATA
n_lau <- nrow(fr_lau)

df_lau2 <- fr_lau |>
  st_drop_geometry() |>
  transmute(
    LAU_ID   = LAU_ID,
    NUTS3_ID = NUTS_ID,
    population = round(runif(n_lau, 1000, 20000))
  )

# Simuler aldersstruktur (summerer til 1)
age_raw <- matrix(runif(n_lau * 3, 0.3, 1.5), ncol = 3)
age_prop <- age_raw / rowSums(age_raw)

df_lau2$prop_0_19   <- age_prop[,1]
df_lau2$prop_20_64  <- age_prop[,2]
df_lau2$prop_65plus <- age_prop[,3]
df_lau2$prop_male   <- runif(n_lau, 0.47, 0.53)

stopifnot(all(!is.na(df_lau2)))


# AGGREGATE FROM LAU2 to NUTS3
df_nuts3 <- df_lau2 |>
  group_by(NUTS3_ID) |>
  summarise(
    population = sum(population),
    prop_0_19   = sum(prop_0_19 * population) / sum(population),
    prop_20_64  = sum(prop_20_64 * population) / sum(population),
    prop_65plus = sum(prop_65plus * population) / sum(population),
    prop_male   = sum(prop_male * population) / sum(population),
    .groups = "drop"
  )


# SIMULATE YLLON NUTS3
base_rate <- 0.02

log_rr <-
  0.8 * df_nuts3$prop_65plus +
  0.2 * df_nuts3$prop_male

centroids <- st_centroid(fr_nuts3)
coords <- st_coordinates(centroids)

north_south <- as.numeric(scale(coords[,2]))
north_south <- pmin(pmax(north_south, -2), 2)

eta <- log(base_rate) + log_rr + 0.1 * north_south
eta <- pmin(pmax(eta, -10), 2)

true_rate <- exp(eta)

df_nuts3$YLL <- rpois(
  nrow(df_nuts3),
  lambda = true_rate * df_nuts3$population
)

summary(df_nuts3$YLL)


# SCALE OF COVARIATES
covars <- c("prop_0_19","prop_20_64","prop_65plus","prop_male")

scale_params <- df_nuts3 |>
  summarise(across(all_of(covars),
                   list(mean = mean, sd = sd),
                   na.rm = TRUE))

for(v in covars){
  m <- scale_params[[paste0(v,"_mean")]]
  s <- scale_params[[paste0(v,"_sd")]]
  df_nuts3[[v]] <- (df_nuts3[[v]] - m) / s
  df_lau2[[v]]  <- (df_lau2[[v]] - m) / s
}


# MODEL NUTS3
df_nuts3 <- df_nuts3 |>
  mutate(
    yll_rate = YLL / population,
    log_yll_rate = log(yll_rate + 1e-9)
  )

model_n3 <- lm(
  log_yll_rate ~ prop_0_19 +
    prop_20_64 +
    prop_65plus +
    prop_male,
  data = df_nuts3
)

#saveRDS(model_n3, "data/model_nuts3_downscaling.rds")


# PREDICTION LAU2
df_lau2 <- df_lau2 |>
  mutate(
    pred_log_rate = predict(model_n3, newdata = df_lau2),
    pred_rate     = exp(pred_log_rate),
    pred_yll_raw  = pred_rate * population
  )


# SCALE WITHIN NUTS3


df_lau2 <- df_lau2 |>
  left_join(df_nuts3 |> select(NUTS3_ID, YLL),
            by = "NUTS3_ID") |>
  group_by(NUTS3_ID) |>
  mutate(
    scale_factor = unique(YLL) / sum(pred_yll_raw),
    YLL_lau2_scaled = pred_yll_raw * scale_factor
  ) |>
  ungroup()

# Kontroll
check <- df_lau2 |>
  group_by(NUTS3_ID) |>
  summarise(
    sum_scaled = sum(YLL_lau2_scaled),
    nuts3_total = unique(YLL),
    diff = sum_scaled - nuts3_total
  )
check

#write_csv(check, "tables/scaling_check.csv")


# MAP 1 – OBSERVED NUTS3
countries <- gisco_get_countries(resolution = "20", epsg = "4326")

bbox_fr <- st_as_sfc(st_bbox(fr_nuts3))

neighbors <- countries |>
  filter(ISO3_CODE != "FRA") |>
  st_intersection(st_buffer(bbox_fr, 2))

fr_nuts3_map <- fr_nuts3 |>
  left_join(df_nuts3,
            by = c("NUTS_ID" = "NUTS3_ID"))

p1 <- ggplot() +
  geom_sf(data = neighbors,
          fill = "grey90",
          color = "grey70",
          linewidth = 0.2) +
  geom_sf(data = fr_nuts3_map,
          aes(fill = YLL),
          color = "white",
          linewidth = 0.2) +
  scale_fill_viridis_c(option = "magma") +
  coord_sf(xlim = c(-6,10),
           ylim = c(41,52),
           expand = FALSE) +
  theme_void()

p1

ggsave("figures/YLL_observed_NUTS3.png", plot = p1, width = 8, height = 8, dpi = 300)


# MAP 2 – PREDICTED & SCALED LAU2
countries <- gisco_get_countries(resolution = "20", epsg = "4326")

bbox_fr <- st_as_sfc(st_bbox(fr_nuts3))

neighbors <- countries |>
  filter(ISO3_CODE != "FRA") |>
  st_intersection(st_buffer(bbox_fr, 2))

fr_lau_map <- fr_lau |>
  left_join(df_lau2, by = "LAU_ID")

p2 <- ggplot() +
  geom_sf(data = neighbors,
          fill = "grey90",
          color = "grey70",
          linewidth = 0.2) +
  geom_sf(data = fr_lau_map,
          aes(fill = YLL_lau2_scaled),
          color = NA) +
  geom_sf(data = fr_nuts3,
          fill = NA,
          color = "black",
          linewidth = 0.3) +
  scale_fill_viridis_c(option = "magma") +
  coord_sf(xlim = c(-6,10),
           ylim = c(41,52),
           expand = FALSE) +
  theme_void()

p2

ggsave("figures/YLL_predicted_LAU2.png", plot = p2, width = 8, height = 8, dpi = 300)

write_csv(df_lau2, "data/lau2_yll_scaled_output.csv")
