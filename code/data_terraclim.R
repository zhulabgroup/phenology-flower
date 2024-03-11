if (FALSE) {
  path_terraclim <- "./data/terraclim/climatology/"

  # sites as points
  sf_meta <- df_meta %>%
    drop_na(site) %>%
    select(site, lon, lat) %>%
    sf::st_as_sf(
      coords = c("lon", "lat"),
      crs = 4326
    )

  # read in raster
  ras_tmax <- list.files(path_terraclim, "tmax", full.names = T) %>%
    terra::rast()
  ras_tmin <- list.files(path_terraclim, "tmin", full.names = T) %>%
    terra::rast()
  ras_ppt <- list.files(path_terraclim, "ppt", full.names = T) %>%
    terra::rast()

  # extract chelsa data at points
  df_terraclim <- sf_meta %>%
    mutate(
      mat = (terra::extract(ras_tmax, sf_meta) %>% select(-ID) %>% rowMeans() + terra::extract(ras_tmin, sf_meta) %>% select(-ID) %>% rowMeans()) / 2,
      tap = terra::extract(ras_ppt, sf_meta) %>% select(-ID) %>% rowSums()
    ) %>%
    as_tibble() %>%
    select(-geometry)

  write_rds(df_terraclim, "./data/processed/dat_terraclim.rds")
}

df_terraclim <- read_rds("./data/processed/dat_terraclim.rds")
