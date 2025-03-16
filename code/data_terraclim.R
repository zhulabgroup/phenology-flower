if (.full_data) {
  path_terraclim <- str_c(.path$input, "terraclim/climatology/raw/")

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

  write_rds(df_terraclim, str_c(.path$input, "terraclim/dat_terraclim.rds"))
} else {
  df_terraclim <- read_rds(str_c(.path$input, "terraclim/dat_terraclim.rds"))
}
