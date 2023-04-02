if (FALSE) {
  # Downloaded from: https://chelsa-climate.org/bioclim/
  # Documentation: https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf

  # ln -s /nfs/turbo/seas-zhukai/climate/CHELSA/climatology
  path_chelsa <- "./data/CHELSA/"
  # read in raster
  ras_tmean <- terra::rast(str_c(path_chelsa, "bio1.tif"))
  ras_ppt <- terra::rast(str_c(path_chelsa, "bio12.tif"))
  ras_vpdmax <- terra::rast(str_c(path_chelsa, "vpd_max.tif"))

  # sites as points
  sf_meta <- df_meta %>%
    drop_na(site) %>%
    select(site, lon, lat) %>%
    sf::st_as_sf(
      coords = c("lon", "lat"),
      crs = 4326
    )

  # extract chelsa data at points
  df_chelsa <- sf_meta %>%
    mutate(
      mat = terra::extract(ras_tmean, sf_meta)[, 2],
      tap = terra::extract(ras_ppt, sf_meta)[, 2],
      vpd = terra::extract(ras_vpdmax, sf_meta)[, 2]
    ) %>%
    as_tibble() %>%
    dplyr::select(-geometry)

  write_rds(df_chelsa, "./data/processed/dat_chelsa.rds")
}

df_chelsa <- read_rds("./data/processed/dat_chelsa.rds")
