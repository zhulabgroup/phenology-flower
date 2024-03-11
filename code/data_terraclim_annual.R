if (FALSE) {
  # Downloaded from: https://chelsa-climate.org/bioclim/
  # Documentation: https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf

  # ln -s /nfs/turbo/seas-zhukai/climate/CHELSA/climatology
  path_terraclim <- "./data/terraclim/annual_2023_upadate/"

  # sites as points
  sf_meta <- df_meta %>%
    drop_na(site) %>%
    select(site, lon, lat) %>%
    sf::st_as_sf(
      coords = c("lon", "lat"),
      crs = 4326
    )

  ls_df_terraclim <- vector(mode = "list")
  for (year in 2018:2022) {
    # read in raster
    ras_tmax <- list.files(path_terraclim, str_c("tmax_", year), recursive = T, full.names = T) %>%
      terra::rast()
    ras_tmin <- list.files(path_terraclim, str_c("tmin_", year), recursive = T, full.names = T) %>%
      terra::rast()
    ras_ppt <- list.files(path_terraclim, str_c("ppt_", year), recursive = T, full.names = T) %>%
      terra::rast()
    # ras_vpd <- list.files(path_terraclim, str_c("vpd_", year), recursive = T,full.names = T) %>%
    #   terra::rast()

    # extract chelsa data at points
    ls_df_terraclim[[year %>% as.character()]] <- sf_meta %>%
      mutate(
        mat = (terra::extract(ras_tmax, sf_meta) %>% select(-ID) %>% rowMeans() + terra::extract(ras_tmin, sf_meta) %>% select(-ID) %>% rowMeans()) / 2,
        tap = terra::extract(ras_ppt, sf_meta) %>% select(-ID) %>% rowSums()
      ) %>%
      as_tibble() %>%
      select(-geometry) %>%
      mutate(year = year)
  }

  df_terraclim_annual <- bind_rows(ls_df_terraclim)
  write_rds(df_terraclim_annual, "./data/processed/dat_terraclim_annual.rds")
}

df_terraclim_annual <- read_rds("./data/processed/dat_terraclim_annual.rds")
