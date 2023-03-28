if (FALSE) {
  # Downloaded from: https://chelsa-climate.org/bioclim/
  # Documentation: https://chelsa-climate.org/wp-admin/download-page/CHELSA_tech_specification_V2.pdf
  
  # ln -s /nfs/turbo/seas-zhukai/climate/CHELSA/climatology
  chelsa_path <- "./data/CHELSA/"
  # read in raster
  tmean_ras <- raster(paste0(chelsa_path, "bio1.tif"))
  ppt_ras <- raster(paste0(chelsa_path, "bio12.tif"))
  vpdmax_ras <- raster(paste0(chelsa_path, "vpd_max.tif"))
  
  # sites as points
  meta_sf <- meta_df %>%
    drop_na(site) %>%
    dplyr::select(site, lon, lat) %>%
    st_as_sf(
      coords = c("lon", "lat"),
      crs = 4326
    )
  
  # extract chelsa data at points
  chelsa_df <- meta_sf %>%
    mutate(
      mat = raster::extract(tmean_ras, meta_sf),
      tap = raster::extract(ppt_ras, meta_sf),
      vpd = raster::extract(vpdmax_ras, meta_sf)
    ) %>%
    as_tibble() %>%
    dplyr::select(-geometry)
  
  write_rds(chelsa_df, "./data/processed/chelsa.rds")
}

chelsa_df <- read_rds("./data/processed/chelsa.rds")