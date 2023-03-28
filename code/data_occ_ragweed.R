trees_df <- read_rds("./data/occurrence/street_trees_20230327.rds")

cl <- makeCluster(length(site_list), outfile = "")
registerDoSNOW(cl)

ragweed_df_city_list <- foreach(
  siteoi = site_list,
  .packages = c("tidyverse", "raster", "sf", "spocc")
) %dopar% {
  # get extent
  trees_df_city <- trees_df %>%
    filter(site == siteoi) %>%
    drop_na(lon, lat)
  bbox <- extent(min(trees_df_city$lon), max(trees_df_city$lon), min(trees_df_city$lat), max(trees_df_city$lat))
  # make it a polygon
  bbox_sp <- as(bbox, "SpatialPolygons")
  projection(bbox_sp) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

  # get gbif data
  res <- occ(
    query = "Ambrosia", from = "gbif", has_coords = TRUE, limit = 1e6,
    geometry = st_bbox(bbox_sp),
    date = c(as.Date("2018-01-01"), as.Date("2021-12-31")),
    gbifopts = list(
      hasGeospatialIssue = FALSE
    )
  )

  # get coordinates
  ragweed_df_city <- res$gbif$data[[1]] %>%
    dplyr::select(lon = longitude, lat = latitude, species) %>%
    mutate(site = siteoi) %>%
    mutate(family = "Asteraceae") %>%
    mutate(genus = "Ambrosia") %>%
    mutate(genus_id = 998)
  ragweed_df_city
}
stopCluster(cl)

ragweed_df <- bind_rows(ragweed_df_city_list)
write_rds(ragweed_df, "./data/occurrence/ragweed.rds")
