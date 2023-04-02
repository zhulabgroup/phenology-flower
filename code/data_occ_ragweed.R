df_tree <- read_rds("./data/occurrence/street_trees_20230327.rds")

cl <- makeCluster(length(v_site), outfile = "")
registerDoSNOW(cl)

ls_df_ragweed_city <- foreach(
  siteoi = v_site,
  .packages = c("tidyverse", "raster", "sf", "spocc")
) %dopar% {
  # get extent
  df_tree_city <- df_tree %>%
    filter(site == siteoi) %>%
    drop_na(lon, lat)
  bbox <- extent(min(df_tree_city$lon), max(df_tree_city$lon), min(df_tree_city$lat), max(df_tree_city$lat))
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
  df_ragweed_city <- res$gbif$data[[1]] %>%
    dplyr::select(lon = longitude, lat = latitude, species) %>%
    mutate(site = siteoi) %>%
    mutate(family = "Asteraceae") %>%
    mutate(genus = "Ambrosia") %>%
    mutate(genus_id = 998)
  df_ragweed_city
}
stopCluster(cl)

df_ragweed <- bind_rows(ls_df_ragweed_city)
write_rds(df_ragweed, "./data/occurrence/ragweed.rds")
