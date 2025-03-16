if (.full_data) {
  file_road <- list.files(str_c(.path$input, "tree/roads/"), pattern = "edges.shp", recursive = T, full.names = T)

  ls_sf_road_site <- vector(mode = "list")

  for (s in 1:length(v_site)) {
    # read shapefile for the site
    siteoi <- v_site[s]
    sitename <- v_site_name[s]
    file_road_site <- str_subset(file_road, pattern = sitename %>% str_replace(" ", "_"))
    sf_road_site <- sf::st_read(dsn = file_road_site)

    # get boundary from plant occurrence data frame
    df_tree_site <- df_tree %>% filter(site == siteoi)
    sf_bbox <- sf::st_bbox(
      c(
        xmin = min(df_tree_site$lon) - 0.005,
        xmax = max(df_tree_site$lon) + 0.005,
        ymin = min(df_tree_site$lat) - 0.005,
        ymax = max(df_tree_site$lat) + 0.005
      ),
      crs = sf::st_crs(sf_road_site)
    )

    # clip the shapefile
    sf_road_crop <- sf::st_crop(sf_road_site, sf_bbox)

    ls_sf_road_site[[s]] <- sf_road_crop %>%
      mutate(
        site = siteoi,
        sitename = sitename
      )
  }
  sf_road <- bind_rows(ls_sf_road_site)

  write_rds(sf_road, str_c(.path$input, "tree/sf_road.rds"))

  write_rds(
    sf_road %>%
      filter(site %in% c("DT")),
    str_c(.path$input, "tree/roads_cities_sample.rds")
  )
} else {
  sf_road_sample <- read_rds(str_c(.path$input, "tree/sf_road_sample.rds"))
}
