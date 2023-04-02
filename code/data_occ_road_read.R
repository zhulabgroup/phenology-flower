file_road <- list.files("./data/occurrence/roads/", pattern = "edges.shp", recursive = T, full.names = T)

# cl <- makeCluster(length(site_list), outfile = "")
# registerDoSNOW(cl)
ls_sf_road_site <- vector(mode = "list")

for (
  s in 1:length(v_site)
) {
  # read shapefile for the site
  siteoi <- v_site[s]
  sitename <- v_site_name[s]
  file_road_site <- str_subset(file_road, pattern = sitename %>% str_replace(" ", "_"))
  sf_road_site <- sf::st_read(dsn = file_road_site)

  # get boundary from plant occurrence data frame
  df_plant_site <- df_plant %>% filter(site == siteoi)
  sf_bbox <- sf::st_bbox(
    c(
      xmin = min(df_plant_site$lon) - 0.005,
      xmax = max(df_plant_site$lon) + 0.005,
      ymin = min(df_plant_site$lat) - 0.005,
      ymax = max(df_plant_site$lat) + 0.005
    ),
    crs = sf::st_crs(sf_road_site)
  )

  # clip the shapefile
  sf_road_crop <- sf::st_crop(sf_road_site, sf_bbox)

  # # transform into a format that can be used in ggplot
  # roads_sldf <- SpatialLinesDataFrame(roads_crop, data = data.frame(value = rep(1, length(roads_crop))), match.ID = F)
  # roads_fort <- roads_sldf %>%
  #   fortify() %>%
  # mutate(
  #   site = siteoi,
  #   sitename = sitename
  # )

  ls_sf_road_site[[s]] <- sf_road_crop %>%
    mutate(
      site = siteoi,
      sitename = sitename
    )
}
sf_road <- bind_rows(ls_sf_road_site)
stopCluster(cl)
write_rds(sf_road, "./data/occurrence/roads/roads_cities.rds")
