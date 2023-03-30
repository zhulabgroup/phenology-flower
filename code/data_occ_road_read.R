roads_files_all <- list.files("./data/occurrence/roads/", pattern = "edges.shp", recursive = T, full.names = T)

# cl <- makeCluster(length(site_list), outfile = "")
# registerDoSNOW(cl)
roads_list <- vector(mode = "list")

for (
  s in 1:length(site_list)
) {
  # read shapefile for the site
  siteoi <- site_list[s]
  sitename <- sitename_list[s]
  roads_file <- str_subset(roads_files_all, pattern = sitename %>% str_replace(" ", "_"))
  roads <- sf::st_read(dsn = roads_file)

  # get boundary from plant occurrence data frame
  plant_df_site <- plant_df %>% filter(site == siteoi)
  area_site <- sf::st_bbox(
    c(
      xmin = min(plant_df_site$lon) - 0.005,
      xmax = max(plant_df_site$lon) + 0.005,
      ymin = min(plant_df_site$lat) - 0.005,
      ymax = max(plant_df_site$lat) + 0.005
    ),
    crs = sf::st_crs(roads)
  )

  # clip the shapefile
  roads_crop <- sf::st_crop(roads, area_site)

  # # transform into a format that can be used in ggplot
  # roads_sldf <- SpatialLinesDataFrame(roads_crop, data = data.frame(value = rep(1, length(roads_crop))), match.ID = F)
  # roads_fort <- roads_sldf %>%
  #   fortify() %>%
  # mutate(
  #   site = siteoi,
  #   sitename = sitename
  # )

  roads_list[[s]] <- roads_crop %>%
    mutate(
      site = siteoi,
      sitename = sitename
    )
}
roads <- bind_rows(roads_list)
stopCluster(cl)
write_rds(roads, "./data/occurrence/roads/roads_cities.rds")
