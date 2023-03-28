if (!file.exists("./data/occurrence/distance_from_plants_to_nab_stations.rds")) {
  distance_df <- plant_df %>%
    filter(genus %in% taxa_short_list | family %in% taxa_short_list) %>%
    left_join(meta_df %>% dplyr::select(site, sitelon = lon, sitelat = lat, sitename) %>% drop_na(), by = "site") %>%
    rowwise() %>%
    mutate(distance = distm(c(lon, lat), c(sitelon, sitelat), fun = distHaversine) %>% as.numeric() %>% `/`(1000)) %>% # distance in the unit of km
    ungroup() %>%
    group_by(site, sitename) %>%
    summarise(
      mindist = min(distance),
      maxdist = max(distance),
      meandist = mean(distance)
    )
  write_rds(distance_df, "./data/occurrence/distance_from_plants_to_nab_stations.rds")
}

  distance_df <- read_rds("./data/occurrence/distance_from_plants_to_nab_stations.rds")