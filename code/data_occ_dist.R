if (!file.exists("./data/occurrence/distance_from_plants_to_nab_stations.rds")) {
  df_distance <- df_plant %>%
    filter(genus %in% v_taxa_short | family %in% v_taxa_short) %>%
    left_join(df_meta %>% select(site, sitelon = lon, sitelat = lat, sitename) %>% drop_na(), by = "site") %>%
    rowwise() %>%
    mutate(distance = geosphere::distm(c(lon, lat), c(sitelon, sitelat), fun = geosphere::distHaversine) %>% as.numeric() %>% `/`(1000)) %>% # distance in the unit of km
    ungroup() %>%
    group_by(site, sitename) %>%
    summarise(
      mindist = min(distance),
      maxdist = max(distance),
      meandist = mean(distance)
    )
  write_rds(df_distance, "./data/occurrence/distance_from_plants_to_nab_stations.rds")
}

df_distance <- read_rds("./data/occurrence/distance_from_plants_to_nab_stations.rds")
