if (.full_data) {
  df_distance <- df_tree_coord %>%
    left_join(df_meta %>% select(site, sitelon = lon, sitelat = lat, sitename) %>% drop_na(), by = "site") %>%
    rowwise() %>%
    mutate(distance = geosphere::distm(c(midlon, midlat), c(sitelon, sitelat), fun = geosphere::distHaversine) %>% as.numeric() %>% `/`(1000)) %>% # distance in the unit of km
    ungroup()
  write_rds(df_distance, str_c(.path$intermediate, "tree/df_distance.rds"))
} else {
  df_distance <- read_rds(str_c(.path$intermediate, "tree/df_distance.rds"))
}
