siteoi <- "DT"

if (.full_data) {
  sf_road_subset <- sf_road %>%
    filter(site == siteoi)

  df_tree_subset <- df_tree %>%
    left_join(genus_to_family, by = "genus") %>%
    filter(site %in% siteoi) %>%
    filter(genus %in% v_taxa_short | family %in% v_taxa_short) %>%
    mutate(taxa = case_when(
      family %in% c("Poaceae", "Cupressaceae", "Pinaceae") ~ family,
      TRUE ~ genus
    ))

  write_rds(sf_road_subset, str_c(.path$intermediate, "tree/sf_road_sample.rds"))
  write_rds(df_tree_subset, str_c(.path$intermediate, "tree/df_tree_sample.rds"))
} else {
  sf_road_subset <- read_rds(str_c(.path$intermediate, "tree/sf_road_sample.rds"))
  df_tree_subset <- read_rds(str_c(.path$intermediate, "tree/df_tree_sample.rds"))
}

set.seed(1)

p_plant_map <- ggplot() +
  theme_void() +
  geom_sf(
    data = sf_road_subset,
    linewidth = .1, alpha = 0.5
  ) +
  geom_point(
    data = df_tree_subset %>%
      group_by(taxa, site) %>%
      sample_n(min(100, n())) %>%
      ungroup() %>%
      left_join(df_meta %>% select(site, sitename), by = "site"),
    aes(x = lon, y = lat, col = taxa), alpha = 0.8
  ) +
  facet_wrap(. ~ sitename, ncol = 1) +
  guides(col = guide_legend(title = "Taxa")) +
  theme(legend.position = "bottom") +
  coord_sf()
