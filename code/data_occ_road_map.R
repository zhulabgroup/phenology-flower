if (!.full_data) {
  sf_road <- sf_road_sample
  df_tree <- df_tree_sample
}

p_plant_map <- ggplot() +
  theme_void() +
  geom_sf(
    data = sf_road,
    linewidth = .1, alpha = 0.5
  ) +
  geom_point(
    data = df_tree %>%
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

# save figure
if (.fig_save) {
  ggsave(
    plot = p_plant_map,
    filename = str_c(.path$output, "supp/supp_plant_map.pdf"),
    width = 12,
    height = 12,
    device = pdf
  )
}
