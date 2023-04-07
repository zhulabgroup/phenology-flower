sf_road <- read_rds("./data/occurrence/roads/roads_cities.rds")

p_plant_map <- ggplot() +
  theme_void() +
  geom_sf(
    data = sf_road %>%
      filter(site %in% c("DT")),
    linewidth = .1, alpha = 0.5
  ) +
  geom_point(
    data = df_plant %>%
      filter(site %in% c("NY")) %>%
      filter(genus %in% v_taxa_short | family %in% v_taxa_short) %>%
      mutate(taxa = case_when(
        family %in% c("Poaceae", "Cupressaceae", "Pinaceae") ~ family,
        TRUE ~ genus
      )) %>%
      filter(!taxa %in% c("Poaceae", "Ambrosia")) %>%
      group_by(taxa, site) %>%
      sample_n(min(100, n())) %>%
      ungroup() %>%
      left_join(df_meta %>% dplyr::select(site, sitename), by = "site"),
    aes(x = lon, y = lat, col = taxa), alpha = 0.8
  ) +
  facet_wrap(. ~ sitename, ncol = 1) +
  guides(col = guide_legend(title = "Taxa")) +
  theme(legend.position = "bottom") +
  # ylab("Latitude")+
  # xlab("Longitude")+
  coord_sf()
