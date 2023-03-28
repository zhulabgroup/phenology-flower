p_nab_plant_map <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  theme_void() +
  geom_point(
    data = plant_df %>%
      filter(site %in% site_list) %>%
      filter(genus %in% taxa_short_list | family %in% taxa_short_list) %>%
      mutate(taxa = case_when(
        family %in% c("Poaceae", "Cupressaceae", "Pinaceae") ~ family,
        TRUE ~ genus
      )) %>%
      filter(!taxa %in% c("Poaceae", "Ambrosia")) %>%
      group_by(site) %>%
      summarize(
        midlon = median(lon, na.rm = T),
        midlat = median(lat, na.rm = T)
      ) %>%
      # sample_n(100) %>%
      ungroup(),
    aes(x = midlon, y = midlat), col = "dark blue", cex = 3, alpha = 1, pch = 0
  ) +
  geom_point(data = meta_df, aes(x = lon, y = lat), cex = 3, pch = 10, col = "black", alpha = 0.5) +
  geom_point(data = meta_df %>% filter(site %in% site_list), aes(x = lon, y = lat), cex = 3, pch = 10, col = "red", alpha = 1) +
  geom_label_repel(
    data = plant_df %>%
      filter(site %in% site_list) %>%
      filter(genus %in% taxa_short_list | family %in% taxa_short_list) %>%
      mutate(taxa = case_when(
        family %in% c("Poaceae", "Cupressaceae", "Pinaceae") ~ family,
        TRUE ~ genus
      )) %>%
      # filter(!taxa%in% c("Poaceae", "Ambrosia")) %>%
      group_by(site) %>%
      summarize(
        midlon = median(lon, na.rm = T),
        midlat = median(lat, na.rm = T)
      ) %>%
      # sample_n(100) %>%
      ungroup() %>%
      filter(site %in% site_list) %>%
      left_join(meta_df %>% dplyr::select(site, sitename) %>% drop_na(),
        by = "site"
      ),
    aes(x = midlon, y = midlat, label = sitename), nudge_x = 1.5, nudge_y = 1.5, col = "dark blue"
  ) +
  coord_map("bonne", lat0 = 50)
