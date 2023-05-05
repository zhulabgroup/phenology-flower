v_site_lat <- df_meta %>%
  filter(site %in% v_site) %>%
  arrange(lat) %>%
  pull(sitename)

p_nab_avail <- df_nab_full %>%
  left_join(df_meta %>% dplyr::select(id, site, sitename), by = "id") %>%
  filter(site %in% v_site) %>%
  mutate(sitename = factor(sitename, levels = v_site_lat)) %>%
  filter(taxa %in% v_taxa_short) %>%
  filter(!taxa %in% c("Poaceae", "Ambrosia")) %>%
  distinct(site, sitename, date) %>%
  ggplot() +
  geom_tile(aes(x = date, y = sitename)) +
  theme_classic() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    x = "",
    y = ""
  )
