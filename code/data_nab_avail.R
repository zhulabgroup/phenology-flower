v_site_lat <- df_meta %>%
  filter(site %in% v_site) %>%
  arrange(lat) %>%
  pull(sitename)

p_nab_avail <- df_nab %>%
  right_join(df_meta %>% select(stationid, site, sitename) %>% drop_na(site), by = "stationid") %>%
  mutate(sitename = factor(sitename, levels = v_site_lat)) %>%
  filter(taxa %in% unique(v_taxa_short)) %>%
  distinct(site, sitename, date) %>%
  ggplot() +
  geom_tile(aes(x = date, y = sitename)) +
  theme_classic() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    x = "",
    y = ""
  )
