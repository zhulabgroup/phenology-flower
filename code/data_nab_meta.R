# summarize station info
df_meta <- df_nab_full %>%
  filter(taxa %in% v_taxa_short) %>% # limit to taxa studied
  drop_na(count) %>%
  group_by(station, location, lat, lon, id) %>%
  summarise(
    mindate = min(date),
    maxdate = max(date),
    n = n()
  ) %>%
  mutate(range = maxdate - mindate) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  mutate(site = case_when(
    location == "San Jose, CA" ~ "SJ",
    location == "Colorado Springs, CO" ~ "DV",
    location == "Sylvania, OH" ~ "DT",
    location == "Seattle, WA" ~ "ST",
    location == "New York, NY" ~ "NY",
    location == "Austin Area, TX" ~ "AT",
    location == "Houston 2, TX" ~ "HT",
    location == "Tampa, FL" ~ "TP"
  )) %>%
  left_join(data.frame(site = v_site, sitename = v_site_name), by = "site")

write_rds(df_meta, "./data/processed/dat_meta.rds")

# make map
p_pollen_map <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  theme_void() +
  geom_point(data = df_meta, aes(x = lon, y = lat), pch = 10, color = "black", cex = 3) +
  ggrepel::geom_label_repel(data = df_meta %>% filter(site %in% v_site), aes(x = lon, y = lat, label = sitename)) +
  geom_point(data = df_meta %>% filter(site %in% v_site), aes(x = lon, y = lat), pch = 10, color = "red", cex = 3) +
  coord_map("bonne", lat0 = 50)
