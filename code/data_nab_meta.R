# summarize station info
meta_df <- nab_with_taxa_df %>%
  filter(taxa %in% taxa_short_list) %>% # limit to taxa studied
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
    location == "Houston 1, TX" ~ "HT",
    location == "Tampa, FL" ~ "TP"
  )) %>%
  left_join(data.frame(site = site_list, sitename = sitename_list), by = "site")

write_rds(meta_df, "./data/processed/meta_dat.rds")

# make map
p_pollen_map <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  theme_void() +
  geom_point(data = meta_df, aes(x = lon, y = lat), pch = 10, color = "black", cex = 3) +
  geom_label_repel(data = meta_df %>% filter(site %in% site_list), aes(x = lon, y = lat, label = sitename)) +
  geom_point(data = meta_df %>% filter(site %in% site_list), aes(x = lon, y = lat), pch = 10, color = "red", cex = 3) +
  # coord_equal()+
  coord_map("bonne", lat0 = 50)
