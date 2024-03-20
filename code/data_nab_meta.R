# summarize station info
df_nab_geo <- tidynab::geolocate_stations()

v_site_mat <- df_terraclim %>%
  arrange(mat) %>%
  pull(site)

df_meta <- df_nab %>%
  mutate(date = as.Date(date)) %>%
  filter(taxa %in% (v_taxa_short %>% unique())) %>% # limit to taxa studied
  drop_na(count) %>%
  group_by(stationid) %>%
  summarise(
    mindate = min(date),
    maxdate = max(date),
    n = n()
  ) %>%
  mutate(range = maxdate - mindate) %>%
  ungroup() %>%
  arrange(desc(n)) %>%
  left_join(
    df_nab_geo %>%
      filter(!state %in% c("AK", "PR")) %>%
      mutate(site = case_when(
        city == "Colorado Springs" ~ "DV",
        city == "Sylvania" ~ "DT",
        city == "Seattle" ~ "ST",
        city == "New York" ~ "NY",
        city == "Georgetown" ~ "AT",
        city == "Houston (Station 2)" ~ "HT",
        city == "Tampa" ~ "TP"
      )),
    by = c("stationid" = "id")
  ) %>%
  left_join(data.frame(site = v_site, sitename = v_site_name), by = "site") %>%
  mutate(site = factor(site, levels = v_site_mat)) %>%
  arrange(site) %>%
  mutate(sitename = factor(sitename, levels = (.) %>% pull(sitename) %>% unique()))

if (FALSE) {
  write_rds(df_meta, str_c(.path$dat_other, "dat_meta.rds"))
}

# make map
p_pollen_map <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  theme_void() +
  geom_point(data = df_meta, aes(x = lon, y = lat), pch = 10, color = "black", cex = 3) +
  ggrepel::geom_label_repel(data = df_meta %>% filter(site %in% v_site), aes(x = lon, y = lat, label = sitename)) +
  geom_point(data = df_meta %>% filter(site %in% v_site), aes(x = lon, y = lat), pch = 10, color = "red", cex = 3) +
  coord_map("bonne", lat0 = 50)
