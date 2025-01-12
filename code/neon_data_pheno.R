df_neon_meta <- list.files(.path$neon, "metadata", full.names = T) %>%
  read_csv() %>%
  filter(!site %in% c("BARR", "TOOL", "HEAL", "BONA", "DEJU", "PUUM", "GUAN", "LAJA")) %>%
  filter(!site %in% c("SJER", "SOAP", "BIGC", "TEAK", "TECR")) %>%
  filter(genus %in% v_taxa_short | family %in% v_taxa_short) %>%
  mutate(species_parse = str_c("italic('", genus, " ", species, "')"))

v_site_neon <- df_neon_meta %>%
  pull(site) %>%
  unique()

df_neon_metric <- list.files(.path$neon, "phenometric", full.names = T) %>%
  read_csv() %>%
  mutate(event = factor(event,
    levels = c("first_leaf", "flower"),
    labels = c("leaf", "flower")
  )) %>%
  drop_na(event) %>%
  select(-tag, -event_code) %>%
  rename(doy = first_yes_doy) %>%
  group_by(site, id, event, year) %>%
  summarise(doy = median(doy)) %>%
  inner_join(df_neon_meta, by = c("site", "id"))

p_neon_map <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  geom_point(
    data = df_neon_meta %>%
      distinct(site, site_lon, site_lat) %>%
      filter(site %in% v_site_neon),
    aes(x = site_lon, y = site_lat),
    pch = 10,
    size = 2
  ) +
  # ggrepel::geom_label_repel(
  #   data = df_neon_sites,
  #   aes(x = lon, y = lat, label = site)
  # ) +
  theme_void() +
  coord_map("bonne", lat0 = 50)

# save figure
if (.fig_save) {
  ggsave(
    plot = p_neon_map,
    filename = str_c(.path$out_fig, "supp_neon_map.pdf"),
    width = 7,
    height = 4,
    device = pdf
  )
}
