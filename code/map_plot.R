df_doy <- read_rds(str_c(.path$dat_other, "df_leaf_pollen_doy.rds"))

df_doy_all <- df_doy %>%
  select(site, genus, year, leaf = leaf_doy, pollen = pollen_doy) %>%
  gather(key = "type", value = "doy", -site, -genus, -year) %>%
  right_join(df_meta %>% select(site, sitename) %>% drop_na(site), by = "site") %>%
  mutate(sitename = factor(sitename, levels = v_site_lat))

siteoi <- "DT"
yearoi <- 2018

p_doy_variation <- df_doy_all %>%
  filter(site == siteoi, year == yearoi) %>%
  ggplot() +
  geom_violin(aes(x = genus, y = doy, fill = type, group = interaction(type, genus)),
    position = position_dodge(width = 0.5),
    alpha = 0.8
  ) +
  labs(
    y = "Day of year",
    x = "Genus",
    fill = "Phenology"
  ) +
  scale_fill_manual(values = c("leaf" = "dark blue", "pollen" = "dark red")) +
  ggthemes::theme_few() +
  theme(axis.text.x = element_text(face = "italic")) +
  theme(legend.position = "bottom")

p_plant_map_doy <- ggplot() +
  theme_void() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  geom_sf(
    data = sf_road %>%
      filter(site == siteoi),
    linewidth = .1, alpha = 0.5
  ) +
  geom_point(
    data = df_doy %>%
      filter(site == siteoi, year == yearoi) %>%
      left_join(genus_to_family, by = "genus") %>%
      mutate(taxa = case_when(
        family %in% c("Poaceae", "Cupressaceae", "Pinaceae") ~ family,
        TRUE ~ genus
      )),
    aes(x = lon, y = lat, col = percentile), alpha = 0.8, size = 0.5
  ) +
  scale_color_viridis_c(
    direction = -1,
    breaks = ecdf(df_doy$pollen_doy)(c(75, 100, 125, 150)),
    labels = c(75, 100, 125, 150)
  ) +
  labs(col = "Day of year") +
  coord_sf() +
  ggspatial::annotation_scale(location = "bl", style = "ticks") +
  facet_wrap(. ~ genus, ncol = 3) +
  theme(strip.text = element_text(face = "italic")) +
  theme(legend.position = "bottom")

# save figure
if (.fig_save) {
  ggsave(
    plot = p_doy_variation,
    filename = str_c(.path$out_fig, "main_doy_variation.pdf"),
    width = 10,
    height = 4,
    device = pdf
  )

  ggsave(
    plot = p_plant_map_doy,
    filename = str_c(.path$out_fig, "main_plant_map_doy.pdf"),
    width = 10,
    height = 12,
    device = pdf
  )
}
