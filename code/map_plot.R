df_doy <- read_rds(str_c(.path$dat_other, "df_pollen_doy.rds"))

df_doy_all <- df_doy %>%
  select(site, leaf = leaf_doy, pollen = pollen_doy) %>%
  gather(key = "type", value = "doy", -site) %>%
  # group_by(site, type) %>%
  # summarise(median = median(doy),
  #           lower = quantile(doy, 0.025),
  #           upper = quantile(doy, 0.975)
  # ) %>%
  right_join(df_meta %>% select(site, sitename) %>% drop_na(site), by = "site") %>%
  mutate(sitename = factor(sitename, levels = v_site_lat))

p_doy_variation <- df_doy_all %>%
  ggplot() +
  geom_violin(aes(x = interaction(sitename, type), y = doy, fill = type), col = NA) +
  labs(
    y = "Day of year",
    x = "City",
    fill = "Phenology"
  ) +
  scale_fill_manual(values = c("leaf" = "dark blue", "pollen" = "dark red")) +
  scale_x_discrete(
    labels = df_doy_all %>% distinct(sitename, type) %>% arrange(type, sitename) %>% pull(sitename)
  ) +
  geom_vline(xintercept = 7.5, lty = 2) +
  theme_classic() +
  theme(legend.position = "bottom")

siteoi <- "DT"
yearoi <- 2018

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
    labels = c(75, 100, 125, 150) #
  ) +
  labs(col = "Day of year") +
  coord_sf() +
  ggspatial::annotation_scale(location = "bl", style = "ticks")
