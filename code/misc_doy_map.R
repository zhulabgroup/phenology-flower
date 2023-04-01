read_rds("./data/output/Quercus/flowering day of year.rds") %>%
  filter(site == siteoi) %>%
  bind_rows(flower_doy_df %>% mutate(year = yearoi)) %>%
  filter(
    thres == 0.5,
    direction == "up"
  ) %>%
  dplyr::select(id, doy, lat, lon, year) %>%
  mutate(year = as.factor(year)) %>%
  bind_rows(detroit_df_ts %>% rename(doy = peak) %>% dplyr::distinct(id, doy, lat, lon) %>% filter(is.finite(doy)) %>% mutate(year = "flower observation (Katz's team)")) %>%
  filter(doy <= 160 & doy >= 110) %>%
  spread(key = "year", value = "doy") %>%
  # drop_na() %>%
  rowwise() %>%
  mutate(
    lat = lat + rnorm(1, 0, 0.002),
    lon = lon + rnorm(1, 0, 0.002)
  ) %>%
  ungroup() %>%
  gather(key = "group", value = "doy", -id, -lat, -lon) %>%
  group_by(group) %>%
  mutate(doy = (doy - mean(doy, na.rm = T)) / (quantile(doy, 0.95, na.rm = T) - quantile(doy, 0.05, na.rm = T))) %>% # standardize to between -0.5 and 0.5
  filter(doy >= -0.5 & doy <= 0.5) %>% # remove outliers
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = lon, y = lat, col = doy)) +
  facet_wrap(. ~ group, ncol = 2) +
  coord_equal() +
  theme_classic() +
  scale_color_viridis_c(direction = -1)
