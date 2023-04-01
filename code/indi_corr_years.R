p_dt_corr_years <- read_rds("./data/output/Quercus/flowering day of year.rds") %>%
  filter(site == siteoi) %>%
  bind_rows(flower_doy_df %>% mutate(year = yearoi)) %>%
  filter(
    thres == 0.5,
    direction == "up"
  ) %>%
  filter(year >= 2018) %>%
  left_join(detroit_df_ts %>% dplyr::distinct(id, peak) %>% filter(is.finite(peak)), by = "id") %>%
  ggplot() +
  geom_jitter(aes(x = peak, y = doy, group = as.factor(year), col = as.factor(year)), alpha = 1) +
  geom_smooth(aes(x = peak, y = doy, group = as.factor(year), col = as.factor(year)), method = "lm") +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  ggpubr::stat_cor(
    aes(
      x = peak, y = doy, group = as.factor(year),
      col = as.factor(year),
      label = paste(..r.label.., ..p.label.., sep = "*`,`~")
    ),
    p.accuracy = 0.05,
    label.x.npc = 0.5,
    label.y.npc = "top",
    show.legend = F
  ) +
  coord_equal() +
  ylim(c(110, 160)) +
  xlim(c(110, 160)) +
  xlab("Flowering time in 2017 (day of year)") +
  ylab("Leafing time in later years (day of year)") +
  labs(col = "Year") +
  theme_classic() +
  scale_color_brewer(palette = "RdBu") +
  theme(legend.position = "bottom")
