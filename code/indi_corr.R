set.seed(1)
p_dt_corr_2017 <- df_flower_doy %>%
  filter(year == 2017) %>%
  filter(thres %in% c(0.4, 0.5, 0.6)) %>%
  left_join(df_dt_flower_peak %>% dplyr::distinct(id, peak = doy) %>% filter(is.finite(peak)), by = "id") %>%
  ggplot() +
  geom_jitter(aes(x = peak, y = doy, group = interaction(direction, thres), col = thres %>% as.factor()), alpha = 0.75) +
  geom_smooth(aes(x = peak, y = doy, group = interaction(direction, thres), col = thres %>% as.factor()), method = "lm") +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  ggpubr::stat_cor(
    aes(
      x = peak, y = doy, group = interaction(direction, thres),
      col = thres %>% as.factor(),
      label = paste(after_stat(r.label), after_stat(p.label), sep = "*`,`~")
    ),
    p.accuracy = 0.05,
    label.x.npc = 0.5,
    label.y.npc = "top",
    show.legend = F
  ) +
  coord_equal() +
  xlim(c(110, 160)) +
  ylim(c(110, 160)) +
  xlab("Flowering time (day of year)") +
  ylab("Leafing time (day of year)") +
  labs(col = "Green-up threshold") +
  theme_classic() +
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "bottom")



p_dt_corr_years <- df_flower_doy %>%
  filter(year >= 2018) %>%
  filter(
    direction == "up",
    thres == 0.5
  ) %>%
  left_join(df_dt_flower_peak %>% dplyr::distinct(id, peak = doy) %>% filter(is.finite(peak)), by = "id") %>%
  ggplot() +
  geom_jitter(aes(x = peak, y = doy, group = as.factor(year), col = as.factor(year)), alpha = 0.75) +
  geom_smooth(aes(x = peak, y = doy, group = as.factor(year), col = as.factor(year)), method = "lm") +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  ggpubr::stat_cor(
    aes(
      x = peak, y = doy, group = as.factor(year),
      col = as.factor(year),
      label = paste(after_stat(r.label), after_stat(p.label), sep = "*`,`~")
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
