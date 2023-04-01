p_dt_corr_2017 <- flower_doy_df %>%
  filter(thres %in% c(0.4, 0.5, 0.6)) %>%
  left_join(df_dt_flower_peak %>% dplyr::distinct(id, peak = doy) %>% filter(is.finite(peak)), by = "id") %>%
  ggplot() +
  geom_jitter(aes(x = peak, y = doy, group = interaction(direction, thres), col = thres %>% as.factor()), alpha = 1) +
  geom_smooth(aes(x = peak, y = doy, group = interaction(direction, thres), col = thres %>% as.factor()), method = "lm") +
  geom_abline(slope = 1, intercept = 0, col = "red") +
  ggpubr::stat_cor(
    aes(
      x = peak, y = doy, group = interaction(direction, thres),
      col = thres %>% as.factor(),
      label = paste(..r.label.., ..p.label.., sep = "*`,`~")
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
