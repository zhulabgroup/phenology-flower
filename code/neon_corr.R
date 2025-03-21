p_neon_ps_corr_leaf <- inner_join(df_neon_metric %>% filter(event == "leaf") %>% rename(neon = doy),
  df_neon_doy %>% filter(direction == "up", thres == 0.5) %>% rename(ps = doy),
  by = c("id", "year")
) %>%
  filter(neon <= 150) %>%
  group_by(genus, species) %>%
  filter(n() >= 50) %>%
  ungroup() %>%
  group_by(genus, species) %>%
  nest() %>%
  mutate(
    p_val = map(data, ~ lm(neon ~ ps, data = .)) %>%
      map_dbl(~ broom::glance(.) %>% pull(p.value))
  ) %>%
  unnest(cols = data) %>%
  ungroup()

p_neon_ps_corr_leaf <- p_neon_ps_corr_leaf %>%
  ggplot() +
  geom_point(aes(x = ps, y = neon, col = site), alpha = 0.25) +
  geom_smooth(aes(x = ps, y = neon, linetype = ifelse(p_val <= 0.05, "sig", "ns")), method = "lm", se = T) +
  scale_linetype_manual(values = c("sig" = "solid", "ns" = "dashed")) +
  ggpubr::stat_cor(
    aes(
      x = ps, y = neon,
      label = paste(after_stat(rr.label), after_stat(p.label), sep = "*`,`~")
    ),
    p.accuracy = 0.05,
    label.x.npc = "left",
    label.y.npc = "top",
    show.legend = F
  ) +
  ggthemes::theme_few() +
  facet_wrap(. ~ species_parse, labeller = label_parsed, nrow = 2) +
  labs(
    x = "Day of 50% green-up (from PlanetScope)",
    y = "Day of leaf onset (from NEON)",
    col = "Site"
  ) +
  guides(
    linetype = "none",
    col = "none", guide_legend(ncol = 2)
  )

df_neon_ps_corr_flower <- inner_join(df_neon_metric %>% filter(event == "flower") %>% rename(neon = doy),
  df_neon_doy %>% filter(direction == "up", thres == 0.5) %>% rename(ps = doy),
  by = c("id", "year")
) %>%
  filter(neon <= 150) %>%
  group_by(genus, species) %>%
  filter(n() >= 50) %>%
  ungroup() %>%
  group_by(genus, species) %>%
  nest() %>%
  mutate(
    p_val = map(data, ~ lm(neon ~ ps, data = .)) %>%
      map_dbl(~ broom::glance(.) %>% pull(p.value))
  ) %>%
  unnest(cols = data) %>%
  ungroup()

p_neon_ps_corr_flower <- df_neon_ps_corr_flower %>%
  ggplot() +
  geom_point(aes(x = ps, y = neon, col = site), alpha = 0.25) +
  geom_smooth(aes(x = ps, y = neon, linetype = ifelse(p_val <= 0.05, "sig", "ns")), method = "lm", se = T) +
  scale_linetype_manual(values = c("sig" = "solid", "ns" = "dashed")) +
  ggpubr::stat_cor(
    aes(
      x = ps, y = neon,
      label = paste(after_stat(rr.label), after_stat(p.label), sep = "*`,`~")
    ),
    p.accuracy = 0.05,
    label.x.npc = "left",
    label.y.npc = "top",
    show.legend = F
  ) +
  ggthemes::theme_few() +
  facet_wrap(. ~ species_parse, labeller = label_parsed, nrow = 2) +
  labs(
    x = "Day of 50% green-up (from PlanetScope)",
    y = "Day of flower onset (from NEON)",
    col = "Site"
  ) +
  guides(
    linetype = "none",
    col = "none"
  )

# save figure
if (.fig_save) {
  ggsave(
    plot = p_neon_ps_corr_flower,
    filename = str_c(.path$output, "main/main_neon_ps_corr_flower.pdf"),
    width = 10,
    height = 6,
    device = pdf
  )
}
