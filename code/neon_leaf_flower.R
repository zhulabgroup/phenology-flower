df_neon_lf <- df_neon_metric %>%
  spread(key = "event", value = "doy") %>%
  drop_na() %>%
  filter(flower <= 150, leaf <= 150) %>%
  group_by(genus, species) %>%
  filter(n() >= 50) %>%
  ungroup() %>%
  group_by(genus, species) %>%
  nest() %>%
  mutate(
    p_val = map(data, ~ lm(flower ~ leaf, data = .)) %>%
      map_dbl(~ broom::glance(.) %>% pull(p.value))
  ) %>%
  unnest(cols = data) %>%
  ungroup() %>%
  right_join(df_neon_ps_corr_flower %>% distinct(species_parse))

p_neon_leaf_flower <- df_neon_lf %>%
  ggplot() +
  geom_point(aes(x = leaf, y = flower, col = site, group = site), alpha = 0.25) +
  geom_smooth(aes(x = leaf, y = flower, linetype = ifelse(p_val <= 0.05, "sig", "ns")), method = "lm", se = T) +
  scale_linetype_manual(values = c("sig" = "solid", "ns" = "dashed")) +
  ggpubr::stat_cor(
    aes(
      x = leaf, y = flower,
      label = paste(after_stat(rr.label), after_stat(p.label), sep = "*`,`~")
    ),
    p.accuracy = 0.05,
    label.x.npc = "left",
    label.y.npc = "top",
    show.legend = F
  ) +
  facet_wrap(. ~ species_parse, labeller = label_parsed, nrow = 2) +
  ggthemes::theme_few() +
  guides(
    linetype = "none",
    col = "none"
  ) +
  labs(
    x = "Day of leaf onset (from NEON)",
    y = "Day of flower onset (from NEON)",
    col = "Site"
  )

df_neon_lf_reg <- df_neon_lf %>%
  group_by(genus, species) %>%
  do(broom::tidy(lm(flower ~ leaf, .))) %>%
  filter(term %in% c("leaf")) %>%
  dplyr::select(-statistic, -term) %>%
  mutate(sig = gtools::stars.pval(p.value))

if (.fig_save) {
  ggsave(
    plot = p_neon_leaf_flower,
    filename = str_c(.path$output, "supp/supp_neon_leaf_flower.pdf"),
    width = 10,
    height = 6,
    device = pdf
  )
}
