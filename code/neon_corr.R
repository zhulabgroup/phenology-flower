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
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = ps, y = neon, col = site), alpha = 0.25) +
  # geom_smooth(aes(x = ps, y = neon, group = site, col = site), method = "lm", se = F, linewidth = 0.5) +
  geom_smooth(aes(x = ps, y = neon, linetype = ifelse(p_val <= 0.05, "sig", "ns")), method = "lm", se = T) +
  scale_linetype_manual(values = c("sig" = "solid", "ns" = "dashed")) +
  ggpubr::stat_cor(aes(x = ps, y = neon)) +
  theme_classic() +
  facet_wrap(. ~ species_parse, labeller = label_parsed) +
  labs(
    x = "Day of 50% green-up (from PlanetScope)",
    y = "Day of leaf onset (from NEON)"
  ) +
  guides(
    linetype = "none",
    col = guide_legend(ncol = 2)
  )

# df_neon_ps %>%
#   filter(event == "flower") %>%
#   group_by(taxa) %>%
#   do(broom::tidy(cor.test(~ neon + ps, .)))
#
# df_neon_ps %>%
#   filter(event == "flower") %>%
#   group_by(taxa, site) %>%
#   filter(n() >= 10) %>%
#   do(broom::tidy(cor.test(~ neon + ps, .))) %>%
#   ungroup() %>%
#   mutate(pos_sig = estimate > 0 & p.value <= 0.05) %>%
#   summarise(
#     total = n(),
#     pos_sig = sum(pos_sig)
#   )

p_neon_ps_corr_flower <- inner_join(df_neon_metric %>% filter(event == "flower") %>% rename(neon = doy),
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
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = ps, y = neon, col = site), alpha = 0.25) +
  # geom_smooth(aes(x = ps, y = neon, group = site, col = site), method = "lm", se = F, linewidth = 0.5) +
  geom_smooth(aes(x = ps, y = neon, linetype = ifelse(p_val <= 0.05, "sig", "ns")), method = "lm", se = T) +
  scale_linetype_manual(values = c("sig" = "solid", "ns" = "dashed")) +
  ggpubr::stat_cor(aes(x = ps, y = neon)) +
  theme_classic() +
  facet_wrap(. ~ species_parse, labeller = label_parsed) +
  labs(
    x = "Day of 50% green-up (from PlanetScope)",
    y = "Day of flower onset (from NEON)"
  ) +
  guides(
    linetype = "none",
    col = guide_legend(ncol = 2)
  )
