df_doy_ps <- read_rds(str_c(.path$dat_other, "neon_doy.rds")) %>%
  filter(site %in% v_site_neon) %>%
  filter(thres == 0.5) %>%
  select(site, id, year, ps = doy)

df_doy_neon <- ls_df_neon_npn$metric %>%
  filter(site %in% v_site_neon) %>%
  # filter(event =="leaf") %>%
  group_by(site, id, year, genus, species, event) %>%
  summarise(doy = median(doy)) %>%
  rename(neon = doy) %>%
  left_join(ls_df_neon_npn$taxa, by = c("genus", "species")) %>%
  filter(genus %in% v_taxa_short | family %in% v_taxa_short) %>%
  mutate(taxa = case_when(
    genus %in% v_taxa_short ~ genus,
    family %in% v_taxa_short ~ family
  ))

df_neon_ps <- inner_join(df_doy_ps, df_doy_neon, by = c("site", "id", "year")) %>%
  group_by(event, taxa) %>%
  filter(n() >= 100) %>%
  ungroup() %>%
  filter(!taxa %in% c("Ambrosia", "Poaceae")) %>%
  mutate(taxa_parse = case_when(
    !taxa %in% c("Cupressaceae", "Pinaceae", "Poaceae") ~ str_c("italic('", taxa, "')"),
    TRUE ~ taxa
  ))

p_neon_ps_corr_leaf <- df_neon_ps %>%
  filter(event == "leaf") %>%
  group_by(taxa) %>%
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
  guides(col = "none") +
  facet_wrap(. ~ taxa_parse, nrow = 1, labeller = label_parsed) +
  labs(
    x = "Day of 50% green-up (from PlanetScope)",
    y = "Day of leaf onset (from NEON)"
  ) +
  guides(linetype = "none")

df_neon_ps %>%
  filter(event == "flower") %>%
  group_by(taxa) %>%
  do(broom::tidy(cor.test(~ neon + ps, .)))

df_neon_ps %>%
  filter(event == "flower") %>%
  group_by(taxa, site) %>%
  filter(n() >= 10) %>%
  do(broom::tidy(cor.test(~ neon + ps, .))) %>%
  ungroup() %>%
  mutate(pos_sig = estimate > 0 & p.value <= 0.05) %>%
  summarise(
    total = n(),
    pos_sig = sum(pos_sig)
  )

p_neon_ps_corr_flower <- df_neon_ps %>%
  filter(!site %in% c("SJER", "SOAP", "BIGC", "TEAK", "TECR")) %>%
  filter(ps > 0 & ps <= 150) %>%
  filter(event == "flower") %>%
  group_by(taxa) %>%
  nest() %>%
  mutate(
    p_val = map(data, ~ lm(neon ~ ps, data = .)) %>%
      map_dbl(~ broom::glance(.) %>% pull(p.value))
  ) %>%
  unnest(cols = data) %>%
  ungroup() %>%
  filter(taxa != "Pinaceae") %>%
  # filter(taxa == "Quercus") %>%
  ggplot() +
  geom_point(aes(x = ps, y = neon, col = site), alpha = 0.25) +
  # geom_smooth(aes(x = ps, y = neon, group = site, col = site), method = "lm", se = F, linewidth = 0.5) +
  geom_smooth(aes(x = ps, y = neon, linetype = ifelse(p_val <= 0.05, "sig", "ns")), method = "lm", se = T) +
  scale_linetype_manual(values = c("sig" = "solid", "ns" = "dashed")) +
  ggpubr::stat_cor(aes(x = ps, y = neon)) +
  theme_classic() +
  # guides(col = "none") +
  facet_wrap(. ~ taxa_parse, labeller = label_parsed, scales = "free") +
  labs(
    x = "Day of 50% green-up (from PlanetScope)",
    y = "Day of flower onset\n(from NEON)",
    col = "Site"
  ) +
  guides(
    linetype = "none",
    col = guide_legend(ncol = 2)
  )
