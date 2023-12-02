df_neon_lf <- ls_df_neon_npn$metric %>%
  filter(site %in% v_site_neon) %>%
  left_join(ls_df_neon_npn$taxa, by = c("genus", "species")) %>%
  filter(genus %in% v_taxa_short | family %in% v_taxa_short) %>%
  mutate(taxa = case_when(
    genus %in% v_taxa_short ~ genus,
    family %in% v_taxa_short ~ family
  )) %>%
  group_by(site, taxa, id, event, year) %>%
  summarise(doy = median(doy)) %>%
  spread(key = "event", value = "doy") %>%
  drop_na() %>%
  group_by(taxa) %>%
  filter(n() >= 100) %>%
  ungroup() %>%
  mutate(taxa_parse = case_when(
    !taxa %in% c("Cupressaceae", "Pinaceae", "Poaceae") ~ str_c("italic('", taxa, "')"),
    TRUE ~ taxa
  )) %>%
  filter(!taxa %in% c("Ambrosia", "Poaceae", "Cupressaceae", "Pinaceae")) %>% 
  filter(site != "CLBJ") %>% 
  filter(flower < 150, leaf < 150)

p_neon_leaf_flower <- df_neon_lf %>%
  group_by(taxa) %>%
  nest() %>%
  mutate(
    p_val = map(data, ~ lm(flower ~ leaf, data = .)) %>%
      map_dbl(~ broom::glance(.) %>% pull(p.value))
  ) %>%
  unnest(cols = data) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = leaf, y = flower, col = site, group = site), alpha = 0.25) +
  # geom_smooth(aes(x = leaf, y = flower, col = site , group = site), method = "lm", se = F, linewidth = 0.5)+
  geom_smooth(aes(x = leaf, y = flower, linetype = ifelse(p_val <= 0.05, "sig", "ns")), method = "lm", se = T) +
  scale_linetype_manual(values = c("sig" = "solid", "ns" = "dashed")) +
  ggpubr::stat_cor(aes(x = leaf, y = flower)) +
  facet_wrap(. ~ taxa_parse, labeller = label_parsed, scales = "free") +
  theme_classic() +
  guides(
    # col = "none",
    linetype = "none",
    col = guide_legend(ncol = 2)
  ) +
  labs(
    x = "Day of leaf onset (from NEON)",
    y = "Day of flower onset (from NEON)",
    col = "Site"
  )


df_neon_lf_reg <- df_neon_lf %>%
  group_by(taxa) %>%
  do(broom::tidy(lm(flower ~ leaf, .))) %>%
  filter(term %in% c("leaf")) %>%
  dplyr::select(-statistic, -term) %>%
  mutate(sig = gtools::stars.pval(p.value))

df_neon_lf_lag <- df_neon_lf %>%
  group_by(taxa) %>%
  do(broom::tidy(lm(flower ~ leaf, .))) %>%
  filter(term %in% c("(Intercept)")) %>%
  dplyr::select(-statistic, -term) %>%
  mutate(sig = gtools::stars.pval(p.value))
