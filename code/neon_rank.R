df_rank_neon <- ls_df_neon_npn$metric %>%
  filter(site %in% v_site_neon) %>%
  left_join(ls_df_neon_npn$taxa, by = c("genus", "species")) %>%
  filter(genus %in% v_taxa_short | family %in% v_taxa_short) %>%
  mutate(taxa = case_when(
    genus %in% v_taxa_short ~ genus,
    family %in% v_taxa_short ~ family
  )) %>%
  group_by(site, taxa, id, event, year) %>%
  summarise(doy = median(doy)) %>%
  mutate(year = case_when(
    year == 2018 ~ "former",
    year == 2019 ~ "later"
  )) %>%
  drop_na(year) %>%
  spread(key = "year", value = "doy") %>%
  drop_na() %>%
  group_by(taxa) %>%
  filter(n() >= 100) %>%
  ungroup() %>%
  filter(!taxa %in% c("Ambrosia", "Poaceae")) %>%
  mutate(taxasite = str_c(taxa, site)) %>%
  mutate(taxa_parse = case_when(
    !taxa %in% c("Cupressaceae", "Pinaceae", "Poaceae") ~ str_c("italic('", taxa, "')"),
    TRUE ~ taxa
  ))


p_rank_neon_leaf <- df_rank_neon %>%
  filter(event == "leaf") %>%
  group_by(taxa, site) %>%
  nest() %>%
  mutate(
    p_val = map(data, ~ lm(later ~ former, data = .)) %>%
      map_dbl(~ broom::glance(.) %>% pull(p.value))
  ) %>%
  unnest(cols = data) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = former, y = later, col = site), alpha = 0.25) +
  geom_smooth(aes(x = former, y = later, col = site, linetype = ifelse(p_val <= 0.05, "sig", "ns")), method = "lm", se = F) +
  scale_linetype_manual(values = c("sig" = "solid", "ns" = "dashed")) +
  guides(
    col = "none",
    linetype = "none"
  ) +
  theme_classic() +
  facet_wrap(. ~ taxa_parse, nrow = 1, labeller = label_parsed) +
  labs(
    x = "Day of leaf onset in 2018 (from NEON)",
    y = "Day of leaf onset in 2019 (from NEON)",
  )

p_rank_neon_flower <- df_rank_neon %>%
  filter(event == "flower") %>%
  group_by(taxa, site) %>%
  nest() %>%
  mutate(
    p_val = map(data, ~ lm(later ~ former, data = .)) %>%
      map_dbl(~ broom::glance(.) %>% pull(p.value))
  ) %>%
  unnest(cols = data) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = former, y = later, col = site), alpha = 0.25) +
  geom_smooth(aes(x = former, y = later, col = site, linetype = ifelse(p_val <= 0.05, "sig", "ns")), method = "lm", se = F) +
  scale_linetype_manual(values = c("sig" = "solid", "ns" = "dashed")) +
  guides(
    col = "none",
    linetype = "none"
  ) +
  theme_classic() +
  facet_wrap(. ~ taxa_parse, nrow = 1, labeller = label_parsed) +
  labs(
    x = "Day of flower onset in 2018 (from NEON)",
    y = "Day of flower onset in 2019 (from NEON)",
  )


df_rank_neon_reg <- df_rank_neon %>%
  group_by(taxa, event, site) %>%
  filter(n() >= 10) %>%
  do(broom::tidy(lm(later ~ former, .))) %>%
  filter(term %in% c("former")) %>%
  dplyr::select(-statistic, -term) %>%
  mutate(sig = gtools::stars.pval(p.value)) %>%
  ungroup()






df_rank_ps_neon <- read_rds(str_c(.path$dat_other, "neon_doy.rds")) %>%
  filter(site %in% v_site_neon) %>%
  left_join(ls_df_neon_npn$metric %>% distinct(site, id, genus, species), by = c("id", "site")) %>%
  left_join(ls_df_neon_npn$taxa, by = c("genus", "species")) %>%
  filter(genus %in% v_taxa_short | family %in% v_taxa_short) %>%
  mutate(taxa = case_when(
    genus %in% v_taxa_short ~ genus,
    family %in% v_taxa_short ~ family
  )) %>%
  filter(doy > 0) %>%
  # filter(year!=2019|doy>20) %>%
  filter(thres == 0.5) %>%
  select(id, doy, year, site, taxa) %>%
  mutate(year = case_when(
    year == 2018 ~ "former",
    year == 2019 ~ "later"
  )) %>%
  drop_na(year) %>%
  spread(key = "year", value = "doy") %>%
  drop_na() %>%
  group_by(taxa) %>%
  filter(n() >= 100) %>%
  ungroup() %>%
  filter(!taxa %in% c("Ambrosia", "Poaceae")) %>%
  mutate(taxasite = str_c(taxa, site)) %>%
  mutate(taxa_parse = case_when(
    !taxa %in% c("Cupressaceae", "Pinaceae", "Poaceae") ~ str_c("italic('", taxa, "')"),
    TRUE ~ taxa
  ))


p_rank_ps_neon <- df_rank_ps_neon %>%
  group_by(taxa, site) %>%
  nest() %>%
  mutate(
    p_val = map(data, ~ lm(later ~ former, data = .)) %>%
      map_dbl(~ broom::glance(.) %>% pull(p.value))
  ) %>%
  unnest(cols = data) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = former, y = later, col = site), alpha = 0.25) +
  geom_smooth(aes(x = former, y = later, col = site, linetype = ifelse(p_val <= 0.05, "sig", "ns")), method = "lm", se = F) +
  scale_linetype_manual(values = c("sig" = "solid", "ns" = "dashed")) +
  guides(
    col = "none",
    linetype = "none"
  ) +
  theme_classic() +
  facet_wrap(. ~ taxa_parse, nrow = 1, labeller = label_parsed) +
  labs(
    x = "Day of 50% green-up in 2018 (from PS)",
    y = "Day of 50% green-up in 2019 (from PS)",
  )


df_rank_ps_reg <- df_rank_ps_neon %>%
  group_by(taxa, site) %>%
  filter(n() >= 10) %>%
  do(broom::tidy(lm(later ~ former, .))) %>%
  filter(term %in% c("former")) %>%
  dplyr::select(-statistic, -term) %>%
  mutate(sig = gtools::stars.pval(p.value)) %>%
  ungroup()

df_rank_compare <- inner_join(df_rank_neon_reg %>% filter(event == "leaf") %>% select(-event),
  df_rank_ps_reg,
  by = c("site", "taxa"),
  suffix = c("_neon", "_ps")
) %>%
  mutate(ps_better = case_when(
    (estimate_neon > 0 & estimate_ps > 0 & p.value_ps < p.value_neon) | (estimate_neon < 0 & estimate_ps > 0) ~ T,
    TRUE ~ F
  ))
df_rank_compare_summ <- df_rank_compare %>%
  group_by(ps_better) %>%
  summarise(n = n())
