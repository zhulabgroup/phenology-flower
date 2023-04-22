df_neon_lf <- ls_df_neon_npn$metric %>% 
  filter(site %in% v_site_neon) %>% 
  left_join(ls_df_neon_npn$taxa, by = c("genus", "species")) %>% 
  filter ( genus %in% v_taxa_short | family %in% v_taxa_short) %>% 
  mutate(taxa = case_when (genus %in% v_taxa_short ~ genus,
                           family %in% v_taxa_short ~ family)) %>% 
  group_by(site, taxa, id, event, year) %>% 
  summarise(doy=median(doy)) %>% 
  spread(key = "event", value = "doy") %>% 
  drop_na() %>% 
  group_by(taxa) %>% 
  filter(n()>=10) %>%
  ungroup() %>% 
  mutate(taxa_parse = case_when(
    !taxa %in% c("Cupressaceae", "Pinaceae", "Poaceae") ~ str_c("italic('", taxa, "')"),
    TRUE ~ taxa
  ))

p_neon_leaf_flower <- ggplot(df_neon_lf %>% 
                               filter(!taxa  %in% c("Ambrosia", "Poaceae")))+
  geom_point(aes(x = leaf, y = flower, col = site , group = site), alpha=0.25)+
  geom_smooth(aes(x = leaf, y = flower, col = site , group = site), method = "lm", se = F, linewidth = 0.5)+
  geom_smooth(aes(x = leaf, y = flower), method = "lm", se = T)+
  facet_wrap(.~taxa_parse, scales = "free", labeller = label_parsed)+
  theme_classic()+
  guides(col = "none")


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
