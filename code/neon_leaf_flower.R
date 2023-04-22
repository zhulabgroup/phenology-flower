df_neon_lf <- df_neon_npn$metric %>% 
  group_by(site, genus, id, event, year) %>% 
  summarise(doy=median(doy)) %>% 
  spread(key = "event", value = "doy") %>% 
  drop_na() %>% 
  group_by(genus) %>% 
  filter(n()>=100) %>% 
  ungroup()

p_neon_leaf_flower <- ggplot(df_neon_lf)+
  geom_point(aes(x = leaf, y = flower, col = site , group = site))+
  geom_smooth(aes(x = leaf, y = flower, col = site , group = site), method = "lm", se = F)+
  facet_wrap(.~genus, scales = "free")+
  theme_classic()+
  guides(col = "none")


df_neon_lf_reg <- df_neon_lf %>% 
  group_by(genus) %>%
  do(broom::tidy(lm(flower ~ leaf, .))) %>%
  filter(term %in% c("leaf")) %>%
  dplyr::select(-statistic, -term) %>% 
  mutate(sig = gtools::stars.pval(p.value))

df_neon_lf_lag <- df_neon_lf %>% 
  group_by(genus) %>%
  do(broom::tidy(lm(flower ~ leaf, .))) %>%
  filter(term %in% c("(Intercept)")) %>%
  dplyr::select(-statistic, -term) %>% 
  mutate(sig = gtools::stars.pval(p.value))
