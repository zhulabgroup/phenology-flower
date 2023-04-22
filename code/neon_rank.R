df_rank_neon <- ls_df_neon_npn$metric %>% 
  filter(site %in% v_site_neon) %>% 
  left_join(ls_df_neon_npn$taxa, by = c("genus", "species")) %>% 
  filter ( genus %in% v_taxa_short | family %in% v_taxa_short) %>% 
  mutate(taxa = case_when (genus %in% v_taxa_short ~ genus,
                           family %in% v_taxa_short ~ family)) %>% 
  group_by(site, taxa, id, event, year) %>% 
  summarise(doy=median(doy)) %>% 
  mutate(year = case_when(year ==2018~"former",
                          year ==2019~"later")) %>% 
  drop_na(year) %>% 
  spread(key = "year", value = "doy") %>% 
  drop_na() %>% 
  group_by(taxa) %>% 
  filter(n()>=30) %>% 
  ungroup()%>% 
  filter(!taxa %in% c("Ambrosia", "Poaceae")) %>% 
  mutate(taxasite = str_c(taxa,site))
  

p_rank_neon<- ggplot(df_rank_neon)+
  geom_point(aes(x = former, y = later, col = site), alpha=0.25)+
  geom_smooth(aes(x = former, y = later, col = site), method = "lm", se = F)+
  guides(col = "none")+
  theme_classic()+
  facet_wrap(.~event*taxa, nrow = 2, scales = "free")
  

df_rank_neon_reg <- df_rank_neon %>% 
  group_by( taxa, event,site) %>%
  filter(n() >= 10) %>%
  do(broom::tidy(lm(later ~ former, .))) %>%
  filter(term %in% c("former")) %>%
  dplyr::select(-statistic, -term) %>% 
  mutate(sig = gtools::stars.pval(p.value)) %>% 
  ungroup()






df_rank_ps <- read_rds( str_c(.path$dat_other,"neon_doy.rds")) %>% 
  filter(site %in% v_site_neon) %>% 
  left_join( ls_df_neon_npn$metric %>% distinct(site, id, genus, species), by =c("id", "site")) %>% 
  left_join(ls_df_neon_npn$taxa, by = c("genus", "species")) %>% 
  filter ( genus %in% v_taxa_short | family %in% v_taxa_short) %>% 
  mutate(taxa = case_when (genus %in% v_taxa_short ~ genus,
                           family %in% v_taxa_short ~ family)) %>% 
  filter(doy>0) %>%
  # filter(year!=2019|doy>20) %>% 
  filter(thres==0.5) %>% 
  select(id, doy, year, site, taxa) %>% 
  mutate(year = case_when(year ==2018~"former",
                          year ==2019~"later")) %>% 
  drop_na(year) %>% 
  spread(key = "year", value = "doy") %>% 
  drop_na() %>% 
  group_by(taxa) %>% 
  filter(n()>=30) %>% 
  ungroup()%>% 
  filter(!taxa %in% c("Ambrosia", "Poaceae")) %>% 
  mutate(taxasite = str_c(taxa,site))


p_rank_ps<- ggplot(df_rank_ps)+
  geom_point(aes(x = former, y = later, col = site), alpha=0.25)+
  geom_smooth(aes(x = former, y = later, col = site), method = "lm", se = F)+
  guides(col = "none")+
  theme_classic()+
  facet_wrap(.~taxa, scales = "free")


df_rank_ps_reg <- df_rank_ps %>% 
  group_by(taxa,site) %>%
  filter(n() >= 10) %>%
  do(broom::tidy(lm(later ~ former, .))) %>%
  filter(term %in% c("former")) %>%
  dplyr::select(-statistic, -term) %>% 
  mutate(sig = gtools::stars.pval(p.value)) %>% 
  ungroup()

df_rank_compare <- inner_join(df_rank_neon_reg %>% filter(event =="leaf") %>% select(-event),
                              df_rank_ps_reg,
                              by = c("site", "taxa"),
                              suffix = c("_neon", "_ps")) %>% 
  mutate(ps_better = case_when ((estimate_neon > 0 & estimate_ps > 0 & p.value_ps < p.value_neon) | (estimate_neon < 0 & estimate_ps > 0) ~T,
                                TRUE ~F)) 
df_rank_compare_summ <- df_rank_compare %>% 
  group_by(ps_better) %>% 
  summarise(n = n())
