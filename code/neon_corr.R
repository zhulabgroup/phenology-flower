df_doy_ps<- read_rds(str_c(.path$dat_other,"neon_doy.rds")) %>% 
  filter(site %in% v_site_neon) %>% 
  filter(thres == 0.5) %>% 
  select(site, id, year, ps = doy)

df_doy_neon <- ls_df_neon_npn$metric %>% 
  filter(site %in% v_site_neon) %>% 
  # filter(event =="leaf") %>% 
  group_by(site,id, year, genus, species, event) %>% 
  summarise(doy=median(doy)) %>% 
  rename(neon = doy) %>% 
  left_join(ls_df_neon_npn$taxa, by = c("genus", "species")) %>% 
  filter ( genus %in% v_taxa_short | family %in% v_taxa_short) %>% 
  mutate(taxa = case_when (genus %in% v_taxa_short ~ genus,
                           family %in% v_taxa_short ~ family)) 
  
df_neon_ps<- inner_join(df_doy_ps,df_doy_neon, by = c("site", "id", "year") ) %>% 
  group_by(taxa) %>% 
  filter(n()>=30) %>% 
  ungroup()%>% 
  filter(!taxa %in% c("Ambrosia", "Poaceae"))

p_neon_ps_corr <- ggplot(df_neon_ps )+
  geom_point(aes(x = neon, y = ps, col=site), alpha = 0.25)+
  geom_smooth(aes(x = neon, y = ps, group =site, col=site), method = "lm", se=F, linewidth = 0.5)+
  geom_smooth(aes(x = neon, y = ps), method = "lm", se=F)+
  ggpubr::stat_cor(aes(x = neon, y = ps))+
  # geom_abline(intercept = 0, slope = 1, col="red")+
  theme_classic()+
  guides(col = "none")+
  facet_wrap(.~event*taxa, scales = "free", nrow =2)
