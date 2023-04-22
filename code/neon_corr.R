df_doy_ps<- read_rds( "data/processed/neon_doy.rds") %>% 
  rename(ps = doy)

df_doy_neon <- df_neon_npn$metric %>% 
  filter(event =="flower") %>% 
  group_by(site,id, year) %>% 
  summarise(doy=median(doy)) %>% 
  rename(neon = doy)
  
df_neon_ps<- inner_join(df_doy_ps,df_doy_neon, by = c("site", "id", "year") )

ggplot(df_neon_ps %>% 
         filter(thres==0.5) 
)+
  geom_jitter(aes(x = neon, y = ps, col=site))+
  geom_smooth(aes(x = neon, y = ps, group =site, col=site), method = "lm", se=F)+
  ggpubr::stat_cor(aes(x = neon, y = ps))+
  geom_abline(intercept = 0, slope = 1, col="red")+
  theme_classic()