df_neon_rank <- df_neon_npn$metric %>% 
  filter(genus == "Quercus") %>%
  group_by(site, genus, id, event, year) %>% 
  summarise(doy=median(doy)) %>% 
  mutate(year = case_when(year ==2018~"former",
                          year ==2019~"later")) %>% 
  drop_na(year) %>% 
  spread(key = "year", value = "doy") %>% 
  drop_na()
  

p_neon_rank<- ggplot(df_neon_rank)+
  geom_point(aes(x = former, y = later, col = site), alpha=0.25)+
  geom_smooth(aes(x = former, y = later, col = site), method = "lm", se = F)+
  guides(col = "none")+
  theme_classic()+
  facet_wrap(.~event)
  

df_neon_rank_reg <- df_neon_rank %>% 
  group_by(event, site) %>%
  filter(n() >= 10) %>%
  do(broom::tidy(lm(later ~ former, .))) %>%
  filter(term %in% c("former")) %>%
  dplyr::select(-statistic, -term) %>% 
  mutate(sig = gtools::stars.pval(p.value))

# nlme::lme(later~former, random = ~1|site, data = df_neon_rank %>% filter(event =="flower")) %>% summary()






read_rds( "data/processed/neon_doy.rds") %>% 
  left_join(df_plant, by =c("id", "site")) %>% 
  filter(str_detect(sciname, "Quercus rubra")) %>% 
  as_tibble() %>% 
  filter(doy>0) %>% 
  filter(year!=2019|doy>20) %>% 
  filter(thres==0.5) %>% 
  select(id, doy, year, site) %>% 
  spread(key = "year", value="doy") %>% 
  ggplot()+
  geom_jitter(aes(x=`2018`, y=`2019`))+
  geom_smooth(aes(x=`2018`, y=`2019`), method="lm")+
  ggpubr::stat_cor(aes(x=`2018`, y=`2019`, group=site))+
  facet_wrap(.~site, scales="free")+
  # geom_abline(intercept = 0, slope = 1, col="red")+
  theme_classic()