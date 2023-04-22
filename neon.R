# process npn neon doy
df_npn_neon<-read_rds("data/processed/NPN_NEON.rds")

df_neon_leaf <- df_npn_neon %>%
  filter(phenophase_id %in% ( npn_phenophases %>% 
           filter(pheno_class_id==1) %>% 
           pull(phenophase_id))) %>% 
  select(site, genus, species,  id=plant_nickname  , year = first_yes_year, doy = first_yes_doy ) %>% 
  drop_na()%>% 
  mutate(year = as.numeric(year), 
         doy = as.numeric(doy)) %>% 
  mutate (year = if_else(doy>273, (year+1),year),
          doy = if_else(doy>273,(doy-365),doy)
  )  %>% 
  rename(neon_leaf = doy)


df_neon_flower <- df_npn_neon %>%
  filter(phenophase_id %in% ( npn_phenophases %>% 
                                filter(pheno_class_id==7) %>% 
                                pull(phenophase_id))) %>% 
  select(site, genus, species,  id=plant_nickname  , year = first_yes_year, doy = first_yes_doy ) %>% 
  mutate(year = as.numeric(year), 
         doy = as.numeric(doy)) %>% 
  mutate (year = if_else(doy>273, (year+1),year),
          doy = if_else(doy>273,(doy-365),doy)
  )  %>% 
  rename(neon_flower = doy)

df_neon_flower_leaf<-full_join(df_neon_leaf, df_neon_flower, by =c("site", "genus", "species", "id", "year")) %>% 
  left_join(df_plant, by = c("site", "id"))


df_neon_flower_leaf %>% 
  ggplot()+
  geom_point(aes(x = lat, y = lon, col= species))+
  guides(col = "none")+
  facet_wrap(.~site, scales = "free")

df_neon_flower_leaf %>%
  drop_na() %>%
  ggplot()+
  geom_point(aes(x = neon_leaf, y = neon_flower, col= year))+
  facet_wrap(.~str_c(genus, " ",species))

df_neon_flower_leaf %>% 
  filter(genus =="Quercus") %>% 
  drop_na(neon_leaf) %>% 
  filter(neon_leaf <200) %>% 
  filter(year >= 2018, year <=2022) %>% 
  ggplot()+
  geom_jitter(aes(x = year, y = neon_leaf, col=species))+
  guides(col = "none")+
  facet_wrap(.~site)





# correlation
df_doy_ps<- read_rds( "data/processed/neon_doy.rds") %>% 
  rename(ps_leaf = doy)

df_neon_ps<- inner_join(df_doy_ps,df_neon_flower_leaf, by = c("site", "id", "year") )

ggplot(df_neon_ps %>% 
         # filter(year ==2022) %>%
         # filter(site=="SJER") %>%
         # filter(genus %in% c("Quercus"
         #                     # , "Acer"
         #                     # # , "Erodium"  , "Bromus", "Aralia"
         #                     # , "Betula"
         #                     )) %>%
         filter(thres==0.5) 
       )+
  geom_jitter(aes(x = neon_leaf, y = ps_leaf, col=site))+
  geom_smooth(aes(x = neon_leaf, y = ps_leaf, group =site, col=site), method = "lm", se=F)+
  ggpubr::stat_cor(aes(x = neon_leaf, y = ps_leaf))+
  geom_abline(intercept = 0, slope = 1, col="red")+
  # facet_wrap(.~site, scales = "free")+
  theme_classic()






# # neon status intensity
# 
# ls_df_status<-vector(mode = "list")
# for (siteoi in v_site) {
#   ls_df_status [[siteoi]] <- ls_df_neon[[siteoi]]$status %>% 
#     filter(phenophaseName=="Colored leaves") %>% 
#     filter(phenophaseStatus=="yes") %>% 
#     filter(phenophaseIntensity=="25-49%") %>% 
#     mutate(year = lubridate::year(date),
#            doy = lubridate::yday(date)) %>% 
#     group_by(id =individualID, year) %>% 
#     summarise(doy  = median(doy)) %>% 
#     select(id ,year, doy) %>% 
#     mutate(site = siteoi)
# }
# df_status <- bind_rows(ls_df_status) %>% 
#   rename(neon_leaf=doy)
# 
# # correlation
# df_doy_ps<- read_rds( "data/processed/neon_doy_down.rds") %>% 
#   rename(ps_leaf = doy)
# 
# df_neon_ps<- left_join(df_doy_ps,df_status, by = c("site", "id", "year") )
# 
# ggplot(df_neon_ps %>% 
#          filter(year ==2018) %>%
#          filter(site=="HARV") %>%
#          # filter(genus %in% c("Quercus"
#          #                     # , "Acer"
#          #                     # # , "Erodium"  , "Bromus", "Aralia"
#          #                     # , "Betula"
#          #                     )) %>%
#          filter(thres==0.5) 
# )+
#   geom_jitter(aes(x = neon_leaf, y = ps_leaf, col=site))+
#   geom_smooth(aes(x = neon_leaf, y = ps_leaf, group =site, col=site), method = "lm", se=F)+
#   ggpubr::stat_cor(aes(x = neon_leaf, y = ps_leaf))+
#   geom_abline(intercept = 0, slope = 1, col="red")+
#   # facet_wrap(.~site, scales = "free")+
#   theme_classic()


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

df_neon_flower_leaf %>% 
  filter(genus =="Quercus", species =="rubra") %>% 
  select(id, doy = neon_leaf, year, site)%>% 
  group_by(id, year, site) %>% 
  summarise(doy = median(doy)) %>% 
  ungroup() %>% 
  spread(key = "year", value="doy") %>% 
  ggplot()+
  geom_jitter(aes(x=`2018`, y=`2019`))+
  geom_smooth(aes(x=`2018`, y=`2019`), method="lm")+
  ggpubr::stat_cor(aes(x=`2018`, y=`2019`, group=site))+
  facet_wrap(.~site, scales="free")+
  theme_classic()

