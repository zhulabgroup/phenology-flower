library(rnpn)
phenophases <- npn_phenophases()
species_list <- npn_species()

taxa_list
for (taxaoi in taxa_list) {
  spid<-species_list %>% 
    filter(genus==taxaoi_short|family_name==taxaoi_short) %>% 
    pull(species_id)
  
  npn_data <- npn_download_status_data(request_source='YS',years=c(1980:2022),species_id=spid)
  
  write_rds(npn_data, paste0("/data/ZHULAB/phenology/NPN/", taxaoi_short, ".rds"))
}

npn_data<-read_rds(paste0("/data/ZHULAB/phenology/NPN/", taxaoi_short, ".rds"))
flower_df<-npn_data %>% 
  filter(latitude>=25 & latitude<=50  & longitude < -60 & longitude> -130) %>% # focus on CONUS
  filter(phenophase_status==1) %>% 
  filter(phenophase_description %in% c("Full pollen release (conifers)", "Pollen release (conifers)", "Pollen cones (conifers)", "Open pollen cones (conifers)","Full flowering (50%)","Flowers or flower buds","Pollen release (flowers)")) %>% 
  mutate(year=as.numeric(substr(observation_date,1,4))) %>% 
  group_by(site_id, latitude, longitude, species_id,species,genus, individual_id) %>%
  summarize(doy=median(day_of_year)) %>% 
  mutate(doy=ifelse(doy>273, doy-365, doy))

if (!taxaoi_short %in% c("Ulmus")) {
  leaf_df<-npn_data %>% 
    filter(latitude>=25 & latitude<=50  & longitude < -60 & longitude> -130) %>% # focus on CONUS
    filter(phenophase_status==1) %>% 
    filter(phenophase_description %in% c("Breaking needle buds (conifers)", "Young needles (conifers)", "Breaking needle buds (deciduous)","Breaking leaf buds","Increasing leaf size" )) %>% 
    mutate(year=as.numeric(substr(observation_date,1,4))) %>% 
    group_by(site_id, latitude, longitude, species_id,species,genus, individual_id) %>%
    summarize(doy=median(day_of_year)) %>% 
    mutate(doy=ifelse(doy>273, doy-365, doy))
} else {
  leaf_df<-npn_data %>% 
    filter(latitude>=25 & latitude<=50  & longitude < -60 & longitude> -130) %>% # focus on CONUS
    filter(phenophase_status==1) %>% 
    filter(phenophase_description %in% c("Falling leaves" ,"Colored leaves")) %>% 
    mutate(year=as.numeric(substr(observation_date,1,4))) %>% 
    group_by(site_id, latitude, longitude, species_id,species,genus, individual_id) %>%
    summarize(doy=median(day_of_year)) %>% 
    mutate(doy=ifelse(doy>273, doy-365, doy))
}

flower_leaf_df<-inner_join(leaf_df, flower_df, by=c("site_id", "latitude", "longitude", "species_id", "species", "genus", "individual_id")) %>% 
  rename(leaf=doy.x, flower=doy.y)
ggplot(flower_leaf_df)+
  geom_point(aes(x=leaf, y=flower, col=species))+
  # geom_smooth(aes(x=leaf, y=flower, col=species, group=species), method="lm")+
  geom_smooth(aes(x=leaf, y=flower, col=species, group=species), method="rlm")+
  geom_abline(slope=1, intercept=0, col="red")+
  theme_classic()+
  coord_equal()+
  facet_wrap(.~species)

ggplot(flower_leaf_df)+
  geom_point(aes(x=leaf, y=flower, col=species))+
  # geom_smooth(aes(x=leaf, y=flower, col=species, group=species), method="lm")+
  geom_smooth(aes(x=leaf, y=flower), method="rlm")+
  geom_abline(slope=1, intercept=0, col="red")+
  # coord_equal()+
  theme_classic()

summary(res <- rlm(flower ~ leaf, data = flower_leaf_df))
plot(flower ~ leaf, data=flower_leaf_df)
abline(res<-lm(flower ~ leaf, data = flower_leaf_df), col="blue")
abline(res<-rlm(flower ~ leaf, data = flower_leaf_df), col="red")
predict(res, newdata=data.frame(leaf=c(100)))
