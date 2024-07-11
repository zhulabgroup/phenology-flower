library(raster)
library(sf)

tmean_ras <- raster("/data/ZHULAB/phenology/CHELSA//bio1.tif")
ppt_ras <- raster("/data/ZHULAB/phenology/CHELSA//bio12.tif")
vpdmax_ras <- raster("/data/ZHULAB/phenology/CHELSA//vpd_max.tif")

meta_sf<-meta_df %>%
  drop_na(site) %>%
  dplyr::select(site, lon, lat) %>%
  st_as_sf(coords=c("lon", "lat"),
           crs = 4326)

chelsa_df <- meta_sf%>%
  mutate(
    mat = raster::extract(tmean_ras, meta_sf),
    tap = raster::extract(ppt_ras, meta_sf),
    vpd = raster::extract(vpdmax_ras, meta_sf)
  ) %>%
  as_tibble() %>%
  dplyr::select(-geometry)
chelsa_df

# site_df<-meta_df %>% 
#   drop_na(site) %>% 
#   dplyr::select(site, lon, lat)
# if(!file.exists( "./RS4flower/data/daymet.rds")) {
#   library(daymetr)
#   daymet_df_list<-vector(mode="list", length=nrow(site_df))
#   for (r in 1:nrow(site_df)) {
#     daymet_df_list[[r]] <- download_daymet(site = site_df$site[r],
#                                            lat = site_df$lat[r],
#                                            lon = site_df$lon[r],
#                                            start = 2018,
#                                            end = 2021,
#                                            internal = TRUE,
#                                            simplify = TRUE) %>% 
#       mutate(date=as.Date(yday, origin = as.Date(paste0(year,"-01-01")))) %>% 
#       spread(key="measurement", value="value") %>% 
#       dplyr::select(date, tmax=`tmax..deg.c.`, tmin=`tmin..deg.c.`, prcp=`prcp..mm.day.`) %>% 
#       mutate(temp=(tmax+ tmin)/2) %>% 
#       dplyr::select(date, temp, prcp) %>% 
#       mutate(site=site_df$site[r])
#   }
#   daymet_df<-bind_rows(daymet_df_list) %>% 
#     mutate(year=format(date, "%Y") %>% as.numeric()) %>% 
#     group_by(year, site) %>% 
#     summarise(mat=mean(temp),
#               tap=sum(prcp))
#   write_rds(daymet_df, "./RS4flower/data/daymet.rds")
# }  else {
#   daymet_df<-read_rds("./RS4flower/data/daymet.rds")
# }


flower_freq_df_check_list<-vector(mode="list") 
for (taxaoi in taxa_list) {
  output_path<-paste0("./RS4flower/output/data/", taxaoi,"/")
  flower_freq_df_check_list[[taxaoi]]<-read_rds(paste0(output_path,"accuracy check.rds")) %>% 
    mutate(taxa=taxaoi)
}
flower_freq_df_check<-bind_rows(flower_freq_df_check_list)
best_thres<-flower_freq_df_check %>% group_by(taxa, thres) %>% summarise(rmse=median(rmse)) %>% arrange(rmse) %>% slice(1) %>% dplyr::select(taxa, thres)

# lag_clim_df<-flower_freq_df_check %>% 
#   dplyr::select(-rmse, -rmse_ps, -rmse_clim) %>% 
#   right_join(best_thres, by=c("taxa", "thres")) %>% 
#   left_join(daymet_df, by=c("region"="site", "year"="year")) 


taxa_chron<-c("Cupressaceae" , "Fraxinus","Ulmus early"  ,"Pinaceae" ,"Acer", "Populus","Quercus",   "Betula","Morus",  "Poaceae early","Poaceae late"  ,    "Ulmus late" ,  "Ambrosia" )

lag_clim_df<-flower_freq_df_check %>% 
  dplyr::select(-rmse, -rmse_ps, -rmse_clim) %>% 
  right_join(best_thres, by=c("taxa", "thres")) %>% 
  left_join(chelsa_df, by=c("region"="site")) %>% 
  mutate(taxa=as.factor(taxa)) %>% 
  mutate(taxa=fct_relevel(taxa, levels=taxa_chron))%>%
  mutate(group=case_when(taxa %in% c("Ulmus late", "Poaceae late", "Ambrosia")~"late",
                                                                              TRUE~"early"))

ggplot(lag_clim_df)+
  geom_point(aes(x=mat, y=lag, col=region, group=region))+
  geom_smooth(aes(x=mat, y=lag), method="lm", se=F)+
  geom_smooth(aes(x=mat, y=lag), method="lm", alpha=0.5)+
  theme_classic()+
  facet_wrap(.~taxa, scales = "free_y", ncol=5)

# ggplot(lag_clim_df%>% 
#          gather(key="var", value="value", -thres, -lag,  -region, -taxa))+
#   geom_point(aes(x=value, y=lag, col=taxa, group=taxa))+
#   geom_smooth(aes(x=value, y=lag, col=taxa, group=taxa), method="lm", se=F)+
#   geom_smooth(aes(x=value, y=lag), method="lm", alpha=0.5)+
#   theme_classic()+
#   facet_wrap(.~var, scales = "free_x")

reg.df<-lag_clim_df  %>% 
  # filter(region!="SJ") %>% 
  group_by(taxa) %>%
  do(broom::tidy(lm(lag ~ mat, .))) %>%
  filter(term %in% c("mat")) %>%
  dplyr::select( -statistic)
ggplot(reg.df)+
  geom_point(aes(x=taxa, y=estimate))+
  geom_errorbar(aes(x=taxa, ymin=estimate-1.95*std.error, ymax=estimate+1.95*std.error))+
  geom_hline(yintercept = 0, lty=2)+
  geom_vline(xintercept = 10.5, lty=1)+
  theme_classic()+
  facet_wrap(.~term, scales = "free_y")
summary(reg.df$p.value<0.05)

library(nlme)
lme.fit<-lme(lag ~ mat, random = ~ 1+mat|taxa, data=lag_clim_df %>% filter(group=="late"),control =lmeControl(opt='optim',optimMethod = "SANN"))
summary(lme.fit)

lme.fit<-lme(lag ~ mat, random = ~ 1+mat|taxa, data=lag_clim_df %>% filter(group=="early"),control =lmeControl(opt='optim',optimMethod = "SANN") )
summary(lme.fit)
