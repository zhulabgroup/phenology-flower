library(geosphere)
lat_oi<-meta_df %>% filter(site==siteoi) %>% pull(lat) %>% mean()
lon_oi<-meta_df %>% filter(site==siteoi) %>% pull(lon)%>% mean()

if (is.na(lat_oi)) {
  lat_oi<-median(trees_genus_df$lat)
  lon_oi<-median(trees_genus_df$lon)
}

npn_df_tree<-read_rds(paste0("/data/ZHULAB/phenology/NPN/", taxaoi_short, ".rds")) %>% 
  filter(phenophase_status==1) %>% 
  filter(phenophase_description %in% c("Full pollen release (conifers)", "Pollen release (conifers)", "Pollen cones (conifers)", "Open pollen cones (conifers)","Full flowering (50%)","Flowers or flower buds","Pollen release (flowers)"))

if (nrow(npn_df_tree>0)) {
  npn_df_tree<-npn_df_tree %>% 
    rowwise() %>% 
    mutate(distance=distm(c(longitude, latitude), c(lon_oi, lat_oi), fun = distHaversine) %>% as.numeric()) %>%  # distance in the unit of m
    ungroup() %>% 
    filter(distance<=500000) %>% 
    dplyr::select(site_id,date=observation_date, lon=longitude, lat=latitude, doy=day_of_year) %>% 
    mutate(date=as.Date(date))
} else {
  npn_df_tree=data.frame(site_id=integer(0), lon=numeric(0), lat=numeric(0), doy=integer(0), date=character(0)) %>% mutate(date=as.Date(date))
}


# npn_df_coord<-npn_df_tree %>% 
#   distinct(site_id, lon, lat ) %>% 
#   mutate(id=row_number())
# 
# npn_df_tree<-npn_df_tree %>% 
#   left_join(npn_df_coord, by=c("site_id", "lon", "lat"))
# # trees_npn_sp<-SpatialPoints(npn_df_coord[, c("lon","lat")],
# #                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

npn_df<-npn_df_tree %>% 
  group_by(date) %>% 
  summarise(count=n()) %>% 
  ungroup()

# p_npn_flower<-
#     ggplot()+
#     geom_segment(data=npn_flower_start_df, aes(x = observation_date,xend= observation_date,y=0,yend=1),color="darkorchid",alpha=1/2)+
#     ylim(0,1)+
#     theme_classic()+
#     theme(axis.line.y=element_blank(),
#           axis.title=element_blank(),
#           axis.text.y=element_blank(),
#           axis.ticks.y=element_blank(),
#           axis.ticks.length.y = unit(0, "pt"),
#           plot.margin=grid::unit(c(0,0,0,0),"cm"))+
#     theme(axis.text=element_text(size=12),
#           legend.title=element_text(size=12))#+
#   # draw_plot_label(label="F2 USA-NPN flower onset",x = mindate, y = 1, hjust = 0, vjust = 1, fontface = "bold", size=16)
#   p_npn_flower