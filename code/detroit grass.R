detroit_class<-read_csv("./RS4flower/data/ragweed_thinned_dataset.csv")
# rag_tif<-raster("./RS4flower/data/rag18_4m_de.tif")
# proj4string(rag_tif)

projection_dt<-"+proj=merc +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +R=6378137 +units=m +no_defs"

grass_df<-detroit_class %>% 
  dplyr::select(Ambrosia=ragp18, Poaceae=ground_4m_de,X=x, Y=y) %>% 
  gather(key="taxa", value="presence", -X, -Y) %>% 
  filter(!is.na(presence)&presence==1) %>% 
  mutate(family=case_when(taxa=="Ambrosia"~"Asteraceae",
                         TRUE~taxa)) %>% 
  mutate(genus=case_when(taxa=="Ambrosia"~taxa,
                         TRUE~"Unknown")) %>% 
  mutate(genus_id=case_when(taxa=="Ambrosia"~998,
                            taxa=="Poaceae"~999)) %>% 
  dplyr::select(-taxa, -presence) %>% 
  group_by(family) %>% 
  mutate(id=row_number()) %>% 
  ungroup()

pts<-SpatialPoints(grass_df[, c("X","Y")],
                   proj4string = CRS(projection_dt))
pts_reproj<-spTransform(pts,
                        CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

grass_df<-cbind(grass_df %>% dplyr::select(-X, -Y), coordinates(pts_reproj) ) %>%
  rename(lat=Y, lon=X)%>%
  mutate(site="DT")


write_rds(grass_df, "./RS4flower/data/detroit_grass.rds")

grass_df<-read_rds("./RS4flower/data/detroit_grass.rds")
plant_df<-bind_rows(trees_df, grass_df)
ggplot(plant_df %>% filter(site=="DT") %>% filter(genus=="Ambrosia"))+
  geom_point(aes(x=lon, y=lat, col=family))+
  geom_label_repel(aes(x=lon, y=lat, label=id))+
  theme_classic()+
  facet_wrap(.~family*genus)
