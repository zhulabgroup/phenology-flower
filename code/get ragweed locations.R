library(spocc)
library(sf)

ragweed_df_city_list<-vector(mode="list")
for (siteoi in site_list) {
  trees_df_city<-trees_df %>% 
    filter(site==siteoi) %>% 
    drop_na(lon, lat)
  bbox         = extent(min(trees_df_city$lon),max(trees_df_city$lon),min(trees_df_city$lat),max(trees_df_city$lat))
  bbox_sp <- as(bbox, 'SpatialPolygons')  
  projection(bbox_sp)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
  
  res <- occ(
    query = "Ambrosia", from = "gbif", has_coords = TRUE, limit = 1e6,
    geometry = st_bbox(bbox_sp),
    date = c(as.Date("2018-01-01"),as.Date("2021-12-31")),
    gbifopts = list(
      # occ_options(from = "gbif", where = "console")
      hasGeospatialIssue = FALSE
    )
  )
  ragweed_df_city<-res$gbif$data[[1]] %>% 
    dplyr::select(lon=longitude, lat=latitude, species) %>% 
    mutate(site=siteoi) %>% 
    mutate(family="Asteraceae") %>% 
    mutate(genus="Ambrosia") %>% 
    mutate(genus_id=998)
  # ggplot(ragweed_df_city)+
  #   geom_point(aes(x=lon, y=lat))
  ragweed_df_city_list[[siteoi]]<-ragweed_df_city
}
ragweed_df<-bind_rows(ragweed_df_city_list)
write_rds(ragweed_df, "./RS4flower/data/ragweed.rds")

ragweed_df<-read_rds("./RS4flower/data/ragweed.rds")
