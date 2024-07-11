# Land cover data downloaded from Sentinel-2 Land Use/ Land Cover Downloader
# https://www.arcgis.com/apps/instant/media/index.html?appid=fc92d38533d440078f17678ebc20e8e2
grass_path<-"/data/ZHULAB/phenology/LULC/"

grass_df_city_list<-vector(mode="list")
for (siteoi in site_list) {
  trees_df_city<-trees_df %>% 
    filter(site==siteoi) %>% 
    drop_na(lon, lat)
  bbox         = extent(min(trees_df_city$lon),max(trees_df_city$lon),min(trees_df_city$lat),max(trees_df_city$lat))
  
  grass_df_city_year_list<-vector(mode="list")
  for (yearoi in year_list) {
    files<-list.files(paste0(grass_path, siteoi),pattern=paste0(yearoi, "0101-"), full.names = T)
    if (length(files)>1) { # in case city is on the boundary of two tiles. not used for our sites.
      ras_list<-vector(mode="list")
      for (file in files) {
        ras_list[[file]]<-raster(file)
      }
      ras <- do.call(merge, ras_list)
    } else {
      ras<-raster(files)
    }
    
    # crop to city
    bbox_sp <- as(bbox, 'SpatialPolygons')  
    projection(bbox_sp)<-"+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    bbox_reproj<-spTransform(bbox_sp, proj4string(ras))
    ras_cr<-crop(ras, bbox_reproj)
    # plot(ras_cr)
    
    # get coordinates of grass
    grass_df_year<-as.data.frame(ras_cr, xy=T) %>% 
      `colnames<-`(c("x", "y", "class")) %>% 
      filter(class==11) %>%
      dplyr::select(-class) %>% 
      mutate(year=yearoi)
    grass_df_city_year_list[[yearoi %>% as.character()]]<-grass_df_year
    # ggplot()+
    #   geom_point(data=trees_df_city, aes(x=lon, y=lat), col="dark green")+
    #   geom_point(data=grass_df_city_year, aes(x=lon, y=lat), col="yellow green")+
    #   theme_classic()
    print(paste0(siteoi, yearoi))
  }
  grass_df_city<-bind_rows(grass_df_city_year_list) %>% 
    mutate(grass=1) %>% 
    spread(key="year", value="grass") %>% 
    drop_na() %>% 
    dplyr::select(x, y) %>% 
    sample_n(min(10000, nrow(.))) 
    
    # reproject
    grass_sp_reproj<-SpatialPoints(grass_df_city[, c("x","y")],
                                   proj4string = CRS(proj4string(ras)))
  grass_sp<-spTransform(grass_sp_reproj,
                        CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  grass_df_city<-coordinates(grass_sp) %>% 
    as_tibble() %>% 
    `colnames<-`(c("lon", "lat"))%>% 
    mutate(site=siteoi) %>% 
    mutate(id=row_number()) %>% 
    mutate(family="Poaceae") %>% 
    mutate(genus="Unknown") %>% 
    mutate(genus_id=999)
  grass_df_city_list[[siteoi]]<-grass_df_city
}
grass_df<-bind_rows(grass_df_city_list)
write_rds(grass_df, "./RS4flower/data/grass.rds")

grass_df<-read_rds("./RS4flower/data/grass.rds")
