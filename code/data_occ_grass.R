# Land cover data manually downloaded from Sentinel-2 Land Use/ Land Cover Downloader
# https://www.arcgis.com/apps/instant/media/index.html?appid=fc92d38533d440078f17678ebc20e8e2
trees_df <- read_rds("./data/occurrence/street_trees_20230327.rds")
grass_path <- "./data/occurrence/LULC/"

cl <- makeCluster(length(site_list), outfile = "")
registerDoSNOW(cl)

grass_df_city_list <- foreach(
  siteoi = site_list,
  .packages = c("tidyverse", "raster")
) %dopar% {
  # get extent from tree inventory
  trees_df_city <- trees_df %>%
    filter(site == siteoi) %>%
    drop_na(lon, lat)
  bbox <- extent(min(trees_df_city$lon), max(trees_df_city$lon), min(trees_df_city$lat), max(trees_df_city$lat))
  
  grass_df_city_year_list <- vector(mode = "list")
  for (yearoi in year_list) {
    # get file name(s) for each site and year
    files <- list.files(paste0(grass_path, siteoi), pattern = paste0(yearoi, "0101-"), full.names = T)
    
    # read raster(s)
    ras <- raster(files)
    
    # crop to city
    bbox_sp <- as(bbox, "SpatialPolygons")
    projection(bbox_sp) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    bbox_reproj <- spTransform(bbox_sp, proj4string(ras))
    ras_cr <- crop(ras, bbox_reproj)
    
    # get coordinates of grass
    grass_df_year <- as.data.frame(ras_cr, xy = T) %>%
      `colnames<-`(c("x", "y", "class")) %>%
      filter(class == 11) %>%
      dplyr::select(-class) %>%
      mutate(year = yearoi)
    grass_df_city_year_list[[yearoi %>% as.character()]] <- grass_df_year
    print(paste0(siteoi, yearoi))
  }
  set.seed(1)
  grass_df_city <- bind_rows(grass_df_city_year_list) %>%
    mutate(grass = 1) %>% # choose pixels that are grass in all years
    spread(key = "year", value = "grass") %>%
    drop_na() %>%
    dplyr::select(x, y) %>%
    sample_n(min(10000, nrow(.))) # subset when there are too many grass pixels
  
  # reproject
  grass_sp_reproj <- SpatialPoints(grass_df_city[, c("x", "y")],
                                   proj4string = CRS(proj4string(ras))
  )
  grass_sp <- spTransform(
    grass_sp_reproj,
    CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  )
  
  # get coordinates
  grass_df_city <- coordinates(grass_sp) %>%
    as_tibble() %>%
    `colnames<-`(c("lon", "lat")) %>%
    mutate(site = siteoi) %>%
    mutate(id = row_number()) %>%
    mutate(family = "Poaceae") %>%
    mutate(genus = "Unknown") %>%
    mutate(genus_id = 999)
  grass_df_city
}
stopCluster(cl)

grass_df <- bind_rows(grass_df_city_list)
write_rds(grass_df, "./data/occurrence/grass.rds")