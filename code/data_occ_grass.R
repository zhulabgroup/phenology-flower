# Land cover data manually downloaded from Sentinel-2 Land Use/ Land Cover Downloader
# https://www.arcgis.com/apps/instant/media/index.html?appid=fc92d38533d440078f17678ebc20e8e2
df_tree <- read_rds("./data/occurrence/street_trees_20230327.rds")
path_grass <- "./data/occurrence/LULC/"

cl <- makeCluster(length(v_site), outfile = "")
registerDoSNOW(cl)

ls_df_grass_city <- foreach(
  siteoi = v_site,
  .packages = c("tidyverse", "raster")
) %dopar% {
  # get extent from tree inventory
  df_tree_city <- df_tree %>%
    filter(site == siteoi) %>%
    drop_na(lon, lat)
  bbox <- extent(min(df_tree_city$lon), max(df_tree_city$lon), min(df_tree_city$lat), max(df_tree_city$lat))

  ls_df_grass_year <- vector(mode = "list")
  for (yearoi in v_year) {
    # get file name(s) for each site and year
    files <- list.files(paste0(path_grass, siteoi), pattern = paste0(yearoi, "0101-"), full.names = T)

    # read raster(s)
    ras <- raster(files)

    # crop to city
    bbox_sp <- as(bbox, "SpatialPolygons")
    projection(bbox_sp) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
    bbox_reproj <- spTransform(bbox_sp, proj4string(ras))
    ras_cr <- crop(ras, bbox_reproj)

    # get coordinates of grass
    df_grass_year <- as.data.frame(ras_cr, xy = T) %>%
      `colnames<-`(c("x", "y", "class")) %>%
      filter(class == 11) %>%
      dplyr::select(-class) %>%
      mutate(year = yearoi)
    ls_df_grass_year[[yearoi %>% as.character()]] <- df_grass_year
    print(str_c(siteoi, ", ", yearoi))
  }
  set.seed(1)
  df_grass_city <- bind_rows(ls_df_grass_year) %>%
    mutate(grass = 1) %>% # choose pixels that are grass in all years
    spread(key = "year", value = "grass") %>%
    drop_na() %>%
    dplyr::select(x, y) %>%
    sample_n(min(10000, nrow(.))) # subset when there are too many grass pixels

  # reproject
  grass_sp_reproj <- SpatialPoints(df_grass_city[, c("x", "y")],
    proj4string = CRS(proj4string(ras))
  )
  grass_sp <- spTransform(
    grass_sp_reproj,
    CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
  )

  # get coordinates
  df_grass_city <- coordinates(grass_sp) %>%
    as_tibble() %>%
    `colnames<-`(c("lon", "lat")) %>%
    mutate(site = siteoi) %>%
    mutate(id = row_number()) %>%
    mutate(family = "Poaceae") %>%
    mutate(genus = "Unknown") %>%
    mutate(genus_id = 999)
  df_grass_city
}
stopCluster(cl)

df_grass <- bind_rows(ls_df_grass_city)
write_rds(df_grass, "./data/occurrence/grass.rds")
