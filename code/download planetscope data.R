# install.packages("remotes")
# remotes::install_github("bevingtona/planetR")
library(planetR)

#####
planet_order_download_new <- function(order_id, order_name, api_key, order_num, overwrite_opt=FALSE) {
  #GET order for download
  #If you lose the order_id, don't redo the request, log onto planet and find it in the orders menu
  #order_id for example SMV2 order: "dab92990-ce3a-456c-8ad6-ca0c569b4a1a"
  url2 = paste0("https://api.planet.com/compute/ops/orders/v2/", order_id)
  
  get_order <- httr::GET(url = url2,
                         username = api_key)
  #Download links are in here, under _links>results>location
  get_content <- httr::content(get_order)
  #When state = 'success', ready for download
  
  #check if order is ready
  while (get_content$state != "success") {
    print("Order still being proccessed, trying again in 60 seconds...")
    print(get_content$state)
    Sys.sleep(60)
    get_order <- httr::GET(url = url2, username = api_key)
    get_content <- httr::content(get_order)
  }
  
  ##Time to download!
  print("Starting download")
  
  #First create download folder:
  dir.create(order_name, showWarnings = F)
  
  #Download each item in order
  for (i in 1:length(get_content$`_links`$results)) {
    print(paste0("Order ", order_num, ", Download: ", round(100 * (
      i / length(get_content$`_links`$results)
    ), 2), "%"))
    #find item names in order contents
    name <- get_content$`_links`$results[[i]]$name
    findslash <- gregexpr("/", name)
    startchar <- findslash[[1]][length(findslash[[1]])] + 1
    filename <- substr(name, startchar, nchar(name))
    
    download_url <- get_content$`_links`$results[[i]]$location
    
    try({
      httr::RETRY(
        "GET",
        url = download_url,
        username = api_key,
        write_disk(
          path = paste(order_name, filename, sep = "/"),
          overwrite = overwrite_opt
        )
      )
    })
    
  }
  
  print(paste0("Download complete"))
  print(paste0("Items located in ", order_name))
  
}

#### LIBRARIES ####
unloadNamespace("RJSONIO") # conflict with jsonlite
library(planetR)
library(httr)
library(jsonlite)
library(raster)
library(stringr)
library(sf)

#### USER VARIABLES ####
# Set API
# api_key = "REMOVED" #ysong67
api_key = "REMOVED" #xcui12

# Metadata filters
cloud_lim    = 0.99 # percent from 0-1
item_name    = "PSScene4Band" 
# PSOrthoTile, PSScene3Band, PSScene4Band, Sentinel2L1C 
# (see https://developers.planet.com/docs/data/items-assets/)
product      = "analytic_sr" 
# analytic_b1, analytic_b2 
# (see https://developers.planet.com/docs/data/items-assets/)

for (siteoi in site_list) {
  path_data<-paste0("/data/ZHULAB/phenology/PlanetScope/", siteoi,"/")
  dir.create(path_data, recursive = T)
  dir.create(paste0(path_data, "orders/"), recursive = T)
  
  # Set AOI (many ways to set this!) ultimately just need an extent()
  # # OPTION 1: Import feature
  # my_aoi       = read_sf("/data/ZHULAB/phenology/PlanetScope/analyses/sj_boundary/Urban_Growth_Boundary.shp") # KML, SHP, SQLITE, or other
  # st_crs(my_aoi)
  # my_aoi_longlat<-st_transform(my_aoi, CRS("+proj=longlat +datum=WGS84"))
  # bbox         = extent(my_aoi_longlat)
  # # OPTION 2: Digitize om map
  # my_aoi       = mapedit::editMap() # Set in GUI
  # bbox         = extent(my_aoi)
  # # OPTION 3: Set bounding box manually
  plant_df_city<-plant_df %>% 
    filter(site==siteoi) %>% 
    drop_na(lon, lat)
  bbox         = extent(min(plant_df_city$lon),max(plant_df_city$lon),min(plant_df_city$lat),max(plant_df_city$lat))
  
  # bbox         = extent(-83.3, -82.8,42.2,42.5) # for DT
  
  for (year_download in 2017:2022) {
    order_df<-data.frame(year=integer(0), month=integer(0), id=character(0))
    if(year_download==2022) {
      month_end<-4
    } else {
      month_end<-12
    }
    for (month_download in 1:month_end)  {
      # Date range of interest
      start_year = year_download
      end_year   = year_download
      date_start = lubridate::floor_date(as.Date(paste0(year_download, '-',str_pad(month_download,2, pad="0"),'-01')), unit = "month")
      date_end   = lubridate::ceiling_date(as.Date(paste0(year_download, '-',str_pad(month_download,2, pad="0"),'-01')), unit = "month")-1
      start_doy  = as.numeric(format(date_start,"%j"))
      end_doy    = as.numeric(format(date_end,"%j"))
      
      
      # Create order name
      order_name<-paste(siteoi, item_name, product, start_year, end_year, start_doy, end_doy, sep = "_")
      
      # Planet Orders API
      order_id<-planetR:::planet_order_request(
        api_key = api_key, 
        bbox = bbox, 
        date_start = date_start, 
        date_end = date_end, 
        start_doy = start_doy, 
        end_doy = end_doy, 
        cloud_lim = cloud_lim, 
        item_name = item_name, 
        product = product,
        order_name = order_name
      )
      if (!is.null(order_id)){
        order_df<-order_df %>% 
          bind_rows(data.frame(year=year_download, month=month_download, id=order_id))
      } else {
        item_num<-nrow(planet_search(bbox, start_doy, end_doy, date_end, 
                                     date_start, cloud_lim, item_name, api_key))
        group_num<-ceiling(item_num/500)
        split_num<-ceiling(30/group_num)
        doy_group<-split(seq(start_doy, end_doy), floor(seq(1:(end_doy-start_doy+1))/split_num))
        for (g in 1:length(doy_group)) {
          order_id<-planetR:::planet_order_request(
            api_key = api_key, 
            bbox = bbox, 
            date_start = date_start, 
            date_end = date_end, 
            start_doy = min(doy_group[[g]]), 
            end_doy = max(doy_group[[g]]), 
            cloud_lim = cloud_lim, 
            item_name = item_name, 
            product = product,
            order_name = order_name
          )
          if (!is.null(order_id)) {
            order_df<-order_df %>% 
              bind_rows(data.frame(year=year_download, month=month_download, id=order_id))
          }
        }
      }
      
      print(paste(year_download,", ", month_download))
    }
    dir.create(paste0(path_data, "orders/"))
    write_rds(order_df, paste0(path_data, "orders/","order_",year_download,".rds"))
  }
}

for (siteoi in site_list) {
  path_data<-paste0("/data/ZHULAB/phenology/PlanetScope/", siteoi,"/")
  for (year_download in 2017:2022) {
    order_df<-read_rds(paste0(path_data, "orders/","order_",year_download,".rds"))
    cl <- makeCluster(nrow(order_df), outfile = "")
    registerDoSNOW(cl)
    foreach (i = 1:nrow(order_df),
             .packages = c("stringr", "planetR","httr")) %dopar% {
               
               # year_download<-order_df$year[i]
               month_download<-order_df$month[i]
               order_id<-order_df$id[i]
               
               # Date range of interest
               start_year = year_download
               end_year   = year_download
               date_start = lubridate::floor_date(as.Date(paste0(year_download, '-',str_pad(month_download,2, pad="0"),'-01')), unit = "month")
               date_end   = lubridate::ceiling_date(as.Date(paste0(year_download, '-',str_pad(month_download,2, pad="0"),'-01')), unit = "month")-1
               start_doy  = as.numeric(format(date_start,"%j"))
               end_doy    = as.numeric(format(date_end,"%j"))
               
               
               # Set/Create Export Folder
               order_name<-paste(siteoi, item_name, product, start_year, end_year, start_doy, end_doy, sep = "_")
               exportfolder = paste0(path_data,order_name)
               dir.create(exportfolder,recursive = T, showWarnings = F)
               
               Sys.sleep(i*0.5) # Otherwise sending request to API at the same time may cause error
               planet_order_download_new(order_id, exportfolder, api_key = api_key, order_num=i, overwrite_opt=FALSE)
               
               print(paste(year_download,", ", month_download))
             }
    
    stopCluster(cl)
  }
}

