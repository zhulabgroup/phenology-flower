library(tidyverse)
library(planetR) # https://github.com/bevingtona/planetR
library(rgdal)
library(lubridate)
library(stringr)
library(httr)

source("./patch for planetR.R")

# Read in tree file
trees_df <- read_csv("./data/juniper_ashei/tree_dates_coords_220930.csv") %>%
  mutate(id=row_number()) %>% 
  distinct( id,species = "Juniper ashei", lon=x, lat=y)  %>%
  as_tibble()

summary(trees_df)


# whole extent is too big so separating into areas
# https://catalog.data.gov/dataset/tiger-line-shapefile-2017-state-texas-current-county-subdivision-state-based
area_sp<-readOGR(dsn = "./data/juniper_ashei/tl_2017_48_cousub/tl_2017_48_cousub.shp")
plot(area_sp)
area_sp_tran<-spTransform(area_sp,CRS("+proj=longlat +datum=WGS84"))

point_sp <- SpatialPoints(trees_df[,c("lon", "lat")],
                          proj4string=CRS("+proj=longlat +datum=WGS84"))

indices <- over(point_sp, area_sp_tran)
trees_df<-trees_df %>% 
  mutate(site=indices$NAME)

ggplot(trees_df )+
  geom_path(data = map_data("county"), aes(x = long, y = lat, group = group), color = "grey", alpha = 0.5) +
  geom_point(aes(x=lon, y=lat, col=site), pch=21, cex=3)+
  # geom_label(aes(x=lon, y=lat, label=id))+
  theme_classic()+
  ylim(29,33)+
  xlim(-101,-96.5)+
  coord_equal()

site_list<-trees_df %>% pull(site) %>% unique() 

# Set API
api_key = "REMOVED" #ysong67
# api_key <- "REMOVED" # xcui12

ps_path <- "./data/PS/TX_DK/" #change to your path

# order images
for (siteoi in site_list) {
  ps_path_site <- paste0(ps_path, siteoi, "/")
  dir.create(ps_path_site, recursive = T)
  dir.create(paste0(ps_path_site, "orders/"), recursive = T)
  
  # Set AOI (many ways to set this!) ultimately just need an extent()
  plant_df_site <- trees_df %>%
    filter(site == siteoi) %>%
    drop_na(lon, lat)
  bbox <- extent(min(plant_df_site$lon), max(plant_df_site$lon), min(plant_df_site$lat), max(plant_df_site$lat))
  
  for (year_download in 2017:2022) {
    order_df <- data.frame(year = integer(0), month = integer(0), id = character(0), images=integer(0))
    for (month_download in 1:12) {
      # Date range of interest
      start_year <- year_download
      end_year <- year_download
      date_start <- lubridate::floor_date(as.Date(paste0(year_download, "-", str_pad(month_download, 2, pad = "0"), "-01")), unit = "month")
      date_end <- lubridate::ceiling_date(as.Date(paste0(year_download, "-", str_pad(month_download, 2, pad = "0"), "-01")), unit = "month") - 1
      start_doy <- as.numeric(format(date_start, "%j"))
      end_doy <- as.numeric(format(date_end, "%j"))
      
      # Create order name
      order_name <- paste(siteoi, item_name, product, start_year, end_year, start_doy, end_doy, sep = "_")
      
      # Planet Orders API
      
      out<-tryCatch({
        images<-planet_search_new(
          api_key = api_key,
          bbox = bbox,
          date_start = date_start,
          date_end = date_end,
          # start_doy = start_doy,
          # end_doy = end_doy,
          cloud_lim = 1,
          ground_control = T,
          quality = "standard",
          item_name = "PSScene",
          # product_bundle="analytic_sr_udm2",
          asset="ortho_analytic_4b_sr"#,
          # order_name = order_name,
          # mostrecent = 0
        )
      },
      error = function(e){ 
        return (numeric(0))
      })
      if (length(out)>0) {
        item_num <- length(images)
        group_num <- ceiling(item_num / 500)
        split_num <- ceiling(31 / group_num)
        doy_group <- split(seq(date_start, date_end, by="day"), floor((seq(1:(end_doy - start_doy + 1)) -0.0001)/ split_num))
        for (g in 1:length(doy_group)) {
          order_id <- planet_order_request_new(
            api_key = api_key,
            bbox = bbox,
            date_start = date_start,
            date_end = date_end,
            # start_doy = min(doy_group[[g]]),
            # end_doy = max(doy_group[[g]]),
            cloud_lim = 1,
            ground_control=T,
            quality="standard",
            item_name = "PSScene",
            product_bundle="analytic_sr_udm2",
            asset="ortho_analytic_4b_sr",
            order_name = order_name,
            mostrecent = 0
          )
          if (!is.null(order_id)) {
            order_df <- order_df %>%
              bind_rows(data.frame(year = year_download, month = month_download, id = order_id,images=item_num))
          }
        }
      }
        
        
      
      
      print(paste(year_download, ", ", month_download))
    }
    dir.create(paste0(ps_path_site, "orders/"))
    write_rds(order_df, paste0(ps_path_site, "orders/", "order_", year_download, ".rds"))
  }
}

# download
for (siteoi in site_list) {
  ps_path_site <- paste0(ps_path, siteoi, "/")
  for (year_download in 2017:2022) {
    order_df <- read_rds(paste0(ps_path_site, "orders/", "order_", year_download, ".rds"))
    cl <- makeCluster(nrow(order_df), outfile = "")
    registerDoSNOW(cl)
    foreach(
      i = 1:nrow(order_df),
      .packages = c("stringr", "planetR", "httr")
    ) %dopar% {
      
      # Get order id
      month_download <- order_df$month[i]
      order_id <- order_df$id[i]
      
      # Date range of interest
      start_year <- year_download
      end_year <- year_download
      date_start <- lubridate::floor_date(as.Date(paste0(year_download, "-", str_pad(month_download, 2, pad = "0"), "-01")), unit = "month")
      date_end <- lubridate::ceiling_date(as.Date(paste0(year_download, "-", str_pad(month_download, 2, pad = "0"), "-01")), unit = "month") - 1
      start_doy <- as.numeric(format(date_start, "%j"))
      end_doy <- as.numeric(format(date_end, "%j"))
      
      # Set/Create Export Folder
      order_name <- paste(siteoi, item_name, product, start_year, end_year, start_doy, end_doy, sep = "_")
      exportfolder <- paste0(ps_path_site, order_name)
      dir.create(exportfolder, recursive = T, showWarnings = F)
      
      # Download
      Sys.sleep(i * 0.5) # Otherwise sending request to API at the same time may cause error
      planet_order_download_new(order_id, exportfolder, api_key = api_key, order_num = i, overwrite_opt = FALSE)
      
      print(paste(year_download, ", ", month_download))
    }
    
    stopCluster(cl)
  }
}
