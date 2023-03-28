for (siteoi in site_list) {
  ps_path_site <- paste0(ps_path, siteoi, "/")
  dir.create(ps_path_site, recursive = T)
  dir.create(paste0(ps_path_site, "orders/"), recursive = T)
  
  # Set AOI (many ways to set this!) ultimately just need an extent()
  plant_df_site <- plant_df %>%
    filter(site == siteoi) %>%
    drop_na(lon, lat)
  bbox <- extent(min(plant_df_site$lon), max(plant_df_site$lon), min(plant_df_site$lat), max(plant_df_site$lat))
  
  for (year_download in 2017:2022) {
    order_df <- data.frame(year = integer(0), month = integer(0), id = character(0), images = integer(0))
    for (month_download in 1:12) {
      # Date range of interest
      start_year <- year_download
      end_year <- year_download
      date_start <- lubridate::floor_date(as.Date(paste0(year_download, "-", str_pad(month_download, 2, pad = "0"), "-01")), unit = "month")
      date_end <- lubridate::ceiling_date(as.Date(paste0(year_download, "-", str_pad(month_download, 2, pad = "0"), "-01")), unit = "month") - 1
      start_doy <- as.numeric(format(date_start, "%j"))
      end_doy <- as.numeric(format(date_end, "%j"))
      
      # Create order name
      order_name <- paste(siteoi, start_year, start_doy, end_doy, sep = "_")
      
      # Planet Orders API
      out <- tryCatch(
        {
          images <- planet_search_new(
            api_key = api_key,
            bbox = bbox,
            date_start = date_start,
            date_end = date_end,
            cloud_lim = cloud_lim,
            ground_control = T,
            quality = "standard",
            item_name = item_name,
            asset = asset
          )
        },
        error = function(e) {
          return(numeric(0))
        }
      )
      if (length(out) > 0) {
        item_num <- length(images)
        group_num <- ceiling(item_num / 450)
        split_num <- ceiling(31 / group_num)
        date_group <- split(seq(date_start, date_end, by = "day"), floor((seq(1:(end_doy - start_doy + 1)) - 0.0001) / split_num))
        for (g in 1:length(date_group)) {
          orderdone <- F
          while (!orderdone) {
            orderdone <- tryCatch(
              {
                order_id <- planet_order_request_new(
                  api_key = api_key,
                  bbox = bbox,
                  date_start = date_group[[g]] %>% min(),
                  date_end = date_group[[g]] %>% max(),
                  cloud_lim = cloud_lim,
                  ground_control = T,
                  quality = "standard",
                  item_name = item_name,
                  asset = asset,
                  product_bundle = product_bundle,
                  order_name = order_name,
                  mostrecent = 0
                )
                orderdone <- T
              },
              error = function(e) {
                Sys.sleep(60)
                print("Sleep for 60 s.")
                return(F)
              }
            )
          }
          
          if (!is.null(order_id)) {
            order_df <- order_df %>%
              bind_rows(data.frame(year = year_download, month = month_download, id = order_id, images = item_num))
          }
        }
      }
      print(str_c(siteoi, ", ", year_download, ", ", month_download))
    }
    dir.create(paste0(ps_path_site, "orders/"))
    write_rds(order_df, paste0(ps_path_site, "orders/", "order_", year_download, ".rds"))
  }
}