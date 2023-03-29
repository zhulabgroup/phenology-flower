for (siteoi in site_list[2]) {
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
      order_name <- paste(siteoi, start_year, start_doy, end_doy, sep = "_")
      exportfolder <- paste0(ps_path_site, order_name)
      dir.create(exportfolder, recursive = T, showWarnings = F)

      # Download
      Sys.sleep(i * 0.5) # Otherwise sending request to API at the same time may cause error
      orderdone <- F
      while (!orderdone) {
        orderdone <- tryCatch(
          {
            planet_order_download_new(order_id, exportfolder, api_key = api_key, order_num = i, overwrite_opt = FALSE)
            orderdone <- T
          },
          error = function(e) {
            Sys.sleep(10)
            print("Sleep for 10 s.")
            return(F)
          }
        )
      }

      print(str_c(siteoi, ", ", year_download, ", ", month_download))
    }

    stopCluster(cl)
  }
}



















cl <- makeCluster(36, outfile = "")
registerDoSNOW(cl)

iscomplete <- F
while (!iscomplete) { # restart when there is error, usually because of cluster connection issues
  iserror <- try(
    for (taxaoi in taxa_list) {
      taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
      for (s in 3:3) {
        # get plant locations
        siteoi <- site_list[s]
        plant_taxa_df <- plant_df %>%
          filter(site == siteoi) %>%
          filter(genus == taxaoi_short | family == taxaoi_short) %>%
          mutate(id = row_number()) %>%
          drop_na(lon, lat)
        id_list <- plant_taxa_df %>% pull(id)
        
        # skip when there are too few plants
        if (taxaoi_short %in% c("Ambrosia", "Poaceae")) {
          min_sample_size <- 1
        } else {
          min_sample_size <- 1
        }
        
        if (nrow(plant_taxa_df) >= min_sample_size) {
          # plants as points
          plant_taxa_sf <- sf::st_as_sf(plant_taxa_df, 
                                        coords = c("lon","lat"),
                                        crs = sf::st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
          )
          
          if (!file.exists(paste0(ps_path, "ts/ps_", siteoi, "_", taxaoi_short, ".rds"))) {
            # read reflectance data
            files <- list.files(path = paste0(ps_path, siteoi), pattern = ".*_SR_clip.tif$", recursive = T, full.names = T) %>% sort()
            nday <- length(files)
            ps_mat <- foreach(
              f = 1:nday,
              .packages = c("terra", "sf", "tidyverse"),
              .combine = "rbind"
            ) %dopar% {
              file <- files[f]
              ps_st <- terra::rast(file)
              
              plant_sf_reproj <- sf::st_transform(plant_taxa_sf,
                                                  crs = sf::st_crs(ps_st))
              
              ps_values <- cbind(terra::extract(ps_st, plant_sf_reproj) %>% select(-ID), f, id = id_list) 
              
              print(str_c("sr: ", f, " out of ", nday))
              ps_values  %>% drop_na()
            }
            
            # read quality assessment data
            # 0 - fully usable data
            # other - potentially problematic/unusable data
            #
            # Full description is in Planet's documentation (Page 91, Section 2. UNUSABLE DATA MASK FILE).
            files <- list.files(path = paste0(ps_path, siteoi), pattern = ".*_udm2_clip.tif$", recursive = T, full.names = T) %>% sort()
            nday <- length(files)
            ps_mask_mat <- foreach(
              f = 1:nday,
              .packages = c("terra", "sf", "tidyverse"),
              .combine = "rbind"
            ) %dopar% {
              file <- files[f]
              ps_st <- terra::rast(file)
              
              plant_sf_reproj <- sf::st_transform(plant_taxa_sf,
                                                  crs = sf::st_crs(ps_st))
              
              ps_values <- cbind(terra::extract(ps_st, plant_sf_reproj)%>% select(-ID), f, id = id_list)
              
              print(str_c("udm: ", f, " out of ", nday))
              ps_values  %>% drop_na()
            }
            
            # get corresponding timing from file names
            time_df <- list.files(path = paste0(ps_path, siteoi), pattern = ".*_SR_clip.tif$", recursive = T) %>%
              sort() %>%
              str_split(pattern = "/", simplify = T) %>%
              data.frame() %>%
              dplyr::select(filename = X2) %>%
              rowwise() %>%
              mutate(time = strptime(paste0(str_split(filename, pattern = "_")[[1]][1], str_split(filename, pattern = "_")[[1]][2]), format = "%Y%m%d%H%M%OS")) %>%
              ungroup() %>%
              mutate(f = row_number()) %>%
              dplyr::select(-filename)
            
            # assign id to each plant
            coord_df <- sf::st_coordinates(plant_taxa_sf) %>%
              as_tibble() %>%
              mutate(id = row_number()) %>% 
              rename(lon = X, lat = Y)
            
            # join data
            ps_df <- ps_mat %>%
              as_tibble() %>%
              left_join(time_df, by = "f") %>%
              left_join(coord_df, by = "id") %>%
              mutate(
                red = red * 0.0001, # scaling following Dixon et al's code
                green = green * 0.0001,
                blue = blue * 0.0001,
                nir = nir * 0.0001
              ) %>%
              # mutate(evi=2.5* (nir-red) / (nir + 6*red - 7.5*blue + 1),
              #        gndvi=(nir-green)/(nir+green),
              #        ebi= (red + green + blue) / (green / blue * (red - blue + 1))) %>%
              left_join(ps_mask_mat %>% as_tibble(), by = c("id", "f")) %>%
              dplyr::select(-f)
            
            # save
            write_rds(ps_df, str_c(ps_path, "ts/ps_", siteoi, "_", taxaoi_short, ".rds"))
          }
        }
      }
    }
  )
  
  if (class(iserror) != "try-error") {
    iscomplete <- T
  } else if (class(iserror) == "try-error") { # restart cluster
    iscomplete <- F
    closeAllConnections()
    cl <- makeCluster(36, outfile = "")
    registerDoSNOW(cl)
  }
}
stopCluster(cl)