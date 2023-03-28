cl <- makeCluster(20, outfile = "")
registerDoSNOW(cl)

iscomplete <- F
while (!iscomplete) { # restart when there is error, usually because of cluster connection issues
  iserror <- try(
    for (taxaoi in taxa_list) {
      taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
      for (s in 1:length(site_list)) {
        # get plant locations
        siteoi <- site_list[s]
        plant_taxa_df <- plant_df %>%
          filter(site == siteoi) %>%
          filter(genus == taxaoi_short | family == taxaoi_short) %>%
          mutate(id = row_number()) %>%
          drop_na(lon, lat)
        
        # skip when there are too few plants
        if (taxaoi_short %in% c("Ambrosia", "Poaceae")) {
          min_sample_size <- 1
        } else {
          min_sample_size <- 1
        }
        
        if (nrow(plant_taxa_df) >= min_sample_size) {
          # plants as points
          plant_taxa_sp <- SpatialPoints(plant_taxa_df[, c("lon", "lat")],
                                         proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
          )
          
          if (!file.exists(paste0(ps_path, "ts/ps_", siteoi, "_", taxaoi_short, ".rds"))) {
            # read reflectance data
            files <- list.files(path = paste0(ps_path, siteoi), pattern = ".*_SR_clip.tif$", recursive = T, full.names = T) %>% sort()
            nday <- length(files)
            nloc <- length(plant_taxa_sp)
            ps_mat <- foreach(
              f = 1:nday,
              .packages = c("raster"),
              .combine = "rbind"
            ) %dopar% {
              file <- files[f]
              ps_st <- stack(file)
              
              trees_sp_reproj <- spTransform(plant_taxa_sp, CRSobj = CRS(proj4string(ps_st)))
              
              ps_values <- cbind(raster::extract(ps_st, trees_sp_reproj), f, id = 1:nloc)
              print(paste0(f, " out of ", nday))
              ps_values[complete.cases(ps_values), ]
            }
            
            # read quality assessment data
            # 0 - fully usable data
            # other - potentially problematic/unusable data
            #
            # Full description is in Planet's documentation (Page 91, Section 2. UNUSABLE DATA MASK FILE).
            files <- list.files(path = paste0(ps_path, siteoi), pattern = ".*_udm2_clip.tif$", recursive = T, full.names = T) %>% sort()
            nday <- length(files)
            nloc <- length(plant_taxa_sp)
            ps_mask_mat <- foreach(
              f = 1:nday,
              .packages = c("raster"),
              .combine = "rbind"
            ) %dopar% {
              file <- files[f]
              ps_ras <- raster(file)
              
              trees_sp_reproj <- spTransform(plant_taxa_sp, CRSobj = CRS(proj4string(ps_ras)))
              
              ps_values <- cbind(qa = raster::extract(ps_ras, trees_sp_reproj), f, id = 1:nloc)
              
              print(paste0(f, " out of ", nday))
              ps_values[complete.cases(ps_values), ]
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
            coord_df <- coordinates(plant_taxa_sp) %>%
              as_tibble() %>%
              mutate(id = row_number())
            
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
            write_rds(ps_df, paste0(ps_path, "ts/ps_", siteoi, "_", taxaoi_short, ".rds"))
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
    cl <- makeCluster(20, outfile = "")
    registerDoSNOW(cl)
  }
}
stopCluster(cl)