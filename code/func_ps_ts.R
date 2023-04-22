
func_ps_batch_ts <- function(dir, tsdir, v_taxa, v_site) {
  if (is.null(v_site)) {
    v_site <- list.dirs(dir, recursive = F, full.names = F)
  }

  cl <- makeCluster(min(36, detectCores()), outfile = "")
  registerDoSNOW(cl)

  iscomplete <- F
  while (!iscomplete) { # restart when there is error, usually because of cluster connection issues
    iserror <- try(
      for (taxaoi in v_taxa) {
        taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
        for (s in 1:length(v_site)) {
          # get plant locations
          siteoi <- v_site[s]
          if ("genus" %in% colnames(df_plant)) {
            df_plant_taxa <- df_plant %>%
              filter(site == siteoi) %>%
              filter(genus == taxaoi_short | family == taxaoi_short) %>%
              mutate(id = row_number()) %>%
              drop_na(lon, lat)
          } else {
            df_plant_taxa <- df_plant %>%
              filter(site == siteoi) %>%
              filter(taxa == taxaoi_short) %>%
              mutate(id = row_number()) %>%
              drop_na(lon, lat)
          }

          v_id <- df_plant_taxa %>% pull(id)

          # skip when there are too few plants
          if (taxaoi_short %in% c("Ambrosia", "Poaceae", "all")) {
            min_sample_size <- 1
          } else {
            min_sample_size <- 1
          }

          if (nrow(df_plant_taxa) >= min_sample_size) {
            # plants as points
            sf_plant_taxa <- sf::st_as_sf(df_plant_taxa,
              coords = c("lon", "lat"),
              crs = sf::st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
            )

            if (!file.exists(str_c(tsdir, "ps_", siteoi, "_", taxaoi_short, ".rds"))) {
              # read reflectance data
              files <- list.files(path = str_c(dir, siteoi), pattern = ".*_SR_clip.tif$", recursive = T, full.names = T) %>% sort()
              nday <- length(files)
              df_ps <- foreach(
                f = 1:nday,
                .packages = c("terra", "sf", "tidyverse"),
                .combine = "rbind",
                .export = c("sf_plant_taxa")
              ) %dopar% {
                file <- files[f]
                ras_ps <- terra::rast(file)

                sf_plant_taxa_reproj <- sf::st_transform(sf_plant_taxa,
                  crs = sf::st_crs(ras_ps)
                )

                df_ps_f <- cbind(terra::extract(ras_ps, sf_plant_taxa_reproj) %>% select(-ID), f, id = v_id)

                print(str_c("sr: ", f, " out of ", nday))
                df_ps_f %>% drop_na()
              }

              # read quality assessment data
              # 0 - fully usable data
              # other - potentially problematic/unusable data
              #
              # Full description is in Planet's documentation (Page 91, Section 2. UNUSABLE DATA MASK FILE).
              files <- list.files(path = str_c(dir, siteoi), pattern = ".*_udm2_clip.tif$", recursive = T, full.names = T) %>% sort()
              nday <- length(files)
              df_ps_qa <- foreach(
                f = 1:nday,
                .packages = c("terra", "sf", "tidyverse"),
                .combine = "rbind",
                .export = c("sf_plant_taxa")
              ) %dopar% {
                file <- files[f]
                ras_ps_qa <- terra::rast(file)

                sf_plant_taxa_reproj <- sf::st_transform(sf_plant_taxa,
                  crs = sf::st_crs(ras_ps_qa)
                )

                df_ps_mask_f <- cbind(terra::extract(ras_ps_qa, sf_plant_taxa_reproj) %>% select(-ID), f, id = v_id)

                print(str_c("udm: ", f, " out of ", nday))
                df_ps_mask_f %>% drop_na()
              }

              # get corresponding timing from file names
              df_time <- list.files(path = str_c(dir, siteoi), pattern = ".*_SR_clip.tif$", recursive = T) %>%
                sort() %>%
                str_split(pattern = "/", simplify = T) %>%
                data.frame() %>%
                dplyr::select(filename = X2) %>%
                rowwise() %>%
                mutate(time = strptime(str_c(str_split(filename, pattern = "_")[[1]][1], str_split(filename, pattern = "_")[[1]][2]), format = "%Y%m%d%H%M%OS")) %>%
                ungroup() %>%
                mutate(f = row_number()) %>%
                dplyr::select(-filename)

              # assign id to each plant
              df_coord <- sf::st_coordinates(sf_plant_taxa) %>%
                as_tibble() %>%
                mutate(id = v_id) %>%
                rename(lon = X, lat = Y)

              # join data
              df_ps_full <- df_ps %>%
                left_join(df_ps_qa, by = c("id", "f")) %>%
                left_join(df_time, by = "f") %>%
                left_join(df_coord, by = "id") %>%
                mutate(
                  red = red * 0.0001, # scaling following Dixon et al's code
                  green = green * 0.0001,
                  blue = blue * 0.0001,
                  nir = nir * 0.0001
                ) %>%
                # mutate(evi=2.5* (nir-red) / (nir + 6*red - 7.5*blue + 1),
                #        gndvi=(nir-green)/(nir+green),
                #        ebi= (red + green + blue) / (green / blue * (red - blue + 1))) %>%
                dplyr::select(-f)

              # save
              write_rds(df_ps_full, str_c(tsdir, "ps_", siteoi, "_", taxaoi_short, ".rds"))
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
      cl <- makeCluster(min(36, detectCores()), outfile = "")
      registerDoSNOW(cl)
    }
  }
  stopCluster(cl)
}
