phenophases <- npn_phenophases()
species_list <- npn_species()

# download all NPN data for taxa studied
npn_path <- "./data/NPN/"

cl <- makeCluster(length(site_list), outfile = "")
registerDoSNOW(cl)

npn_df_list <- vector(mode = "list")
for (taxaoi_short in taxa_short_list %>% unique()) {
  if (!file.exists(paste0(npn_path, taxaoi_short, ".rds"))) {
    spid <- species_list %>%
      filter(genus == taxaoi_short | family_name == taxaoi_short) %>%
      pull(species_id)
    
    npn_data <- npn_download_status_data(request_source = "YS", years = c(2000:2022), species_id = spid)
    
    write_rds(npn_data, paste0(npn_path, taxaoi_short, ".rds"))
  } else {
    npn_data <- read_rds(paste0(npn_path, taxaoi_short, ".rds"))
  }
  
  npn_taxa_df_list <- foreach(
    siteoi = site_list,
    .packages = c("tidyverse", "geosphere")
  ) %dopar% {
    lat_oi <- meta_df %>%
      filter(site == siteoi) %>%
      pull(lat) %>%
      mean()
    lon_oi <- meta_df %>%
      filter(site == siteoi) %>%
      pull(lon) %>%
      mean()
    
    npn_flower_df <- npn_data %>%
      filter(phenophase_status == 1) %>%
      filter(phenophase_description %in% c("Full pollen release (conifers)", "Pollen release (conifers)", "Pollen cones (conifers)", "Open pollen cones (conifers)", "Full flowering (50%)", "Flowers or flower buds", "Pollen release (flowers)"))
    
    if (nrow(npn_flower_df > 0)) {
      npn_flower_df <- npn_flower_df %>%
        rowwise() %>%
        mutate(distance = distm(c(longitude, latitude), c(lon_oi, lat_oi), fun = distHaversine) %>% as.numeric()) %>% # distance in the unit of m
        ungroup() %>%
        filter(distance <= 500000) %>% # within 500 km of the NAB station
        dplyr::select(date = observation_date, lon = longitude, lat = latitude, doy = day_of_year) %>%
        mutate(date = as.Date(date)) %>%
        mutate(site = siteoi)
    } else {
      npn_flower_df <- data.frame(lon = numeric(0), lat = numeric(0), doy = integer(0), date = character(0), site = character(0)) %>% mutate(date = as.Date(date))
    }
    
    npn_count_df <- npn_flower_df %>%
      group_by(site, date) %>%
      summarise(count = n()) %>%
      ungroup()
    
    print(paste0(taxaoi_short, ", ", siteoi))
    npn_count_df
  }
  
  npn_df_list[[taxaoi_short]] <- bind_rows(npn_taxa_df_list) %>%
    mutate(taxa = taxaoi_short)
}
stopCluster(cl)
npn_df <- bind_rows(npn_df_list)
write_rds(npn_df, "./data/processed/npn_dat.rds")