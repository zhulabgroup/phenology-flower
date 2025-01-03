# npn_phenophases <- rnpn::npn_phenophases()
npn_species <- rnpn::npn_species()

# download all NPN data for taxa studied
path_npn <- "./data/npn/"

cl <- makeCluster(length(v_site), outfile = "")
registerDoSNOW(cl)

ls_df_npn_taxa <- vector(mode = "list")
for (taxaoi_short in v_taxa_short %>% unique()) {
  if (!file.exists(str_c(path_npn, taxaoi_short, ".rds"))) {
    spid <- npn_species %>%
      filter(genus == taxaoi_short | family_name == taxaoi_short) %>%
      pull(species_id)

    df_npn_raw <- rnpn::npn_download_status_data(request_source = "YS", years = c(2000:2024), species_id = spid)

    write_rds(df_npn_raw, str_c(path_npn, taxaoi_short, ".rds"))
  } else {
    df_npn_raw <- read_rds(str_c(path_npn, taxaoi_short, ".rds"))
  }

  ls_df_npn_site <- foreach(
    siteoi = v_site,
    .packages = c("tidyverse", "geosphere")
  ) %dopar% {
    lat_oi <- df_meta %>%
      filter(site == siteoi) %>%
      pull(lat) %>%
      mean()
    lon_oi <- df_meta %>%
      filter(site == siteoi) %>%
      pull(lon) %>%
      mean()

    df_npn_flower <- df_npn_raw %>%
      # filter(phenophase_status == 1) %>%
      filter(phenophase_description %in% c("Full pollen release (conifers)", "Pollen release (conifers)", "Pollen cones (conifers)", "Open pollen cones (conifers)", "Full flowering (50%)", "Flowers or flower buds", "Pollen release (flowers)"))

    if (nrow(df_npn_flower > 0)) {
      df_npn_buff <- df_npn_flower %>%
        rowwise() %>%
        mutate(distance = geosphere::distm(c(longitude, latitude), c(lon_oi, lat_oi), fun = geosphere::distHaversine) %>% as.numeric()) %>% # distance in the unit of m
        ungroup() %>%
        filter(distance <= 500000) %>% # within 500 km of the NAB station
        dplyr::select(date = observation_date, status = phenophase_status, lon = longitude, lat = latitude, doy = day_of_year) %>%
        mutate(date = as.Date(date)) %>%
        mutate(site = siteoi)
    } else {
      df_npn_buff <- data.frame(lon = numeric(0), status = numeric(0), lat = numeric(0), doy = integer(0), date = character(0), site = character(0)) %>% mutate(date = as.Date(date))
    }

    df_npn_perc <- df_npn_buff %>%
      filter(status != -1) %>%
      group_by(site, date) %>%
      summarise(perc = mean(status)) %>%
      ungroup()

    print(str_c(taxaoi_short, ", ", siteoi))
    df_npn_perc
  }

  ls_df_npn_taxa[[taxaoi_short]] <- bind_rows(ls_df_npn_site) %>%
    mutate(taxa = taxaoi_short)
}
stopCluster(cl)
df_npn <- bind_rows(ls_df_npn_taxa)
write_rds(df_npn, "./data/processed/dat_npn.rds")
