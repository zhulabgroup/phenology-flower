for (taxaoi in taxa_list) {
  taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
  flower_window <- seq(flower_window_df %>% filter(taxa == taxaoi) %>% pull(start),
    flower_window_df %>% filter(taxa == taxaoi) %>% pull(end),
    by = 1
  )
  thres_df_taxa <- get_thres_taxa(thres_df, taxaoi)

  flower_freq_df <- read_rds(str_c("data/results/", taxaoi, "/flower_freq.rds"))
  for (s in 1:length(site_list)) {
    siteoi <- site_list[s]
    # plant_taxa_df <- plant_df %>%
    #   filter(site == siteoi) %>%
    #   filter(genus == taxaoi_short | family == taxaoi_short) %>%
    #   mutate(id = row_number()) %>%
    #   drop_na(lon, lat)

    if (nrow(plant_taxa_df) >= min_sample_size) {
      set.seed(1)
      random_id <<- sample(plant_taxa_df$id, min(2000, length(unique(plant_taxa_df$id)))) %>% sort()

      # preprocess ps data
      ps_df <- read_rds(paste0(ps_path, "ts/ps_", siteoi, "_", taxaoi_short, ".rds"))
      ps_df_proc <- ps_df %>%
        drop_na() %>%
        filter(id %in% random_id) %>%
        mutate(date = as.Date(time)) %>%
        mutate(
          year = format(time, "%Y") %>% as.integer(),
          doy = format(time, "%j") %>% as.integer(),
          hour = format(strptime(time, "%Y-%m-%d %H:%M:%S"), "%H") %>% as.integer()
        ) %>%
        filter(qa == 0) %>%
        group_by(id, lon, lat, date, year, doy) %>%
        summarise(
          blue = mean(blue),
          green = mean(green),
          red = mean(red),
          nir = mean(nir)
        ) %>%
        ungroup() %>%
        mutate(evi = 2.5 * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)) %>%
        # mutate(ndvi =  (nir - red) / (nir + red)) %>%
        filter(evi > 0, evi <= 1) %>%
        filter(red > 0, green > 0, blue > 0)

      # subset nab data
      pollen_df <- nab_with_taxa_df %>%
        left_join(meta_df %>% dplyr::select(id, site), by = "id") %>%
        filter(site == siteoi) %>%
        filter(genus == taxaoi_short | family == taxaoi_short)

      # subset npn data
      npn_df <- npn_df_all %>%
        filter(site == siteoi) %>%
        filter(taxa == taxaoi_short)

      ts_df <- ps_df_proc %>%
        left_join(plant_taxa_df, by = c("id", "lon", "lat")) %>%
        dplyr::select(id, date,
          `EVI (PS)` = evi # ,
          # `G2R (PS)`=g2r, `EBI (PS)`=ebi
        ) %>%
        mutate(id = as.factor(id)) %>%
        full_join(
          pollen_df %>%
            dplyr::select(date, `pollen count (NAB)` = count) %>%
            mutate(id = "pollen"),
          by = c("date", "id")
        ) %>%
        full_join(
          npn_df %>%
            dplyr::select(date, `flower observation (USA-NPN)` = count) %>%
            mutate(id = "npn"),
          by = c("date", "id")
        ) %>%
        arrange(id, date) %>%
        mutate(doy = format(date, "%j") %>% as.numeric()) %>%
        mutate(year = format(date, "%Y") %>% as.numeric())

      flower_freq_df_years_list <- vector(mode = "list", length = length(year_list))
      for (y in 1:length(year_list)) {
        yearoi <- year_list[y]

        flower_doy_df <- read_rds(str_c("data/results/", taxaoi, "/flower_doy.rds"))
        flower_freq_df_thres_list <- vector(mode = "list", length = nrow(thres_df_taxa))
        for (t in 1:nrow(thres_df_taxa)) {
          flower_freq_df_list[[t]] <- flower_doy_df %>%
            drop_na(start, end) %>%
            filter(
              direction == thres_df_taxa$direction[t],
              thres == thres_df_taxa$threshold[t]
            ) %>%
            group_by(doy, thres, direction) %>%
            summarise(count = n()) %>%
            ungroup() %>%
            mutate(freq = count / n())
        }
        flower_freq_thres_df <- bind_rows(flower_freq_df_thres_list) %>%
          mutate(doy = factor(doy, levels = c((274 - 365):(365 + 151)))) %>%
          complete(doy, thres, direction, fill = list(count = 0, freq = 0)) %>%
          mutate(doy = doy %>% as.character() %>% as.numeric()) %>%
          arrange(doy) %>%
          full_join(
            ts_df_subset %>%
              filter(id == "pollen") %>%
              filter(var == "pollen (NAB)") %>%
              filter(year == yearoi) %>%
              select(doy, pollen = value),
            by = "doy"
          ) %>%
          full_join(
            ts_df_subset %>%
              filter(id == "npn") %>%
              filter(var == "flower (NPN)") %>%
              filter(year == yearoi) %>%
              select(doy, npn = value),
            by = "doy"
          ) %>%
          full_join(
            ts_df_subset_summary %>%
              filter(var == "EVI (PS)") %>%
              filter(year == yearoi) %>%
              select(doy, evi),
            by = "doy"
          ) %>%
          mutate(doy = as.numeric(doy)) %>%
          mutate(site = siteoi, year = yearoi) %>%
          drop_na(doy, thres)

        print(paste0(siteoi, ", ", yearoi))

        flower_freq_df_years_list[[y]] <- flower_freq_thres_df %>%
          mutate(year = yearoi)
      }
      flower_freq_df_siteyears_list[[s]] <- bind_rows(flower_freq_df_years_list) %>%
        mutate(site = siteoi)
    }
  }

  flower_freq_df <- bind_rows(flower_freq_df_siteyears_list) %>%
    group_by(site, year, thres, direction) %>%
    mutate(freq_sm = whitfun(freq, 30)) %>%
    mutate(pollen_in = na.approx(pollen, doy, na.rm = F, maxgap = 14)) %>%
    mutate(pollen_fill = replace_na(pollen_in, 0)) %>%
    mutate(pollen_sm = whitfun(pollen_fill, 30)) %>%
    ungroup() %>%
    dplyr::select(-pollen_in, -pollen_fill) %>%
    mutate(pollen = case_when(doy %in% flower_window ~ pollen)) %>%
    mutate(pollen_sm = case_when(
      doy %in% flower_window ~ pollen_sm,
      TRUE ~ 0
    )) %>%
    mutate(npn = case_when(doy %in% flower_window ~ npn))

  # # pollen climatology (historical mean of smoothed pollen count)
  # pollen_clim <- flower_freq_df %>%
  #   group_by(site, direction, thres, doy) %>%
  #   summarise(pollen_clim = mean(pollen_sm, na.rm = T))
  #
  # flower_freq_df <- flower_freq_df %>%
  #   left_join(pollen_clim, by = c("site", "direction", "thres", "doy"))

  output_path <- paste0("data/results", taxaoi, "/")
  write_rds(flower_freq_df, paste0(output_path, "flower_freq.rds"))
}
stopCluster(cl)
