cl <- makeCluster(36, outfile = "")
registerDoSNOW(cl)

for (taxaoi in v_taxa) {
  taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
  df_thres_taxa <- get_thres_taxa(df_thres, taxaoi)

  ls_df_ts_ext_site <- ls_df_flower_doy_site <- vector(mode = "list", length = length(v_site))
  for (s in 1:length(v_site)) {
    siteoi <- v_site[s]
    df_plant_taxa <- df_plant %>%
      filter(site == siteoi) %>%
      filter(genus == taxaoi_short | family == taxaoi_short) %>%
      mutate(id = row_number()) %>%
      drop_na(lon, lat)

    if (taxaoi_short %in% c("Ambrosia", "Poaceae")) {
      min_sample_size <- 10
    } else {
      min_sample_size <- 20
    }

    if (nrow(df_plant_taxa) >= min_sample_size) {
      set.seed(1)
      v_id <- sample(df_plant_taxa$id, min(2000, length(unique(df_plant_taxa$id)))) %>% sort()

      # preprocess ps data
      df_ps_site <- read_rds(paste0(path_ps, "ts/ps_", siteoi, "_", taxaoi_short, ".rds"))
      df_ps_site_proc <- process_ps(df_ps_site %>% filter(id %in% v_id))

      # subset nab data
      df_pollen_site <- df_nab_full %>%
        left_join(df_meta %>% select(id, site), by = "id") %>%
        filter(site == siteoi) %>%
        filter(genus == taxaoi_short | family == taxaoi_short)

      # subset npn data
      df_npn_site <- df_npn %>%
        filter(site == siteoi) %>%
        filter(taxa == taxaoi_short)

      df_ts_site <- df_ps_site_proc %>%
        left_join(df_plant_taxa, by = c("id", "lon", "lat")) %>%
        select(id, date,
          `EVI (PS)` = evi
        ) %>%
        mutate(id = as.character(id)) %>%
        full_join(
          df_pollen_site %>%
            select(date,
              `pollen (NAB)` = count
            ) %>%
            mutate(id = "pollen"),
          by = c("date", "id")
        ) %>%
        full_join(
          df_npn_site %>%
            select(date, `flower (NPN)` = count) %>%
            mutate(id = "npn"),
          by = c("date", "id")
        ) %>%
        arrange(id, date) %>%
        mutate(doy = format(date, "%j") %>% as.numeric()) %>%
        mutate(year = format(date, "%Y") %>% as.numeric())

      ls_df_ts_ext_year <- ls_df_flower_doy_year <- vector(mode = "list", length = length(v_year))
      for (y in 1:length(v_year)) {
        yearoi <- v_year[y]
        df_ts_year <- df_ts_site %>%
          filter(doy != 366) %>%
          filter(year == yearoi | year == (yearoi - 1) | year == (yearoi + 1)) %>%
          mutate(doy = ifelse(doy > 273 & year == yearoi - 1, doy - 365, doy)) %>%
          mutate(year = ifelse(doy <= 0 & year == yearoi - 1, year + 1, year)) %>%
          mutate(doy = ifelse(doy < 152 & year == yearoi + 1, doy + 365, doy)) %>%
          mutate(year = ifelse(doy > 365 & year == yearoi + 1, year - 1, year)) %>%
          filter(year == yearoi) %>%
          gather(key = "var", value = "value", -date, -id, -doy, -year) %>%
          mutate(var = factor(var,
            labels = c("enhanced vegetation index (PS)", "pollen concentration (NAB)", "flower observation (USA-NPN)", "flower observation (Katz's team)"),
            levels = c("EVI (PS)", "pollen (NAB)", "flower (NPN)", "flower (DK)")
          )) %>%
          arrange(doy)
        ls_df_ts_ext_year[[y]] <- df_ts_year

        df_ts_year_evi <- df_ts_year %>%
          filter(var == "enhanced vegetation index (PS)") %>%
          select(id, doy, evi = value)

        ls_df_flower_doy_id <-
          foreach(
            i = 1:length(v_id),
            .packages = c("tidyverse", "ptw", "segmented")
          ) %dopar% {
            # pacman::p_unload("all")
            # pacman::p_load("tidyverse")
            idoi <- as.character(v_id)[i]

            print(paste0(i, " out of ", length(v_id)))
            get_doy(df_thres_taxa, df_ts_year_evi, idoi)
          }

        ls_df_flower_doy_year[[y]] <- bind_rows(ls_df_flower_doy_id) %>%
          mutate(year = yearoi)
        print(paste0(siteoi, ", ", yearoi))
      }
      ls_df_ts_ext_site[[s]] <- bind_rows(ls_df_ts_ext_year) %>%
        mutate(site = siteoi)

      ls_df_flower_doy_site[[s]] <- bind_rows(ls_df_flower_doy_year) %>%
        mutate(site = siteoi)
    }
  }
  ts_df_ext <- bind_rows(ls_df_ts_ext_site)
  df_flower_doy <- bind_rows(ls_df_flower_doy_site)

  path_output <- paste0("data/results/", taxaoi, "/")
  dir.create(path_output, recursive = T)
  write_rds(ts_df_ext, str_c(path_output, "ts_ext.rds"))
  write_rds(df_flower_doy, str_c(path_output, "flower_doy.rds"))
}
stopCluster(cl)
