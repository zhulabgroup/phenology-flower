cl <- makeCluster(36, outfile = "")
registerDoSNOW(cl)

for (taxaoi in taxa_list) {
  taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
  thres_df_taxa <- get_thres_taxa(thres_df, taxaoi)

  ts_df_ext_site_list <- flower_doy_df_site_list <- vector(mode = "list", length = length(site_list))
  for (s in 1:length(site_list)) {
    siteoi <- site_list[s]
    plant_taxa_df <- plant_df %>%
      filter(site == siteoi) %>%
      filter(genus == taxaoi_short | family == taxaoi_short) %>%
      mutate(id = row_number()) %>%
      drop_na(lon, lat)

    if (taxaoi_short %in% c("Ambrosia", "Poaceae")) {
      min_sample_size <- 10
    } else {
      min_sample_size <- 20
    }

    if (nrow(plant_taxa_df) >= min_sample_size) {
      set.seed(1)
      random_id <- sample(plant_taxa_df$id, min(2000, length(unique(plant_taxa_df$id)))) %>% sort()

      # preprocess ps data
      ps_df <- read_rds(paste0(ps_path, "ts/ps_", siteoi, "_", taxaoi_short, ".rds"))
      ps_df_proc <- process_ps(ps_df %>% filter(id %in% random_id))

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
          `EVI (PS)` = evi
        ) %>%
        mutate(id = as.character(id)) %>%
        full_join(
          pollen_df %>%
            dplyr::select(date,
              `pollen (NAB)` = count
            ) %>%
            mutate(id = "pollen"),
          by = c("date", "id")
        ) %>%
        full_join(
          npn_df %>%
            dplyr::select(date, `flower (NPN)` = count) %>%
            mutate(id = "npn"),
          by = c("date", "id")
        ) %>%
        arrange(id, date) %>%
        mutate(doy = format(date, "%j") %>% as.numeric()) %>%
        mutate(year = format(date, "%Y") %>% as.numeric())

      ts_df_ext_year_list <- flower_doy_df_year_list <- vector(mode = "list", length = length(year_list))
      for (y in 1:length(year_list)) {
        yearoi <- year_list[y]
        ts_df_subset <- ts_df %>%
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
        ts_df_ext_year_list[[y]] <- ts_df_subset

        flower_doy_df_id_list <-
          foreach(
            i = 1:length(random_id),
            .packages = c("tidyverse", "ptw", "segmented")
          ) %dopar% {
            idoi <- as.character(random_id)[i]

            flower_doy_df <- get_doy(thres_df_taxa, ts_df_subset, idoi)
            print(paste0(i, " out of ", length(random_id)))
            flower_doy_df
          }
        flower_doy_df_id <- bind_rows(flower_doy_df_id_list)

        print(paste0(siteoi, ", ", yearoi))

        flower_doy_df_year_list[[y]] <- flower_doy_df_id %>%
          mutate(year = yearoi)
      }
      ts_df_ext_site_list[[s]] <- bind_rows(ts_df_ext_year_list) %>%
        mutate(site = siteoi)

      flower_doy_df_site_list[[s]] <- bind_rows(flower_doy_df_year_list) %>%
        mutate(site = siteoi)
    }
  }
  ts_df_ext_site <- bind_rows(ts_df_ext_site_list)
  flower_doy_df_site <- bind_rows(flower_doy_df_site_list)

  output_path <- paste0("data/results/", taxaoi, "/")
  dir.create(output_path, recursive = T)
  write_rds(ts_df_ext_site, str_c(output_path, "ts_ext.rds"))
  write_rds(flower_doy_df_site, str_c(output_path, "flower_doy.rds"))
}
stopCluster(cl)
