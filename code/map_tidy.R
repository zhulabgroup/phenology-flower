ls_df_doy_site <- vector(mode = "list")
for (siteoi in v_site) {
  ls_df_doy_year <- vector(mode = "list")
  for (yearoi in v_year) {
    ls_df_doy <- vector(mode = "list")
    for (taxaoi in v_taxa %>% setdiff("Ulmus late")) {
      taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
      flower_window <- seq(df_flower_window %>% filter(taxa == taxaoi) %>% pull(start),
        df_flower_window %>% filter(taxa == taxaoi) %>% pull(end),
        by = 1
      )

      path_tune <- list.files(str_c(.path$res, taxaoi), "tune.rds", full.names = T)

      df_best_thres <- read_rds(path_tune) %>%
        group_by(direction, thres) %>%
        summarise(nrmse_tune = mean(nrmse_tune)) %>%
        ungroup() %>%
        arrange(nrmse_tune) %>%
        slice(1) %>%
        select(direction, thres)

      df_tune_subset <- read_rds(path_tune) %>%
        filter(site == siteoi, year == yearoi)

      if (nrow(df_tune_subset) > 0) {
        lag <- df_tune_subset %>%
          right_join(df_best_thres, by = c("direction", "thres")) %>%
          select(site, year, lag) %>%
          pull(lag)

        path_doy <- list.files(str_c(.path$ps, "urban/doy/"), str_c(siteoi, "_", taxaoi_short), full.names = T)

        df_leaf <- read_rds(path_doy) %>%
          filter(year == yearoi) %>%
          left_join(
            df_tree %>%
              filter(site == siteoi),
            by = "id"
          ) %>%
          filter(direction == "up", thres == 0.5) %>%
          select(genus, site, year, id, lat, lon, leaf_doy = doy)

        df_pollen <- read_rds(path_doy) %>%
          filter(year == yearoi) %>%
          left_join(
            df_tree %>%
              filter(site == siteoi),
            by = "id"
          ) %>%
          right_join(df_best_thres) %>%
          mutate(doy = doy + lag) %>%
          filter(doy %in% flower_window) %>%
          select(genus, site, year, id, lat, lon, pollen_doy = doy)

        ls_df_doy[[taxaoi]] <- inner_join(df_leaf, df_pollen)
      }
    }
    if (nrow(bind_rows(ls_df_doy)) > 0) {
      ls_df_doy_year[[yearoi %>% as.character()]] <- bind_rows(ls_df_doy) %>%
        mutate(percentile = percent_rank(pollen_doy))
    }
  }
  ls_df_doy_site[[siteoi]] <- bind_rows(ls_df_doy_year)
}
df_doy <- bind_rows(ls_df_doy_site)

write_rds(df_doy, str_c(.path$dat_other, "df_leaf_pollen_doy.rds"))
