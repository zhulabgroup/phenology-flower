for (taxaoi in v_taxa) {
  taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
  flower_window <- seq(df_flower_window %>% filter(taxa == taxaoi) %>% pull(start),
    df_flower_window %>% filter(taxa == taxaoi) %>% pull(end),
    by = 1
  )

  path_output <- str_c("./data/results/", taxaoi, "/")

  # read in nab data
  df_nab_freq <- list.files(path_output, "nab_freq", full.names = T) %>% read_rds()

  ls_df_pollen_clim_ts_site <- vector(mode = "list")
  for (siteoi in v_site_tune) {
    df_nab_freq_site <- df_nab_freq %>%
      filter(site == siteoi)

    non_zero_sample <- df_nab_freq_site %>%
      filter(pollen_scale > 0 & !is.na(pollen_scale)) %>%
      nrow()

    if (non_zero_sample >= 30) {
      df_pollen_clim_ts_allyear <- df_nab_freq_site %>%
        group_by(doy) %>%
        summarize(pollen_clim = mean(pollen, na.rm = T)) %>%
        # mutate(pollen_tr = pollen^(1 / 2)) %>%
        # mutate(pollen_fill = case_when(
        #   doy >= min(flower_window) & doy <= max(flower_window) ~ pollen_tr,
        #   TRUE ~ 0
        # )) %>% # disregard values out of window
        # mutate(pollen_sm = util_fill_whit(x = pollen_fill, maxgap = Inf, lambda = 10, minseg = 1)) %>%
        # mutate(pollen_sum = sum(pollen_sm, na.rm = T)) %>%
        # mutate(pollen_freq = pollen_sm / pollen_sum) %>% # normalize
        # mutate(pollen_scale = pollen_tr / pollen_sum) %>% # scale similarly
        # mutate(pollen_clim = pollen_freq) %>%
        # mutate(pollen_clim_pred = (pollen_clim * pollen_sum)^2) %>%
        select(doy, pollen_clim)

      ls_df_pollen_clim_ts_year <- vector(mode = "list")
      for (yearoi in v_year) {
        ls_df_pollen_clim_ts_year[[yearoi %>% as.character()]] <- df_pollen_clim_ts_allyear %>%
          mutate(year = yearoi)

        # ggplot()+
        #   geom_density(data = data.frame(sam), aes(sam))+
        #   geom_line(data = ls_df_pollen_gaus_ts_year[[yearoi %>% as.character]] , aes(x = doy, y=pollen_gaus), col = "red")
        print(str_c(taxaoi, ", ", siteoi, ", ", yearoi))
      }

      ls_df_pollen_clim_ts_site[[siteoi]] <- bind_rows(ls_df_pollen_clim_ts_year) %>%
        mutate(site = siteoi)
    }
  }

  df_pollen_clim_ts <- bind_rows(ls_df_pollen_clim_ts_site) %>%
    left_join(df_nab_freq, by = c("site", "year", "doy")) %>%
    select(site, year, doy, everything())
  write_rds(df_pollen_clim_ts, str_c(path_output, "ts_clim.rds"))

  df_tune_clim <- df_pollen_clim_ts %>%
    group_by(site, year) %>%
    drop_na(pollen_clim) %>%
    filter(sum(pollen_scale, na.rm = T) > 0) %>%
    summarise(
      rmse_raw = (mean((pollen_clim - pollen)^2, na.rm = T)) %>% sqrt(),
      nrmse = (mean((pollen_clim - pollen)^2, na.rm = T)) %>% sqrt() %>% `/`(max(pollen, na.rm = T)),
      spearman = cor(pollen_clim, pollen, method = "spearman", use = "complete.obs"),
      spearman_sig = cor.test(pollen_clim, pollen, method = "spearman", use = "complete.obs")$p.value
    )

  write_rds(df_tune_clim, str_c(path_output, "tune_clim.rds"))
}
