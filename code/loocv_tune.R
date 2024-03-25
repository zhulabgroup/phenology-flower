for (taxaoi in v_taxa) {
  flower_window <- seq(df_flower_window %>% filter(taxa == taxaoi) %>% pull(start),
    df_flower_window %>% filter(taxa == taxaoi) %>% pull(end),
    by = 1
  )

  path_output <- str_c(.path$res, taxaoi, "/")

  df_tune <- read_rds(str_c(path_output, "tune.rds")) %>%
    filter(site %in% v_site_tune)

  site_num <- df_tune %>%
    distinct(site) %>%
    nrow()

  if (site_num >= 7) {
    # get ps data
    df_ps_freq <- list.files(path_output, "ps_freq", full.names = T) %>% read_rds()

    # get nab data
    df_nab_freq <- list.files(path_output, "nab_freq", full.names = T) %>% read_rds()

    # get evi data
    df_evi <- list.files(path_output, "evi", full.names = T) %>% read_rds()

    df_ps_nab <- df_ps_freq %>%
      rename(ps_freq = freq) %>%
      left_join(df_nab_freq,
        by = c("site", "year", "doy")
      ) %>%
      left_join(df_evi,
        by = c("site", "year", "doy")
      ) %>%
      select(site, year, doy, everything())

    # leave one city out
    ls_df_tune_cv <- ls_df_ps_nab_best_cv_site <- vector(mode = "list")
    # ls_df_lag_clim_cv_site <- vector(mode = "list")

    for (siteout in df_tune %>%
      pull(site) %>%
      unique()) {
      # new best threshold
      df_best_thres_cv <- df_tune %>%
        filter(site != siteout) %>%
        group_by(direction, thres) %>%
        summarise(nrmse_tune = mean(nrmse_tune)) %>% # mean nrmse for each threshold
        arrange(nrmse_tune) %>%
        head(1) %>% # keep threshold giving the smallest median rmse
        select(direction, thres)

      df_fit_cv <- df_tune %>%
        right_join(df_best_thres_cv, by = c("direction", "thres")) %>%
        left_join(df_meta %>% select(site, sitename), by = "site")

      df_lag_clim_cv <- df_fit_cv %>%
        distinct(site, sitename, direction, thres, lag) %>%
        left_join(df_terraclim, by = "site") %>%
        mutate(lag_new = case_when(site != siteout ~ lag)) %>%
        mutate(siteout = siteout) %>%
        mutate(taxa = taxaoi)

      lm_model_cv <- lm(lag_new ~ mat, data = df_lag_clim_cv)

      df_lag_clim_cv <- df_lag_clim_cv %>%
        mutate(lag_fit = predict(lm_model_cv, df_lag_clim_cv))

      # ls_df_lag_clim_cv_site[[siteout]] <- df_lag_clim_cv

      # get new lag for site out
      lag_fit <- df_lag_clim_cv %>%
        filter(site == siteout) %>%
        pull(lag_fit) %>%
        round(0)

      # getting time series with new lag
      df_ps_nab_best_cv_site <- df_ps_nab %>%
        filter(site == siteout) %>%
        filter(
          direction == df_best_thres_cv$direction,
          thres == df_best_thres_cv$thres
        )

      if (lag_fit < 0) {
        df_ps_nab_best_cv_site_lag <- df_ps_nab_best_cv_site %>%
          group_by(year) %>%
          mutate(ps_freq_lag = lead(ps_freq, n = -lag_fit)) %>%
          mutate(ps_freq_lag = replace_na(ps_freq_lag, 0))
      } else if (lag_fit == 0) {
        df_ps_nab_best_cv_site_lag <- df_ps_nab_best_cv_site %>%
          group_by(year) %>%
          mutate(ps_freq_lag = replace_na(freq, 0))
      } else if (lag_fit > 0) {
        df_ps_nab_best_cv_site_lag <- df_ps_nab_best_cv_site %>%
          group_by(year) %>%
          mutate(ps_freq_lag = lag(ps_freq, n = lag_fit)) %>%
          mutate(ps_freq_lag = replace_na(ps_freq_lag, 0))
      }

      ls_df_ps_nab_best_cv_site[[siteout]] <- df_ps_nab_best_cv_site_lag %>%
        ungroup() %>%
        mutate(pollen_pred = (ps_freq_lag * pollen_sum)^2) %>%
        mutate(lag = "") %>%
        left_join(df_meta %>%
          distinct(site, sitename))

      df_accuracy_cv <- df_ps_nab_best_cv_site_lag %>%
        mutate(pollen_pred = (ps_freq_lag * pollen_sum)^2) %>%
        drop_na(pollen_pred) %>%
        summarise(
          nrmse_tune = (mean((ps_freq_lag - pollen_freq)^2, na.rm = T)) %>% sqrt() %>% `/`(max(pollen_scale, na.rm = T)),
          rmse_raw = (mean((pollen_pred - pollen)^2, na.rm = T)) %>% sqrt(),
          nrmse = (mean((ps_freq_lag - pollen_scale)^2, na.rm = T)) %>% sqrt() %>% `/`(max(pollen_scale, na.rm = T)),
          # pearson = cor(ps_freq_lag, pollen_scale, method = "pearson", use = "complete.obs"),
          spearman = cor(ps_freq_lag, pollen_scale, method = "spearman", use = "complete.obs"),
          spearman_sig = cor.test(ps_freq_lag, pollen_scale, method = "spearman", use = "complete.obs")$p.value
        ) %>%
        mutate(lag = lag_fit)

      ls_df_tune_cv[[siteout]] <- df_accuracy_cv %>%
        mutate(site = siteout) %>%
        left_join(df_meta %>%
          distinct(site, sitename))

      print(str_c(taxaoi, ", ", siteout))
    }

    df_ps_nab_best_cv <- bind_rows(ls_df_ps_nab_best_cv_site)
    df_tune_cv <- bind_rows(ls_df_tune_cv)
    # df_lag_clim_cv <- bind_rows(ls_df_lag_clim_cv_site)

    write_rds(df_ps_nab_best_cv, str_c(path_output, "ts_best_cv.rds"))
    write_rds(df_tune_cv, str_c(path_output, "tune_cv.rds"))


    # make plots
    p_ts_siteyear_cv <- plot_ts_siteyear(df_ps_nab_best_cv, save = T, path_output, cv = T)

    p_ts_site_cv <- plot_ts_site(df_ps_nab_best_cv, save = T, path_output, cv = T)

    p_corr_cv <- plot_corr(df_ps_nab_best_cv, save = T, path_output, cv = T)
  }
}
