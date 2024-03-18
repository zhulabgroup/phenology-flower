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

  if (site_num >= 6) {
    # read in inferred flowering phenology data
    df_ps_freq <- read_rds(str_c(path_output, "ps_freq.rds"))

    # other site-level data

    # get in-sample fit
    df_best_thres <- df_tune %>%
      group_by(direction, thres) %>%
      summarise(nrmse = mean(nrmse)) %>%
      ungroup() %>%
      arrange(nrmse) %>%
      slice(1) %>%
      select(direction, thres)

    df_fit_new <- df_tune %>%
      right_join(df_best_thres, by = c("direction", "thres")) %>%
      left_join(df_meta %>% select(site, sitename), by = "site") %>%
      mutate(
        # nrmse_clim_cv = NA,
        nrmse_ps_cv = NA
      ) %>%
      select(-direction, -thres, -lag)

    # leave one city out
    ls_df_standard_best_cv_site <- vector(mode = "list")
    ls_df_lag_clim_cv_site <- vector(mode = "list")
    for (siteout in df_tune %>%
      pull(site) %>%
      unique()) {
      # new best threshold
      df_best_thres_cv <- df_tune %>%
        filter(site != siteout) %>%
        group_by(direction, thres) %>%
        summarise(mse = mean(mse)) %>% # mean rmse for each threshold
        arrange(mse) %>%
        head(1) %>% # keep threshold giving the smallest median rmse
        select(direction, thres)

      df_fit_cv <- df_tune %>%
        right_join(df_best_thres_cv, by = c("direction", "thres")) %>%
        left_join(df_meta %>% select(site, sitename), by = "site")

      df_lag_clim_cv <- df_fit_cv %>%
        select(-mse, -mse_ps, -mse_clim) %>%
        left_join(df_chelsa, by = "site") %>%
        mutate(lag_new = case_when(site != siteout ~ lag)) %>%
        mutate(siteout = siteout) %>%
        mutate(taxa = taxaoi)

      lm_model_cv <- lm(lag_new ~ mat, data = df_lag_clim_cv)

      df_lag_clim_cv <- df_lag_clim_cv %>%
        mutate(lag_fit = predict(lm_model_cv, df_lag_clim_cv))

      ls_df_lag_clim_cv_site[[siteout]] <- df_lag_clim_cv

      # get new lag for site out
      lag_fit <- df_lag_clim_cv %>%
        filter(site == siteout) %>%
        pull(lag_fit) %>%
        round(0)

      # getting time series with new lag
      df_standard_best_cv <- df_standard %>%
        filter(site == siteout) %>%
        filter(
          direction == df_best_thres_cv$direction,
          thres == df_best_thres_cv$thres
        ) %>%
        ungroup() %>%
        group_by(site, year)

      if (lag_fit < 0) {
        df_standard_best_cv <- df_standard_best_cv %>%
          mutate(freq = lead(freq, n = -lag_fit)) %>%
          mutate(freq = replace_na(freq, 0))
      } else if (lag_fit == 0) {
        df_standard_best_cv <- df_standard_best_cv %>% mutate(freq = replace_na(freq, 0))
      } else if (lag_fit > 0) {
        df_standard_best_cv <- df_standard_best_cv %>%
          mutate(freq = lag(freq, n = lag_fit)) %>%
          mutate(freq = replace_na(freq, 0))
      }

      ts_pollen_cv <- df_standard_best_cv %>% pull(pollen) # pollen count, for calculating accuracy
      ts_pollen_gaus_cv <- df_standard_best_cv %>% pull(pollen_gaus_cv) # climatology
      mse_clim_cv <- (weighted.mean((ts_pollen_gaus_cv - ts_pollen_cv)^2, w = ts_pollen_cv, na.rm = T)) # mse between climatology and pollen count

      # ts_pollen_sm_cv <- df_standard_best_cv %>% pull(pollen_sm) # pollen count, for calculating accuracy
      ts_freq_lag_cv <- df_standard_best_cv %>% pull(freq)
      mse_ps_cv <- (weighted.mean((ts_freq_lag_cv - ts_pollen_cv)^2, w = ts_pollen_cv, na.rm = T)) # mse between remotely-sensed flowering phenology and pollen count

      df_fit_new[df_fit_new$site == siteout, ]$mse_clim_cv <- mse_clim_cv
      df_fit_new[df_fit_new$site == siteout, ]$mse_ps_cv <- mse_ps_cv
      ls_df_standard_best_cv_site[[siteout]] <- df_standard_best_cv
      print(str_c(taxaoi, ", ", siteout))
    }
    df_standard_best_cv <- bind_rows(ls_df_standard_best_cv_site)
    df_lag_clim_cv <- bind_rows(ls_df_lag_clim_cv_site)

    # make plots
    p_ts_siteyear_cv <- ggplot(df_standard_best_cv) +
      geom_point(aes(x = doy, y = evi, col = "enhanced vegetation index (PS)"), alpha = 0.2) +
      # geom_point(aes(x = doy, y = npn, col = "flower observation (USA-NPN)"), alpha = 0.5) +
      geom_point(aes(x = doy, y = pollen, col = "pollen concentration (NAB)")) +
      geom_line(aes(x = doy, y = pollen_gaus_cv, col = "pollen concentration (NAB)"), alpha = 0.5, lwd = 1) +
      geom_line(aes(x = doy, y = freq, col = "flowering frequency (PS)"), lwd = 1) +
      theme_classic() +
      facet_wrap(. ~ sitename * year, ncol = 4) +
      scale_color_manual(values = cols) +
      theme(legend.position = "bottom") +
      theme(legend.title = element_blank()) +
      ylab("") +
      ylim(0, 0.05) +
      ggtitle(str_c("Taxa: ", taxaoi))

    jpeg(paste0(path_output, "ts_siteyear_cv.jpg"),
      height = 3200, width = 3200, res = 300
    )
    print(p_ts_siteyear_cv)
    dev.off()

    p_ts_site_cv <-
      ggplot(df_standard_best_cv %>%
        mutate(year = as.factor(year))) +
      geom_point(aes(x = doy, y = pollen, group = year, col = year)) +
      geom_line(aes(x = doy, y = freq, group = year, col = year)) +
      facet_wrap(. ~ sitename, ncol = 1) +
      theme_classic() +
      ylab("") +
      ylim(0, 0.05) +
      ggtitle(paste0("Taxa: ", taxaoi))

    jpeg(paste0(path_output, "ts_site_cv.jpg"),
      height = 3200, width = 3200, res = 300
    )
    print(p_ts_site_cv)
    dev.off()

    p_corr_cv <-
      ggplot(df_standard_best_cv %>% mutate(year = as.factor(year))) +
      geom_point(aes(x = freq, y = pollen, group = year, col = year)) +
      geom_smooth(aes(x = freq, y = pollen, group = year, col = year), method = "lm", se = F, lwd = 0.5) +
      geom_smooth(aes(x = freq, y = pollen), method = "lm") +
      theme_classic() +
      facet_wrap(. ~ sitename, ncol = 4) +
      coord_equal() +
      xlim(0, 0.05) +
      ylim(0, 0.05) +
      ylab("standardized smoothed pollen count^(1/2)") +
      xlab("standardized flowering frequency")
    jpeg(paste0(path_output, "corr_cv.jpg"),
      height = 2400, width = 3200, res = 300
    )
    print(p_corr_cv)
    dev.off()

    write_rds(df_fit_new, paste0(path_output, "tune_cv.rds"))
    write_rds(df_lag_clim_cv, paste0(path_output, "lag_clim_cv.rds"))
  }
}
