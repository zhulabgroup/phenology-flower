cl <- makeCluster(20, outfile = "")
registerDoSNOW(cl)

# df_pollen_gaus_ts <- read_rds("data/processed/pollen_gaus_ts.rds")
for (taxaoi in v_taxa) {
  taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
  df_thres_taxa <- get_thres_taxa(df_thres, taxaoi)

  path_output <- str_c(.path$res, taxaoi, "/")
  dir.create(path_output, showWarnings = F)

  # get ps data
  df_ps_freq <- list.files(path_output, "ps_freq", full.names = T) %>% read_rds()

  # get nab data
  df_nab_freq <- list.files(path_output, "nab_freq", full.names = T) %>% read_rds()

  # get npn data
  df_npn <- list.files(path_output, "npn", full.names = T) %>% read_rds()

  # get evi data
  df_evi <- list.files(path_output, "evi", full.names = T) %>% read_rds()

  df_ps_nab <- df_ps_freq %>%
    rename(ps_freq = freq) %>%
    left_join(df_nab_freq,
      by = c("site", "year", "doy")
    ) %>%
    left_join(df_npn,
      by = c("site", "year", "doy")
    ) %>%
    left_join(df_evi,
      by = c("site", "year", "doy")
    ) %>%
    select(site, year, doy, everything())

  # optimize threshold and lag
  ls_df_tune_site <- vector(mode = "list")
  for (siteoi in v_site_tune) {
    ls_df_tune_thres <- vector(mode = "list")
    for (t in 1:nrow(df_thres_taxa)) { # select a threshold
      df_ps_nab_site <- df_ps_nab %>%
        filter(site == siteoi) %>%
        filter(
          direction == df_thres_taxa$direction[t],
          thres == df_thres_taxa$threshold[t]
        )

      non_zero_sample <- df_ps_nab_site %>%
        filter(pollen_scale > 0 & !is.na(pollen_scale)) %>%
        nrow()

      if (non_zero_sample >= 30) { # only do tuning when there are more than 100 none-zero pollen count. skip the taxa and site otherwise.
        # ts_pollen_freq <- df_ps_nab %>% pull(pollen_freq) # smoothed pollen count, for tuning
        # ts_pollen <- df_ps_nab %>% pull(pollen_scale) # standardized pollen count, for calculating accuracy
        # pollen_max <- df_ps_nab %>% pull(pollen_max) %>% max() # max of standardized pollen count, for calculating rmse
        # ts_pollen_sum <- df_ps_nab %>% pull(pollen_sum) # sum of smoothed pollen count, for scaling rmse back

        # ts_pollen_gaus <- df_standard_thres %>% pull(pollen_gaus) # climatology
        # mse_clim <- (weighted.mean((ts_pollen_gaus - ts_pollen)^2, w = ts_pollen, na.rm = T)) # mse between climatology and pollen count

        if (!(taxaoi == "Fraxinus" & siteoi %in% c("NY", "DT"))) {
          v_lag <- -90:90 # possible lags to try
          ls_df_tune_lag <-
            foreach(
              l = 1:length(v_lag),
              .packages = c("tidyverse")
            ) %dopar% {
              lag <- v_lag[l]
              if (lag < 0) {
                df_ps_nab_site_lag <- df_ps_nab_site %>%
                  group_by(year) %>%
                  mutate(ps_freq_lag = lead(ps_freq, n = -lag)) %>%
                  mutate(ps_freq_lag = replace_na(ps_freq_lag, 0)) # shift leafing phenology earlier by "lag" days, fill the NA with 0
              } else if (lag == 0) {
                df_ps_nab_site_lag <- df_ps_nab_site %>%
                  group_by(year) %>%
                  mutate(ps_freq_lag = replace_na(ps_freq, 0))
              } else if (lag > 0) {
                df_ps_nab_site_lag <- df_ps_nab_site %>%
                  group_by(year) %>%
                  mutate(ps_freq_lag = lag(ps_freq, n = lag)) %>%
                  mutate(ps_freq_lag = replace_na(ps_freq_lag, 0)) # shift leafing phenology later by "lag" days, fill the NA with 0
              }
              # ts_ps_freq_lag <- df_ps_nab_lag %>% pull(ps_freq)

              df_accuracy <- df_ps_nab_site_lag %>%
                mutate(pollen_pred = (ps_freq_lag * pollen_sum)^2) %>%
                drop_na(pollen_pred) %>%
                summarise(
                  nrmse_tune = (mean((ps_freq_lag - pollen_freq)^2, na.rm = T)) %>% sqrt() %>% `/`(max(pollen_scale, na.rm = T) - min(pollen_scale, na.rm = T)),
                  rmse_raw = (mean((pollen_pred - pollen)^2, na.rm = T)) %>% sqrt(),
                  nrmse = (mean((ps_freq_lag - pollen_scale)^2, na.rm = T)) %>% sqrt() %>% `/`(max(pollen_scale, na.rm = T) - min(pollen_scale, na.rm = T)),
                  # pearson = cor(ps_freq_lag, pollen_scale, method = "pearson", use = "complete.obs"),
                  spearman = cor(ps_freq_lag, pollen_scale, method = "spearman", use = "complete.obs"),
                  spearman_sig = cor.test(ps_freq_lag, pollen_scale, method = "spearman", use = "complete.obs")$p.value,
                  spearman_npn = ifelse(
                    sum(perc > 0, na.rm = T) >= 5,
                    cor(ps_freq_lag, perc, method = "spearman", use = "complete.obs"),
                    NA
                  ),
                  spearman_sig_npn = ifelse(
                    sum(perc > 0, na.rm = T) >= 5,
                    cor.test(ps_freq_lag, perc, method = "spearman", use = "complete.obs")$p.value,
                    NA
                  )
                ) %>%
                mutate(lag = lag)

              print(str_c(taxaoi, ", ", siteoi, ", threshold ", t, ", lag ", l))

              df_accuracy
            }
          ls_df_tune_thres[[t]] <- bind_rows(ls_df_tune_lag) %>%
            filter(lag == bind_rows(ls_df_tune_lag) %>%
              group_by(lag) %>%
              summarise(nrmse_tune = mean(nrmse_tune)) %>%
              arrange(nrmse_tune) %>% # choose the lag giving the smallest rmse in the threshold
              head(1) %>%
              pull(lag)) %>%
            mutate(
              direction = df_thres_taxa$direction[t],
              thres = df_thres_taxa$thres[t],
              n = non_zero_sample
            )
        }
      }
    }
    ls_df_tune_site[[siteoi]] <- bind_rows(ls_df_tune_thres) %>%
      mutate(site = siteoi)
  }
  df_tune <- bind_rows(ls_df_tune_site)
  write_rds(df_tune, str_c(path_output, "tune.rds"))

  df_best_thres <- df_tune %>%
    group_by(direction, thres) %>%
    summarise(nrmse_tune = mean(nrmse_tune)) %>% # mean rmse for each threshold
    arrange(nrmse_tune) %>%
    head(1) %>% # keep threshold giving the smallest mean rmse
    select(direction, thres)

  # getting time series with best threshold and lag
  ls_df_ps_nab_best_site <- vector(mode = "list")
  for (siteoi in v_site_tune) {
    lagoi <- df_tune %>%
      filter(direction == df_best_thres$direction, thres == df_best_thres$thres, site == siteoi) %>%
      pull(lag) %>%
      unique()

    if (length(lagoi) > 0) {
      df_ps_nab_best_site <- df_ps_nab %>%
        filter(site == siteoi) %>%
        filter(
          direction == df_best_thres$direction,
          thres == df_best_thres$thres
        )

      if (lagoi < 0) {
        df_ps_nab_best_site_lag <- df_ps_nab_best_site %>%
          group_by(year) %>%
          mutate(ps_freq_lag = lead(ps_freq, n = -lagoi)) %>%
          mutate(ps_freq_lag = replace_na(ps_freq_lag, 0))
      } else if (lagoi == 0) {
        df_ps_nab_best_site_lag <- df_ps_nab_best_site %>%
          group_by(year) %>%
          mutate(ps_freq_lag = replace_na(ps_freq, 0))
      } else if (lagoi > 0) {
        df_ps_nab_best_site_lag <- df_ps_nab_best_site %>%
          group_by(year) %>%
          mutate(ps_freq_lag = lag(ps_freq, n = lagoi)) %>%
          mutate(ps_freq_lag = replace_na(ps_freq_lag, 0))
      }

      ls_df_ps_nab_best_site[[siteoi]] <- df_ps_nab_best_site_lag %>%
        mutate(pollen_pred = (ps_freq_lag * pollen_sum)^2) %>%
        drop_na(pollen_pred) %>%
        ungroup() %>%
        mutate(
          lag = lagoi
        ) %>%
        left_join(df_meta %>%
          distinct(site, sitename))
    }
  }
  df_ps_nab_best <- bind_rows(ls_df_ps_nab_best_site)
  write_rds(df_ps_nab_best, str_c(path_output, "ts_best.rds"))

  # make plots
  p_ts_siteyear <- plot_ts_siteyear(df_ps_nab_best, save = T, path_output)

  p_ts_site <- plot_ts_site(df_ps_nab_best, save = T, path_output)

  p_corr <- plot_corr(df_ps_nab_best, save = T, path_output)
}
stopCluster(cl)
