# set color palette
cols <- c(
  "enhanced vegetation index (PS)" = "dark green",
  "Enhanced Vegetation Index (PlanetScope)" = "dark green",
  "pollen concentration (NAB)" = "dark red",
  # "flower observation (USA-NPN)" = "dark orchid",
  "flowering frequency (PS)" = "dark blue",
  "flower observation (Katz et al., 2019)" = "coral",
  "Percentage of open flowers (Katz et al., 2019)" = "coral"
)

util_extend_ts <- function(df) {
  df_current <- df
  df_next <- df_current %>%
    mutate(
      doy = doy + 365,
      year = year + 1
    ) %>%
    filter(doy <= 365 + 90)
  df_previous <- df_current %>%
    mutate(
      doy = doy - 365,
      year = year - 1
    ) %>%
    filter(doy >= -90)

  df_extend <- bind_rows(list(df_current, df_next, df_previous)) %>%
    arrange(year, doy)

  return(df_extend)
}

cl <- makeCluster(20, outfile = "")
registerDoSNOW(cl)

# df_pollen_gaus_ts <- read_rds("data/processed/pollen_gaus_ts.rds")
for (taxaoi in v_taxa) {
  taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
  flower_window <- seq(df_flower_window %>% filter(taxa == taxaoi) %>% pull(start),
    df_flower_window %>% filter(taxa == taxaoi) %>% pull(end),
    by = 1
  )
  df_thres_taxa <- get_thres_taxa(df_thres, taxaoi)

  path_output <- str_c(.path$res, taxaoi, "/")
  dir.create(path_output, showWarnings = F)

  # get ps data
  df_ps_freq <- list.files(path_output, "ps_freq", full.names = T) %>% read_rds()

  # get nab data
  df_nab_freq <- df_nab_short %>%
    filter(year >= 2017) %>%
    filter(taxa == taxaoi_short) %>%
    select(site, year, doy, pollen = count) %>%
    util_extend_ts() %>%
    group_by(site, year) %>%
    complete(doy = c(-90:(365 + 90))) %>%
    mutate(pollen = pollen %>% sqrt()) %>%
    mutate(pollen = case_when(doy >= min(flower_window) & doy <= max(flower_window) ~ pollen)) %>% # disregard values out of window
    mutate(pollen_fill = case_when(
      doy >= min(flower_window) & doy <= max(flower_window) ~ pollen,
      TRUE ~ 0
    )) %>% # disregard values out of window
    mutate(pollen_sm = util_fill_whit(x = pollen_fill, maxgap = Inf, lambda = 10, minseg = 1)) %>%
    mutate(pollen_freq = pollen_sm / sum(pollen_sm, na.rm = T)) %>% # normalize
    mutate(pollen_scale = pollen / sum(pollen_sm, na.rm = T)) %>% # scale similarly
    ungroup() %>%
    drop_na(pollen_freq)
  write_rds(df_nab_freq, str_c(path_output, "nab_freq.rds"))

  # get evi data
  df_evi <- df_ps_evi %>%
    filter(taxa == taxaoi_short) %>%
    util_extend_ts() %>%
    select(-taxa) %>%
    group_by(site) %>%
    mutate(
      evi = (evi - min(evi, na.rm = T)) / (max(evi, na.rm = T) - min(evi, na.rm = T)),
    ) %>%
    mutate(evi = evi * 0.05) %>%
    ungroup()

  write_rds(df_evi, str_c(path_output, "evi.rds"))

  # optimize threshold and lag
  ls_df_tune_site <- vector(mode = "list")
  for (siteoi in v_site_tune) {
    ls_df_tune_thres <- vector(mode = "list")
    for (t in 1:nrow(df_thres_taxa)) { # select a threshold
      df_ps_freq_sub <- df_ps_freq %>%
        filter(site == siteoi) %>%
        filter(
          direction == df_thres_taxa$direction[t],
          thres == df_thres_taxa$threshold[t]
        ) %>%
        ungroup() %>%
        select(year, doy, ps_freq = freq)

      df_nab_freq_sub <- df_nab_freq %>%
        filter(site == siteoi) %>%
        select(year, doy, pollen_freq, pollen = pollen_scale)

      non_zero_sample <- df_nab_freq_sub %>%
        filter(pollen > 0 & !is.na(pollen)) %>%
        nrow()

      pollen_max <- df_nab_freq_sub %>%
        pull(pollen) %>%
        max(na.rm = T)

      df_ps_nab <- inner_join(df_ps_freq_sub,
        df_nab_freq_sub,
        by = c("year", "doy")
      ) %>%
        group_by(year)

      if (non_zero_sample >= 30) { # only do tuning when there are more than 100 none-zero pollen count. skip the taxa and site otherwise.
        ts_pollen_freq <- df_ps_nab %>% pull(pollen_freq) # smoothed pollen count, for tuning
        ts_pollen <- df_ps_nab %>% pull(pollen) # pollen count, for calculating accuracy

        # ts_pollen_gaus <- df_standard_thres %>% pull(pollen_gaus) # climatology
        # mse_clim <- (weighted.mean((ts_pollen_gaus - ts_pollen)^2, w = ts_pollen, na.rm = T)) # mse between climatology and pollen count

        v_lag <- -100:100 # possible lags to try
        ls_df_tune_lag <-
          foreach(
            l = 1:length(v_lag),
            .packages = c("tidyverse")
          ) %dopar% {
            lag <- v_lag[l]
            if (lag < 0) {
              df_ps_nab_lag <- df_ps_nab %>%
                mutate(ps_freq = lead(ps_freq, n = -lag)) %>%
                mutate(ps_freq = replace_na(ps_freq, 0)) # shift leafing phenology earlier by "lag" days, fill the NA with 0
            } else if (lag == 0) {
              df_ps_nab_lag <- df_ps_nab %>%
                mutate(ps_freq = replace_na(ps_freq, 0))
            } else if (lag > 0) {
              df_ps_nab_lag <- df_ps_nab %>%
                mutate(ps_freq = lag(ps_freq, n = lag)) %>%
                mutate(ps_freq = replace_na(ps_freq, 0)) # shift leafing phenology later by "lag" days, fill the NA with 0
            }
            ts_ps_freq_lag <- df_ps_nab_lag %>% pull(ps_freq)
            nrmse <- (mean((ts_ps_freq_lag - ts_pollen_freq)^2, na.rm = T)) %>%
              sqrt() %>%
              `/`(pollen_max) # nrmse between remotely-sensed flowering phenology and smoothed pollen count
            # mse_ps <- (weighted.mean((ts_ps_freq_lag - ts_pollen)^2, w = ts_pollen, na.rm = T)) # rmse between remotely-sensed flowering phenology and pollen count
            nrmse_ps <- (mean((ts_ps_freq_lag - ts_pollen)^2, na.rm = T)) %>%
              sqrt() %>%
              `/`(pollen_max) # nrmse between remotely-sensed flowering phenology and pollen count

            print(str_c(taxaoi, ", ", siteoi, ", threshold ", t, ", lag ", l))

            data.frame(
              direction = df_thres_taxa$direction[t], thres = df_thres_taxa$threshold[t], lag = lag, nrmse = nrmse, nrmse_ps = nrmse_ps # , mse_clim = mse_clim
            )
          }
        ls_df_tune_thres[[t]] <- bind_rows(ls_df_tune_lag) %>%
          # mutate(mse_rm = zoo::rollmean(mse, 7, fill=NA)) %>%
          # arrange(mse_rm) %>%
          # head(5) %>%
          arrange(nrmse) %>% # choose the lag giving the smallest rmse in the threshold
          head(1) %>%
          mutate(
            n = non_zero_sample,
            pollen_max = pollen_max
          )
      } else {
        ls_df_tune_thres[[t]] <- data.frame(
          thres = numeric(0), lag = numeric(0), nrmse = numeric(0), nrmse_ps = numeric(0),
          # mse_clim = numeric(0),
          n = numeric(0),
          pollen_max = numeric(0)
        )
      }
    }
    ls_df_tune_site[[siteoi]] <- bind_rows(ls_df_tune_thres) %>%
      mutate(site = siteoi) %>%
      arrange(nrmse)
  }
  df_tune <- bind_rows(ls_df_tune_site)
  write_rds(df_tune, str_c(path_output, "tune.rds"))

  df_best_thres <- df_tune %>%
    group_by(direction, thres) %>%
    summarise(nrmse = mean(nrmse)) %>% # mean rmse for each threshold
    arrange(nrmse) %>%
    head(1) %>% # keep threshold giving the smallest mean rmse
    select(direction, thres)

  # getting time series with best threshold and lag
  ls_df_ps_freq_best_site <- vector(mode = "list")
  for (siteoi in v_site_tune) {
    lagoi <- df_tune %>%
      filter(direction == df_best_thres$direction, thres == df_best_thres$thres, site == siteoi) %>%
      pull(lag)

    if (length(lagoi) > 0) {
      df_ps_freq_best_site <- df_ps_freq %>%
        filter(site == siteoi) %>%
        filter(
          direction == df_best_thres$direction,
          thres == df_best_thres$thres
        ) %>%
        ungroup() %>%
        select(year, doy, ps_freq = freq) %>%
        group_by(year)

      if (lagoi < 0) {
        df_ps_freq_best_site <- df_ps_freq_best_site %>%
          mutate(ps_freq = lead(ps_freq, n = -lagoi)) %>%
          mutate(ps_freq = replace_na(ps_freq, 0))
      } else if (lagoi == 0) {
        df_ps_freq_best_site <- df_ps_freq_best_site %>%
          mutate(ps_freq = replace_na(ps_freq, 0))
      } else if (lagoi > 0) {
        df_ps_freq_best_site <- df_ps_freq_best_site %>%
          mutate(ps_freq = lag(ps_freq, n = lagoi)) %>%
          mutate(ps_freq = replace_na(ps_freq, 0))
      }

      ls_df_ps_freq_best_site[[siteoi]] <- df_ps_freq_best_site %>%
        ungroup() %>%
        left_join(df_nab_freq %>%
          filter(site == siteoi) %>%
          select(year, doy, pollen_freq, pollen_scale), by = c("year", "doy")) %>%
        left_join(df_evi %>% filter(site == siteoi) %>% select(-site), by = c("year", "doy")) %>%
        mutate(
          direction = df_best_thres$direction,
          thres = df_best_thres$thres,
          lag = lagoi,
          taxa = taxaoi,
          site = siteoi
        ) %>%
        left_join(df_meta %>%
          distinct(site, sitename))
    }
  }
  df_ps_freq_best <- bind_rows(ls_df_ps_freq_best_site)
  write_rds(df_ps_freq_best, str_c(path_output, "ts_best.rds"))

  # make plots

  p_ts_siteyear <- ggplot(df_ps_freq_best %>%
    arrange(sitename) %>%
    mutate(site_lag = str_c(sitename, " (Lag: ", lag, ")")) %>%
    mutate(site_lag = factor(site_lag, levels = (.)$site_lag %>% unique()))) +
    geom_point(aes(x = doy, y = evi, col = "enhanced vegetation index (PS)"), alpha = 0.2) +
    geom_point(aes(x = doy, y = pollen_scale, col = "pollen concentration (NAB)")) +
    # geom_line(aes(x = doy, y = pollen_gaus, col = "pollen concentration (NAB)"), alpha = 0.5, lwd = 1) +
    geom_line(aes(x = doy, y = ps_freq, col = "flowering frequency (PS)"), lwd = 1) +
    theme_classic() +
    facet_wrap(. ~ site_lag * year, ncol = 5) +
    scale_color_manual(values = cols) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    ylab("") +
    # ylim(0, 0.05) +
    ggtitle(str_c("Taxa: ", taxaoi, " (Threshold: ", df_best_thres$direction, " ", df_best_thres$thres, ")"))

  jpeg(str_c(path_output, "ts_siteyear.jpg"),
    height = 3200, width = 3200, res = 300
  )
  print(p_ts_siteyear)
  dev.off()

  p_ts_site <- ggplot(df_ps_freq_best %>%
    mutate(year = as.factor(year)) %>%
    arrange(sitename) %>%
    mutate(site_lag = str_c(sitename, " (Lag: ", lag, ")")) %>%
    mutate(site_lag = factor(site_lag, levels = (.)$site_lag %>% unique()))) +
    geom_point(aes(x = doy, y = pollen_scale, group = year, col = year)) +
    geom_line(aes(x = doy, y = ps_freq, group = year, col = year)) +
    facet_wrap(. ~ site_lag, ncol = 1) +
    theme_classic() +
    ylab("") +
    # ylim(0, 0.05) +
    ggtitle(str_c("Taxa: ", taxaoi, " (Threshold: ", df_best_thres$direction, " ", df_best_thres$thres, ")"))

  jpeg(str_c(path_output, "ts_site.jpg"),
    height = 3200, width = 3200, res = 300
  )
  print(p_ts_site)
  dev.off()

  p_corr <- ggplot(df_ps_freq_best %>% mutate(year = as.factor(year))) +
    geom_point(aes(x = ps_freq, y = pollen_scale, group = year, col = year)) +
    geom_smooth(aes(x = ps_freq, y = pollen_scale, group = year, col = year), method = "lm", se = F, lwd = 0.5) +
    geom_smooth(aes(x = ps_freq, y = pollen_scale), method = "lm") +
    theme_classic() +
    facet_wrap(. ~ sitename, ncol = 4) +
    coord_equal() +
    ylab("standardized smoothed pollen count^(1/2)") +
    xlab("standardized flowering frequency") +
    # xlim(0, 0.05) +
    # ylim(0, 0.05) +
    ggtitle(str_c("Taxa: ", taxaoi, " (Threshold: ", df_best_thres$direction, " ", df_best_thres$thres, ")"))

  jpeg(str_c(path_output, "corr.jpg"),
    height = 2400, width = 3200, res = 300
  )
  print(p_corr)
  dev.off()
}
stopCluster(cl)
