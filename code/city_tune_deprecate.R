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

  path_output <- paste0("./data/results/", taxaoi, "/")

  # read in leafing phenology data
  # df_flower_doy <- read_rds(paste0(path_output, "flower_doy.rds"))
  df_ts_ext <- read_rds(str_c(path_output, "ts_ext.rds"))
  df_flower_freq <- read_rds(str_c(path_output, "flower_freq.rds"))

  # other site-level data
  df_standard <- df_flower_freq %>%
    left_join(
      df_ts_ext %>%
        filter(id == "pollen") %>%
        filter(var == "pollen concentration (NAB)") %>%
        select(doy, pollen = value, year, site) %>%
        group_by(site, year) %>%
        complete(doy = c((274 - 365):(365 + 151))) %>%
        # mutate(pollen = case_when(pollen > 5 ~ pollen)) %>%
        mutate(pollen_raw = pollen) %>%
        mutate(pollen = pollen %>% sqrt()) %>%
        mutate(pollen_in = zoo::na.approx(pollen, doy, na.rm = F, maxgap = 14)) %>% # fill gap
        mutate(pollen_fill = replace_na(pollen_in, 0)) %>% # fill long gap with 0
        mutate(pollen_sm = whitfun(pollen_fill, 10)) %>% # smooth
        mutate(pollen = case_when(doy %in% flower_window ~ pollen)) %>% # disregard values out of window
        mutate(pollen_raw = case_when(doy %in% flower_window ~ pollen_raw)) %>% # disregard values out of window
        mutate(pollen_sm = case_when(
          doy %in% flower_window ~ pollen_sm,
          TRUE ~ 0
        )) %>% # disregard values out of window
        mutate(pollen_freq = pollen_sm / sum(pollen_sm)) %>% # normalize
        mutate(pollen = pollen / sum(pollen_sm)) %>% # scale similarly
        ungroup() %>%
        select(-pollen_in, -pollen_fill) %>%
        left_join(
          (.) %>%
            group_by(site, doy) %>%
            summarise(pollen_clim = mean(pollen_freq, na.rm = T)) %>%
            ungroup(),
          by = c("site", "doy")
        ),
      by = c("doy", "year", "site")
    ) %>%
    left_join(
      df_pollen_gaus_ts %>%
        filter(taxa == taxaoi),
      by = c("doy", "site")
    ) %>%
    # left_join(
    #   df_ts_ext %>%
    #     filter(id == "npn") %>%
    #     filter(var == "flower observation (USA-NPN)") %>%
    #     select(doy, npn = value, year, site) %>%
    #     group_by(site, year) %>%
    #     complete(doy = c((274 - 365):(365 + 151)), fill = list(npn = 0)) %>%
    #     mutate(npn = case_when(doy %in% flower_window ~ npn)) %>%
    #     mutate(npn_freq = npn/sum(npn, na.rm = T)) ,
    #   by = c("doy", "year", "site")
    # ) %>%
    left_join(
      df_ts_ext %>%
        filter(var == "enhanced vegetation index (PS)") %>%
        group_by(doy, year, site) %>%
        summarise(
          evi = quantile(value, 0.5, na.rm = T),
        ) %>%
        ungroup() %>%
        group_by(site) %>%
        mutate(
          evi = (evi - min(evi, na.rm = T)) / (max(evi, na.rm = T) - min(evi, na.rm = T)),
        ) %>%
        mutate(evi = evi * 0.05) %>%
        ungroup(),
      by = c("doy", "year", "site")
    ) %>%
    left_join(
      df_meta %>%
        drop_na(sitename) %>%
        select(site, sitename),
      by = "site"
    )

  # optimize threshold and lag
  ls_df_tune_site <- vector(mode = "list", length = length(v_site_tune))
  for (s in 1:length(v_site_tune)) {
    siteoi <- v_site_tune[s]
    ls_df_tune_thres <- vector(mode = "list", length = nrow(df_thres_taxa))

    for (t in 1:nrow(df_thres_taxa)) { # select a threshold
      df_standard_thres <- df_standard %>%
        filter(site == siteoi) %>%
        filter(
          direction == df_thres_taxa$direction[t],
          thres == df_thres_taxa$threshold[t]
        ) %>%
        group_by(site, year) %>%
        ungroup()
      non_zero_sample <- df_standard_thres %>%
        filter(pollen_raw > 5 & !is.na(pollen_raw)) %>%
        nrow()
      if (non_zero_sample >= 5 * 4) { # only do tuning when there are more than 20 none-zero pollen count. skip the taxa and site otherwise.
        ts_pollen_freq <- df_standard_thres %>% pull(pollen_freq) # smoothed pollen count, for tuning
        ts_pollen <- df_standard_thres %>% pull(pollen) # pollen count, for calculating accuracy
        sample_size <- df_standard_thres %>%
          filter(!is.na(pollen) & pollen > 0) %>%
          nrow()

        ts_pollen_gaus <- df_standard_thres %>% pull(pollen_gaus) # climatology
        mse_clim <- (weighted.mean((ts_pollen_gaus - ts_pollen)^2, w = ts_pollen, na.rm = T)) # mse between climatology and pollen count

        v_lag <- -100:100 # possible lags to try
        ls_df_tune_lag <-
          foreach(
            l = 1:length(v_lag),
            .packages = c("tidyverse")
          ) %dopar% {
            lag <- v_lag[l]
            if (lag < 0) {
              df_standard_thres_lag <- df_standard_thres %>%
                mutate(freq = lead(freq, n = -lag)) %>%
                mutate(freq = replace_na(freq, 0)) # shift leafing phenology earlier by "lag" days, fill the NA with 0
            } else if (lag == 0) {
              df_standard_thres_lag <- df_standard_thres %>% mutate(freq = replace_na(freq, 0))
            } else if (lag > 0) {
              df_standard_thres_lag <- df_standard_thres %>%
                mutate(freq = lag(freq, n = lag)) %>%
                mutate(freq = replace_na(freq, 0)) # shift leafing phenology later by "lag" days, fill the NA with 0
            }
            ts_freq_lag <- df_standard_thres_lag %>% pull(freq)
            mse <- (mean((ts_freq_lag - ts_pollen_freq)^2, na.rm = T)) # rmse between remotely-sensed flowering phenology and smoothed pollen count
            mse_ps <- (weighted.mean((ts_freq_lag - ts_pollen)^2, w = ts_pollen, na.rm = T)) # rmse between remotely-sensed flowering phenology and pollen count

            print(str_c("site ", s, ", threshold ", t, ", lag ", l))

            data.frame(direction = df_thres_taxa$direction[t], thres = df_thres_taxa$threshold[t], lag = lag, mse = mse, mse_ps = mse_ps, mse_clim = mse_clim)
          }
        ls_df_tune_thres[[t]] <- bind_rows(ls_df_tune_lag) %>%
          # mutate(mse_rm = zoo::rollmean(mse, 7, fill=NA)) %>%
          # arrange(mse_rm) %>%
          # head(5) %>%
          arrange(mse) %>% # choose the lag giving the smallest rmse in the threshold
          head(1) %>%
          mutate(n = sample_size)
      } else {
        ls_df_tune_thres[[t]] <- data.frame(thres = numeric(0), lag = numeric(0), mse = numeric(0), mse_ps = numeric(0), mse_clim = numeric(0), n = numeric(0))
      }
    }
    ls_df_tune_site[[s]] <- bind_rows(ls_df_tune_thres) %>%
      mutate(site = siteoi) %>%
      arrange(mse)
  }
  df_tune <- bind_rows(ls_df_tune_site)
  write_rds(df_tune, paste0(path_output, "tune.rds"))

  df_best_thres <- df_tune %>%
    group_by(direction, thres) %>%
    summarise(mse = mean(mse)) %>% # mean rmse for each threshold
    arrange(mse) %>%
    head(1) %>% # keep threshold giving the smallest mean rmse
    select(direction, thres)

  # getting time series with best threshold and lag
  ls_df_standard_best_site <- vector(mode = "list", length = length(v_site_tune))
  for (s in 1:length(v_site_tune)) {
    siteoi <- v_site_tune[s]
    lagoi <- df_tune %>%
      filter(direction == df_best_thres$direction, thres == df_best_thres$thres, site == siteoi) %>%
      pull(lag)
    if (length(lagoi) > 0) {
      df_standard_best <- df_standard %>%
        filter(site == siteoi) %>%
        filter(
          direction == df_best_thres$direction,
          thres == df_best_thres$thres
        ) %>%
        ungroup() %>%
        group_by(site, year)

      if (lagoi < 0) {
        df_standard_best <- df_standard_best %>%
          mutate(freq = lead(freq, n = -lagoi)) %>%
          mutate(freq = replace_na(freq, 0))
      } else if (lagoi == 0) {
        df_standard_best <- df_standard_best %>% mutate(freq = replace_na(freq, 0))
      } else if (lagoi > 0) {
        df_standard_best <- df_standard_best %>%
          mutate(freq = lag(freq, n = lagoi)) %>%
          mutate(freq = replace_na(freq, 0))
      }

      ls_df_standard_best_site[[s]] <- df_standard_best %>%
        ungroup() %>%
        mutate(lag = lagoi)
    }
  }
  df_standard_best <- bind_rows(ls_df_standard_best_site)
  write_rds(df_standard_best, str_c(path_output, "ts_best.rds"))

  # make plots
  p_ts_siteyear <- ggplot(df_standard_best) +
    geom_point(aes(x = doy, y = evi, col = "enhanced vegetation index (PS)"), alpha = 0.2) +
    # geom_point(aes(x = doy, y = npn, col = "flower observation (USA-NPN)"), alpha = 0.5) +
    geom_point(aes(x = doy, y = pollen, col = "pollen concentration (NAB)")) +
    geom_line(aes(x = doy, y = pollen_gaus, col = "pollen concentration (NAB)"), alpha = 0.5, lwd = 1) +
    geom_line(aes(x = doy, y = freq, col = "flowering frequency (PS)"), lwd = 1) +
    theme_classic() +
    facet_wrap(. ~ paste0(sitename, " (Lag: ", lag, ")") * year, ncol = 4) +
    scale_color_manual(values = cols) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    ylab("") +
    ylim(0, 0.05) +
    ggtitle(paste0("Taxa: ", taxaoi, " (Threshold: ", df_best_thres$direction, " ", df_best_thres$thres, ")"))

  jpeg(paste0(path_output, "ts_siteyear.jpg"),
    height = 3200, width = 3200, res = 300
  )
  print(p_ts_siteyear)
  dev.off()

  p_ts_site <- ggplot(df_standard_best %>%
    mutate(year = as.factor(year))) +
    geom_point(aes(x = doy, y = pollen, group = year, col = year)) +
    geom_line(aes(x = doy, y = freq, group = year, col = year)) +
    facet_wrap(. ~ paste0(sitename, " (Lag: ", lag, ")"), ncol = 1) +
    theme_classic() +
    ylab("") +
    ylim(0, 0.05) +
    ggtitle(paste0("Taxa: ", taxaoi, " (Threshold: ", df_best_thres$direction, " ", df_best_thres$thres, ")"))

  jpeg(paste0(path_output, "ts_site.jpg"),
    height = 3200, width = 3200, res = 300
  )
  print(p_ts_site)
  dev.off()

  p_corr <- ggplot(df_standard_best %>% mutate(year = as.factor(year))) +
    geom_point(aes(x = freq, y = pollen, group = year, col = year)) +
    geom_smooth(aes(x = freq, y = pollen, group = year, col = year), method = "lm", se = F, lwd = 0.5) +
    geom_smooth(aes(x = freq, y = pollen), method = "lm") +
    theme_classic() +
    facet_wrap(. ~ sitename, ncol = 4) +
    coord_equal() +
    ylab("standardized smoothed pollen count^(1/2)") +
    xlab("standardized flowering frequency") +
    xlim(0, 0.05) +
    ylim(0, 0.05) +
    ggtitle(paste0("Taxa: ", taxaoi, " (Threshold: ", df_best_thres$direction, " ", df_best_thres$thres, ")"))

  jpeg(paste0(path_output, "corr.jpg"),
    height = 2400, width = 3200, res = 300
  )
  print(p_corr)
  dev.off()
}
stopCluster(cl)
