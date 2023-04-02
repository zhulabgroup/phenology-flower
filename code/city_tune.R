cl <- makeCluster(36, outfile = "")
registerDoSNOW(cl)
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
  df_comp <- df_flower_freq %>%
    left_join(
      df_ts_ext %>%
        filter(id == "pollen") %>%
        filter(var == "pollen concentration (NAB)") %>%
        select(doy, pollen = value, year, site) %>%
        mutate(pollen = pollen %>% sqrt()) %>%
        group_by(site, year) %>%
        complete(doy = c((274 - 365):(365 + 151))) %>%
        mutate(pollen_in = zoo::na.approx(pollen, doy, na.rm = F, maxgap = 14)) %>%
        mutate(pollen_fill = replace_na(pollen_in, 0)) %>%
        mutate(pollen_sm = whitfun(pollen_fill, 30)) %>%
        ungroup() %>%
        dplyr::select(-pollen_in, -pollen_fill) %>%
        mutate(pollen = case_when(doy %in% flower_window ~ pollen)) %>%
        mutate(pollen_sm = case_when(
          doy %in% flower_window ~ pollen_sm,
          TRUE ~ 0
        )) %>%
        left_join(
          (.) %>%
            group_by(site, doy) %>%
            summarise(pollen_clim = mean(pollen_sm, na.rm = T)) %>%
            ungroup(),
          by = c("site", "doy")
        ),
      by = c("doy", "year", "site")
    ) %>%
    left_join(
      df_ts_ext %>%
        filter(id == "npn") %>%
        filter(var == "flower observation (USA-NPN)") %>%
        select(doy, npn = value, year, site) %>%
        mutate(npn = case_when(doy %in% flower_window ~ npn)),
      by = c("doy", "year", "site")
    ) %>%
    left_join(
      df_ts_ext %>%
        filter(var == "enhanced vegetation index (PS)") %>%
        group_by(doy, year, site) %>%
        summarise(
          evi_lower = quantile(value, 0.25, na.rm = T),
          evi = quantile(value, 0.5, na.rm = T),
          evi_upper = quantile(value, 0.75, na.rm = T)
        ),
      by = c("doy", "year", "site")
    ) %>%
    left_join(
      df_meta %>%
        drop_na(sitename) %>%
        select(site, sitename),
      by = "site"
    )

  # standardize all data to be between 0 and 1
  df_standard <- df_comp %>%
    group_by(site, sitename, thres, direction) %>%
    mutate(freq = (freq - min(freq, na.rm = T)) / (max(freq, na.rm = T) - min(freq, na.rm = T))) %>%
    mutate(freq_sm = (freq_sm - min(freq_sm, na.rm = T)) / (max(freq_sm, na.rm = T) - min(freq_sm, na.rm = T))) %>%
    mutate(
      pollen = (pollen - min(pollen, na.rm = T)) / (max(pollen, na.rm = T) - min(pollen, na.rm = T)),
      pollen_clim = (pollen_clim - min(pollen_sm, na.rm = T)) / (max(pollen_sm, na.rm = T) - min(pollen_sm, na.rm = T)),
      pollen_sm = (pollen_sm - min(pollen_sm, na.rm = T)) / (max(pollen_sm, na.rm = T) - min(pollen_sm, na.rm = T))
    ) %>%
    mutate(npn = (npn - min(npn, na.rm = T)) / (max(npn, na.rm = T) - min(npn, na.rm = T))) %>%
    mutate(
      evi_lower = (evi_lower - min(evi, na.rm = T)) / (max(evi, na.rm = T) - min(evi, na.rm = T)),
      evi_upper = (evi_upper - min(evi, na.rm = T)) / (max(evi, na.rm = T) - min(evi, na.rm = T)),
      evi = (evi - min(evi, na.rm = T)) / (max(evi, na.rm = T) - min(evi, na.rm = T))
    ) %>%
    ungroup()

  # optimize threshold and lag
  ls_df_tune_site <- vector(mode = "list", length = length(v_site))
  for (s in 1:length(v_site)) { # select a region, here each site is treated as an individual region
    siteoi <- v_site[s]
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
      sample_size <- df_standard_thres %>%
        filter(!is.na(pollen), pollen > 0) %>%
        nrow()
      if (sample_size >= 5 * 4) { # only do tuning when there are more than 20 none-zero pollen count. skip the taxa and site otherwise.
        ts_pollen_sm <- df_standard_thres %>% pull(pollen_sm) # smoothed pollen count, for tuning
        ts_pollen <- df_standard_thres %>% pull(pollen) # pollen count, for calculating accuracy

        ts_pollen_clim <- df_standard_thres %>% pull(pollen_clim) # climatology
        rmse_clim <- sqrt(mean((ts_pollen_clim - ts_pollen)^2, na.rm = T)) # rmse between climatology and pollen count

        v_lag <- -200:200 # possible lags to try
        ls_df_tune_lag <-
          foreach(
            l = 1:length(v_lag),
            .packages = c("tidyverse")
          ) %dopar% {
            lag <- v_lag[l]
            if (lag < 0) {
              df_standard_thres_lag <- df_standard_thres %>%
                mutate(freq_sm = lead(freq_sm, n = -lag)) %>%
                mutate(freq_sm = replace_na(freq_sm, 0)) # shift leafing phenology earlier by "lag" days, fill the NA with 0
            } else if (lag == 0) {
              df_standard_thres_lag <- df_standard_thres %>% mutate(freq_sm = replace_na(freq_sm, 0))
            } else if (lag > 0) {
              df_standard_thres_lag <- df_standard_thres %>%
                mutate(freq_sm = lag(freq_sm, n = lag)) %>%
                mutate(freq_sm = replace_na(freq_sm, 0)) # shift leafing phenology later by "lag" days, fill the NA with 0
            }
            ts_freq_lag <- df_standard_thres_lag %>% pull(freq_sm)
            rmse <- sqrt(mean((ts_freq_lag - ts_pollen_sm)^2, na.rm = T)) # rmse between remotely-sensed flowering phenology and smoothed pollen count
            rmse_ps <- sqrt(mean((ts_freq_lag - ts_pollen)^2, na.rm = T)) # rmse between remotely-sensed flowering phenology and pollen count

            print(str_c("site ", s, ", threshold ", t, ", lag ", l))

            data.frame(direction = df_thres_taxa$direction[t], thres = df_thres_taxa$threshold[t], lag = lag, rmse = rmse, rmse_ps = rmse_ps, rmse_clim = rmse_clim)
          }
        ls_df_tune_thres[[t]] <- bind_rows(ls_df_tune_lag) %>%
          arrange(rmse) %>% # choose the lag giving the smallest rmse in the threshold
          head(1)
      } else {
        ls_df_tune_thres[[t]] <- data.frame(thres = numeric(0), lag = numeric(0), rmse = numeric(0), rmse_ps = numeric(0), rmse_clim = numeric(0))
      }
    }
    ls_df_tune_site[[s]] <- bind_rows(ls_df_tune_thres) %>%
      mutate(site = siteoi) %>%
      arrange(rmse)
  }
  df_tune <- bind_rows(ls_df_tune_site)
  write_rds(df_tune, paste0(path_output, "tune.rds"))

  df_best_thres <- df_tune %>%
    group_by(direction, thres) %>%
    summarise(rmse = median(rmse)) %>% # median rmse for each threshold
    arrange(rmse) %>%
    head(1) %>% # keep threshold giving the smallest median rmse
    select(direction, thres)

  # getting time series with best threshold and lag
  ls_df_standard_best_site <- vector(mode = "list", length = length(v_site))
  for (s in 1:length(v_site)) {
    siteoi <- v_site[s]
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
          mutate(freq_sm = lead(freq_sm, n = -lagoi)) %>%
          mutate(freq_sm = replace_na(freq_sm, 0))
      } else if (lagoi == 0) {
        df_standard_best <- df_standard_best %>% mutate(freq_sm = replace_na(freq_sm, 0))
      } else if (lagoi > 0) {
        df_standard_best <- df_standard_best %>%
          mutate(freq_sm = lag(freq_sm, n = lagoi)) %>%
          mutate(freq_sm = replace_na(freq_sm, 0))
      }

      ls_df_standard_best_site[[s]] <- df_standard_best %>% mutate(lag = lagoi)
    }
  }
  df_standard_best <- bind_rows(ls_df_standard_best_site)
  write_rds(df_standard_best, str_c(path_output, "ts_stan.rds"))

  # make plots
  p_ts_siteyear <- ggplot(df_standard_best) +
    geom_point(aes(x = doy, y = npn, col = "flower observation (USA-NPN)"), alpha = 0.5) +
    geom_point(aes(x = doy, y = pollen, col = "pollen concentration (NAB)")) +
    geom_line(aes(x = doy, y = pollen_clim, col = "pollen concentration (NAB)"), alpha = 0.5, lwd = 1) +
    geom_point(aes(x = doy, y = evi, col = "enhanced vegetation index (PS)"), alpha = 0.2) +
    geom_line(aes(x = doy, y = freq_sm, col = "flowering frequency"), lwd = 1) +
    theme_classic() +
    facet_wrap(. ~ paste0(sitename, " (Lag: ", lag, ")") * year, ncol = 4) +
    scale_color_manual(values = cols) +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    ylab("") +
    ggtitle(paste0("Taxa: ", taxaoi, " (Threshold: ", df_best_thres$direction, " ", df_best_thres$thres, ")"))

  jpeg(paste0(path_output, "ts_siteyear.jpg"),
    height = 3200, width = 3200, res = 300
  )
  print(p_ts_siteyear)
  dev.off()

  p_ts_site <- ggplot(df_standard_best %>%
    mutate(year = as.factor(year))) +
    geom_point(aes(x = doy, y = pollen, group = year, col = year)) +
    geom_line(aes(x = doy, y = freq_sm, group = year, col = year)) +
    facet_wrap(. ~ paste0(sitename, " (Lag: ", lag, ")"), ncol = 1) +
    theme_classic() +
    ylab("") +
    ggtitle(paste0("Taxa: ", taxaoi, " (Threshold: ", df_best_thres$direction, " ", df_best_thres$thres, ")"))

  jpeg(paste0(path_output, "ts_site.jpg"),
    height = 3200, width = 3200, res = 300
  )
  print(p_ts_site)
  dev.off()

  p_corr <- ggplot(df_standard_best %>% mutate(year = as.factor(year))) +
    geom_point(aes(x = freq_sm, y = pollen, group = year, col = year)) +
    geom_smooth(aes(x = freq_sm, y = pollen, group = year, col = year), method = "lm", se = F, lwd = 0.5) +
    geom_smooth(aes(x = freq_sm, y = pollen), method = "lm") +
    theme_classic() +
    facet_wrap(. ~ sitename, ncol = 4) +
    coord_equal() +
    xlim(0, 1) +
    ylim(0, 1) +
    ylab("standardized pollen count^(1/2)") +
    xlab("standardized flowering frequency") +
    ggtitle(paste0("Taxa: ", taxaoi, " (Threshold: ", df_best_thres$direction, " ", df_best_thres$thres, ")"))

  jpeg(paste0(path_output, "corr.jpg"),
    height = 2400, width = 3200, res = 300
  )
  print(p_corr)
  dev.off()
}
stopCluster(cl)
