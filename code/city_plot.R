plot_ts_siteyear <- function(df, save = T, path_output, cv = F) {
  if (!cv) {
    direction <- df %>%
      pull(direction) %>%
      unique()
    thres <- df %>%
      pull(thres) %>%
      unique()
  } else {
    direction <- thres <- ""
  }

  p_ts_siteyear <- ggplot(df %>%
    arrange(sitename) %>%
    mutate(site_lag = str_c(sitename, " (Lag: ", lag, ")")) %>%
    mutate(site_lag = factor(site_lag, levels = (.)$site_lag %>% unique()))) +
    # geom_point(aes(x = doy, y = evi, col = "enhanced vegetation index (PS)"), alpha = 0.2) +
    geom_point(aes(x = doy, y = pollen_scale, col = "NAB")) +
    geom_line(aes(x = doy, y = ps_freq_lag, col = "PlanetScope"), lwd = 1) +
    scale_color_manual(values = c("PlanetScope" = "dark blue", "NAB" = "dark red")) +
    theme_classic() +
    # scale_y_continuous(trans = "sqrt") +
    facet_wrap(. ~ site_lag * year, ncol = 5, scales = "free_y") +
    theme(legend.position = "bottom") +
    theme(legend.title = element_blank()) +
    labs(
      y = "Standardized pollen concentration",
      x = "Day of Year",
      col = "",
    ) +
    # ylim(0, 0.05) +
    ggtitle(str_c("Taxa: ", taxaoi, " (Threshold: ", direction, " ", thres, ")"))

  if (!cv) {
    filename <- "ts_siteyear.jpg"
  } else {
    filename <- "ts_siteyear_cv.jpg"
  }

  if (save) {
    jpeg(str_c(path_output, filename),
      height = 3200, width = 3200, res = 300
    )
    print(p_ts_siteyear)
    dev.off()
  }

  return(p_ts_siteyear)
}


plot_ts_site <- function(df, save = T, path_output, cv = F) {
  if (!cv) {
    direction <- df %>%
      pull(direction) %>%
      unique()
    thres <- df %>%
      pull(thres) %>%
      unique()
  } else {
    direction <- thres <- ""
  }

  p_ts_site <- ggplot(df %>%
    mutate(year = as.factor(year)) %>%
    arrange(sitename) %>%
    mutate(site_lag = str_c(sitename, " (Lag: ", lag, ")")) %>%
    mutate(site_lag = factor(site_lag, levels = (.)$site_lag %>% unique()))) +
    geom_point(aes(x = doy, y = pollen_scale, group = year, col = year), alpha = 0.5) +
    geom_line(aes(x = doy, y = ps_freq_lag, group = year, col = year), alpha = 0.75) +
    # scale_y_continuous(trans = "sqrt") +
    facet_wrap(. ~ site_lag, ncol = 1, scales = "free_y") +
    theme_classic() +
    labs(
      y = "Standardized pollen concentration",
      x = "Day of year",
      col = "Year"
    ) +
    # ylim(0, 0.05) +
    ggtitle(str_c("Taxa: ", taxaoi, " (Threshold: ", direction, " ", thres, ")"))

  if (!cv) {
    filename <- "ts_site.jpg"
  } else {
    filename <- "ts_site_cv.jpg"
  }

  if (save) {
    jpeg(str_c(path_output, filename),
      height = 3200, width = 3200, res = 300
    )
    print(p_ts_site)
    dev.off()
  }

  return(p_ts_site)
}


plot_corr <- function(df, save = T, path_output, cv = F) {
  if (!cv) {
    direction <- df %>%
      pull(direction) %>%
      unique()
    thres <- df %>%
      pull(thres) %>%
      unique()
  } else {
    direction <- thres <- ""
  }

  p_corr <- ggplot(df %>% mutate(year = as.factor(year))) +
    geom_point(aes(x = ps_freq_lag, y = pollen_scale, group = year, col = year), alpha = 0.5) +
    geom_smooth(aes(x = ps_freq_lag, y = pollen_scale, group = year, col = year), method = "lm", se = F, lwd = 0.5) +
    geom_smooth(aes(x = ps_freq_lag, y = pollen_scale), method = "lm") +
    theme_classic() +
    facet_wrap(. ~ sitename, ncol = 4, scales = "free") +
    geom_abline(intercept = 0, slope = 1, col = "red", linetype = 2) +
    # coord_equal() +
    labs(
      x = "Predicted standardized pollen concentration",
      y = "Observed standardized pollen concentration",
      col = "Year"
    ) +
    # scale_x_continuous(trans = "sqrt") +
    # scale_y_continuous(trans = "sqrt") +
    ggtitle(str_c("Taxa: ", taxaoi, " (Threshold: ", direction, " ", thres, ")"))

  if (!cv) {
    filename <- "corr.jpg"
  } else {
    filename <- "corr_cv.jpg"
  }

  if (save) {
    jpeg(str_c(path_output, filename),
      height = 2400, width = 3200, res = 300
    )
    print(p_corr)
    dev.off()
  }

  return(p_corr)
}
