process_ps <- function(df_ps) {
  df_ps_proc <- df_ps %>%
    drop_na() %>%
    mutate(date = as.Date(time)) %>%
    mutate(
      year = format(time, "%Y") %>% as.integer(),
      doy = format(time, "%j") %>% as.integer(),
      hour = format(strptime(time, "%Y-%m-%d %H:%M:%S"), "%H") %>% as.integer()
    ) %>%
    filter(clear == 1, snow == 0, shadow == 0, haze_light == 0, haze_heavy == 0, cloud == 0, confidence >= 80) %>%
    # select(id, time, lon, lat, blue, green, red, nir) %>%
    group_by(id, lon, lat, date, year, doy) %>%
    summarise(
      blue = mean(blue),
      green = mean(green),
      red = mean(red),
      nir = mean(nir)
    ) %>%
    ungroup() %>%
    mutate(evi = 2.5 * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)) %>%
    filter(evi > 0, evi <= 1) %>%
    filter(red > 0, green > 0, blue > 0)

  # remote outliers using climatology
  df_ps_clim <- df_ps_proc %>%
    group_by(doy) %>%
    summarise(evi_clim = median(evi, na.rm = T)) %>%
    ungroup() %>%
    complete(doy = seq(1, 365, by = 1)) %>%
    mutate(evi_clim = zoo::na.approx(evi_clim, rule = 2)) %>%
    mutate(evi_clim = whitfun(evi_clim, lambda = 50))

  df_ps_proc <- df_ps_proc %>%
    filter(doy <= 365) %>%
    left_join(df_ps_clim, by = "doy") %>%
    filter(abs(evi_clim - evi) <= 0.2)

  return(df_ps_proc)
}
