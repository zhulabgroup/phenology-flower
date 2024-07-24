util_extend_ts <- function(df) {
  df_current <- df
  df_next <- df_current %>%
    mutate(
      doy = doy + 365,
      year = year - 1
    ) %>%
    filter(doy <= 365 + 90)
  df_previous <- df_current %>%
    mutate(
      doy = doy - 365,
      year = year + 1
    ) %>%
    filter(doy >= -90)

  df_extend <- bind_rows(list(df_current, df_next, df_previous)) %>%
    arrange(year, doy)

  return(df_extend)
}
