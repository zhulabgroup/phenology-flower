if (.full_data) {
  for (taxaoi in v_taxa) {
    taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
    flower_window <- seq(df_flower_window %>% filter(taxa == taxaoi) %>% pull(start),
      df_flower_window %>% filter(taxa == taxaoi) %>% pull(end),
      by = 1
    )
    df_thres_taxa <- get_thres_taxa(df_thres, taxaoi)

    path_output <- str_c(.path$intermediate, "urban/", taxaoi, "/")
    dir.create(path_output, recursive = T, showWarnings = F)

    # get NAB data
    df_nab_freq <- df_nab_short %>%
      filter(year >= 2017) %>%
      filter(taxa == taxaoi_short) %>%
      select(site, year, doy, pollen = count) %>%
      util_extend_ts() %>%
      filter(year >= 2018) %>%
      group_by(site, year) %>%
      complete(doy = c(-90:(365 + 90))) %>%
      mutate(pollen = case_when(doy >= min(flower_window) & doy <= max(flower_window) ~ pollen)) %>% # disregard values out of window
      mutate(pollen_tr = pollen^(1 / 2)) %>%
      mutate(pollen_fill = case_when(
        doy >= min(flower_window) & doy <= max(flower_window) ~ pollen_tr,
        TRUE ~ 0
      )) %>% # disregard values out of window
      mutate(pollen_sm = util_fill_whit(x = pollen_fill, maxgap = Inf, lambda = 10, minseg = 1)) %>%
      mutate(pollen_sum = sum(pollen_sm, na.rm = T)) %>%
      mutate(pollen_freq = pollen_sm / pollen_sum) %>% # normalize
      mutate(pollen_scale = pollen_tr / pollen_sum) %>% # scale similarly
      ungroup() %>%
      drop_na(pollen_freq)
    write_rds(df_nab_freq, str_c(path_output, "nab_freq.rds"))

    # get NPN data
    df_npn <- df_npn_short %>%
      filter(year >= 2017) %>%
      filter(taxa == taxaoi_short) %>%
      select(site, year, doy, perc) %>%
      util_extend_ts() %>%
      filter(year >= 2018) %>%
      group_by(site, year) %>%
      complete(doy = c(-90:(365 + 90))) %>%
      mutate(perc = case_when(
        doy >= min(flower_window) & doy <= max(flower_window) ~ perc
      )) #  disregard values out of window

    write_rds(df_npn, str_c(path_output, "npn.rds"))
  }
}
