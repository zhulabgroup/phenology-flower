# summarize frequency on the site level
if (!file.exists("data/processed/dt_flower_freq.rds")) {
  df_flower_doy <- read_rds("data/processed/dt_flower_doy.rds")
  ls_df_flower_freq_year <- vector(mode = "list", length = length(v_year) + 1)
  for (y in 1:(length(v_year) + 1)) {
    yearoi <- c(2017, v_year)[y]
    df_flower_doy_year <- df_flower_doy %>%
      filter(year == yearoi)
    ls_df_flower_freq_thres <- vector(mode = "list", length = nrow(df_thres_taxa))
    for (t in 1:nrow(df_thres_taxa)) {
      ls_df_flower_freq_thres[[t]] <- df_flower_doy_year %>%
        drop_na(start, end) %>%
        filter(
          direction == df_thres_taxa$direction[t],
          thres == df_thres_taxa$threshold[t]
        ) %>%
        group_by(doy, thres, direction) %>%
        summarise(count = n()) %>%
        mutate(freq = count / n()) %>%
        ungroup() %>%
        group_by(thres, direction) %>%
        complete(doy = seq(1, 365, by = 1), fill = list(count = 0, freq = 0)) %>%
        ungroup() %>%
        mutate(freq_sm = freq %>% whitfun(lambda = 30))
    }
    ls_df_flower_freq_year[[y]] <- bind_rows(ls_df_flower_freq_thres) %>%
      mutate(year = yearoi)
    print(str_c(siteoi, ", ", yearoi))
  }
  df_flower_freq <- bind_rows(ls_df_flower_freq_year)

  write_rds(df_flower_freq, "data/processed/dt_flower_freq.rds")
}
df_flower_freq <- read_rds("data/processed/dt_flower_freq.rds")

# join frequency distribution with EVI, NAB and NPN data
# df_flower_freq <- df_flower_freq %>%
#   mutate(doy = factor(doy, levels = c((274 - 365):(365 + 151)))) %>%
#   complete(doy, thres, direction, fill = list(count = 0, freq = 0)) %>%
#   mutate(doy = doy %>% as.character() %>% as.numeric()) %>%
#   arrange(doy) %>%
#   full_join(ts_df_subset %>%
#               filter(id == "pollen") %>%
#               filter(var == "pollen concentration (NAB)") %>%
#               filter(year == yearoi) %>%
#               dplyr::select(doy, pollen = value),
#             by = "doy"
#   ) %>%
#   full_join(ts_df_subset %>%
#               filter(id == "npn") %>%
#               filter(var == "flower observation (USA-NPN)") %>%
#               filter(year == yearoi) %>%
#               dplyr::select(doy, npn = value),
#             by = "doy"
#   ) %>%
#   full_join(ts_df_subset_summary %>%
#               filter(var == "enhanced vegetation index (PS)") %>%
#               filter(year == yearoi) %>%
#               dplyr::select(doy, evi = q2),
#             by = "doy"
#   ) %>%
#   mutate(doy = as.numeric(doy)) %>%
#   mutate(site = siteoi, year = yearoi) %>%
#   drop_na(doy, thres)

p_dt_freq <- ggplot() +
  geom_line(
    data = df_flower_freq %>%
      filter(year == 2017) # %>%
    # filter(thres %in% c(0.4, 0.5, 0.6))
    ,
    aes(x = doy, y = freq_sm, group = thres, col = thres)
  ) +
  # geom_line(data = df_dt_flower_peak %>%
  #             complete(doy = seq(1, 365)))
  theme_classic() +
  scale_color_viridis_c()
