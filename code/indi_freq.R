# summarize frequency on the site level

if (!file.exists("data/processed/dt_flower_freq.rds")) {
  flower_doy_df <- read_rds("data/processed/dt_flower_doy.rds")
  flower_freq_df_year_list <- vector(mode = "list", length = length(year_list) + 1)
  for (y in 1:(length(year_list) + 1)) {
    yearoi <- c(2017, year_list)[y]
    flower_doy_df_year <- flower_doy_df %>%
      filter(year == yearoi)
    flower_freq_df_thres_list <- vector(mode = "list", length = nrow(thres_df_taxa))
    for (t in 1:nrow(thres_df_taxa)) {
      flower_freq_df_thres_list[[t]] <- flower_doy_df_year %>%
        drop_na(start, end) %>%
        filter(
          direction == thres_df_taxa$direction[t],
          thres == thres_df_taxa$threshold[t]
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
    flower_freq_df_year_list[[y]] <- bind_rows(flower_freq_df_thres_list) %>%
      mutate(year = yearoi)
    print(str_c(siteoi, ", ", yearoi))
  }
  flower_freq_df <- bind_rows(flower_freq_df_year_list)

  write_rds(flower_freq_df, "data/processed/dt_flower_freq.rds")
}
flower_freq_df <- read_rds("data/processed/dt_flower_freq.rds")

# join frequency distribution with EVI, NAB and NPN data
# flower_freq_df <- flower_freq_df %>%
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
    data = flower_freq_df %>%
      filter(year == 2017) # %>%
    # filter(thres %in% c(0.4, 0.5, 0.6))
    ,
    aes(x = doy, y = freq_sm, group = thres, col = thres)
  ) +
  # geom_line(data = df_dt_flower_peak %>%
  #             complete(doy = seq(1, 365)))
  theme_classic() +
  scale_color_viridis_c()
