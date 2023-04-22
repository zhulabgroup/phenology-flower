cl <- makeCluster(36, outfile = "")
registerDoSNOW(cl)

df_thres_taxa <- get_thres_taxa(df_thres, "neon_up")

ls_df_ts_ext_site <- ls_df_doy_site <- vector(mode = "list", length = length(df_neon_meta$site))
for (s in 1:length(df_neon_meta$site)) {
  siteoi <- df_neon_meta$site[s]
  df_plant_site <- df_plant %>%
    filter(site == siteoi) %>%
    drop_na(lon, lat)

  v_id <- df_plant_site %>% pull(id)

  # preprocess ps data
  df_ps_site <- read_rds(paste0(path_ps, "ts/ps_", siteoi, ".rds"))
  df_ps_site_proc <- process_ps(df_ps_site %>% filter(id %in% v_id))

  df_ts_site <- df_ps_site_proc %>%
    left_join(df_plant_site, by = c("id", "lon", "lat")) %>%
    dplyr::select(id, date, evi) %>%
    arrange(id, date) %>%
    mutate(doy = format(date, "%j") %>% as.numeric()) %>%
    mutate(year = format(date, "%Y") %>% as.numeric())

  ls_df_ts_ext_year <- ls_df_doy_year <- vector(mode = "list", length = length(v_year))
  for (y in 1:length(v_year)) {
    yearoi <- v_year[y]
    df_ts_year <- df_ts_site %>%
      filter(doy != 366) %>%
      filter(year == yearoi | year == (yearoi - 1) | year == (yearoi + 1)) %>%
      mutate(doy = ifelse(doy > 273 & year == yearoi - 1, doy - 365, doy)) %>%
      mutate(year = ifelse(doy <= 0 & year == yearoi - 1, year + 1, year)) %>%
      mutate(doy = ifelse(doy < 152 & year == yearoi + 1, doy + 365, doy)) %>%
      mutate(year = ifelse(doy > 365 & year == yearoi + 1, year - 1, year)) %>%
      filter(year == yearoi) %>%
      arrange(doy)
    ls_df_ts_ext_year[[y]] <- df_ts_year

    df_ts_year_evi <- df_ts_year %>%
      dplyr::select(id, doy, evi)

    ls_df_doy_id <-
      foreach(
        i = 1:length(v_id),
        .packages = c("tidyverse", "ptw", "segmented")
      ) %dopar% {
        i <-sample(1:length(v_id), 1)
        idoi <- as.character(v_id)[i]

        print(paste0(i, " out of ", length(v_id)))
        df_doy_id<-get_doy(df_thres_taxa, df_ts_year_evi, idoi, min_days = 30)
        
        p<-ggplot() +
          geom_point(
            data = df_ts_year_evi %>% filter(id==idoi),
            aes(x = doy, y = evi)
          ) +
          theme_classic() 
        if (nrow(df_doy_id)>0) {
          p<-p+
          geom_vline(data = df_doy_id, aes(xintercept = doy), col = "dark green", alpha = 0.2) +
          geom_vline(data = df_doy_id %>%
                       filter(thres == 0.5) , aes(xintercept = doy), col = "dark green", alpha = 0.8)
        }
        p
        
        df_doy_id
        
      }

    ls_df_doy_year[[y]] <- bind_rows(ls_df_doy_id) %>%
      mutate(year = yearoi)
    print(paste0(siteoi, ", ", yearoi))
  }
  ls_df_ts_ext_site[[s]] <- bind_rows(ls_df_ts_ext_year) %>%
    mutate(site = siteoi)

  ls_df_doy_site[[s]] <- bind_rows(ls_df_doy_year) %>%
    mutate(site = siteoi)
}
ts_df_ext <- bind_rows(ls_df_ts_ext_site)
df_doy <- bind_rows(ls_df_doy_site)

write_rds(ts_df_ext, "data/processed/neon_ts_ext.rds")
write_rds(df_doy, "data/processed/neon_doy.rds")
stopCluster(cl)
