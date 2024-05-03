pacman::p_load("mclust")
for (taxaoi in v_taxa) {
  path_output <- str_c("./data/results/", taxaoi, "/")

  # read in nab data
  df_nab_freq <- list.files(path_output, "nab_freq", full.names = T) %>% read_rds()

  ls_df_pollen_gaus_ts_site <- ls_df_pollen_gaus_param_site <- vector(mode = "list")
  for (siteoi in v_site_tune) {
    df_nab_freq_site <- df_nab_freq %>%
      filter(site == siteoi)

    non_zero_sample <- df_nab_freq_site %>%
      filter(pollen_scale > 0 & !is.na(pollen_scale)) %>%
      nrow()

    if (non_zero_sample >= 30) {
      if (!(taxaoi == "Fraxinus" & siteoi %in% c("NY", "DT"))) {
        ls_df_pollen_gaus_param_year <- ls_df_pollen_gaus_ts_year <- vector(mode = "list")
        for (yearoi in v_year) {
          df_nab_freq_year <- df_nab_freq_site %>%
            filter(year == yearoi) %>%
            drop_na()
          
          if (df_nab_freq_year %>%
              filter(pollen_scale > 0 & !is.na(pollen_scale)) %>%
              nrow() > 0) {
            # fit Gaussian mixture model
            set.seed(1)
            sam <- sample(x = df_nab_freq_year$doy, size = 1000, replace = T, prob = df_nab_freq_year$pollen_tr)
            fit <- mclust::Mclust(sam, G = 1, model = "V")
            mean <- fit$parameters$mean
            sd <- fit$parameters$variance$sigmasq %>% sqrt()
            
            ls_df_pollen_gaus_param_year[[yearoi %>% as.character()]] <- data.frame(mean = mean, sd = sd, year = yearoi)
            ls_df_pollen_gaus_ts_year[[yearoi %>% as.character()]] <- data.frame(doy = c(-90:(365 + 90))) %>%
              mutate(pollen_gaus = dnorm(doy, mean, sd)) %>%
              mutate(year = yearoi)
            
            # ggplot()+
            #   geom_density(data = data.frame(sam), aes(sam))+
            #   geom_line(data = ls_df_pollen_gaus_ts_year[[yearoi %>% as.character]] , aes(x = doy, y=pollen_gaus), col = "red")
            print(str_c(taxaoi, ", ", siteoi, ", ", yearoi))
          }
        }
        ls_df_pollen_gaus_param_site[[siteoi]] <- bind_rows(ls_df_pollen_gaus_param_year) %>%
          mutate(site = siteoi)
        ls_df_pollen_gaus_ts_site[[siteoi]] <- bind_rows(ls_df_pollen_gaus_ts_year) %>%
          mutate(site = siteoi)
      }
    }
  }

  df_pollen_gaus_ts <- bind_rows(ls_df_pollen_gaus_ts_site) %>%
    left_join(df_nab_freq, by = c("site", "year", "doy")) %>%
    mutate(pollen_gaus_pred = (pollen_gaus * pollen_sum)^2) %>%
    select(site, year, doy, everything())
  write_rds(df_pollen_gaus_ts, str_c(path_output, "ts_gaus.rds"))

  df_tune_gaus <- bind_rows(ls_df_pollen_gaus_param_site) %>%
    left_join(
      df_pollen_gaus_ts %>%
        group_by(site, year) %>%
        drop_na(pollen_gaus) %>%
        summarise(
          rmse_raw = (mean((pollen_gaus_pred - pollen)^2, na.rm = T)) %>% sqrt(),
          nrmse = (mean((pollen_gaus - pollen_scale)^2, na.rm = T)) %>% sqrt() %>% `/`(max(pollen_scale, na.rm = T)),
          # pearson = cor(pollen_gaus, pollen_scale, method = "pearson", use = "complete.obs"),
          spearman = cor(pollen_gaus, pollen_scale, method = "spearman", use = "complete.obs"),
          spearman_sig = cor.test(pollen_gaus, pollen_scale, method = "spearman", use = "complete.obs")$p.value
        ),
      by = c("site", "year")
    )

  write_rds(df_tune_gaus, str_c(path_output, "tune_gaus.rds"))
}

pacman::p_unload("mclust")
