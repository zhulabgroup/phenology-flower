
for (taxaoi in v_taxa) {
  path_output <- str_c("./data/results/", taxaoi, "/")

  df_tune_gaus <- read_rds(str_c(path_output, "tune_gaus.rds"))

  df_pollen_gaus_ts <- read_rds(str_c(path_output, "ts_gaus.rds"))

  site_num <- df_pollen_gaus_ts %>%
    distinct(site) %>%
    nrow()

  if (site_num >= 7) {
    ls_df_pollen_gaus_ts_site_cv <- ls_df_pollen_gaus_param_site_cv <- vector(mode = "list")
    for (siteout in df_tune_gaus %>%
      pull(site) %>%
      unique()) {
      df_pollen_gaus_param_siteout <- df_tune_gaus %>%
        filter(site == siteout) %>%
        left_join(df_terraclim_annual, by = c("site", "year"))
      df_pollen_gaus_param_sitein <- df_tune_gaus %>%
        filter(site != siteout) %>%
        mutate(
          sos = mean - 1.5 * sd,
          eos = mean + 1.5 * sd
        ) %>%
        left_join(df_terraclim_annual, by = c("site", "year"))

      lm_model_cv_sos <- lm(sos ~ mat, data = df_pollen_gaus_param_sitein)
      lm_model_cv_eos <- lm(eos ~ mat, data = df_pollen_gaus_param_sitein)

      sos_fit <- predict(lm_model_cv_sos, df_pollen_gaus_param_siteout)
      eos_fit <- predict(lm_model_cv_eos, df_pollen_gaus_param_siteout)

      mean_fit <- (sos_fit + eos_fit) / 2
      sd_fit <- (eos_fit - sos_fit) / (2 * 1.5)
      sd_fit[sd_fit <= 0] <- NA

      df_pollen_gaus_param_siteout_cv <- df_pollen_gaus_param_siteout %>%
        mutate(
          mean = mean_fit,
          sd = sd_fit
        ) %>%
        select(mean, sd, year, site)

      ls_df_pollen_gaus_param_site_cv[[siteout]] <- df_pollen_gaus_param_siteout_cv

      ls_df_pollen_gaus_ts_site_cv[[siteout]] <- df_pollen_gaus_ts %>%
        select(-pollen_gaus) %>%
        filter(site == siteout) %>%
        left_join(df_pollen_gaus_param_siteout_cv, by = c("site", "year")) %>%
        mutate(pollen_gaus = dnorm(doy, mean, sd))

      print(str_c(taxaoi, ", ", siteout))
    }
    df_pollen_gaus_ts_cv <- bind_rows(ls_df_pollen_gaus_ts_site_cv) %>%
      mutate(pollen_gaus_pred = (pollen_gaus * pollen_sum)^2) %>%
      select(site, year, doy, everything())
    write_rds(df_pollen_gaus_ts_cv, str_c(path_output, "ts_gaus_cv.rds"))

    df_tune_gaus_cv <- bind_rows(ls_df_pollen_gaus_param_site_cv) %>%
      left_join(
        df_pollen_gaus_ts_cv %>%
          group_by(site, year) %>%
          drop_na(pollen_gaus) %>%
          summarise(
            rmse_raw = (mean((pollen_gaus_pred - pollen)^2, na.rm = T)) %>% sqrt(),
            nrmse = (mean((pollen_gaus - pollen_scale)^2, na.rm = T)) %>% sqrt() %>% `/`(max(pollen_scale, na.rm = T)),
            pearson = cor(pollen_gaus, pollen_scale, method = "pearson", use = "complete.obs"),
            spearman = cor(pollen_gaus, pollen_scale, method = "spearman", use = "complete.obs")
          ),
        by = c("site", "year")
      )

    write_rds(df_tune_gaus_cv, str_c(path_output, "tune_gaus_cv.rds"))
  }
}

# df_pollen_gaus_ts_cv %>%
#   filter(site=="DT") %>%
#   ggplot()+
#   geom_point(aes(x = doy, y = pollen_scale, col = year %>% as.factor()), alpha=0.5)+
#   geom_line(aes(x = doy, y = pollen_gaus, col = year%>% as.factor()), alpha=0.75)+
#   facet_wrap(.~site, ncol = 1, scales = "free_y")+
#   theme_classic()
