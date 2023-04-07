pacman::p_load("mclust")
ls_df_pollen_gaus_ts_taxa <- ls_df_pollen_gaus_param_taxa <- vector(mode = "list")
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
  df_ts_ext <- read_rds(str_c(path_output, "ts_ext.rds")) %>%
    filter(site %in% v_site_tune) %>%
    filter(id == "pollen", var == "pollen concentration (NAB)") %>%
    rename(pollen = value) %>%
    select(doy, year, pollen, site) %>%
    group_by(year, site) %>%
    complete(doy = c((274 - 365):(365 + 151))) %>%
    mutate(pollen_raw = pollen) %>%
    mutate(pollen = pollen %>% sqrt()) %>%
    mutate(pollen = case_when(
      doy %in% flower_window ~ pollen
    )) %>% # disregard values out of window
    mutate(pollen_raw = case_when(doy %in% flower_window ~ pollen_raw)) %>% # disregard values out of window
    # mutate(pollen_in = zoo::na.approx(pollen, doy, na.rm = F, maxgap = 14)) %>% # fill gap
    # mutate(pollen_fill = replace_na(pollen_in, 0)) %>% # fill long gap with 0
    # mutate(pollen_sm = whitfun(pollen_fill, 10)) %>% # smooth
    # mutate(pollen_sm = case_when(
    #   doy %in% flower_window ~ pollen_sm,
    #   TRUE ~ 0
    # )) %>% # disregard values out of window
    # mutate(pollen_freq = pollen_sm/sum(pollen_sm)) %>% # normalize
    ungroup() %>%
    select(doy, pollen, pollen_raw, site)

  ls_df_pollen_gaus_ts_site <- ls_df_pollen_gaus_param_site <- vector(mode = "list")
  for (siteoi in df_ts_ext %>%
    pull(site) %>%
    unique()) {
    df_ts_ext_site <- df_ts_ext %>%
      filter(site == siteoi)

    non_zero_sample <- df_ts_ext_site %>%
      filter(pollen_raw > 5 & !is.na(pollen_raw)) %>%
      nrow()

    if (non_zero_sample >= 5 * 4) {
      df_pollen_mean <- df_ts_ext_site %>%
        group_by(doy) %>%
        summarise(pollen = mean(pollen, na.rm = T)) %>%
        drop_na()

      # fit Gaussian mixture model
      set.seed(1)
      sam <- sample(x = df_pollen_mean$doy, size = 1000, replace = T, prob = df_pollen_mean$pollen)
      fit <- mclust::Mclust(sam, G = 1, model = "V")
      mean <- fit$parameters$mean
      sd <- fit$parameters$variance$sigmasq %>% sqrt()

      ls_df_pollen_gaus_param_site[[siteoi]] <- data.frame(mean = mean, sd = sd, site = siteoi)
      ls_df_pollen_gaus_ts_site[[siteoi]] <- data.frame(doy = c((274 - 365):(365 + 151))) %>%
        mutate(pollen_gaus = dnorm(doy, mean, sd)) %>%
        mutate(site = siteoi)

      # ggplot()+
      #   geom_density(data = data.frame(sam), aes(sam))+
      #   geom_line(data = test, aes(x = doy, y=pollen_gaus), col = "red")+
      #   geom_point(data=df_pollen_sum %>% mutate(n = n/sum(n)), aes(x = doy, y = n), col="blue")
      print(str_c(taxaoi, ", ", siteoi))
    }
  }
  ls_df_pollen_gaus_param_taxa[[taxaoi]] <- bind_rows(ls_df_pollen_gaus_param_site) %>%
    mutate(taxa = taxaoi)
  ls_df_pollen_gaus_ts_taxa[[taxaoi]] <- bind_rows(ls_df_pollen_gaus_ts_site) %>%
    mutate(taxa = taxaoi)
}

df_pollen_gaus_param <- bind_rows(ls_df_pollen_gaus_param_taxa) %>%
  left_join(df_chelsa, by = "site")
df_pollen_gaus_ts <- bind_rows(ls_df_pollen_gaus_ts_taxa)

df_pollen_gaus_param %>%
  ggplot() +
  geom_point(aes(x = mat, y = mean, col = taxa)) +
  facet_wrap(. ~ taxa, scales = "free_y") +
  theme_classic()

# loocv
ls_df_pollen_gaus_ts_cv_taxa <- vector(mode = "list")
for (taxaoi in v_taxa) {
  df_pollen_gaus_param_taxa <- df_pollen_gaus_param %>%
    filter(taxa == taxaoi)
  df_pollen_gaus_ts_taxa <- df_pollen_gaus_ts %>%
    filter(taxa == taxaoi)

  site_num <- df_pollen_gaus_param_taxa %>%
    distinct(site) %>%
    nrow()

  if (site_num >= 4) {
    ls_df_pollen_gaus_ts_cv_site <- vector(mode = "list")
    for (siteout in df_pollen_gaus_param_taxa %>%
      pull(site) %>%
      unique()) {
      df_pollen_gaus_param_siteout <- df_pollen_gaus_param_taxa %>%
        filter(site == siteout)
      df_pollen_gaus_param_sitein <- df_pollen_gaus_param_taxa %>%
        filter(site != siteout) %>%
        mutate(
          sos = mean - 1.5 * sd,
          eos = mean + 1.5 * sd
        )

      lm_model_cv_sos <- lm(sos ~ mat, data = df_pollen_gaus_param_sitein)
      lm_model_cv_eos <- lm(eos ~ mat, data = df_pollen_gaus_param_sitein)

      sos_fit <- predict(lm_model_cv_sos, df_pollen_gaus_param_siteout)
      eos_fit <- predict(lm_model_cv_eos, df_pollen_gaus_param_siteout)

      mean_fit <- (sos_fit + eos_fit) / 2
      sd_fit <- (eos_fit - sos_fit) / (2 * 1.5)

      if (sos_fit > eos_fit) {
        sd_fit <- 5
      }

      ls_df_pollen_gaus_ts_cv_site[[siteout]] <- df_pollen_gaus_ts_taxa %>%
        filter(site == siteout) %>%
        select(-pollen_gaus) %>%
        mutate(pollen_gaus_cv = dnorm(doy, mean_fit, sd_fit))

      print(str_c(taxaoi, ", ", siteout, ", ", mean_fit, ",", sd_fit))
    }
  }

  ls_df_pollen_gaus_ts_cv_taxa[[taxaoi]] <- bind_rows(ls_df_pollen_gaus_ts_cv_site)
}
df_pollen_gaus_ts_cv <- bind_rows(ls_df_pollen_gaus_ts_cv_taxa)

df_pollen_gaus_ts_cv <- df_pollen_gaus_ts %>%
  left_join(df_pollen_gaus_ts_cv, by = c("doy", "site", "taxa"))
ggplot(df_pollen_gaus_ts_cv) +
  geom_line(aes(x = doy, y = pollen_gaus), col = "red") +
  geom_line(aes(x = doy, y = pollen_gaus_cv), col = "blue") +
  facet_wrap(. ~ taxa * site, scales = "free_y") +
  theme_classic()

write_rds(df_pollen_gaus_ts_cv, "data/processed/pollen_gaus_ts.rds")
write_rds(df_pollen_gaus_param, "data/processed/pollen_gaus_param.rds")

pacman::p_unload("mclust")
