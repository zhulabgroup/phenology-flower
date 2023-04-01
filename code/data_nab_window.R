pacman::p_load("mclust")
df_nab_hist_list <- flower_window_df_list <- vector(mode = "list")
for (taxaoi in taxa_list) {
  taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
  df_doy_sum <- nab_with_taxa_df %>%
    left_join(meta_df %>% dplyr::select(id, site, sitename), by = "id") %>%
    filter(site %in% site_list) %>%
    filter(genus == taxaoi_short | family == taxaoi_short) %>%
    mutate(doy = lubridate::yday(date)) %>%
    filter(doy <= 365) %>%
    group_by(doy) %>%
    summarise(count = sum(count, na.rm = T)) %>%
    complete(doy = seq(1, 365, 1), fill = list(count_sum = 0))

  df_doy_sum_prev <- df_doy_sum %>%
    mutate(doy = ifelse(doy > 273, doy - 365, doy)) %>%
    filter(doy <= 0)

  df_doy_sum_next <- df_doy_sum %>%
    mutate(doy = ifelse(doy < 152, doy + 365, doy)) %>%
    filter(doy >= 365)

  df_doy_sum_full <- bind_rows(list(
    df_doy_sum,
    df_doy_sum_prev,
    df_doy_sum_next
  )) %>%
    arrange(doy) %>%
    mutate(taxa = taxaoi)
  df_nab_hist_list[[taxaoi]] <- df_doy_sum_full

  # fit Gaussian mixture model
  set.seed(1)
  sam <- sample(x = df_doy_sum_full$doy, size = 1000, replace = T, prob = df_doy_sum_full$count)

  set.seed(1)
  if (!taxaoi_short %in% c("Poaceae", "Ulmus")) {
    fit <- mclust::Mclust(sam, G = 2, model = "V")
    df_param <- fit$parameters %>%
      discard(is.null) %>%
      as.data.frame() %>%
      arrange(mean) %>%
      filter(
        mean > 0,
        mean < 365
      )

    peak_mean <- df_param %>% pull(mean)
    peak_sd <- df_param %>%
      pull(variance.sigmasq) %>%
      sqrt()
    peak_start <- peak_mean - 1.96 * peak_sd - 50
    peak_end <- peak_mean + 1.96 * peak_sd + 50

    if (peak_start < -91) {
      peak_start <- -91
    }
    if (peak_end > 516) {
      peak_end <- 516
    }
  }
  if (taxaoi_short == "Ulmus" | taxaoi_short == "Poaceae") {
    if (taxaoi_short == "Ulmus") {
      fit <- mclust::Mclust(sam, G = 5, model = "V")
    }
    if (taxaoi_short == "Poaceae") {
      fit <- mclust::Mclust(sam, G = 4, model = "V")
    }

    df_param <- fit$parameters %>%
      as.data.frame() %>%
      arrange(mean)

    df_param_early <- df_param %>%
      filter(mean > 0, mean < 365 / 2) %>%
      arrange(desc(pro)) %>%
      head(1)
    peak_mean1 <- df_param_early %>% pull(mean)
    peak_sd1 <- df_param_early %>%
      pull(variance.sigmasq) %>%
      sqrt()
    peak_start1 <- peak_mean1 - 1.96 * peak_sd1 - 50
    peak_end1 <- peak_mean1 + 1.96 * peak_sd1 + 50

    df_param_late <- df_param %>%
      filter(mean > 365 / 2, mean < 365) %>%
      arrange(desc(pro)) %>%
      head(1)
    peak_mean2 <- df_param_late %>% pull(mean)
    peak_sd2 <- df_param_late %>%
      pull(variance.sigmasq) %>%
      sqrt()
    peak_start2 <- peak_mean2 - 1.96 * peak_sd2 - 50
    peak_end2 <- peak_mean2 + 1.96 * peak_sd2 + 50

    if (peak_end1 > peak_start2) {
      peak_end1 <- peak_start2 <- (peak_end1 + peak_start2) / 2
    }

    if (taxaoi == "Ulmus early" | taxaoi == "Poaceae early") {
      peak_mean <- peak_mean1
      peak_start <- peak_start1
      peak_end <- peak_end1
    }
    if (taxaoi == "Ulmus late" | taxaoi == "Poaceae late") {
      peak_mean <- peak_mean2
      peak_start <- peak_start2
      peak_end <- peak_end2
    }
  }

  flower_window_df_list[[taxaoi]] <- data.frame(
    taxa = taxaoi,
    peak = peak_mean %>% round(),
    start = peak_start %>% round(),
    end = peak_end %>% round()
  )
}
df_nab_hist <- bind_rows(df_nab_hist_list)
flower_window_df <- bind_rows(flower_window_df_list)

write_csv(flower_window_df, "data/processed/flower_window_auto.csv")
pacman::p_unload("mclust")

p_flower_window <- ggplot() +
  geom_line(data = df_nab_hist, aes(x = doy, y = count)) +
  geom_vline(data = flower_window_df, aes(xintercept = start)) +
  geom_vline(data = flower_window_df, aes(xintercept = end)) +
  theme_classic() +
  facet_wrap(. ~ taxa, scales = "free_y")
