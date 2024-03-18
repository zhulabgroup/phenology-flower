f_flower_hist <- str_c(.path$dat_other, "flower_window_hist.rds")
f_flower_window <- str_c(.path$dat_other, "flower_window_auto.csv")
if (!file.exists(f_flower_window)) {
  pacman::p_load("mclust")
  ls_df_nab_hist <- ls_df_flower_window <- vector(mode = "list")
  for (taxaoi in v_taxa) {
    taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
    df_doy_sum <- df_nab_short %>%
      filter(taxa == taxaoi_short) %>%
      filter(doy <= 365) %>%
      group_by(doy) %>%
      summarise(count = sum(count, na.rm = T)) %>%
      complete(doy = seq(1, 365, 1), fill = list(count_sum = 0))

    df_doy_sum_prev <- df_doy_sum %>%
      mutate(doy = ifelse(doy > 365-90, doy - 365, doy)) %>%
      filter(doy <= 0)

    df_doy_sum_next <- df_doy_sum %>%
      mutate(doy = ifelse(doy < 90, doy + 365, doy)) %>%
      filter(doy >= 365)

    df_doy_sum_full <- bind_rows(list(
      df_doy_sum,
      df_doy_sum_prev,
      df_doy_sum_next
    )) %>%
      arrange(doy) %>%
      mutate(taxa = taxaoi)

    ls_df_nab_hist[[taxaoi]] <- df_doy_sum_full

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

      if (peak_start < -90) {
        peak_start <- -90
      }
      if (peak_end > 365+180) {
        peak_end <- 360+180
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
        peak_sd <- peak_sd1
        peak_start <- peak_start1
        peak_end <- peak_end1
      }
      if (taxaoi == "Ulmus late" | taxaoi == "Poaceae late") {
        peak_mean <- peak_mean2
        peak_sd <- peak_sd2
        peak_start <- peak_start2
        peak_end <- peak_end2
      }
    }

    ls_df_flower_window[[taxaoi]] <- data.frame(
      taxa = taxaoi,
      peak = peak_mean,
      sd = peak_sd,
      start = peak_start %>% round(),
      end = peak_end %>% round()
    )
  }
  df_nab_hist <- bind_rows(ls_df_nab_hist)
  df_flower_window <- bind_rows(ls_df_flower_window)

  write_rds(df_nab_hist, f_flower_hist)
  write_csv(df_flower_window, f_flower_window)
  pacman::p_unload("mclust")
} else {
  df_nab_hist <- read_rds(f_flower_hist)
  df_flower_window <- read_csv(f_flower_window)
}

p_flower_window <- ggplot() +
  geom_rect(
    data = df_flower_window %>%
      filter(!taxa %in% c("Ambrosia", "Poaceae early", "Poaceae late")) %>%
      mutate(taxa_parse = case_when(
        taxa %in% c("Cupressaceae", "Pinaceae", "Poaceae early", "Poaceae late") ~ taxa,
        taxa %in% c("Ulmus early") ~ paste0("italic('Ulmus')~early"),
        taxa %in% c("Ulmus late") ~ paste0("italic('Ulmus')~late"),
        TRUE ~ paste0("italic('", taxa, "')")
      )),
    aes(xmin = start + lubridate::date("2023-01-01") - 1, xmax = end + lubridate::date("2023-01-01") - 1, ymin = -Inf, ymax = Inf), fill = "mistyrose"
  ) +
  geom_line(
    data = df_nab_hist %>%
      filter(!taxa %in% c("Ambrosia", "Poaceae early", "Poaceae late")) %>%
      mutate(taxa_parse = case_when(
        taxa %in% c("Cupressaceae", "Pinaceae", "Poaceae early", "Poaceae late") ~ taxa,
        taxa %in% c("Ulmus early") ~ paste0("italic('Ulmus')~early"),
        taxa %in% c("Ulmus late") ~ paste0("italic('Ulmus')~late"),
        TRUE ~ paste0("italic('", taxa, "')")
      )),
    aes(x = doy + lubridate::date("2023-01-01") - 1, y = count)
  ) +
  scale_x_date(
    date_labels = "%b",
    breaks = seq(lubridate::date("2023-01-01"),
      lubridate::date("2023-12-31") + 1,
      by = "3 months"
    )
  ) +
  theme_classic() +
  facet_wrap(. ~ taxa_parse, scales = "free", labeller = label_parsed) +
  labs(
    x = "Day of year",
    y = expression(Total ~ pollen ~ concentration ~ (grains ~ m^-3))
  )
