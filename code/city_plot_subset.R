siteoi <- "DT"
yearoi <- 2018

ls_df_best <- vector(mode = "list")
for (t in 1:length(v_taxa)) {
  taxaoi <- v_taxa[t]
  path_output <- str_c(.path$res, taxaoi, "/")

  df_best <- read_rds(str_c(path_output, "ts_best.rds"))

  df_best_subset <- df_best %>%
    filter(site == siteoi) %>%
    filter(year == yearoi) %>%
    mutate(taxa = taxaoi)

  ls_df_best[[t]] <- df_best_subset
}
df_best_DT2018 <- bind_rows(ls_df_best)

# make plots
p_comp_1city <- ggplot(df_best_DT2018 %>%
  arrange(taxa) %>%
  mutate(doy = as.Date(doy, origin = "2018-01-01"))) +
  geom_point(aes(x = doy, y = pollen_scale, col = "NAB"), alpha = 0.5) +
  # geom_line(aes(x = doy, y = pollen_gaus, col = "pollen concentration (NAB)"), alpha = 0.5, lwd = 1) +
  # geom_point(aes(x = doy, y = evi, col = "enhanced vegetation index (PS)"), alpha = 0.05) +
  geom_line(aes(x = doy, y = ps_freq_lag, col = "PlanetScope"), lwd = 1, alpha = 0.75) +
  ggthemes::theme_few() +
  # scale_y_continuous(trans = "sqrt") +
  facet_wrap(. ~ taxa, ncol = 3, scales = "free_y") +
  theme(strip.text = element_text(face = "italic")) +
  scale_color_manual(values = c("PlanetScope" = "dark blue", "NAB" = "dark red")) +
  xlab("Day of year") +
  ylab("Standardized pollen concentration") +
  scale_x_date(date_labels = "%b", date_breaks = "3 month") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(title = ""))

# taxa-specific figure directly from saved data
taxaoi <- "Quercus"
df_ps_freq_best <- read_rds(str_c(.path$res, taxaoi, "/ts_best.rds"))

p_comp_1taxa <- ggplot(df_ps_freq_best %>%
  mutate(year = as.factor(year)) %>%
  mutate(doy = as.Date(doy, origin = str_c(2017, "-01-01"))) %>%
  arrange(sitename) %>%
  mutate(site_lag = str_c(sitename, " (Lag: ", lag, " day)")) %>%
  mutate(site_lag = factor(site_lag, levels = (.)$site_lag %>% unique()))) +
  geom_point(aes(x = doy, y = pollen_scale, group = year, col = year), alpha = 0.5) +
  geom_line(aes(x = doy, y = ps_freq_lag, group = year, col = year), alpha = 0.75) +
  # scale_y_continuous(trans = "sqrt") +
  facet_wrap(. ~ site_lag, ncol = 1, scales = "free_y") +
  ggthemes::theme_few() +
  labs(
    y = "Standardized pollen concentration",
    x = "Day of year",
    col = "Year"
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "3 month")

p_comp_1taxa2city <- ggplot(df_ps_freq_best %>%
  filter(site %in% c("DT", "HT")) %>%
  filter(doy >= 0, doy <= 210) %>%
  mutate(doy = as.Date(doy, origin = str_c(2017, "-01-01"))) %>%
  mutate(year = as.factor(year))) +
  geom_point(aes(x = doy, y = pollen_scale, group = year, col = year), alpha = 0.5) +
  geom_line(aes(x = doy, y = ps_freq_lag, group = year, col = year), alpha = 0.75) +
  facet_wrap(. ~ str_c(sitename, " (green-up threshold: 50%, leaf-pollen lag: ", lag, " days)"), ncol = 1, scales = "free_y") +
  ggthemes::theme_few() +
  labs(
    y = "Standardized pollen concentration",
    x = "Time of year",
    col = "Year"
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month")

# all taxa and city
ls_df_best <- vector(mode = "list")
for (t in 1:length(v_taxa)) {
  taxaoi <- v_taxa[t]
  path_output <- str_c(.path$res, taxaoi, "/")

  df_best <- read_rds(str_c(path_output, "ts_best.rds"))

  df_best_subset <- df_best %>%
    mutate(taxa = taxaoi)

  ls_df_best[[t]] <- df_best_subset
}
df_best_all <- bind_rows(ls_df_best)

p_city_corr <- ggplot(df_best_all) +
  # geom_point(aes(x = freq, y = pollen_freq, col = sitename), alpha=0.25)+
  geom_hex(aes(x = ps_freq_lag, y = pollen_scale)) +
  geom_smooth(aes(x = ps_freq_lag, y = pollen_scale), alpha = 1, method = "lm") +
  ggpubr::stat_cor(aes(x = ps_freq_lag, y = pollen_scale),
    col = "red",
    method = "pearson",
    p.accuracy = 0.05,
    label.x.npc = "left",
    label.y.npc = "top"
  ) +
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = 2) +
  ggthemes::theme_few() +
  # geom_abline(slope = 1, intercept = 0, col = "red")+
  labs(
    x = "Predicted standardized pollen concentration",
    y = "Observed standardized pollen concentration",
    col = "Count"
  ) +
  # scale_x_continuous(trans = "sqrt") +
  # scale_y_continuous(trans = "sqrt") +
  facet_wrap(. ~ taxa, nrow = 3, scales = "free") +
  theme(strip.text = element_text(face = "italic")) +
  scale_fill_gradientn(colors = scales::viridis_pal()(9), limits = c(0, 30), na.value = "#FDE725FF") +
  guides(fill = "none")
# scale_fill_gradient(low = "white", high = "black",limits = c(0, 80))

# save figures
if (.fig_save) {
  ggsave(
    plot = p_comp_1city,
    filename = str_c(.path$out_fig, "supp_comp_1city.pdf"),
    width = 8,
    height = 8,
    device = pdf
  )

  ggsave(
    plot = p_comp_1taxa,
    filename = str_c(.path$out_fig, "supp_comp_1taxa.pdf"),
    width = 8,
    height = 10,
    device = pdf
  )

  ggsave(
    plot = p_comp_1taxa2city,
    filename = str_c(.path$out_fig, "supp_comp_1taxa2city.pdf"),
    width = 7,
    height = 5,
    device = pdf
  )

  ggsave(
    plot = p_city_corr,
    filename = str_c(.path$out_fig, "supp_city_corr.pdf"),
    width = 12,
    height = 8,
    device = pdf
  )
}
