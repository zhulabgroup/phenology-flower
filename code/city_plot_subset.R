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
  mutate(taxa = factor(taxa, levels = v_taxa_chron)) %>%
  arrange(taxa) %>%
  mutate(doy = as.Date(doy, origin = "2018-01-01")) %>%
  mutate(taxa_p = str_c(taxa, "\nThreshold: ", thres %>% scales::percent(), " green-", direction, "\nLag: ", lag, " days)")) %>%
  mutate(taxa_p = factor(taxa_p, levels = unique(taxa_p)))) +
  geom_point(aes(x = doy, y = pollen_scale, col = "pollen concentration (NAB)"), alpha = 0.5) +
  # geom_line(aes(x = doy, y = pollen_gaus, col = "pollen concentration (NAB)"), alpha = 0.5, lwd = 1) +
  # geom_point(aes(x = doy, y = evi, col = "enhanced vegetation index (PS)"), alpha = 0.05) +
  geom_line(aes(x = doy, y = ps_freq_lag, col = "flowering frequency (PS)"), lwd = 1, alpha = 0.75) +
  theme_classic() +
  # scale_y_continuous(trans = "sqrt") +
  facet_wrap(. ~ taxa_p, ncol = 3, scales = "free_y") +
  scale_color_manual(values = cols) +
  xlab("Day of year") +
  ylab("Standardized pollen concentration") +
  theme(
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    # axis.line = element_blank(),
    strip.background = element_rect(
      color = NA, fill = "grey"
    )
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "3 month") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(title = ""))

ggsave(
  plot = p_comp_1city,
  filename = str_c(.path$out_fig, "1city.png"),
  width = 8,
  height = 6,
  device = png, type = "cairo"
)

# taxa-specific figure directly from saved data
taxaoi <- "Quercus"
df_ps_freq_best <- read_rds(str_c(.path$res, taxaoi, "/ts_best.rds"))

p_comp_1taxa <- ggplot(df_ps_freq_best %>%
  mutate(year = as.factor(year)) %>%
  arrange(sitename) %>%
  mutate(site_lag = str_c(sitename, " (Lag: ", lag, ")")) %>%
  mutate(site_lag = factor(site_lag, levels = (.)$site_lag %>% unique()))) +
  geom_point(aes(x = doy, y = pollen_scale, group = year, col = year), alpha = 0.5) +
  geom_line(aes(x = doy, y = ps_freq_lag, group = year, col = year), alpha = 0.75) +
  # scale_y_continuous(trans = "sqrt") +
  facet_wrap(. ~ site_lag, ncol = 1, scales = "free_y") +
  theme_classic() +
  labs(
    y = "Standardized pollen concentration",
    x = "Time of year",
    col = "Year"
  ) +
  theme(
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    # axis.line = element_blank(),
    strip.background = element_rect(
      color = NA, fill = "grey"
    )
  ) +
  ggtitle(str_c(taxaoi, " (Threshold: ", df_ps_freq_best$thres[1] %>% scales::percent(), " green-", df_ps_freq_best$direction[1], ")"))

p_comp_1taxa2city <- ggplot(df_ps_freq_best %>%
  filter(site %in% c("DT", "HT")) %>%
  filter(doy >= 0, doy <= 210) %>%
  mutate(doy = as.Date(doy, origin = str_c(2017, "-01-01"))) %>%
  mutate(year = as.factor(year))) +
  geom_point(aes(x = doy, y = pollen_scale, group = year, col = year), alpha = 0.5) +
  geom_line(aes(x = doy, y = ps_freq_lag, group = year, col = year), alpha = 0.75) +
  # scale_y_continuous(trans = "sqrt") +
  facet_wrap(. ~ str_c(sitename, " (green-up threshold: 50%, leaf-flower lag: ", lag, " days)"), ncol = 1, scales = "free_y") +
  theme_classic() +
  labs(
    y = "Standardized pollen concentration",
    x = "Time of year",
    col = "Year"
  ) +
  theme(
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    # axis.line = element_blank(),
    strip.background = element_rect(
      color = NA, fill = "grey"
    )
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "1 month") #+
# ggtitle(paste0("Taxa: ", taxaoi, " (Threshold: ", df_best_thres$direction, " ", df_best_thres$thres, ")"))
ggsave(
  plot = p_comp_1taxa2city,
  filename = str_c(.path$out_fig, "1taxa2city.png"),
  width = 8,
  height = 6,
  device = png, type = "cairo"
)


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
  ggpubr::stat_cor(aes(x = pollen_pred, y = pollen),
    col = "red",
    method = "pearson",
    p.accuracy = 0.05,
    label.x.npc = "left",
    label.y.npc = "top"
  ) +
  geom_abline(intercept = 0, slope = 1, col = "red", linetype = 2) +
  theme_classic() +
  # geom_abline(slope = 1, intercept = 0, col = "red")+
  labs(
    x = "Predicted standardized pollen concentration",
    y = "Observed standardized pollen concentration",
    col = "Count"
  ) +
  # scale_x_continuous(trans = "sqrt") +
  # scale_y_continuous(trans = "sqrt") +
  facet_wrap(. ~ taxa, nrow = 2, scales = "free") +
  theme(strip.text = element_text(face = "italic")) +
  scale_fill_gradientn(colors = scales::viridis_pal()(9), limits = c(0, 30), na.value = "#FDE725FF") +
  guides(col = "none")
# scale_fill_gradient(low = "white", high = "black",limits = c(0, 80))

ggsave(
  plot = p_city_corr,
  filename = str_c(.path$out_fig, "city_corr.png"),
  width = 12,
  height = 6,
  device = png, type = "cairo"
)
