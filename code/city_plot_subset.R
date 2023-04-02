siteoi <- "NY"
yearoi <- 2018
ls_df_best <- vector(mode = "list")
for (t in 1:length(v_taxa)) {
  taxaoi <- v_taxa[t]
  path_output <- paste0("./data/results/", taxaoi, "/")

  df_best <- read_rds(paste0(path_output, "ts_best.rds"))

  df_best_subset <- df_best %>%
    filter(site == siteoi) %>%
    filter(year == yearoi) %>%
    mutate(taxa = taxaoi)

  ls_df_best[[t]] <- df_best_subset
}
df_best <- bind_rows(ls_df_best)

# make plots
p_comp_1city <- ggplot(df_best %>%
  filter(!taxa %in% c("Poaceae early", "Poaceae late", "Ambrosia")) %>%
  mutate(doy = as.Date(doy, origin = "2018-01-01")) %>%
  mutate(taxa_p = paste0(taxa, " (Threshold: ", thres %>% scales::percent(), " green-", direction, ", Lag: ", lag, " days)")) %>%
  mutate(taxa_p = factor(taxa_p, levels = unique(taxa_p)))) +
  geom_point(aes(x = doy, y = npn, col = "flower observation (USA-NPN)"), alpha = 0.5) +
  geom_point(aes(x = doy, y = pollen, col = "pollen concentration (NAB)")) +
  # geom_line(aes(x=doy, y=pollen_clim, col="pollen count (NAB)"),alpha=0.5, lwd=1)+
  geom_point(aes(x = doy, y = evi, col = "enhanced vegetation index (PS)"), alpha = 0.05) +
  geom_line(aes(x = doy, y = freq_sm, col = "flowering frequency"), lwd = 1) +
  theme_classic() +
  facet_wrap(. ~ taxa_p, ncol = 2) +
  scale_color_manual(values = cols) +
  xlab("Day of year") +
  ylab("Standardized value") +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    strip.background = element_rect(
      color = NA, fill = "grey"
    )
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "3 month") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(title = ""))

# taxa-specific figure directly from saved data
taxaoi <- "Fraxinus"
df_best_thres <- read_rds(str_c("data/results/", taxaoi, "tune.rds")) %>%
  group_by(direction, thres) %>%
  summarise(mse = weighted.mean(mse, n)) %>% # mean rmse for each threshold
  arrange(mse) %>%
  head(1) %>% # keep threshold giving the smallest mean rmse
  select(direction, thres)
df_standard_best <- read_rds(str_c("data/results/", taxaoi, "ts_best.rds"))

p_comp_1taxa <- ggplot(df_standard_best %>%
  mutate(year = as.factor(year))) +
  geom_point(aes(x = doy, y = pollen, group = year, col = year)) +
  geom_line(aes(x = doy, y = freq_sm, group = year, col = year)) +
  facet_wrap(. ~ paste0(sitename, " (Lag: ", lag, ")"), ncol = 1) +
  theme_classic() +
  ylab("") +
  ylim(-0.1, 1.1) +
  ggtitle(paste0("Taxa: ", taxaoi, " (Threshold: ", df_best_thres$direction, " ", df_best_thres$thres, ")"))
