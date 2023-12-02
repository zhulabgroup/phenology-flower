taxaoi <- "Quercus"
siteoi <- "DT"
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
  geom_point(aes(x = doy, y = pollen, col = "pollen concentration (NAB)")) +
  geom_line(aes(x = doy, y = pollen_gaus, col = "pollen concentration (NAB)"), alpha = 0.5, lwd = 1) +
  # geom_point(aes(x = doy, y = evi, col = "enhanced vegetation index (PS)"), alpha = 0.05) +
  geom_line(aes(x = doy, y = freq, col = "flowering frequency (PS)"), lwd = 1) +
  theme_classic() +
  facet_wrap(. ~ taxa_p, ncol = 3, scales = "free_y") +
  scale_color_manual(values = cols) +
  xlab("Day of year") +
  ylab("Probability density") +
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

# taxa-specific figure directly from saved data
taxaoi <- "Quercus"
df_best_thres <- read_rds(str_c("data/results/", taxaoi, "/tune.rds")) %>%
  group_by(direction, thres) %>%
  summarise(mse = mean(mse)) %>% # mean rmse for each threshold
  arrange(mse) %>%
  head(1) %>% # keep threshold giving the smallest mean rmse
  select(direction, thres)
df_standard_best <- read_rds(str_c("data/results/", taxaoi, "/ts_best.rds"))

p_comp_1taxa <- ggplot(df_standard_best %>%
  mutate(year = as.factor(year))) +
  geom_point(aes(x = doy, y = pollen, group = year, col = year)) +
  geom_line(aes(x = doy, y = freq, group = year, col = year)) +
  facet_wrap(. ~ paste0(sitename, " (Lag: ", lag, ")"), ncol = 2, scales = "free_y") +
  theme_classic() +
  ylab("Probability density") +
  theme(
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    # axis.line = element_blank(),
    strip.background = element_rect(
      color = NA, fill = "grey"
    )
  ) +
  ggtitle(paste0("Taxa: ", taxaoi, " (Threshold: ", df_best_thres$direction, " ", df_best_thres$thres, ")"))


p_comp_1taxa2city <- ggplot(df_standard_best %>%
  filter(site %in% c("DT", "HT")) %>%
  filter(doy >= 0, doy <= 200) %>%
  mutate(doy = as.Date(doy, origin = str_c(2017, "-01-01"))) %>%
  mutate(year = as.factor(year))) +
  geom_point(aes(x = doy, y = pollen, group = year, col = year), alpha = 0.75) +
  geom_line(aes(x = doy, y = freq, group = year, col = year)) +
  facet_wrap(. ~ paste0(sitename, " (green-up threshold: 50%, leaf-flower lag: ", lag, " days)"), ncol = 1, scales = "free_y") +
  theme_classic() +
  ylab("Probability density") +
  xlab("Time of year") +
  labs(color = "Year") +
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


# all taxa and city
ls_df_best_all<-vector(mode = "list")
for (taxaoi in v_taxa) {
  df_best_thres <- read_rds(str_c("data/results/", taxaoi, "/tune.rds")) %>%
    group_by(direction, thres) %>%
    summarise(mse = mean(mse)) %>% # mean rmse for each threshold
    arrange(mse) %>%
    head(1) %>% # keep threshold giving the smallest mean rmse
    select(direction, thres)
  ls_df_best_all[[taxaoi]] <- read_rds(str_c("data/results/", taxaoi, "/ts_best.rds"))
}
df_best_all <- bind_rows(ls_df_best_all)

p_city_corr <- ggplot(df_best_all %>%
                        filter(!taxa %in% c("Poaceae early", "Poaceae late", "Ambrosia","Cupressaceae", "Pinaceae"))) +
  # geom_point(aes(x = freq, y = pollen_freq, col = sitename), alpha=0.25)+
  geom_hex(aes(x = freq, y = pollen_freq))+
  geom_smooth(aes(x = freq, y = pollen_freq), alpha=1, method = "lm")+
  ggpubr::stat_cor(aes(x = freq, y = pollen_freq),method = "pearson",
                   p.accuracy = 0.05,
                   label.x.npc = "left",
                   label.y.npc = "top")+
  theme_classic()+
  # geom_abline(slope = 1, intercept = 0, col = "red")+
  labs ( x = "Probability density of flowering \n (from PlanetScope)",
         y = "Probability density of flowering \n (from NAB)",
         col = "Site")+
  facet_wrap(.~taxa, nrow = 2)+
  theme(strip.text = element_text(face = "italic"))+
  coord_equal()+
  ylim (0, 0.1)+
  xlim (0, 0.1)+
  scale_fill_gradientn(colors = scales::viridis_pal()(9),limits = c(0, 30), na.value = "#FDE725FF")+
  guides (col = "none")
  # scale_fill_gradient(low = "white", high = "black",limits = c(0, 80))

p_city_corr

ggsave(
  plot = p_city_corr,
  filename = str_c(.path$out_fig, "city_corr.png"),
  width = 12,
  height = 6,
  device = png, type = "cairo"
)
