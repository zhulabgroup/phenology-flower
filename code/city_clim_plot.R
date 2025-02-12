ls_df_tune <- vector(mode = "list")
for (taxaoi in v_taxa) {
  path_output <- str_c(.path$res, taxaoi, "/")
  ls_df_tune[[taxaoi]] <- read_rds(str_c(path_output, "tune.rds")) %>%
    mutate(taxa = taxaoi)
}
df_tune <- bind_rows(ls_df_tune)

df_best_thres <- df_tune %>%
  group_by(taxa, direction, thres) %>%
  summarise(nrmse_tune = mean(nrmse_tune)) %>%
  ungroup() %>%
  arrange(nrmse_tune) %>%
  group_by(taxa) %>%
  slice(1) %>%
  select(taxa, direction, thres)

df_fit <- df_tune %>%
  right_join(df_best_thres, by = c("taxa", "direction", "thres")) %>%
  left_join(df_meta %>% select(site, sitename), by = "site")

# data frame with flowering frequency and climate info, grouped into early and late taxa
df_lag_clim <- df_fit %>%
  distinct(site, sitename, taxa, direction, thres, lag) %>%
  left_join(df_terraclim, by = "site") %>%
  mutate(group = case_when(
    taxa %in% c("Ulmus late", "Poaceae late", "Ambrosia") ~ "late",
    TRUE ~ "early"
  ))

# plot lag vs. climate
p_lag_clim <- ggplot(df_lag_clim %>%
  filter(taxa == "Quercus") %>%
  filter(site %in% v_site_tune)) +
  geom_point(aes(x = mat, y = lag)) +
  ggrepel::geom_label_repel(aes(x = mat, y = lag, label = sitename)) +
  geom_smooth(aes(x = mat, y = lag), method = "lm", se = T) +
  ggpubr::stat_cor(
    aes(
      x = mat, y = lag,
      label = paste(after_stat(rr.label), after_stat(p.label), sep = "*`,`~")
    ),
    p.accuracy = 0.05,
    label.x.npc = "left",
    label.y.npc = "bottom",
    show.legend = F
  ) +
  ggthemes::theme_few() +
  xlab("Mean annual temperature (°C)") +
  ylab("Lag between leaf and pollen phenology (day)") +
  guides(col = "none")

# save figure
if (.fig_save) {
  ggsave(
    plot = p_lag_clim,
    filename = str_c(.path$out_fig, "supp_lag_clim.pdf"),
    width = 7,
    height = 5,
    device = pdf
  )
}
