ls_df_tune <- vector(mode = "list")
for (taxaoi in v_taxa) {
  path_output <- paste0("./data/results/", taxaoi, "/")
  ls_df_tune[[taxaoi]] <- read_rds(str_c(path_output, "tune.rds")) %>%
    mutate(taxa = taxaoi)
}
df_tune <- bind_rows(ls_df_tune)
df_best_thres <- df_tune %>%
  group_by(taxa, direction, thres) %>%
  summarise(mse = mean(mse)) %>%
  ungroup() %>%
  arrange(mse) %>%
  group_by(taxa) %>%
  slice(1) %>%
  select(taxa, direction, thres)

df_fit <- df_tune %>%
  right_join(df_best_thres, by = c("taxa", "direction", "thres")) %>%
  left_join(df_meta %>% select(site, sitename), by = "site")

# data frame with flowering frequency and climate info, grouped into early and late taxa
df_lag_clim <- df_fit %>%
  select(-mse, -mse_ps, -mse_clim) %>%
  left_join(df_chelsa, by = "site") %>%
  mutate(taxa = factor(taxa, levels = v_taxa_chron)) %>%
  mutate(group = case_when(
    taxa %in% c("Ulmus late", "Poaceae late", "Ambrosia") ~ "late",
    TRUE ~ "early"
  ))

v_taxa_sig <- df_lag_clim %>%
  group_by(taxa) %>%
  do(broom::tidy(lm(lag ~ mat, .))) %>%
  filter(term == "mat") %>%
  filter(p.value <= 0.05) %>%
  pull(taxa)

# plot lag vs. climate
p_lag_clim <- ggplot(df_lag_clim %>%
  filter(!taxa %in% c("Poaceae early", "Poaceae late", "Ambrosia")) %>%
  filter(site %in% v_site_tune)) +
  geom_point(aes(x = mat, y = lag, col = taxa)) +
  geom_smooth(aes(x = mat, y = lag, col = taxa), method = "lm", se = F) +
  ggpubr::stat_cor(
    aes(
      x = mat, y = lag,
      label = paste(after_stat(rr.label), after_stat(p.label), sep = "*`,`~")
    ),
    p.accuracy = 0.05,
    label.x.npc = "left",
    label.y.npc = "top",
    show.legend = F
  ) +
  theme_classic() +
  facet_wrap(~taxa, scales = "free", ncol = 5) +
  xlab("Mean annual temperature (°C)") +
  ylab("Lag between leafing and pollen phenology (day)") +
  guides(col = "none")
p_lag_clim
