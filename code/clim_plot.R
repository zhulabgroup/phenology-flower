# data frame with flowering frequency and climate info, grouped into early and late taxa
df_lag_clim <- df_fit %>%
  select(-rmse, -rmse_ps, -rmse_clim) %>%
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
  filter(site != "SJ")) +
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
  guides(col = F)
p_lag_clim
