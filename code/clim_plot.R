ls_df_tune <- vector(mode = "list")
for (taxaoi in v_taxa) {
  path_output <- str_c(.path$res, taxaoi, "/")
  ls_df_tune[[taxaoi]] <- read_rds(str_c(path_output, "tune.rds")) %>%
    mutate(taxa = taxaoi)
}
df_tune <- bind_rows(ls_df_tune)

# df_best_thres <- df_tune %>%
#   group_by(taxa, direction, thres) %>%
#   summarise(nrmse = mean(nrmse)) %>%
#   ungroup() %>%
#   arrange(nrmse) %>%
#   group_by(taxa) %>%
#   slice(1) %>%
#   select(taxa, direction, thres)

df_thres_50 <- df_tune %>%
  distinct(taxa) %>% 
  filter(taxa!="Ulmus late") %>% 
  mutate(direction = "up",
         thres = 0.5)

df_fit <- df_tune %>%
  right_join(df_thres_50, by = c("taxa", "direction", "thres")) %>%
  left_join(df_meta %>% select(site, sitename), by = "site")

# data frame with flowering frequency and climate info, grouped into early and late taxa
df_lag_clim <- df_fit %>%
  select(-nrmse, -nrmse_ps) %>%
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
                       filter(taxa =="Quercus") %>% 
  # filter(!taxa %in% c("Poaceae early", "Poaceae late", "Ambrosia")) %>%
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
  theme_classic() +
  # facet_wrap(~taxa, scales = "free", ncol = 5) +
  xlab("Mean annual temperature (°C)") +
  ylab("Lag between leafing \n and flowering phenology (day)") +
  guides(col = "none")
# p_lag_clim
