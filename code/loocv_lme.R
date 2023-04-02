ls_df_lag_clim_cv <- vector(mode = "list")
for (taxaoi in v_taxa) {
  path_output <- str_c("./data/results/", taxaoi, "/")
  file_output <- str_c(path_output, "lag_clim_cv.rds")
  if (file.exists(file_output)) {
    ls_df_lag_clim_cv[[taxaoi]] <- read_rds(file_output)
  }
}
df_lag_clim_cv <- bind_rows(ls_df_lag_clim_cv) %>%
  mutate(taxa = factor(taxa, levels = v_taxa_chron)) %>%
  mutate(group = case_when(
    taxa %in% c("Ulmus late", "Poaceae late", "Ambrosia") ~ "late",
    TRUE ~ "early"
  ))

ls_df_lme_cv <- vector(mode = "list")
for (siteoutoi in v_site) {
  df_lag_clim_cv_site <- df_lag_clim_cv %>%
    filter(siteout == siteoutoi) %>%
    filter(!taxa %in% c("Poaceae early", "Poaceae late", "Ambrosia"))
  p_lag_clim_cv <- ggplot(df_lag_clim_cv_site) +
    geom_point(aes(x = mat, y = lag_new, col = taxa)) +
    geom_smooth(aes(x = mat, y = lag_new, col = taxa), method = "lm", se = F) +
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
  # p_lag_clim_cv

  if (nrow(df_lag_clim_cv_site) > 0) {
    lme_fit <- nlme::lme(lag ~ mat,
      random = ~ 1 | taxa,
      data = df_lag_clim_cv_site %>%
        filter(group == "early"),
      control = nlme::lmeControl(opt = "optim", optimMethod = "SANN")
    )
    slope <- summary(lme_fit)$tTable[2, 1]
    p_val <- summary(lme_fit)$tTable[2, 5]
  } else {
    slope <- p_val <- NA
  }

  ls_df_lme_cv[[siteoutoi]] <- data.frame(siteout = siteoutoi, slope = slope, p_val = p_val)
}
df_lme_cv <- bind_rows(ls_df_lme_cv) %>%
  mutate(
    sig = (p_val <= 0.05)
  )
df_lme_cv
