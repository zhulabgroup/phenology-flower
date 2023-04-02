ls_df_tune_cv <- vector(mode = "list")
for (taxaoi in v_taxa) {
  path_output <- str_c("./data/results/", taxaoi, "/")
  file_output <- str_c(path_output, "tune_cv.rds")
  if (file.exists(file_output)) {
    ls_df_tune_cv[[taxaoi]] <- read_rds(file_output) %>%
      mutate(taxa = taxaoi)
  }
}

df_tune_cv <- bind_rows(ls_df_tune_cv) %>%
  filter(!taxa %in% c("Poaceae early", "Poaceae late", "Ambrosia")) %>%
  mutate(
    rmse_clim = mse_clim %>% sqrt(),
    rmse_ps = mse_ps %>% sqrt(),
    rmse_cv = mse_cv %>% sqrt(),
  ) %>%
  select(taxa, city = sitename, rmse_clim, rmse_ps, rmse_cv) %>%
  gather(key = "method", value = "rmse", -taxa, -city) %>%
  mutate(method = case_when(
    method == "rmse_clim" ~ "Climatology",
    method == "rmse_ps" ~ "PS (in-sample)",
    method == "rmse_cv" ~ "PS (out-of-sample)"
  )) %>%
  mutate(taxa = factor(taxa, levels = v_taxa_chron)) %>%
  mutate(city = factor(city, levels = v_site_lat))


p_rmse_cv_taxa <- ggplot(df_tune_cv) +
  geom_boxplot(aes(x = taxa, y = rmse, fill = method)) +
  ylab("nRMSE") +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu") +
  guides(fill = "none")
p_rmse_cv_taxa

p_rmse_cv_city <- ggplot(df_tune_cv) +
  geom_boxplot(aes(x = city, y = rmse, fill = method)) +
  ylab("nRMSE") +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu") +
  guides(fill = "none")


# p_rmse_cv_all<-ggplot(df_tune_cv) +
#   geom_boxplot(aes(x = "all taxa and cities", y = rmse, fill = method)) +
#   xlab("")+
#   ylab("nRMSE") +
#   theme_classic() +
#   scale_fill_brewer(palette = "RdYlBu")+
#   theme(legend.position="bottom", legend.direction="vertical")

tb_rmse_cv <- df_tune_cv %>%
  group_by(method) %>%
  summarise(
    median = median(rmse, na.rm = T),
    lower = quantile(rmse, 0.025, na.rm = T),
    upper = quantile(rmse, 0.975, na.rm = T),
    mean = mean(rmse, na.rm = T)
  )

p_rmse_cv_all <-
  ggpubr::ggboxplot(
    data = df_tune_cv,
    x = "method",
    fill = "method",
    y = "rmse"
  ) +
  xlab("") +
  ylab("nRMSE") +
  scale_fill_brewer(palette = "RdYlBu") +
  ggpubr::stat_compare_means(
    label = "p.signif",
    hide.ns = FALSE,
    comparisons = list(c("Climatology", "PS (in-sample)"), c("Climatology", "PS (out-of-sample)"), c("PS (in-sample)", "PS (out-of-sample)")),
    size = 3
  ) +
  theme_classic()
