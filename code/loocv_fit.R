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
  select(taxa, site, mse_ps, mse_clim, n) %>%
  mutate(
    rmse_ps = mse_ps %>% sqrt(),
    rmse_clim = mse_clim %>% sqrt()
  ) %>%
  select(-mse_ps, -mse_clim)


ls_df_tune_cv <- vector(mode = "list")
for (taxaoi in v_taxa) {
  path_output <- str_c("./data/results/", taxaoi, "/")
  file_output <- str_c(path_output, "tune_cv.rds")
  if (file.exists(file_output)) {
    ls_df_tune_cv[[taxaoi]] <- read_rds(file_output) %>%
      mutate(taxa = taxaoi)
  }
}

df_fit_cv <- bind_rows(ls_df_tune_cv) %>%
  mutate(
    rmse_clim_cv = mse_clim_cv %>% sqrt(),
    rmse_ps_cv = mse_ps_cv %>% sqrt(),
  ) %>%
  select(taxa, site, rmse_clim_cv, rmse_ps_cv)

df_fit_all_wide <- left_join(df_fit, df_fit_cv,
  by = c("taxa", "site")
) %>%
  left_join(df_meta %>% select(site, sitename), by = "site") %>%
  filter(!taxa %in% c("Poaceae early", "Poaceae late", "Ambrosia")) %>%
  select(taxa,
    city = sitename, rmse_clim, rmse_ps,
    rmse_clim_cv, rmse_ps_cv
  )

df_fit_in_long <- df_fit_all_wide %>%
  gather(key = "method", value = "rmse", -taxa, -city) %>%
  mutate(method = factor(method,
    levels = c("rmse_clim", "rmse_ps", "rmse_clim_cv", "rmse_ps_cv"),
    labels = c("Gaussian (in-sample)", "PS (in-sample)", "Gaussian (out-of-sample)", "PS (out-of-sample)")
  )) %>%
  mutate(taxa = factor(taxa, levels = v_taxa)) %>%
  mutate(city = factor(city, levels = v_site_lat)) %>%
  drop_na()

df_fit_out_long <- df_fit_all_wide %>%
  drop_na() %>%
  gather(key = "method", value = "rmse", -taxa, -city) %>%
  mutate(method = factor(method,
    levels = c("rmse_clim", "rmse_ps", "rmse_clim_cv", "rmse_ps_cv"),
    labels = c("Gaussian (in-sample)", "PS (in-sample)", "Gaussian (out-of-sample)", "PS (out-of-sample)")
  )) %>%
  mutate(taxa = factor(taxa, levels = v_taxa)) %>%
  mutate(city = factor(city, levels = v_site_lat)) %>%
  drop_na()

tb_rmse_in <- df_fit_in_long %>%
  group_by(method) %>%
  summarise(
    median = median(rmse),
    mean = mean(rmse),
    lower = quantile(rmse, 0.025),
    upper = quantile(rmse, 0.975)
  )

tb_rmse_out <- df_fit_out_long %>%
  group_by(method) %>%
  filter(taxa == "Quercus") %>%
  summarise(
    median = median(rmse),
    mean = mean(rmse),
    lower = quantile(rmse, 0.025),
    upper = quantile(rmse, 0.975)
  )

p_rmse_taxa <- ggplot(df_fit_out_long) +
  geom_boxplot(aes(x = taxa, y = rmse, fill = method)) +
  ylab("RMSE") +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu") #+
# ggpubr::stat_compare_means( # significance
#   aes(x = taxa, y = rmse, group = method),
#   method = "wilcox.test", paired = F,
#   label = "p.signif", hide.ns = FALSE
# )

# p_rmse_cv_city <- ggplot(df_fit_all ) +
#   geom_boxplot(aes(x = city, y = rmse, fill = method)) +
#   ylab("RMSE") +
#   theme_classic() +
#   scale_fill_brewer(palette = "RdYlBu") #+
#   # ggpubr::stat_compare_means( # significance
#   #   aes(x = city, y = rmse, group = method),
#   #   method = "wilcox.test", paired = F,
#   #   label = "p.signif", hide.ns = FALSE
#   # )
# p_rmse_cv_city

# p_rmse_cv_all<-ggplot(df_tune_cv) +
#   geom_boxplot(aes(x = "all taxa and cities", y = rmse, fill = method)) +
#   xlab("")+
#   ylab("nRMSE") +
#   theme_classic() +
#   scale_fill_brewer(palette = "RdYlBu")+
#   theme(legend.position="bottom", legend.direction="vertical")

p_rmse_quercus <-
  ggpubr::ggboxplot(
    data = df_fit_out_long %>%
      filter(taxa == "Quercus") # %>%
    # filter(!taxa %in% c("Poaceae early", "Poaceae late", "Ambrosia"))
    ,
    x = "method",
    fill = "method",
    y = "rmse"
  ) +
  xlab("") +
  ylab("RMSE") +
  scale_fill_brewer(palette = "RdYlBu") +
  ggpubr::stat_compare_means(
    method = "wilcox.test",
    paired = T,
    label = "p.format",
    hide.ns = FALSE,
    comparisons = list(
      c("Gaussian (in-sample)", "PS (in-sample)"),
      c("Gaussian (out-of-sample)", "PS (out-of-sample)")
    ),
    size = 3
  ) +
  theme_classic()

wtest1 <- wilcox.test(
  df_fit_all_wide %>%
    filter(taxa == "Quercus") %>%
    pull(rmse_clim),
  df_fit_all_wide %>%
    filter(taxa == "Quercus") %>%
    pull(rmse_ps),
  paired = T
)

wtest2 <- wilcox.test(
  df_fit_all_wide %>%
    filter(taxa == "Quercus") %>%
    pull(rmse_clim_cv),
  df_fit_all_wide %>%
    filter(taxa == "Quercus") %>%
    pull(rmse_ps_cv),
  paired = T
)
