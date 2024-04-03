ls_df_tune <- vector(mode = "list")
for (taxaoi in v_taxa) {
  path_output <- str_c("./data/results/", taxaoi, "/")
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
  select(taxa, site, year, nrmse, spearman, spearman_sig) %>%
  mutate(method = "ps")

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
  select(taxa, site, year, nrmse, spearman, spearman_sig) %>%
  mutate(method = "ps_cv")

ls_df_tune_gaus <- vector(mode = "list")
for (taxaoi in v_taxa) {
  path_output <- str_c("./data/results/", taxaoi, "/")
  file_output <- str_c(path_output, "tune_gaus.rds")
  if (file.exists(file_output)) {
    ls_df_tune_gaus[[taxaoi]] <- read_rds(file_output) %>%
      mutate(taxa = taxaoi)
  }
}
df_fit_gaus <- bind_rows(ls_df_tune_gaus) %>%
  select(taxa, site, year, nrmse, spearman, spearman_sig) %>%
  mutate(method = "gaus")

ls_df_tune_gaus_cv <- vector(mode = "list")
for (taxaoi in v_taxa) {
  path_output <- str_c("./data/results/", taxaoi, "/")
  file_output <- str_c(path_output, "tune_gaus_cv.rds")
  if (file.exists(file_output)) {
    ls_df_tune_gaus_cv[[taxaoi]] <- read_rds(file_output) %>%
      mutate(taxa = taxaoi)
  }
}
df_fit_gaus_cv <- bind_rows(ls_df_tune_gaus_cv) %>%
  select(taxa, site, year, nrmse, spearman, spearman_sig) %>%
  mutate(method = "gaus_cv")

df_fit_all <- bind_rows(list(
  df_fit,
  df_fit_cv,
  df_fit_gaus,
  df_fit_gaus_cv
)) %>%
  mutate(method = factor(method,
    levels = c("gaus", "ps", "gaus_cv", "ps_cv"),
    labels = c("Gaussian (in-sample)", "PlanetScope (in-sample)", "Gaussian (out-of-sample)", "PlanetScope (out-of-sample)")
  )) %>%
  mutate(taxa = factor(taxa, levels = v_taxa_chron)) %>%
  mutate(site = factor(site, levels = v_site_mat)) %>%
  drop_na() %>%
  mutate(sig = if_else(spearman_sig <= 0.05, "sig", "ns"))

df_fit_all %>%
  group_by(method) %>%
  summarise(
    median = median(nrmse),
    mean = mean(nrmse),
    lower = quantile(nrmse, 0.025),
    upper = quantile(nrmse, 0.975),
    n = n()
  )

df_fit_all %>%
  group_by(method) %>%
  summarise(
    median = median(spearman),
    mean = mean(spearman),
    lower = quantile(spearman, 0.025),
    upper = quantile(spearman, 0.975),
    n = n()
  )

# tb_quercus <- df_fit_all %>%
#   filter(taxa=="Quercus") %>%
#   group_by(method) %>%
#   summarise(
#     median = median(nrmse),
#     mean = mean(nrmse),
#     lower = quantile(nrmse, 0.025),
#     upper = quantile(nrmse, 0.975),
#     n = n()
#   )

# df_fit_all %>%
#   filter(taxa=="Quercus") %>%
#   group_by(method) %>%
#   summarise(
#     median = median(spearman),
#     mean = mean(spearman),
#     lower = quantile(spearman, 0.025),
#     upper = quantile(spearman, 0.975),
#     n = n()
#   )

# p_quercus <-
#   ggpubr::ggboxplot(
#     data = df_fit_all %>%
#       mutate(nrmse = nrmse*100) %>%
#       filter(taxa == "Quercus") # %>%
#     # filter(!taxa %in% c("Poaceae early", "Poaceae late", "Ambrosia"))
#     ,
#     x = "method",
#     fill = "method",
#     y = "nrmse"
#   ) +
#   xlab("") +
#   ylab("nRMSE (%)") +
#   scale_fill_brewer(palette = "RdYlBu") +
#   ggpubr::stat_compare_means(
#     method = "wilcox.test",
#     paired = T,
#     label = "p.format",
#     hide.ns = FALSE,
#     comparisons = list(
#       c("Gaussian (in-sample)", "PlanetScope (in-sample)"),
#       c("Gaussian (out-of-sample)", "PlanetScope (out-of-sample)")#,
#       # c("PlanetScope (in-sample)", "PlanetScope (out-of-sample)")
#     ),
#     size = 3
#   ) +
#   theme_classic()

p_taxa_nrmse <-
  ggplot(df_fit_all %>% filter(str_detect(method, "Planet"))) +
  geom_boxplot(aes(x = interaction(taxa, method), y = nrmse * 100, col = method), outlier.colour = NA) +
  geom_jitter(aes(x = interaction(taxa, method), y = nrmse * 100, col = method), width = 0.2) +
  scale_x_discrete(
    labels = df_fit_all %>% filter(str_detect(method, "Planet")) %>% distinct(taxa, method) %>% arrange(method, taxa) %>% pull(taxa)
  ) +
  labs(
    x = "Genus",
    y = "nRMSE(%)"
  ) +
  theme_classic() +
  theme(axis.text.x = element_text(face = "italic")) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())

p_taxa_spearman <-
  ggplot(df_fit_all %>% filter(str_detect(method, "Planet"))) +
  geom_boxplot(aes(x = interaction(taxa, method), y = spearman, col = method), outlier.colour = NA) +
  geom_jitter(aes(x = interaction(taxa, method), y = spearman, col = method, pch = sig), width = 0.2) +
  scale_shape_manual(values = c("sig" = 19, "ns" = 1)) +
  scale_x_discrete(
    labels = df_fit_all %>% filter(str_detect(method, "Planet")) %>% distinct(taxa, method) %>% arrange(method, taxa) %>% pull(taxa)
  ) +
  labs(
    x = "Genus",
    y = "Spearman correlation coefficient"
  ) +
  scale_fill_brewer(palette = "RdYlBu") +
  theme_classic() +
  theme(axis.text.x = element_text(face = "italic")) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())

df_fit_nrmse <- df_fit_all %>%
  select(-spearman, -spearman_sig) %>%
  spread(key = "method", value = "nrmse") %>%
  drop_na()

# wilcox.test(
#   df_fit_nrmse$`PlanetScope (in-sample)`,
#   df_fit_nrmse$`PlanetScope (out-of-sample)`,
#   paired = T,
#   
# )
# 
# wilcox.test(
#   df_fit_nrmse$`Gaussian (out-of-sample)`,
#   df_fit_nrmse$`PlanetScope (out-of-sample)`,
#   paired = T
# )
# 
# df_fit_spearman <- df_fit_all %>%
#   select(-nrmse, -spearman_sig) %>%
#   spread(key = "method", value = "spearman") %>%
#   drop_na()
# 
# wilcox.test(
#   df_fit_spearman$`PlanetScope (in-sample)`,
#   df_fit_spearman$`PlanetScope (out-of-sample)`,
#   paired = T
# )
# 
# wilcox.test(
#   df_fit_spearman$`Gaussian (out-of-sample)`,
#   df_fit_spearman$`PlanetScope (out-of-sample)`,
#   paired = T
# )
