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
  select(taxa, site, year, rmse_raw, nrmse, spearman, spearman_sig, spearman_npn, spearman_sig_npn) %>%
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
  select(taxa, site, year, rmse_raw, nrmse, spearman, spearman_sig, spearman_npn, spearman_sig_npn) %>%
  mutate(method = "ps_cv")

df_fit_all <- bind_rows(list(
  df_fit,
  df_fit_cv
)) %>%
  mutate(method = factor(method,
    levels = c("ps", "ps_cv"),
    labels = c("in-sample", "out-of-sample")
  )) %>%
  mutate(site = factor(site, levels = v_site_mat)) %>%
  mutate(
    sig = if_else(spearman_sig <= 0.05, "significant", "non-significant"),
    sig_npn = if_else(spearman_sig_npn <= 0.05, "significant", "non-significant")
  )


set.seed(1)
p_taxa_nrmse <-
  ggplot(df_fit_all %>% filter(!str_detect(method, "\\("))) +
  geom_boxplot(aes(x = interaction(taxa, method), y = nrmse * 100, col = method), outlier.colour = NA) +
  geom_jitter(aes(x = interaction(taxa, method), y = nrmse * 100, col = method), width = 0.2) +
  scale_x_discrete(
    labels = df_fit_all %>% filter(!str_detect(method, "\\(")) %>% distinct(taxa, method) %>% arrange(method, taxa) %>% pull(taxa)
  ) +
  labs(
    x = "Genus",
    y = "nRMSE(%)"
  ) +
  ggthemes::theme_few() +
  theme(axis.text.x = element_text(face = "italic", angle = 45, hjust = 1, vjust = 1)) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())

set.seed(1)
p_taxa_spearman <-
  ggplot(df_fit_all %>% filter(!str_detect(method, "\\("))) +
  geom_boxplot(aes(x = interaction(taxa, method), y = spearman, col = method), outlier.colour = NA) +
  geom_jitter(aes(x = interaction(taxa, method), y = spearman, col = method, pch = sig), width = 0.2) +
  scale_shape_manual(values = c("significant" = 19, "non-significant" = 1)) +
  scale_x_discrete(
    labels = df_fit_all %>% filter(!str_detect(method, "\\(")) %>% distinct(taxa, method) %>% arrange(method, taxa) %>% pull(taxa)
  ) +
  labs(
    x = "Genus",
    y = "Spearman correlation coefficient"
  ) +
  scale_fill_brewer(palette = "RdYlBu") +
  ggthemes::theme_few() +
  theme(axis.text.x = element_text(face = "italic", angle = 45, hjust = 1, vjust = 1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())

set.seed(1)
p_taxa_spearman_npn <-
  ggplot(df_fit_all %>% filter(!str_detect(method, "\\("))) +
  geom_boxplot(aes(x = interaction(taxa, method), y = spearman_npn, col = method), outlier.colour = NA) +
  geom_jitter(aes(x = interaction(taxa, method), y = spearman_npn, col = method, pch = sig_npn), width = 0.2) +
  scale_shape_manual(values = c("significant" = 19, "non-significant" = 1)) +
  scale_x_discrete(
    labels = df_fit_all %>% filter(!str_detect(method, "\\(")) %>% distinct(taxa, method) %>% arrange(method, taxa) %>% pull(taxa)
  ) +
  labs(
    x = "Genus",
    y = "Spearman correlation coefficient"
  ) +
  scale_fill_brewer(palette = "RdYlBu") +
  ggthemes::theme_few() +
  theme(axis.text.x = element_text(face = "italic", angle = 45, hjust = 1, vjust = 1)) +
  geom_hline(yintercept = 0, linetype = 2) +
  theme(legend.position = "bottom") +
  theme(legend.title = element_blank())

# save figure
if (.fig_save) {
  ggsave(
    plot = p_taxa_nrmse,
    filename = str_c(.path$out_fig, "supp_taxa_nrmse.pdf"),
    width = 10,
    height = 6,
    device = pdf
  )

  ggsave(
    plot = p_taxa_spearman,
    filename = str_c(.path$out_fig, "supp_taxa_spearman.pdf"),
    width = 10,
    height = 6,
    device = pdf
  )

  ggsave(
    plot = p_taxa_spearman_npn,
    filename = str_c(.path$out_fig, "supp_taxa_spearman_npn.pdf"),
    width = 10,
    height = 6,
    device = pdf
  )
}
