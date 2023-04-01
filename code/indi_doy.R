# one tree
idoi <- "5"
flower_df <- get_doy(thres_df_taxa, ts_df_subset, idoi)
p_dt_doy_one <- plot_tree(ts_df_subset, flower_df, idoi)





# multiple trees
set.seed(3)
idoi_list <- df_dt_flower_peak %>%
  # left_join(df_dt_meta %>% select(id, species) %>%
  #             mutate(id = as.character(id)),
  #           by="id") %>%
  left_join(
    df_dt_meta %>% select(id, lat, lon) %>%
      mutate(id = as.character(id)) %>%
      mutate(cluster = kmeans(df_dt_meta[, c("lon", "lat")], centers = 9)$cluster),
    by = "id"
  ) %>%
  group_by(cluster) %>%
  sample_n(1) %>%
  ungroup() %>%
  sample_n(min(4, n())) %>%
  arrange(doy) %>%
  pull(id)

flower_doy_df_list <- vector(mode = "list")
for (idoi in idoi_list) {
  flower_doy_df_list[[idoi]] <- get_doy(thres_df_taxa, ts_df_subset, idoi)
}
flower_doy_df <- bind_rows(flower_doy_df_list)


p_dt_doy_mult <- ggplot() +
  geom_point(
    data = ts_df_subset %>%
      filter(var == "enhanced vegetation index (PS)" | var == "flower observation (Katz's team)") %>%
      group_by(id, var) %>%
      mutate(value_st = (value - min(value, na.rm = T)) / (max(value, na.rm = T) - min(value, na.rm = T))) %>% # standardize
      ungroup() %>%
      filter(id %in% idoi_list) %>%
      mutate(id = factor(id, levels = idoi_list)),
    aes(x = date, y = value_st, col = var)
  ) +
  geom_vline(
    data = flower_doy_df %>%
      mutate(doy = as.Date(doy, origin = "2017-01-01")) %>%
      left_join(
        df_dt_meta %>% select(id, species) %>%
          mutate(id = as.character(id)),
        by = "id"
      ) %>%
      mutate(id = factor(id, levels = idoi_list)),
    aes(xintercept = doy), col = "dark green", alpha = 0.2
  ) +
  geom_vline(
    data = flower_doy_df %>%
      filter(thres == 0.5) %>%
      mutate(doy = as.Date(doy, origin = "2017-01-01")) %>%
      left_join(
        df_dt_meta %>% select(id, species) %>%
          mutate(id = as.character(id)),
        by = "id"
      ) %>%
      mutate(id = factor(id, levels = idoi_list)),
    aes(xintercept = doy), col = "dark green", alpha = 0.8
  ) +
  facet_wrap(. ~ id, ncol = 1, scales = "free_y") +
  scale_color_manual(values = cols) +
  xlab("Day of year") +
  ylab("Standardized value") +
  theme_classic() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line = element_blank(),
    strip.background = element_rect(
      color = NA, fill = "grey"
    )
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "3 month") +
  theme(legend.position = "bottom") +
  guides(col = guide_legend(title = ""))



# get green-up/green-down doy for each tree
if (!file.exists("data/processed/dt_flower_doy.rds")) {
  random_id <- df_dt_flower$id %>% unique()
  flower_doy_df_list <- vector(mode = "list", length = length(random_id))
  for (i in 1:length(random_id)) {
    idoi <- as.character(random_id)[i]

    flower_df <- get_doy(thres_df_taxa, ts_df_subset, idoi)

    # print(paste0(i , " out of ", length(random_id)))
    flower_doy_df_list[[i]] <- flower_df
  }

  # bind with species and site info
  flower_doy_df <- bind_rows(flower_doy_df_list) %>%
    as_tibble() %>%
    left_join(df_dt_meta %>% select(id, species, lat, lon) %>% mutate(id = as.character(id)), by = "id") %>%
    mutate(site = siteoi, year = yearoi)
  write_rds(flower_doy_df, "data/processed/dt_flower_doy.rds")
}
flower_doy_df <- read_rds("data/processed/dt_flower_doy.rds")
