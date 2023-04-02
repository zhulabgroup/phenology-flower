# get green-up/green-down doy for each tree
if (!file.exists("data/processed/dt_flower_doy.rds")) {
  id_list <- df_dt_flower$id %>% unique()
  ts_df_ext_list <- flower_doy_df_years_list <- vector(mode = "list", length = length(year_list) + 1)
  for (y in 1:(length(year_list) + 1)) {
    yearoi <- c(2017, year_list)[y]
    ts_df_subset <- ts_df %>%
      filter(doy != 366) %>%
      filter(year == yearoi | year == (yearoi - 1) | year == (yearoi + 1)) %>%
      mutate(doy = ifelse(doy > 273 & year == yearoi - 1, doy - 365, doy)) %>%
      mutate(year = ifelse(doy <= 0 & year == yearoi - 1, year + 1, year)) %>%
      mutate(doy = ifelse(doy < 152 & year == yearoi + 1, doy + 365, doy)) %>%
      mutate(year = ifelse(doy > 365 & year == yearoi + 1, year - 1, year)) %>%
      filter(year == yearoi) %>%
      gather(key = "var", value = "value", -date, -id, -doy, -year) %>%
      mutate(var = factor(var,
        labels = c("enhanced vegetation index (PS)", "pollen concentration (NAB)", "flower observation (USA-NPN)", "flower observation (Katz's team)"),
        levels = c("EVI (PS)", "pollen (NAB)", "flower (NPN)", "flower (DK)")
      )) %>%
      arrange(doy)
    ts_df_ext_list[[y]] <- ts_df_subset

    flower_doy_df_id_list <- vector(mode = "list", length = length(id_list))
    for (i in 1:length(id_list)) {
      idoi <- as.character(id_list)[i]

      flower_doy_df_id_list[[i]] <- get_doy(thres_df_taxa, ts_df_subset, idoi)
      print(paste0(i, " out of ", length(id_list)))
    }
    flower_doy_df_id <- bind_rows(flower_doy_df_id_list)

    print(paste0(siteoi, ", ", yearoi))

    flower_doy_df_years_list[[y]] <- flower_doy_df_id %>%
      mutate(year = yearoi)
  }
  ts_df_ext <- bind_rows(ts_df_ext_list)
  flower_doy_df <- bind_rows(flower_doy_df_years_list)

  write_rds(ts_df, "data/processed/dt_ts_ext.rds")
  write_rds(flower_doy_df, "data/processed/dt_flower_doy.rds")
}

ts_df_ext <- read_rds("data/processed/dt_ts_ext.rds")
flower_doy_df <- read_rds("data/processed/dt_flower_doy.rds")


# one tree
p_dt_doy_one <- plot_tree_one(ts_df_ext, flower_doy_df, yearoi = 2017, idoi = "5")

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


p_dt_doy_mult <- plot_tree_mult(ts_df_ext, flower_doy_df, yearoi = 2017, idoi = idoi_list)
