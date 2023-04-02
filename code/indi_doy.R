# get green-up/green-down doy for each tree
if (!file.exists("data/processed/dt_flower_doy.rds")) {
  v_id <- df_dt_flower$id %>% unique()
  ls_df_ts_ext_year <- ls_df_flower_doy_year <- vector(mode = "list", length = length(v_year) + 1)
  for (y in 1:(length(v_year) + 1)) {
    yearoi <- c(2017, v_year)[y]
    df_ts_year <- df_ts_site %>%
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
    ls_df_ts_ext_year[[y]] <- df_ts_year

    ls_df_flower_doy_id <- vector(mode = "list", length = length(v_id))
    for (i in 1:length(v_id)) {
      idoi <- as.character(v_id)[i]

      ls_df_flower_doy_id[[i]] <- get_doy(df_thres_taxa, df_ts_year, idoi)
      print(str_c(i, " out of ", length(v_id)))
    }
    df_flower_doy_id <- bind_rows(ls_df_flower_doy_id)

    print(paste0(siteoi, ", ", yearoi))

    ls_df_flower_doy_year[[y]] <- df_flower_doy_id %>%
      mutate(year = yearoi)
  }
  df_ts_ext <- bind_rows(ls_df_ts_ext_year)
  df_flower_doy <- bind_rows(ls_df_flower_doy_year)

  write_rds(df_ts_ext, "data/processed/dt_ts_ext.rds")
  write_rds(df_flower_doy, "data/processed/dt_flower_doy.rds")
}
df_ts_ext <- read_rds("data/processed/dt_ts_ext.rds")
df_flower_doy <- read_rds("data/processed/dt_flower_doy.rds")


# one tree
p_dt_doy_one <- plot_tree_one(df_ts_ext, df_flower_doy, yearoi = 2017, idoi = "5")

# multiple trees
set.seed(3)
v_id <- df_dt_flower_peak %>%
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


p_dt_doy_mult <- plot_tree_mult(df_ts_ext, df_flower_doy, yearoi = 2017, idoi = v_id)
