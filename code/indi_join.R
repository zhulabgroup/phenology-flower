# preprocess ps data
ps_path <- "./data/PS/"
ps_df <- read_rds(paste0(ps_path, "ts/ps_", siteoi, "_", taxaoi_short, ".rds"))

ps_df_proc <- process_ps(ps_df %>% filter(id %in% df_dt_meta$id_ps)) %>%
  rename(id_ps = id) %>%
  left_join(df_dt_meta %>% select(id, lon, lat, species, id_ps), by = c("id_ps", "lon", "lat")) %>%
  select(-id_ps)

# subset nab data
pollen_df <- nab_with_taxa_df %>%
  left_join(meta_df %>% dplyr::select(id, site), by = "id") %>%
  filter(site == siteoi) %>%
  filter(genus == taxaoi_short | family == taxaoi_short)

# subset npn data
npn_df <- npn_df_all %>%
  filter(site == siteoi) %>%
  filter(taxa == taxaoi_short)

# join ps, nab, and npn data
ts_df <- ps_df_proc %>%
  select(id, date,
    `EVI (PS)` = evi
  ) %>%
  mutate(id = as.character(id)) %>%
  full_join(
    pollen_df %>%
      dplyr::select(date, `pollen (NAB)` = count) %>%
      mutate(id = "pollen"),
    by = c("date", "id")
  ) %>%
  full_join(
    npn_df %>%
      dplyr::select(date, `flower (NPN)` = count) %>%
      mutate(id = "npn"),
    by = c("date", "id")
  ) %>%
  full_join(
    df_dt_flower %>%
      select(id, date, `flower (DK)` = flowering_raw),
    by = c("date", "id")
  ) %>%
  arrange(id, date) %>%
  mutate(
    doy = lubridate::yday(date),
    year = lubridate::year(date)
  )

# focus on one year, spanning from day 274 in the previous year to day 151 in the next year
ts_df_subset <- ts_df %>%
  filter(doy != 366) %>%
  # filter(doy>start_doy,doy<=end_doy) %>%
  filter(year == yearoi | year == (yearoi - 1) | year == (yearoi + 1)) %>%
  mutate(doy = ifelse(doy > 273 & year == yearoi - 1, doy - 365, doy)) %>%
  mutate(year = ifelse(doy <= 0 & year == yearoi - 1, year + 1, year)) %>%
  mutate(doy = ifelse(doy < 152 & year == yearoi + 1, doy + 365, doy)) %>%
  mutate(year = ifelse(doy > 365 & year == yearoi + 1, year - 1, year)) %>%
  filter(year == yearoi) %>%
  gather(key = "var", value = "value", -date, -id, -doy, -year) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(var = factor(var,
    levels = c("EVI (PS)", "pollen (NAB)", "flower (NPN)", "flower (DK)"),
    labels = c("enhanced vegetation index (PS)", "pollen concentration (NAB)", "flower observation (USA-NPN)", "flower observation (Katz's team)")
  )) %>%
  arrange(doy)


# summarize quantiles on the site level
ts_df_subset_summary <- ts_df_subset %>%
  drop_na(value) %>%
  group_by(date, var, doy, year) %>%
  summarise(
    q1 = quantile(value, 0.05, na.rm = T),
    q2 = quantile(value, 0.5, na.rm = T),
    q3 = quantile(value, 0.95, na.rm = T),
    n = n()
  ) %>%
  filter(n > 1) %>%
  ungroup()

# visualize for one tree
idoi <- "5"
plant_sp <- df_dt_flower %>%
  filter(id == idoi) %>%
  slice(1) %>%
  pull(Species)
p_dt_join <- ts_df_subset %>%
  filter(id == idoi | id == "pollen" | id == "npn") %>%
  ggplot() +
  geom_point(aes(x = date, y = value, col = var)) +
  facet_wrap(. ~ var, ncol = 1, scales = "free_y") +
  scale_color_manual(values = cols) +
  labs(
    ylab = "standardized value",
    col = "data source"
  ) +
  ggtitle(paste0("Site: ", siteoi, "  Year: ", yearoi, "  ID: ", idoi, "  Species: ", plant_sp)) +
  theme_classic()
