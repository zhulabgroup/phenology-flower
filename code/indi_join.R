# preprocess ps data
df_ps_site <- read_rds(paste0(.path$ps, "ts_main/ps_", siteoi, "_", taxaoi_short, ".rds"))

df_ps_site_proc <- process_ps(df_ps_site %>% filter(id %in% df_dt_meta$id_ps)) %>%
  rename(id_ps = id) %>%
  left_join(df_dt_meta %>% select(id, lon, lat, species, id_ps), by = c("id_ps", "lon", "lat")) %>%
  select(-id_ps)

# subset nab data
df_pollen_site <- df_nab_full %>%
  left_join(df_meta %>% select(id, site), by = "id") %>%
  filter(site == siteoi) %>%
  filter(genus == taxaoi_short | family == taxaoi_short)

# subset npn data
df_npn_site <- df_npn %>%
  filter(site == siteoi) %>%
  filter(taxa == taxaoi_short)

# join ps, nab, and npn data
df_ts_site <- df_ps_site_proc %>%
  select(id, date,
    `EVI (PS)` = evi
  ) %>%
  mutate(id = as.character(id)) %>%
  full_join(
    df_pollen_site %>%
      select(date, `pollen (NAB)` = count) %>%
      mutate(id = "pollen"),
    by = c("date", "id")
  ) %>%
  full_join(
    df_npn_site %>%
      select(date, `flower (NPN)` = count) %>%
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
df_ts_year <- df_ts_site %>%
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


# # summarize quantiles on the site level
# df_ts_year_summary <- df_ts_year %>%
#   drop_na(value) %>%
#   group_by(date, var, doy, year) %>%
#   summarise(
#     q1 = quantile(value, 0.05, na.rm = T),
#     q2 = quantile(value, 0.5, na.rm = T),
#     q3 = quantile(value, 0.95, na.rm = T),
#     n = n()
#   ) %>%
#   filter(n > 1) %>%
#   ungroup()

# visualize for one tree
idoi <- "5"
species <- df_dt_flower %>%
  filter(id == idoi) %>%
  slice(1) %>%
  pull(Species)
p_dt_join <- df_ts_year %>%
  filter(id == idoi | id == "pollen" | id == "npn") %>%
  ggplot() +
  geom_point(aes(x = date, y = value, col = var)) +
  facet_wrap(. ~ var, ncol = 1, scales = "free_y") +
  scale_color_manual(values = cols) +
  labs(
    ylab = "standardized value",
    col = "data source"
  ) +
  ggtitle(paste0("Site: ", siteoi, "  Year: ", yearoi, "  ID: ", idoi, "  Species: ", species)) +
  theme_classic()
