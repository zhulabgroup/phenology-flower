# preprocess ps data
ps_path <- "./data/PS/"
ps_df <- read_rds(paste0(ps_path, "ts/ps_", siteoi, "_", taxaoi_short, ".rds"))
ps_df_proc <- ps_df %>%
  rename(id_ps = id) %>%
  right_join(df_dt_meta %>% select(id, species, lat, lon, id_ps), by = c("id_ps", "lon", "lat")) %>%
  drop_na() %>%
  mutate(date = lubridate::as_date(time)) %>%
  mutate(
    date = lubridate::date(date),
    year = lubridate::year(date),
    doy = lubridate::yday(date),
    # hour = lubridate::hour(time)
  ) %>%
  filter(clear > 0.9, snow < 0.1, shadow < 0.1, haze_light < 0.1, haze_heavy < 0.1, cloud < 0.1, confidence >= 80) %>%
  group_by(id, lon, lat, date, year, doy) %>%
  summarise(
    blue = mean(blue),
    green = mean(green),
    red = mean(red),
    nir = mean(nir),
    species = head(species, 1)
  ) %>%
  ungroup() %>%
  mutate(evi = 2.5 * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)) %>%
  # mutate(ndvi =  (nir - red) / (nir + red)) %>%
  filter(evi > 0, evi <= 1) %>%
  filter(red > 0, green > 0, blue > 0)

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
    `enhanced vegetation index (PS)` = evi,
    species
  ) %>%
  mutate(id = as.factor(id)) %>%
  full_join(
    pollen_df %>%
      dplyr::select(date, `pollen concentration (NAB)` = count) %>%
      mutate(id = "pollen"),
    by = c("date", "id")
  ) %>%
  full_join(
    npn_df %>%
      dplyr::select(date, `flower observation (USA-NPN)` = count) %>%
      mutate(id = "npn"),
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
  gather(key = "var", value = "value", -date, -id, -doy, -year, -species) %>%
  mutate(value = as.numeric(value)) %>%
  mutate(var = fct_relevel(var, levels = c("enhanced vegetation index (PS)", "G2R (PS)", "EBI (PS)", "pollen concentration (NAB)", "flower observation (USA-NPN)"))) %>%
  arrange(doy)

# join with flowering data
ts_df_subset <- ts_df_subset %>%
  bind_rows(df_dt_flower %>% dplyr::select(id, date, lon, lat, value = flowering_raw, doy = julian_day, species = Species) %>%
    mutate(
      var = "flower observation (Katz's team)",
      year = 2017
    ))

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
