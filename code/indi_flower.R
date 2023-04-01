df_dt_flower <- read_csv("data/Detroit_oak_pheno_obs_spring_2017.csv") %>%
  mutate(date = as.Date("2017-01-01") + julian_day - 1) %>%
  rename(id = tree) %>%
  left_join(df_dt_meta %>% select(id, lon, lat, id_ps), by = "id") %>%
  mutate(id = as.character(id))

df_dt_flower_ts <- df_dt_flower %>%
  select(id, flowering_interp, flowering_raw, date)

df_dt_flower_peak <- df_dt_flower %>%
  group_by(id) %>%
  filter(flowering_interp >= 95) %>%
  arrange(julian_day) %>%
  slice(1) %>%
  ungroup() %>%
  select(id, doy = julian_day, date)


set.seed(1)
id_view <- df_dt_flower %>%
  distinct(id) %>%
  pull(id) %>%
  sample(6)
p_dt_flower <- ggplot() +
  geom_point(data = df_dt_flower_ts %>% filter(id %in% id_view), aes(x = date, y = flowering_raw), col = "coral") +
  geom_line(data = df_dt_flower_ts %>% filter(id %in% id_view), aes(x = date, y = flowering_interp)) +
  geom_vline(data = df_dt_flower_peak %>% filter(id %in% id_view), aes(xintercept = date), col = "dark orchid") +
  facet_wrap(. ~ id, ncol = 1) +
  ylab("flowering status") +
  theme_classic()
