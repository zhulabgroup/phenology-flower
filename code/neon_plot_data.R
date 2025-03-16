set.seed(1)

df_sample <- df_neon_doy %>%
  left_join(df_neon_meta, by = "id") %>%
  filter(site %in% c("HARV", "ORNL")) %>%
  group_by(site) %>%
  distinct(id) %>%
  sample_n(1)

df_neon_doy_sample <- df_neon_doy %>%
  right_join(df_sample) %>%
  filter(direction == "up", thres == 0.5) %>%
  mutate(date = as.Date(str_c(year, "-01-01")) + doy - 1) %>%
  mutate(
    start_date = as.Date(str_c(year, "-01-01")) + start - 1,
    end_date = as.Date(str_c(year, "-01-01")) + end - 1
  ) %>%
  filter(year >= 2018) %>%
  rowwise() %>%
  mutate(label = str_c("NEON site: ", id %>% str_split("\\.", simplify = T) %>% `[`(4), ", individual ID: ", id %>% str_split("\\.", simplify = T) %>% `[`(5))) %>%
  ungroup()

df_neon_evi_sample <- df_neon_evi %>%
  right_join(df_sample) %>%
  group_by(site, id, year) %>%
  util_extend_ts() %>%
  fill(site, id, .direction = "updown") %>%
  mutate(date = as.Date(str_c(year, "-01-01")) + doy - 1) %>%
  complete(doy = c(-90:(365 + 90))) %>%
  mutate(evi_sm = util_fill_whit(x = evi, maxgap = 60, lambda = 50, minseg = 2)) %>%
  filter(year >= 2018) %>%
  rowwise() %>%
  mutate(label = str_c("NEON site: ", id %>% str_split("\\.", simplify = T) %>% `[`(4), ", individual ID: ", id %>% str_split("\\.", simplify = T) %>% `[`(5))) %>%
  ungroup()

p_neon_data <- ggplot() +
  geom_point(
    data = df_neon_evi_sample,
    aes(x = date, y = evi), alpha = 0.5
  ) +
  geom_line(
    data = df_neon_evi_sample,
    aes(x = date, y = evi_sm, group = year), col = "dark green"
  ) +
  geom_vline(
    data = df_neon_doy_sample,
    aes(xintercept = date), col = "dark green"
  ) +
  geom_rect(
    data = df_neon_doy_sample,
    aes(xmin = start_date, xmax = end_date, ymin = Inf, ymax = -Inf), fill = "dark green", alpha = 0.25
  ) +
  ggthemes::theme_few() +
  facet_wrap(. ~ label, ncol = 1) +
  labs(
    x = "Time",
    y = "Enhanced Vegetation Index"
  )

# save figure
if (.fig_save) {
  ggsave(
    plot = p_neon_data,
    filename = str_c(.path$output, "main/main_neon_data.pdf"),
    width = 9,
    height = 6,
    device = pdf
  )
}
