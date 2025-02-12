v_site_lat <- df_meta %>%
  filter(site %in% v_site) %>%
  arrange(lat) %>%
  pull(sitename)

df_nab_short <- df_nab %>%
  right_join(df_meta %>% select(stationid, site, sitename) %>% drop_na(site), by = "stationid") %>%
  filter(taxa %in% unique(v_taxa_short)) %>%
  mutate(doy = lubridate::yday(date)) %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(taxa = factor(taxa, levels = v_taxa %>% sort() %>% str_split(" ", simplify = T) %>% as.data.frame() %>% pull(V1) %>% unique()))

df_nab_summ <- df_nab_short %>%
  group_by(taxa, sitename, doy) %>%
  summarise(count = mean(count, na.rm = T)) %>%
  ungroup()

p_nab_calen <- df_nab_summ %>%
  mutate(count = (count + 1) %>% log(10)) %>%
  mutate(sitename = factor(sitename, levels = v_site_lat)) %>%
  mutate(doy = doy + lubridate::date("2023-01-01") - 1) %>%
  ggplot() +
  geom_tile(aes(x = doy, y = sitename, fill = count), alpha = 1) +
  facet_wrap(. ~ taxa, nrow = 3) +
  scale_x_date(
    date_labels = "%b",
    breaks = seq(lubridate::date("2023-01-01"),
      lubridate::date("2023-12-31") + 1,
      by = "3 months"
    )
  ) +
  ylab("") +
  xlab("") +
  ggthemes::theme_few() +
  theme(
    strip.text = element_text(face = "italic"),
    axis.line.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1, "cm")
  ) +
  scale_fill_gradient(
    low = "light yellow", high = "red", na.value = "white",
    breaks = (c(0, 1, 10, 100, 1000, 10000) + 1) %>% log(10),
    labels = c(0, 1, 10, 100, 1000, 10000),
    name = expression(Pollen ~ concentration ~ (grains ~ m^-3))
  )

# save figure
if (.fig_save) {
  ggsave(
    plot = p_nab_calen,
    filename = str_c(.path$out_fig, "main_nab_calen.pdf"),
    width = 9,
    height = 6,
    device = pdf
  )
}
