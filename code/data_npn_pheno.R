df_npn <- read_rds("./data/processed/dat_npn.rds")

df_npn_short <- df_npn %>%
  right_join(df_meta %>% select(stationid, site, sitename), by = "site") %>%
  filter(taxa %in% unique(v_taxa_short)) %>%
  mutate(doy = lubridate::yday(date)) %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(taxa = factor(taxa, levels = v_taxa %>% sort() %>% str_split(" ", simplify = T) %>% as.data.frame() %>% pull(V1) %>% unique()))

df_npn_summ <- df_npn_short %>%
  group_by(taxa, sitename, doy) %>%
  summarise(perc = mean(perc, na.rm = T)) %>%
  ungroup()

p_npn_calen <- df_npn_summ %>%
  mutate(sitename = factor(sitename, levels = v_site_lat)) %>%
  mutate(doy = doy + lubridate::date("2023-01-01") - 1) %>%
  ggplot() +
  geom_tile(aes(x = doy, y = sitename, fill = perc), alpha = 1) +
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
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "bottom",
    legend.key.width = unit(1, "cm")
  ) +
  scale_fill_gradient(
    low = "light yellow", high = "red", na.value = "white",
    breaks = c(0, 0.5, 1),
    labels = c("0%", "50%", "100%"),
    name = "Percentage of Yes observations"
  )
