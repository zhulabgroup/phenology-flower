v_site_lat <- df_meta %>%
  filter(site %in% v_site) %>%
  arrange(lat) %>%
  pull(sitename)

df_nab_short <- df_nab %>%
  right_join(df_meta %>% select(stationid, site, sitename) %>% drop_na(site), by = "stationid") %>%
  filter(taxa %in% unique(v_taxa_short)) %>%
  mutate(doy = lubridate::yday(date)) %>%
  mutate(year = lubridate::year(date)) %>%
  mutate(taxa = factor(taxa, levels = v_taxa_chron %>% str_split(" ", simplify = T) %>% as.data.frame() %>% pull(V1) %>% unique()))

p_nab_calen <- df_nab_short %>%
  mutate(count = (count + 1) %>% log(10)) %>%
  # mutate(count_st=(count-min(count, na.rm = T))/(max(count, na.rm = T)-min(count, na.rm = T))) %>%
  # mutate(taxa_parse = case_when(
  #   !taxa %in% c("Cupressaceae", "Pinaceae", "Poaceae") ~ paste0("italic('", taxa, "')"),
  #   TRUE ~ taxa
  # )) %>%
  # mutate(sitename=as.factor(sitename)) %>%
  mutate(sitename = factor(sitename, levels = v_site_lat)) %>%
  # mutate(taxa_parse = factor(taxa_parse, levels = data.frame(taxa = unique(v_taxa_short)) %>%
  #   mutate(taxa_parse = case_when(
  #     !taxa %in% c("Cupressaceae", "Pinaceae", "Poaceae") ~ paste0("italic('", taxa, "')"),
  #     TRUE ~ taxa
  #   )) %>%
  #   pull(taxa_parse))) %>%
  mutate(doy = doy + lubridate::date("2023-01-01") - 1) %>%
  ggplot() +
  geom_tile(aes(x = doy, y = sitename, fill = count), alpha = 1) +
  facet_wrap(. ~ taxa, nrow = 2, scales = "free") +
  scale_x_date(
    date_labels = "%b",
    breaks = seq(lubridate::date("2023-01-01"),
      lubridate::date("2023-12-31") + 1,
      by = "3 months"
    )
  ) +
  ylab("") +
  xlab("") +
  theme_classic() +
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
    breaks = (c(0, 1, 10, 100, 1000, 10000) + 1) %>% log(10),
    labels = c(0, 1, 10, 100, 1000, 10000),
    name = expression(Pollen ~ concentration ~ (grains ~ m^-3))
  )
