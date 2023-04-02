v_site_lat <- df_meta %>%
  filter(site %in% v_site) %>%
  arrange(lat) %>%
  pull(sitename)

labelfunc_x <- function(x) {
  origin <- as.Date("2021-01-01")
  format(origin + x, format = "%b")
}

p_nab_calen <- df_nab_full %>%
  left_join(df_meta %>% dplyr::select(id, site, sitename), by = "id") %>%
  filter(site %in% v_site) %>%
  filter(taxa %in% v_taxa_short) %>%
  filter(!taxa %in% c("Poaceae", "Ambrosia")) %>%
  mutate(doy = format(date, "%j") %>% as.integer()) %>%
  mutate(year = format(date, "%Y") %>% as.integer()) %>%
  # filter(year %in% year_list) %>%
  # group_by(taxa, site) %>%
  mutate(count = (count + 1) %>% log(10)) %>%
  # mutate(count_st=(count-min(count, na.rm = T))/(max(count, na.rm = T)-min(count, na.rm = T))) %>%
  mutate(taxa_parse = case_when(
    !taxa %in% c("Cupressaceae", "Pinaceae", "Poaceae") ~ paste0("italic('", taxa, "')"),
    TRUE ~ taxa
  )) %>%
  # mutate(sitename=as.factor(sitename)) %>%
  mutate(sitename = factor(sitename, levels = v_site_lat)) %>%
  mutate(taxa_parse = factor(taxa_parse, levels = data.frame(taxa = unique(v_taxa_short)) %>%
    mutate(taxa_parse = case_when(
      !taxa %in% c("Cupressaceae", "Pinaceae", "Poaceae") ~ paste0("italic('", taxa, "')"),
      TRUE ~ taxa
    )) %>%
    pull(taxa_parse))) %>%
  ggplot() +
  geom_tile(aes(x = doy, y = sitename, fill = count), alpha = 1) +
  facet_wrap(. ~ taxa_parse, labeller = label_parsed) +
  ylab("") +
  xlab("") +
  theme_classic() +
  # scale_x_continuous(breaks = scales::pretty_breaks(n = 2)) +
  scale_x_continuous(labels = labelfunc_x) +
  theme(
    axis.line.y = element_blank(),
    # axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  # theme(strip.text.y= element_text(angle = 0))+
  theme(
    legend.position = "bottom",
    legend.key.width = unit(2, "cm")
  ) +
  scale_fill_gradient(
    low = "light yellow", high = "red", na.value = "white",
    breaks = (c(0, 1, 10, 100, 1000, 10000) + 1) %>% log(10),
    labels = c(0, 1, 10, 100, 1000, 10000),
    name = expression(Pollen ~ concentration ~ (grains / m^3))
  )
