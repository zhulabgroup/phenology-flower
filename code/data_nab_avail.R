v_site_lat <- df_meta %>%
  filter(site %in% v_site) %>%
  arrange(lat) %>%
  pull(sitename)

if (.full_data) {
  df_nab_avail <- df_nab %>%
    right_join(df_meta %>% select(stationid, site, sitename) %>% drop_na(site), by = "stationid") %>%
    mutate(sitename = factor(sitename, levels = v_site_lat)) %>%
    filter(taxa %in% unique(v_taxa_short)) %>%
    distinct(site, sitename, date)
  write_rds(df_nab_avail, str_c(.path$intermediate, "nab/df_nab_avail.rds"))
} else {
  df_nab_avail <- read_rds(str_c(.path$intermediate, "nab/df_nab_avail.rds"))
}

p_nab_avail <- df_nab_avail %>%
  ggplot() +
  geom_tile(aes(x = date, y = sitename)) +
  theme_classic() +
  scale_x_date(date_labels = "%Y", date_breaks = "1 year") +
  labs(
    x = "",
    y = ""
  )

# save figure
if (.fig_save) {
  ggsave(
    plot = p_nab_avail,
    filename = str_c(.path$output, "supp/supp_nab_avail.pdf"),
    width = 7,
    height = 5,
    device = pdf
  )
}
