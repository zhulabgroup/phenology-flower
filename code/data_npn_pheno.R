df_npn <- read_rds("./data/processed/dat_npn.rds")
p_npn_calen <- df_npn %>%
  left_join(df_meta %>% dplyr::select(id, site, sitename), by = "site") %>%
  filter(site %in% v_site) %>%
  filter(taxa %in% v_taxa_short) %>%
  mutate(doy = format(date, "%j") %>% as.integer()) %>%
  mutate(year = format(date, "%Y") %>% as.integer()) %>%
  # filter(year %in% year_list) %>%
  group_by(taxa, site) %>%
  mutate(count_st = (count - min(count, na.rm = T)) / (max(count, na.rm = T) - min(count, na.rm = T))) %>%
  ggplot() +
  geom_point(aes(x = doy, y = count_st, group = taxa, col = taxa), alpha = 0.3) +
  facet_grid(cols = vars(taxa), rows = vars(sitename), scales = "free_y") +
  theme_classic()
