npn_df_all <- read_rds("./data/processed/npn_dat.rds")
p_npn_calen <- npn_df_all %>%
  left_join(meta_df %>% dplyr::select(id, site, sitename), by = "site") %>%
  filter(site %in% site_list) %>%
  filter(taxa %in% taxa_short_list) %>%
  mutate(doy = format(date, "%j") %>% as.integer()) %>%
  mutate(year = format(date, "%Y") %>% as.integer()) %>%
  # filter(year %in% year_list) %>%
  group_by(taxa, site) %>%
  mutate(count_st = (count - min(count, na.rm = T)) / (max(count, na.rm = T) - min(count, na.rm = T))) %>%
  ggplot() +
  geom_point(aes(x = doy, y = count_st, group = taxa, col = taxa), alpha = 0.3) +
  facet_grid(cols = vars(taxa), rows = vars(sitename), scales = "free_y") +
  theme_classic()
