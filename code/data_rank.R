if (!file.exists(str_c(.path$dat_other, "main_ts.rds"))) {
  cl <- makeCluster(length(v_site), outfile = "")
  registerDoSNOW(cl)

  ls_df_ps_doy_taxa <- ls_df_ps_ts_taxa <- vector(mode = "list")
  for (taxaoi in v_taxa) {
    taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
    ls_df_ps_ts_site <-
      foreach(
        siteoi = v_site,
        .packages = c("tidyverse")
      ) %dopar% {
        f_ts <- str_c(.path$ps, "ts_main/ps_", siteoi, "_", taxaoi_short, ".rds")
        if (file.exists(f_ts)) {
          df_ps <- read_rds(f_ts)
          set.seed(1)
          v_id <- df_ps %>%
            pull(id) %>%
            sample(min(100, length(.)))

          df_ps_proc <- process_ps(df_ps %>% filter(id %in% v_id))

          df_ps_proc %>% mutate(site = siteoi)
        }
      }


    ls_df_ps_ts_taxa[[taxaoi_short]] <- bind_rows(ls_df_ps_ts_site) %>% mutate(taxa = taxaoi_short)

    f_doy <- str_c(.path$res, taxaoi_short, "/flower_doy.rds")
    if (file.exists(f_doy)) {
      df_ps_doy <- read_rds(f_doy)

      ls_df_ps_doy_taxa[[taxaoi]] <- df_ps_doy %>% mutate(taxa = taxaoi_short)
    }
  }
  df_ps_ts_alltaxa <- bind_rows(ls_df_ps_ts_taxa)
  df_ps_doy_alltaxa <- bind_rows(ls_df_ps_doy_taxa)

  write_rds(df_ps_ts_alltaxa, str_c(.path$dat_other, "main_ts.rds"))
  write_rds(df_ps_doy_alltaxa, str_c(.path$dat_other, "main_doy.rds"))
} else {
  df_ps_ts_alltaxa <- read_rds(str_c(.path$dat_other, "main_ts.rds"))
  df_ps_doy_alltaxa <- read_rds(str_c(.path$dat_other, "main_doy.rds"))
}



df_rank_ps <- df_ps_doy_alltaxa %>%
  filter(!taxa %in% c("Ambrosia", "Poaceae")) %>%
  filter(thres == 0.5) %>%
  mutate(id = as.numeric(id)) %>%
  rename(ps_id = id) %>%
  left_join(
    df_plant %>%
      filter(genus %in% v_taxa_short | family %in% v_taxa_short) %>%
      mutate(taxa = case_when(
        genus %in% v_taxa_short ~ genus,
        family %in% v_taxa_short ~ family
      )) %>%
      group_by(site, taxa) %>%
      mutate(ps_id = row_number()) %>%
      ungroup() %>%
      drop_na(lon, lat) %>%
      select(id, ps_id, taxa, genus, species, site),
    by = c("ps_id", "taxa", "site")
  ) %>%
  drop_na(id) %>%
  select(site, id, taxa, genus, species, year, doy) %>%
  mutate(year = case_when(
    year == 2018 ~ "former",
    year == 2019 ~ "later"
  )) %>%
  drop_na(year) %>%
  spread(key = "year", value = "doy")

df_ts_ps <- df_ps_ts_alltaxa %>%
  filter(year %in% c(2018, 2019)) %>%
  filter(!taxa %in% c("Ambrosia", "Poaceae")) %>%
  mutate(id = as.numeric(id)) %>%
  rename(ps_id = id) %>%
  left_join(
    df_plant %>%
      filter(genus %in% v_taxa_short | family %in% v_taxa_short) %>%
      mutate(taxa = case_when(
        genus %in% v_taxa_short ~ genus,
        family %in% v_taxa_short ~ family
      )) %>%
      group_by(site, taxa) %>%
      mutate(ps_id = row_number()) %>%
      ungroup() %>%
      drop_na(lon, lat) %>%
      select(id, ps_id, taxa, genus, species, site),
    by = c("ps_id", "taxa", "site")
  ) %>%
  drop_na(id) %>%
  select(site, id, taxa, genus, species, year, date, year, doy, evi)


siteoi <- "DT"
taxaoi <- "Quercus"
speciesoi <- "Quercus rubra"


# rank individual
set.seed(1)
p_ts_ps_indi <- df_ts_ps %>%
  filter(site == siteoi) %>%
  filter(taxa == taxaoi) %>%
  filter(species == speciesoi) %>%
  filter(id %in% ((.) %>% pull(id) %>% unique() %>% sample(3))) %>%
  mutate(id = as.factor(id)) %>%
  ggplot() +
  geom_line(aes(x = date, y = evi, col = id, group = id), alpha = 1) +
  theme_classic() +
  ylab("evi") +
  guides(col = "none")

p_rank_ps_indi <- df_rank_ps %>%
  filter(taxa == taxaoi) %>%
  group_by(site, species) %>%
  filter(n() > 100) %>%
  ungroup() %>%
  group_by(site, species) %>%
  nest() %>%
  mutate(
    p_val = map(data, ~ lm(later ~ former, data = .)) %>%
      map_dbl(~ broom::glance(.) %>% pull(p.value))
  ) %>%
  unnest(cols = data) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = former, y = later), alpha = 0.25) +
  geom_smooth(aes(x = former, y = later, linetype = ifelse(p_val <= 0.05, "sig", "ns")), method = "lm", se = F) +
  scale_linetype_manual(values = c("sig" = "solid", "ns" = "dashed")) +
  ggpubr::stat_cor(aes(x = former, y = later)) +
  theme_classic() +
  facet_wrap(. ~ species * site, scales = "free") +
  labs(
    x = "Day of 50% green-up in 2018 (from PS)",
    y = "Day of 50% green-up in 2019 (from PS)",
  ) +
  guides(linetype = "none")

# rank species
p_ts_ps_sp <- df_ts_ps %>%
  filter(site == siteoi) %>%
  filter(taxa == taxaoi) %>%
  filter(species %in% ((.) %>%
    group_by(species) %>%
    summarise(n = n()) %>%
    arrange(desc(n)) %>%
    head(3) %>%
    pull(species))) %>%
  group_by(species, date) %>%
  summarise(
    median = median(evi),
    upper = quantile(evi, 0.95),
    lower = quantile(evi, 0.05)
  ) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = date, y = median, col = species), alpha = 1) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = species), alpha = 0.1) +
  theme_classic() +
  ylab("evi")

p_rank_ps_sp <- df_rank_ps %>%
  # filter(site == siteoi) %>%
  group_by(site, taxa, species) %>%
  filter(n() > 30) %>%
  summarise(
    former_lower = quantile(former, 0.25, na.rm = T),
    former_upper = quantile(former, 0.75, na.rm = T),
    former = median(former, na.rm = T),
    later_lower = quantile(later, 0.25, na.rm = T),
    later_upper = quantile(later, 0.75, na.rm = T),
    later = median(later, na.rm = T),
    n = n()
  ) %>%
  group_by(site, taxa) %>%
  filter(n() >= 4) %>%
  ungroup() %>%
  group_by(site, taxa) %>%
  nest() %>%
  mutate(
    p_val = map(data, ~ lm(later ~ former, data = .)) %>%
      map_dbl(~ broom::glance(.) %>% pull(p.value))
  ) %>%
  unnest(cols = data) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = former, y = later), alpha = 0.25) +
  geom_errorbar(aes(x = former, ymin = later_lower, ymax = later_upper), alpha = 0.25) +
  geom_errorbarh(aes(y = later, xmin = former_lower, xmax = former_upper), alpha = 0.25) +
  geom_smooth(aes(x = former, y = later, linetype = ifelse(p_val <= 0.05, "sig", "ns")), method = "lm", se = F) +
  scale_linetype_manual(values = c("sig" = "solid", "ns" = "dashed")) +
  ggpubr::stat_cor(aes(x = former, y = later)) +
  theme_classic() +
  facet_wrap(. ~ taxa * site, scales = "free") +
  labs(
    x = "Day of 50% green-up in 2018 (from PS)",
    y = "Day of 50% green-up in 2019 (from PS)",
  ) +
  guides(linetype = "none")

# rank genus/family
p_ts_ps_taxa <- df_ts_ps %>%
  filter(site == siteoi) %>%
  filter(taxa %in% c("Quercus", "Acer", "Cupressaceae")) %>%
  group_by(taxa, date) %>%
  summarise(
    median = median(evi),
    upper = quantile(evi, 0.95),
    lower = quantile(evi, 0.05)
  ) %>%
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = date, y = median, col = taxa), alpha = 1) +
  geom_ribbon(aes(x = date, ymin = lower, ymax = upper, fill = taxa), alpha = 0.1) +
  theme_classic() +
  ylab("evi")

p_rank_ps_taxa <- df_rank_ps %>%
  # filter(site == siteoi) %>%
  group_by(site, taxa) %>%
  filter(n() > 100) %>%
  summarise(
    former_lower = quantile(former, 0.25, na.rm = T),
    former_upper = quantile(former, 0.75, na.rm = T),
    former = median(former, na.rm = T),
    later_lower = quantile(later, 0.25, na.rm = T),
    later_upper = quantile(later, 0.75, na.rm = T),
    later = median(later, na.rm = T),
    n = n()
  ) %>%
  group_by(site) %>%
  filter(n() >= 5) %>%
  ungroup() %>%
  ungroup() %>%
  group_by(site) %>%
  nest() %>%
  mutate(
    p_val = map(data, ~ lm(later ~ former, data = .)) %>%
      map_dbl(~ broom::glance(.) %>% pull(p.value))
  ) %>%
  unnest(cols = data) %>%
  ungroup() %>%
  ggplot() +
  geom_point(aes(x = former, y = later, col = taxa), alpha = 0.75) +
  geom_errorbar(aes(x = former, ymin = later_lower, ymax = later_upper, col = taxa), alpha = 0.5) +
  geom_errorbarh(aes(y = later, xmin = former_lower, xmax = former_upper, col = taxa), alpha = 0.5) +
  geom_smooth(aes(x = former, y = later, linetype = ifelse(p_val <= 0.05, "sig", "ns")), method = "lm", se = F) +
  scale_linetype_manual(values = c("sig" = "solid", "ns" = "dashed")) +
  ggpubr::stat_cor(aes(x = former, y = later)) +
  theme_classic() +
  facet_wrap(. ~ site, scales = "free") +
  labs(
    x = "Day of 50% green-up in 2018 (from PS)",
    y = "Day of 50% green-up in 2019 (from PS)",
  ) +
  guides(linetype = "none")
