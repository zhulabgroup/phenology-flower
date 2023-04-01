site_vis <- "DT"
taxa_vis <- "Quercus"
set.seed(1)
p_ps_ref <- read_rds(paste0(ps_path, "ts/ps_", site_vis, "_", taxa_vis, ".rds")) %>%
  filter(id %in% ((.) %>% pull(id) %>% sample(4))) %>% # four random trees
  filter(clear == 1, snow == 0, shadow == 0, haze_light == 0, haze_heavy == 0, cloud == 0, confidence >= 80) %>%
  select(id, time, lon, lat, blue, green, red, nir) %>%
  gather(key = "band", value = "value", -id, -time, -lon, -lat) %>%
  ggplot() +
  geom_line(aes(x = time, y = value, col = band), alpha = 0.5) +
  facet_wrap(. ~ id, ncol = 1) +
  theme_classic() +
  ggtitle(paste0("Taxa: ", taxa_vis, "; Site: ", site_vis))



siteoi <- "DT"
evi_list <- vector(mode = "list")
for (taxaoi_short in taxa_short_list %>% unique()) {
  file <- str_c(ps_path, "ts/ps_", siteoi, "_", taxaoi_short, ".rds")
  if (file.exists(file)) {
    ps_df <- read_rds(file)
    set.seed(1)
    random_id <- ps_df %>%
      pull(id) %>%
      sample(min(100, length(.)))

    ps_df_proc <- process_ps(ps_df %>% filter(id %in% random_id))

    evi_list[[taxaoi_short]] <- ps_df_proc %>% mutate(taxa = taxaoi_short)
  }
}
evi_alltaxa_df <- bind_rows(evi_list)

p_ps_taxa <- ggplot(evi_alltaxa_df %>%
  filter(taxa %in% c("Poaceae", "Quercus", "Cupressaceae")) %>%
  filter(year == 2019) %>%
  # filter(doy >= 80, doy < 200) %>%
  group_by(taxa, doy) %>%
  summarise(
    median = median(evi),
    upper = quantile(evi, 0.95),
    lower = quantile(evi, 0.05)
  ) %>%
  ungroup()) +
  geom_line(aes(x = doy, y = median, col = taxa), alpha = 1) +
  geom_ribbon(aes(x = doy, ymin = lower, ymax = upper, fill = taxa), alpha = 0.1) +
  # geom_vline(xintercept = 125)+
  theme_classic() +
  ylab("evi") +
  guides(fill = "none")





siteoi <- "DT"
taxaoi_short <- "Quercus"
ps_df <- read_rds(paste0(ps_path, "ts/ps_", siteoi, "_", taxaoi_short, ".rds"))
set.seed(1)
random_id <- ps_df %>%
  pull(id) %>%
  sample(min(1000, length(.)))

ps_df_proc <- process_ps(ps_df %>% filter(id %in% random_id))
p_ps_species <- ggplot(ps_df_proc %>%
  left_join(
    plant_df %>%
      filter(site == siteoi) %>%
      filter(genus == taxaoi_short | family == taxaoi_short) %>%
      mutate(id = row_number()) %>%
      drop_na(lon, lat) %>%
      select(id, species),
    by = "id"
  ) %>%
  filter(species %in% c(
    "Quercus virginiana", "Quercus fusiformis", "Quercus shumardii",
    "Quercus alba", "Quercus bicolor", "Quercus rubra", "Quercus macrocarpa", "Quercus velutina"
  )) %>%
  filter(year == 2019) %>%
  group_by(species, doy) %>%
  summarise(
    median = median(evi),
    upper = quantile(evi, 0.95),
    lower = quantile(evi, 0.05)
  ) %>%
  ungroup()) +
  geom_line(aes(x = doy, y = median, col = species), alpha = 1) +
  geom_ribbon(aes(x = doy, ymin = lower, ymax = upper, fill = species), alpha = 0.1) +
  # geom_vline(xintercept = 125)+
  theme_classic() +
  ylab("evi") +
  guides(fill = "none")
