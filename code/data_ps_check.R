# check reflectance
siteoi <- "DT"
taxaoi <- "Quercus"
set.seed(1)
p_ps_ref <- read_rds(str_c(path_ps, "ts/ps_", siteoi, "_", taxaoi, ".rds")) %>%
  filter(id %in% ((.) %>% pull(id) %>% sample(4))) %>% # four random trees
  filter(clear == 1, snow == 0, shadow == 0, haze_light == 0, haze_heavy == 0, cloud == 0, confidence >= 80) %>%
  select(id, time, lon, lat, blue, green, red, nir) %>%
  gather(key = "band", value = "value", -id, -time, -lon, -lat) %>%
  ggplot() +
  geom_line(aes(x = time, y = value, col = band), alpha = 0.5) +
  facet_wrap(. ~ id, ncol = 1) +
  theme_classic() +
  ggtitle(str_c("Taxa: ", taxaoi, "; Site: ", siteoi))

# check evi of different species
siteoi <- "DT"
taxaoi_short <- "Quercus"
df_ps <- read_rds(str_c(path_ps, "ts/ps_", siteoi, "_", taxaoi_short, ".rds"))
set.seed(1)
v_id <- df_ps %>%
  pull(id) %>%
  sample(min(1000, length(.)))

df_ps_proc <- process_ps(df_ps %>% filter(id %in% v_id))
p_ps_species <- df_ps_proc %>%
  left_join(
    df_plant %>%
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
  ungroup() %>%
  ggplot() +
  geom_line(aes(x = doy, y = median, col = species), alpha = 1) +
  geom_ribbon(aes(x = doy, ymin = lower, ymax = upper, fill = species), alpha = 0.1) +
  # geom_vline(xintercept = 125)+
  theme_classic() +
  ylab("evi") +
  guides(fill = "none")



# check evi of different genus/family
siteoi <- "DT"
ls_df_ps_taxa <- vector(mode = "list")
for (taxaoi_short in v_taxa_short %>% unique()) {
  file <- str_c(path_ps, "ts/ps_", siteoi, "_", taxaoi_short, ".rds")
  if (file.exists(file)) {
    df_ps <- read_rds(file)
    set.seed(1)
    v_id <- df_ps %>%
      pull(id) %>%
      sample(min(100, length(.)))

    df_ps_proc <- process_ps(df_ps %>% filter(id %in% v_id))

    ls_df_ps_taxa[[taxaoi_short]] <- df_ps_proc %>% mutate(taxa = taxaoi_short)
  }
}
df_ps_alltaxa <- bind_rows(ls_df_ps_taxa)

p_ps_taxa <- ggplot(df_ps_alltaxa %>%
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
