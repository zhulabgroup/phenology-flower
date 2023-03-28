site_vis <- "AT"
taxa_vis <- "Quercus"
set.seed(1)
p_ps_ref <- read_rds(paste0(ps_path, "ts/ps_", site_vis, "_", taxa_vis, ".rds")) %>%
  filter(id %in% ((.) %>% pull(id) %>% sample(4))) %>% # four random trees
  filter(clear > 0.9, snow < 0.1, shadow < 0.1, haze_light < 0.1, haze_heavy < 0.1, cloud < 0.1, confidence >= 50) %>%
  select(id, time, lon, lat, blue, green, red, nir) %>%
  gather(key = "band", value = "value", -id, -time, -lon, -lat) %>%
  ggplot() +
  geom_point(aes(x = time, y = value, col = band), alpha = 0.25) +
  facet_wrap(. ~ id, ncol = 1) +
  theme_classic() +
  ggtitle(paste0("Taxa: ", taxa_vis, "; Site: ", site_vis))






siteoi <- "AT"
evi_list <- vector(mode = "list")
for (taxaoi_short in taxa_short_list %>% unique()) {
  file <- str_c(ps_path, "ts/ps_", siteoi, "_", taxaoi_short, ".rds")
  if(file.exists(file)) {
    ps_df <- read_rds(file)
    random_id <- ps_df %>%
      pull(id) %>%
      sample(100)
    ps_df_proc <- ps_df %>%
      drop_na() %>%
      filter(id %in% random_id) %>%
      mutate(date = as.Date(time)) %>%
      mutate(
        year = format(time, "%Y") %>% as.integer(),
        doy = format(time, "%j") %>% as.integer(),
        hour = format(strptime(time, "%Y-%m-%d %H:%M:%S"), "%H") %>% as.integer()
      ) %>%
      filter(clear > 0.9, snow < 0.1, shadow < 0.1, haze_light < 0.1, haze_heavy < 0.1, cloud < 0.1, confidence >= 50) %>%
      group_by(id, lon, lat, date, year, doy) %>%
      summarise(
        blue = mean(blue),
        green = mean(green),
        red = mean(red),
        nir = mean(nir)
      ) %>%
      ungroup() %>%
      mutate(evi = 2.5 * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)) %>%
      filter(evi > 0, evi <= 1) %>%
      filter(red > 0, green > 0, blue > 0)
    evi_list[[taxaoi_short]] <- ps_df_proc %>% mutate(taxa = taxaoi_short)
  }
  
}
evi_alltaxa_df <- bind_rows(evi_list)

# ggplot(evi_alltaxa_df %>%
#   filter(taxa %in% c("Poaceae", "Quercus", "Cupressaceae")) %>%
#            filter(year == 2019) %>%
#     filter(doy>=80, doy<200)
#   )+
#   geom_line(aes(x=doy, y=evi, group=id, col=taxa), alpha=0.1)+
#   theme_classic()

p_ps_taxa<- ggplot(evi_alltaxa_df %>%
  filter(taxa %in% c("Poaceae", "Quercus", "Cupressaceae")) %>%
  filter(year == 2019) %>%
  filter(doy >= 80, doy < 200) %>%
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
  # facet_wrap(.~taxa, ncol=1)+
  guides(fill = "none")









siteoi <- "AT"
taxaoi_short <- "Quercus"
ps_df <- read_rds(paste0(ps_path, "ts/ps_", siteoi, "_", taxaoi_short, ".rds"))
set.seed(1)
random_id <- ps_df %>%
  pull(id) %>%
  sample(1000)
ps_df_proc <- ps_df %>%
  drop_na() %>%
  filter(id %in% random_id) %>%
  mutate(date = as.Date(time)) %>%
  mutate(
    year = format(time, "%Y") %>% as.integer(),
    doy = format(time, "%j") %>% as.integer(),
    hour = format(strptime(time, "%Y-%m-%d %H:%M:%S"), "%H") %>% as.integer()
  ) %>%
  filter(clear > 0.9, snow < 0.1, shadow < 0.1, haze_light < 0.1, haze_heavy < 0.1, cloud < 0.1, confidence >= 50) %>%
  # select(id, time, lon, lat, blue, green, red, nir) %>%
  group_by(id, lon, lat, date, year, doy) %>%
  summarise(
    blue = mean(blue),
    green = mean(green),
    red = mean(red),
    nir = mean(nir)
  ) %>%
  ungroup() %>%
  mutate(evi = 2.5 * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)) %>%
  filter(evi > 0, evi <= 1) %>%
  filter(red > 0, green > 0, blue > 0)

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
  filter(species %in% c("Quercus virginiana", "Quercus fusiformis", "Quercus shumardii")) %>%
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
  # facet_wrap(.~taxa, ncol=1)+
  guides(fill = "none")
