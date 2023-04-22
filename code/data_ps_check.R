# check reflectance
siteoi <- "DT"
taxaoi <- "Quercus"
set.seed(1)
p_ps_ref <- read_rds(str_c(.path$ps, "ts_main/ps_", siteoi, "_", taxaoi, ".rds")) %>%
  filter(id %in% ((.) %>% pull(id) %>% sample(4))) %>% # four random trees
  filter(clear == 1, snow == 0, shadow == 0, haze_light == 0, haze_heavy == 0, cloud == 0, confidence >= 80) %>%
  select(id, time, lon, lat, blue, green, red, nir) %>%
  gather(key = "band", value = "value", -id, -time, -lon, -lat) %>%
  ggplot() +
  geom_line(aes(x = time, y = value, col = band), alpha = 0.5) +
  facet_wrap(. ~ id, ncol = 1) +
  theme_classic() +
  ggtitle(str_c("Taxa: ", taxaoi, "; Site: ", siteoi))
