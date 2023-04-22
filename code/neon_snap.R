ras_eg <- terra::rast("data/PS/neon/SJER/SJER_2020_153_182/20200603_162353_0f36_3B_AnalyticMS_SR_clip.tif")
# terra::plot(ras_eg_crop[[1]])

df_plant_eg <- df_plant %>%
  filter(site == "SJER") %>%
  left_join(ls_df_neon_npn$metric %>% distinct(id, functype), by = "id") %>% 
  filter(!functype %in% c("Forb","Graminoid")) %>% 
  drop_na(functype) %>% 
  drop_na(lon, lat)

sf_plant_eg <- sf::st_as_sf(df_plant_eg,
                            coords = c("lon", "lat"),
                            crs = sf::st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
)

sf_plant_eg_reproj <- sf::st_transform(sf_plant_eg,
                                       crs = sf::st_crs(ras_eg)
) %>%
  select( geometry) 



df_ras_eg <- ras_eg %>%
  as.data.frame(xy = T) %>%
  as_tibble() %>%
  select(
    b = blue,
    g = green,
    r = red,
    x,
    y
  ) %>%
  mutate(
    b = b * 0.0001,
    g = g * 0.0001,
    r = r * 0.0001
  ) %>%
  filter(
    b > 0,
    g > 0,
    r > 0
  ) %>%
  mutate(
    b = b * 5,
    g = g * 5,
    r = r * 5,
  ) %>%
  filter(
    b <= 1,
    g <= 1,
    r <= 1
  ) %>%
  mutate(rgb = rgb(r, g, b, maxColorValue = 1))


p_ps_snap <- ggplot(data = df_ras_eg) +
  geom_tile(aes(x = x, y = y, fill = rgb), col = NA) +
  geom_sf(data = sf_plant_eg_reproj, pch = 1) +
  theme_void() +
  scale_fill_identity() +
  guides(col = "none")
