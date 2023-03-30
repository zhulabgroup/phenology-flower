ras_eg <- terra::rast("data/PS/DT/DT_2017_121_151/20170508_153427_1008_3B_AnalyticMS_SR_clip.tif")
# terra::plot(ras_eg_crop[[1]])
ras_crop_eg <- ras_eg %>% terra::crop(terra::ext(c(xmin = 320000, xmax = 325000, ymin = 4695000, ymax = 4697000)))
# xmin = 320000, xmax = 320500, ymin = 4695000, ymax = 4695500

plant_df_eg <- plant_df %>%
  filter(site == "DT") %>%
  # filter(genus == "Quercus") %>%
  drop_na(lon, lat)

plant_sf_eg <- sf::st_as_sf(plant_df_eg,
  coords = c("lon", "lat"),
  crs = sf::st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
)

plant_sf_reproj_eg <- sf::st_transform(plant_sf_eg,
  crs = sf::st_crs(ras_eg)
) %>%
  select(genus, geometry) %>%
  mutate(genus = as.factor(genus))

plant_sf_crop_eg <- sf::st_crop(
  plant_sf_reproj_eg,
  sf::st_bbox(c(xmin = 320000, xmax = 325000, ymin = 4695000, ymax = 4697000))
)

# plot(plant_sf_crop_eg , add=T)


ras_crop_df <- ras_crop_eg %>%
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


p_ps_snap <- ggplot(data = ras_crop_df) +
  geom_tile(aes(x = x, y = y, fill = rgb), col = NA) +
  geom_sf(data = plant_sf_crop_eg, aes(col = genus), pch = 1) +
  theme_void() +
  scale_fill_identity() +
  guides(col = "none")
