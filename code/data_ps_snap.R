file_eg <- list.files(str_c(.path$input, "ps/urban/"), pattern = "harmonized", full.names = T)[1]
ras_eg <- terra::rast(file_eg)

bbox <- c(xmin = 322000, xmax = 324000, ymin = 4695000, ymax = 4697000)

ras_eg_crop <- ras_eg %>% terra::crop(terra::ext(bbox))

# get extent in lon lat
ras_eg_crop %>%
  terra::project("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  terra::ext()

if (.full_data) {
  df_tree_subset <- df_tree %>%
    left_join(genus_to_family, by = "genus") %>%
    filter(site %in% siteoi) %>%
    filter(genus %in% v_taxa_short | family %in% v_taxa_short) %>%
    mutate(taxa = case_when(
      family %in% c("Poaceae", "Cupressaceae", "Pinaceae") ~ family,
      TRUE ~ genus
    ))

  write_rds(df_tree_subset, str_c(.path$intermediate, "tree/df_tree_sample.rds"))
} else {
  df_tree_subset <- read_rds(str_c(.path$intermediate, "tree/df_tree_sample.rds"))
}

sf_tree_eg <- sf::st_as_sf(df_tree_subset,
  coords = c("lon", "lat"),
  crs = sf::st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
)

sf_tree_eg_reproj <- sf::st_transform(sf_tree_eg,
  crs = sf::st_crs(ras_eg)
) %>%
  select(geometry, taxa)

sf_tree_eg_crop <- sf::st_crop(
  sf_tree_eg_reproj,
  sf::st_bbox(bbox)
)

df_ras_eg_crop <- ras_eg_crop %>%
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
    b = b * 4,
    g = g * 4,
    r = r * 4,
  ) %>%
  filter(
    b <= 1,
    g <= 1,
    r <= 1
  ) %>%
  mutate(rgb = rgb(r, g, b, maxColorValue = 1))

p_ps_snap <- ggplot(data = df_ras_eg_crop) +
  geom_tile(aes(x = x, y = y, fill = rgb), col = NA) +
  geom_sf(data = sf_tree_eg_crop, aes(col = taxa), pch = 1) +
  theme_void() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_identity() +
  theme(legend.text = element_text(face = "italic")) +
  labs(col = "Genus") +
  coord_sf() +
  ggspatial::annotation_scale(
    location = "bl", style = "ticks",
    line_col = "white", text_col = "white",
  )

# save figure
if (.fig_save) {
  ggsave(
    plot = p_ps_snap,
    filename = str_c(.path$output, "main/main_ps_snap.pdf"),
    width = 7,
    height = 5,
    device = pdf
  )
}
