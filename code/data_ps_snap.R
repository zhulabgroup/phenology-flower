file_eg <- list.files(.path$dat_other, pattern = "harmonized", full.names = T)[1]
ras_eg <- terra::rast(file_eg)

bbox <- c(xmin = 322000, xmax = 324000, ymin = 4695000, ymax = 4697000)

ras_eg_crop <- ras_eg %>% terra::crop(terra::ext(bbox))
# terra::plot(ras_eg_crop[[1]])

df_tree_eg <- df_tree %>%
  left_join(genus_to_family, by = "genus") %>%
  filter(site == "DT") %>%
  drop_na(lon, lat) %>%
  mutate(taxa = case_when(
    genus %in% v_taxa_short ~ genus,
    family %in% v_taxa_short ~ family
  )) %>%
  mutate(taxa_parse = case_when(
    !taxa %in% c("Cupressaceae", "Pinaceae", "Poaceae") ~ str_c("*", taxa, "*"),
    TRUE ~ taxa
  )) %>%
  drop_na(taxa)

sf_tree_eg <- sf::st_as_sf(df_tree_eg,
  coords = c("lon", "lat"),
  crs = sf::st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
)

sf_tree_eg_reproj <- sf::st_transform(sf_tree_eg,
  crs = sf::st_crs(ras_eg)
) %>%
  select(geometry, taxa, taxa_parse)

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
  geom_sf(data = sf_tree_eg_crop %>%
    mutate(taxa = factor(taxa, levels = v_taxa_chron %>% str_split(" ", simplify = T) %>% as.data.frame() %>% pull(V1) %>% unique())), aes(col = taxa), pch = 1) +
  theme_void() +
  scale_fill_identity() +
  theme(legend.text = element_text(face = "italic")) +
  labs(col = "Genus") +
  # scale_color_discrete(
  #   "Taxa",
  #   breaks = sf_tree_eg_crop %>% distinct(taxa, taxa_parse) %>% arrange(taxa) %>% pull(taxa),
  #   labels = sf_tree_eg_crop %>% distinct(taxa, taxa_parse) %>% arrange(taxa) %>% pull(taxa_parse)
  # ) +
  theme(legend.text = ggtext::element_markdown())
