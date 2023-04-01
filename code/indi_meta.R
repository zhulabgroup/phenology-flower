taxaoi <- "Quercus"
siteoi <- "DT"
yearoi <- 2017

taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
flower_window <- seq(flower_window_df %>% filter(taxa == taxaoi) %>% pull(start),
  flower_window_df %>% filter(taxa == taxaoi) %>% pull(end),
  by = 1
)
thres_df_taxa <- thres_df %>% filter(direction == "up")

df_dt_meta <- plant_df %>%
  filter(site == siteoi) %>%
  filter(genus == taxaoi_short | family == taxaoi_short) %>%
  mutate(id_ps = row_number()) %>%
  drop_na(lon, lat) %>%
  filter(!is.na(comment))

p_dt_map <- df_dt_meta %>%
  ggplot() +
  geom_point(aes(x = lon, y = lat, col = species)) +
  theme_classic()
