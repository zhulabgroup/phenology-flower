taxaoi <- "Quercus"
siteoi <- "DT"
yearoi <- 2017

taxaoi_short <- str_split(taxaoi, " ", simplify = T)[1]
df_thres_taxa <- get_thres_taxa(df_thres, taxaoi)

df_dt_meta <- df_plant %>%
  filter(site == siteoi) %>%
  filter(genus == taxaoi_short | family == taxaoi_short) %>%
  mutate(id_ps = row_number()) %>%
  drop_na(lon, lat) %>%
  filter(!is.na(comment))

p_dt_map <- df_dt_meta %>%
  ggplot() +
  geom_point(aes(x = lon, y = lat, col = species)) +
  theme_classic()
