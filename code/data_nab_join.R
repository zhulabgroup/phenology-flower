df_nab <- read_rds("./data/nab/clean/dat_20230327.rds")
df_nab_taxa <- read_rds("./data/nab/clean/taxonomy.rds")

df_nab_full <- df_nab %>%
  mutate(date = lubridate::date(date)) %>%
  rename(taxa_raw = taxa) %>%
  left_join(df_nab_taxa, by = "taxa_raw") %>%
  rename(taxa = taxa_clean) %>%
  mutate(family = case_when(
    taxa_raw == "Total Pollen Count" ~ "Total",
    TRUE ~ family
  )) %>%
  mutate(genus = case_when(
    taxa_raw == "Total Pollen Count" ~ "Total",
    TRUE ~ genus
  )) %>%
  filter(kingdom == "Viridiplantae" | is.na(kingdom)) %>%
  group_by(date, lat, lon, station, location, id, family, genus, taxa) %>%
  summarise(count = sum(count)) %>%
  ungroup()
