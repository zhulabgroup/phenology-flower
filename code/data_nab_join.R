nab_df <- read_rds("./data/nab/clean/nab_dat_20230327.rds")
nab_taxa_df <- read_rds("./data/nab/clean/nab_taxa.rds")

nab_with_taxa_df <- nab_df %>%
  rename(taxa_raw = taxa) %>%
  left_join(nab_taxa_df, by = "taxa_raw") %>%
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