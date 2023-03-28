nab_df <- read_rds("/data/ZHULAB/phenology/nab/nab_dat_20230327.rds")

## get all distinct taxa and clean up their names
nab_taxa_df <- nab_df %>%
  distinct(taxa) %>%
  filter(!taxa %in% c("Total Pollen Count", "Total Spore Count")) %>%
  rename(taxa_raw = taxa) %>%
  rowwise() %>%
  mutate(taxa_clean = str_split(taxa_raw, pattern = "/", simplify = T)[1]) %>% # some taxa have alternative names, e.g., Chenopodiaceae/Amaranthaceae
  mutate(taxa_clean = str_split(taxa_clean, pattern = " \\(", simplify = T)[1]) %>% # e.g., Asteraceae (Excluding Ambrosia and Artemisia)
  mutate(taxa_clean = str_replace(taxa_clean, pattern = "-type", "")) %>% # e.g., Leptosphaeria-type
  mutate(taxa_clean = str_replace(taxa_clean, pattern = "Unidentified ", "")) %>% # e.g., Unidentified Fungi
  mutate(taxa_clean = str_replace(taxa_clean, pattern = "Undifferentiated ", "")) %>% # e.g., Undifferentiated Ascospores
  mutate(taxa_clean = str_replace(taxa_clean, pattern = "Other ", "")) %>% # e.g., Other Grass Pollen
  mutate(taxa_clean = str_replace(taxa_clean, pattern = "spores", "mycota")) %>% # e.g., Undifferentiated Ascospores
  mutate(taxa_clean = str_replace(taxa_clean, pattern = " ", "")) %>% # remove space
  mutate(taxa_clean = case_when(
    taxa_clean == "Rusts" ~ "Pucciniales",
    taxa_clean == "Smuts" ~ "Myxomycetes",
    taxa_clean == "Dreshslera" ~ "Helminthosporium",
    (taxa_clean == "GrassPollen" | taxa_clean == "WeedPollen") ~ "Poaceae",
    taxa_clean == "TreePollen" ~ "Tracheophyta",
    taxa_clean == "Pollen" ~ "Viridiplantae",
    TRUE ~ taxa_clean
  )) %>% # manual cleaning
  arrange(taxa_clean)

## Use taxize to resolve taxonomy
# Find sources id
# gnr_datasources()

# match with names in databases
resolve_df <- nab_taxa_df %>%
  pull(taxa_clean) %>%
  gnr_resolve(data_source_ids = c(4, 11), with_context = T, best_match_only = T, fields = "all") %>% # NCBI and GBIF databases
  full_join(nab_taxa_df,
            by = c("user_supplied_name" = "taxa_clean")
  ) %>%
  rename(taxa_clean = user_supplied_name) %>%
  mutate(same = (taxa_clean == matched_name)) # check if all taxa names are valid

# some taxa are incorrectly identified as Metazoa. Resolve again for those.
resolve_df_correct <- resolve_df %>%
  filter(str_detect(classification_path, "Metazoa")) %>%
  pull(taxa_clean) %>%
  gnr_resolve(data_source_ids = c(4, 11), best_match_only = F, fields = "all") %>% # NCBI and GBIF. Keep all matches here, not only the best match.
  filter(!str_detect(classification_path, "Animalia")) %>%
  filter(!str_detect(classification_path, "Metazoa")) %>%
  rename(taxa_clean = user_supplied_name) %>%
  group_by(taxa_clean) %>%
  slice(1) %>%
  ungroup()

# Correct previously resolved taxonomy
resolve_df <- resolve_df %>%
  filter(!str_detect(classification_path, "Metazoa")) %>%
  bind_rows(resolve_df_correct) %>%
  arrange(taxa_clean)

# make sure all taxa are resolved
# resolve_df %>% filter(!same) %>% View()

# get full classification
taxa_id_df <- resolve_df %>%
  dplyr::select(taxa_clean, data_source_id, taxon_id) %>%
  distinct(taxa_clean, .keep_all = T) %>%
  mutate(data_source = case_when(
    data_source_id == 4 ~ "ncbi",
    data_source_id == 11 ~ "gbif"
  ))

taxa_class_df <- vector(mode = "list", length = nrow(taxa_id_df))
for (i in 1:nrow(taxa_id_df)) {
  taxa_class_df[[i]] <-
    classification(taxa_id_df$taxon_id[i], db = taxa_id_df$data_source[i])[[1]] %>%
    as_tibble() %>%
    filter(rank %in% c("kingdom", "phylum", "class", "order", "family", "genus", "species")) %>%
    mutate(rank = factor(rank, levels = c("kingdom", "phylum", "class", "order", "family", "genus", "species"))) %>%
    dplyr::select(-id) %>%
    spread(key = "rank", value = "name") %>%
    mutate(taxa_clean = taxa_id_df$taxa_clean[i])
}
taxa_class_df <- bind_rows(taxa_class_df)

nab_taxa_df <- nab_taxa_df %>%
  left_join(taxa_class_df, by = "taxa_clean") %>%
  mutate(kingdom = str_replace(kingdom, "Plantae", "Viridiplantae")) %>% # manual correction
  mutate(kingdom = case_when(
    phylum == "Oomycota" ~ "Chromista",
    TRUE ~ kingdom
  ))

write_rds(nab_taxa_df, "/data/ZHULAB/phenology/nab/nab_taxa.rds")