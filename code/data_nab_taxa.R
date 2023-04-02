df_nab <- read_rds("data/nab/clean/dat_20230327.rds")

## get all distinct taxa and clean up their names
df_nab_taxa <- df_nab %>%
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
df_resolve <- df_nab_taxa %>%
  pull(taxa_clean) %>%
  taxize::gnr_resolve(data_source_ids = c(4, 11), with_context = T, best_match_only = T, fields = "all") %>% # NCBI and GBIF databases
  full_join(df_nab_taxa,
    by = c("user_supplied_name" = "taxa_clean")
  ) %>%
  rename(taxa_clean = user_supplied_name) %>%
  mutate(same = (taxa_clean == matched_name)) # check if all taxa names are valid

# some taxa are incorrectly identified as Metazoa. Resolve again for those.
df_correct <- df_resolve %>%
  filter(str_detect(classification_path, "Metazoa")) %>%
  pull(taxa_clean) %>%
  taxize::gnr_resolve(data_source_ids = c(4, 11), best_match_only = F, fields = "all") %>% # NCBI and GBIF. Keep all matches here, not only the best match.
  filter(!str_detect(classification_path, "Animalia")) %>%
  filter(!str_detect(classification_path, "Metazoa")) %>%
  rename(taxa_clean = user_supplied_name) %>%
  group_by(taxa_clean) %>%
  slice(1) %>%
  ungroup()

# Correct previously resolved taxonomy
df_resolve <- df_resolve %>%
  filter(!str_detect(classification_path, "Metazoa")) %>%
  bind_rows(df_correct) %>%
  arrange(taxa_clean)

# make sure all taxa are resolved
# resolve_df %>% filter(!same) %>% View()

# get full classification
df_taxa_id <- df_resolve %>%
  dplyr::select(taxa_clean, data_source_id, taxon_id) %>%
  distinct(taxa_clean, .keep_all = T) %>%
  mutate(data_source = case_when(
    data_source_id == 4 ~ "ncbi",
    data_source_id == 11 ~ "gbif"
  ))

ls_df_taxa_class <- vector(mode = "list", length = nrow(df_taxa_id))
for (i in 1:nrow(df_taxa_id)) {
  ls_df_taxa_class[[i]] <-
    taxize::classification(df_taxa_id$taxon_id[i], db = df_taxa_id$data_source[i])[[1]] %>%
    as_tibble() %>%
    filter(rank %in% c("kingdom", "phylum", "class", "order", "family", "genus", "species")) %>%
    mutate(rank = factor(rank, levels = c("kingdom", "phylum", "class", "order", "family", "genus", "species"))) %>%
    dplyr::select(-id) %>%
    spread(key = "rank", value = "name") %>%
    mutate(taxa_clean = df_taxa_id$taxa_clean[i])
}
df_taxa_class <- bind_rows(ls_df_taxa_class)

df_nab_taxa_full <- df_nab_taxa %>%
  left_join(df_taxa_class, by = "taxa_clean") %>%
  mutate(kingdom = str_replace(kingdom, "Plantae", "Viridiplantae")) %>% # manual correction
  mutate(kingdom = case_when(
    phylum == "Oomycota" ~ "Chromista",
    TRUE ~ kingdom
  ))

write_rds(df_nab_taxa_full, "/data/nab/clean/taxonomy.rds")
