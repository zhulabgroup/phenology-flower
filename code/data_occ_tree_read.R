df_tree <- read_rds(str_c(.path$occ,"StreetTrees/df_tree.rds")) %>%
  rowwise() %>%
  mutate(genus = str_split(taxa, pattern = " ", simplify = T)[1]) %>% # get genus name from species name
  ungroup() %>%
  distinct(id, site, .keep_all = T) %>% # in case one tree is surveyed repeatedly
  mutate(genus_id = as.integer(as.factor(genus)))