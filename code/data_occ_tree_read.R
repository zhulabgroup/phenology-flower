if (.full_data) {
  df_tree <- read_rds(str_c(.path$input, "tree/df_tree.rds")) %>%
    rowwise() %>%
    mutate(genus = str_split(taxa, pattern = " ", simplify = T)[1]) %>% # get genus name from species name
    ungroup() %>%
    distinct(id, site, .keep_all = T)

  df_tree %>%
    filter(site %in% c("DT")) %>%
    left_join(genus_to_family, by = "genus") %>%
    filter(genus %in% v_taxa_short | family %in% v_taxa_short) %>%
    mutate(taxa = case_when(
      family %in% c("Poaceae", "Cupressaceae", "Pinaceae") ~ family,
      TRUE ~ genus
    )) %>%
    select(-id) %>%
    write_rds(str_c(.path$input, "tree/df_tree_sample.rds"))
} else {
  df_tree_sample <- read_rds(str_c(.path$input, "tree/df_tree_sample.rds"))
}
