if (.full_data) {
  df_tree <- read_rds(str_c(.path$input, "tree/df_tree.rds")) %>%
    rowwise() %>%
    mutate(genus = str_split(taxa, pattern = " ", simplify = T)[1]) %>% # get genus name from species name
    ungroup() %>%
    distinct(id, site, .keep_all = T)
}
