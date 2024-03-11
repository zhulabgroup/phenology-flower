file <- str_c(.path$occ,"genus_to_family.rds")
if (!file.exists(file)) {
  keywords <- c("unknown", "vacant", "tbd", "na", "x", "other")
  key_regex <- regex(paste("\\b(?i)", keywords, "\\b", sep = "", collapse = "|"))
  genus_to_family <- df_tree %>%
    distinct(genus) %>%
    mutate(genus = str_replace_all(genus, "[^[:alnum:]]", "")) %>%
    mutate(genus = str_replace_all(genus, key_regex, "")) %>%
    drop_na() %>%
    filter(genus != "") %>%
    mutate(family = NA)
  
  for (i in 1:nrow(genus_to_family)) {
    family <- taxize::tax_name(sci = genus_to_family$genus[i], get = "family", messages = F, db = "ncbi")$family
    genus_to_family$family[i] <- family
    Sys.sleep(0.5)
  }
  
  write_rds(genus_to_family, file)
} else {
  genus_to_family <- read_rds(file)
}


