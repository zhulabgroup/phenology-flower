df_tree <- read_rds("./data/occurrence/street_trees_20230327.rds") %>%
  left_join(read_rds("./data/occurrence/genus_to_family.rds"), by = "genus")
df_grass <- read_rds("./data/occurrence/grass.rds")
df_ragweed <- read_rds("./data/occurrence/ragweed.rds")
df_plant <- bind_rows(df_tree, df_grass, df_ragweed)
