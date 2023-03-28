trees_df <- read_rds("./data/occurrence/street_trees_20230327.rds") %>%
  left_join(read_rds("./data/occurrence/genus_to_family.rds"), by = "genus")
grass_df <- read_rds("./data/occurrence/grass.rds")
ragweed_df <- read_rds("./data/occurrence/ragweed.rds")
plant_df <- bind_rows(trees_df, grass_df, ragweed_df)
