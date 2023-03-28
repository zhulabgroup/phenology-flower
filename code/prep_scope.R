taxa_list <- c("Quercus", "Cupressaceae", "Ambrosia", "Morus", "Pinaceae", "Ulmus early", "Ulmus late", "Fraxinus", "Betula", "Poaceae early", "Poaceae late", "Acer", "Populus")
taxa_short_list <- str_split(taxa_list, pattern = " ", simplify = T)[, 1]

site_list <- c(
  "NY", "SJ", "AT", "ST", "HT", "TP", "DT", "DV" # , "KC", "SL"
)
sitename_list <- c(
  "New York", "San Jose", "Austin", "Seattle", "Houston", "Tampa", "Detroit", "Denver" # , "Kansas City", "St. Louis"
)

year_list <- 2018:2021