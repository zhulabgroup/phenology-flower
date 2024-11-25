v_taxa <- c(
  "Quercus",
  # "Cupressaceae",
  # "Ambrosia",
  "Morus",
  # "Pinaceae",
  "Ulmus early",
  "Ulmus late",
  "Fraxinus",
  "Betula",
  # "Poaceae early",
  # "Poaceae late",
  "Acer",
  "Populus",
  "Juglans",
  "Platanus",
  "Alnus",
  "Carya",
  "Liquidambar",
  "Salix",
  "Celtis"
)
v_taxa_short <- str_split(v_taxa, pattern = " ", simplify = T)[, 1]
# taxa in chronological order
# v_taxa_chron <- c( # "Cupressaceae",
#   "Fraxinus",
#   "Ulmus early",
#   # "Pinaceae",
#   "Acer",
#   "Populus",
#   "Quercus",
#   "Betula",
#   "Morus",
#   # "Poaceae early",
#   # "Poaceae late",
#   "Ulmus late" ,
#   # "Ambrosia"
# )

v_site <- c(
  "NY",
  # "SJ",
  "AT", "ST", "HT", "TP", "DT", "DV"
)
v_site_name <- c(
  "New York",
  # "San Jose",
  "Austin", "Seattle", "Houston", "Tampa", "Detroit", "Denver"
)

v_site_tune <- c(
  "NY", "AT", "ST", "HT", "TP", "DT", "DV"
)

v_year <- 2018:2022
