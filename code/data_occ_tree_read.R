cl <- makeCluster(length(site_list), outfile = "")
registerDoSNOW(cl)

trees_df_list <- foreach(
  site = site_list,
  .packages = c("tidyverse", "rgdal", "sf")
) %dopar% {
  if (site == "KC" | site == "SL") {
    # Data from urban FIA
    # Download https://experience.arcgis.com/experience/3641cea45d614ab88791aef54f3a1849/page/Urban-Datamart/
    # Manual: https://www.fia.fs.fed.us/library/database-documentation/urban/dbDescription/Urban_FIADB_User_Guides_Database_Description_ver3-0_2021_03_17.pdf

    # tree table
    id_tree_df <- read_csv("./data/occurrence/StreetTrees/UrbanFIA/ID_TREE.csv") %>%
      filter(statecd == 29) %>% # Missouri
      filter(countycd %in% case_when(site == "KC" ~ 95, site == "SL" ~ c(189, 510))) %>%
      dplyr::select(plotid, id = cn, spcd, statuscd) %>%
      group_by(id) %>%
      filter(sum(statuscd != 1) == 0) %>% # living trees
      ungroup() %>%
      distinct(id, .keep_all = T) %>% # in case one tree is surveyed repeatedly
      dplyr::select(-statuscd)

    # plot table
    id_plot_df <- read_csv("./data/occurrence/StreetTrees/UrbanFIA/ID_PLOT.csv") %>%
      dplyr::select(plotid, lat, lon)

    # species reference table
    ref_sp_df <- read_csv("./data/occurrence/StreetTrees/UrbanFIA/REF_SPECIES.csv") %>%
      dplyr::select(spcd, genus, species) %>%
      mutate(species = paste(genus, species))

    # join tables
    trees_df <- id_tree_df %>%
      left_join(id_plot_df, by = "plotid") %>%
      left_join(ref_sp_df, by = "spcd") %>%
      dplyr::select(-plotid, -spcd) %>%
      mutate(site = site)
  }
  if (site == "DT") {
    # Data requested from DT P&R, shared by Dan Katz
    trees_df1 <- st_read(dsn = "./data/occurrence/StreetTrees/Tree_Inventory_Detroit/dvy46298.shp") %>%
      st_set_crs(st_crs("+proj=lcc +lat_0=41.5 +lon_0=-84.3666666666667 +lat_1=42.1 +lat_2=43.6666666666667 +x_0=4000000 +y_0=0 +ellps=GRS80 +units=ft +no_defs")) %>%
      st_transform(st_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>%
      cbind(st_coordinates(.)) %>%
      distinct(id = ID, species = SPP, lon = X, lat = Y) %>%
      mutate(site = site)
    # ESRI:102290: NAD 1983 HARN StatePlane Michigan South FIPS 2113
    # https://spatialreference.org/ref/?search=michigan

    # Data from Dan Katz
    trees_df2 <- read_csv("./data/Detroit_oak_pheno_obs_spring_2017.csv") %>%
      distinct(id = tree, species = Species, x, y) %>%
      mutate(comment = "DK")
    # reproject to WGS84
    pts <- SpatialPoints(trees_df2[, c("x", "y")],
      proj4string = CRS("+init=EPSG:3857")
    )
    # +proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs
    pts_reproj <- spTransform(
      pts,
      CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    )
    trees_df2 <- cbind(trees_df2 %>% dplyr::select(-x, -y), coordinates(pts_reproj)) %>%
      rename(lat = y, lon = x) %>%
      as_tibble() %>%
      mutate(site = site)

    trees_df <- bind_rows(trees_df1, trees_df2)
  }
  if (site == "DV") {
    # Data from OpenTrees.org
    trees_df <- read_csv("./data/occurrence/StreetTrees/Tree_Inventory_Denver.csv") %>%
      dplyr::select(id = SITE_ID, species = SPECIES_BO, time = INVENTORY_DATE, lat = Y_LAT, lon = X_LONG) %>%
      arrange(desc(time)) %>%
      distinct(id, species, .keep_all = T) %>%
      dplyr::select(-time) %>%
      filter(lon != 0) %>%
      mutate(site = site)
  }
  if (site == "TP") {
    # Data from https://www.opentreemap.org/tampa/map/
    trees_df <- read_csv("./data/occurrence/StreetTrees/Tree_Inventory_Tampa.csv") %>%
      dplyr::select(id = `Tree Id`, genus = Genus, species = Species, lat = `Point Y`, lon = `Point X`) %>% # checked that there is no repeated tree id
      mutate(species = paste0(genus, " ", species)) %>%
      dplyr::select(-genus) %>%
      mutate(site = site)
  }
  if (site == "HT") {
    # Data from  https://koordinates.com/layer/25245-houston-texas-street-tree-inventory/data/
    trees_df <- read_csv("./data/occurrence/StreetTrees/Tree_Inventory_Houston/houston-texas-street-tree-inventory.csv") %>%
      dplyr::select(species_common = Species, X = Shape_X, Y = Shape_Y) %>%
      rowwise() %>%
      mutate(common = case_when(
        str_detect(species_common, ", spp.") ~ str_replace(species_common, ", spp.", ""), # for coarse common names, e.g., "Oak, spp."
        (str_detect(species_common, ", ") & !str_detect(species_common, ", spp.")) ~ paste0(str_split(species_common, ", ")[[1]][2], " ", str_split(species_common, ", ")[[1]][1]), # reverse order of common name, e.g., "Oak, Water"
        TRUE ~ species_common
      )) %>%
      ungroup() %>%
      distinct(X, Y, .keep_all = T) %>% # in case same tree is sampled repeatedly
      mutate(id = row_number())

    # reproject to WGS84
    pts <- SpatialPoints(trees_df[, c("X", "Y")],
      proj4string = CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs")
    )
    pts_reproj <- spTransform(
      pts,
      CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    )

    trees_df <- cbind(trees_df %>% dplyr::select(-X, -Y), coordinates(pts_reproj)) %>%
      rename(lat = Y, lon = X)

    # Use taxize to match common name with scientific name
    if (!file.exists("./data/occurrence/StreetTrees/Tree_Inventory_Houston/species_reference.csv")) {
      id2comm <- function(pageid) {
        common_matches <- eol_pages(taxonconceptID = pageid, common_names = TRUE)$vernacular
        if (is.null(common_matches)) {
          common_match <- ""
        } else {
          common_matches <- common_matches %>% filter(language == "en")
          if (nrow(common_matches) > 0) {
            common_match <- common_matches %>%
              pull(vernacularname) %>%
              table() %>%
              sort(decreasing = TRUE) %>%
              head(1) %>%
              names()
          }
        }
        return(common_match)
      }

      comm2sci_new <- function(common) {
        species <- comm2sci(common, simplify = T)[[1]]
        if (length(species) == 0) {
          res <- comm2sci(common, db = "eol", simplify = F)[[1]]
          if (length(res) == 0) {
            species <- NA
          } else {
            species <- res %>%
              as_tibble() %>%
              rowwise() %>%
              mutate(common_match = id2comm(pageid)) %>%
              ungroup() %>%
              stringdist_inner_join(data.frame(common_name = common), by = c("common_match" = "common_name"), max_dist = 20, distance_col = "distance") %>%
              arrange(distance, pageid) %>%
              head(1) %>%
              filter(common_match != "") %>%
              pull(name)
            if (length(species) == 0) {
              species <- NA
            }
          }
          print(paste(common, species))
        }

        if (is.na(species)) {
          species <- readline(prompt = paste0(species_df$common[i], ". Manually enter scientific name: "))
        }

        return(species)
      }

      species_df <- trees_df %>%
        distinct(common) %>%
        rowwise() %>%
        mutate(species = comm2sci_new(common))
      write_csv(species_df, "./data/occurrence/StreetTrees/Tree_Inventory_Houston/species_reference.csv")
    } else {
      species_df <- read_csv("./data/occurrence/StreetTrees/Tree_Inventory_Houston/species_reference.csv")
    }

    trees_df <- trees_df %>%
      left_join(species_df, by = "common") %>%
      dplyr::select(-species_common, -common) %>%
      mutate(site = site)
  }
  if (site == "NY") {
    trees_df <- read_csv("./data/occurrence/StreetTrees/Tree_Inventory_NewYork.csv") %>%
      dplyr::select(id = tree_id, species = spc_latin, lat = latitude, lon = longitude) %>% # checked that there is no repeated tree id
      mutate(site = site)
  }
  if (site == "AT") {
    trees_df <- read_csv("./data/occurrence/StreetTrees/Tree_Inventory_Austin/Tree_Inventory_Austin.csv") %>%
      dplyr::select(id = OBJECTID, species = SPECIES, coordinates = the_geom) %>% # checked that there is no repeated tree id
      mutate(coordinates = str_replace(coordinates, "POINT \\(", "")) %>%
      mutate(coordinates = str_replace(coordinates, "\\)", "")) %>%
      rowwise() %>%
      mutate(
        lon = str_split(coordinates, pattern = " ", simplify = T)[1],
        lat = str_split(coordinates, pattern = " ", simplify = T)[2]
      ) %>%
      ungroup() %>%
      mutate(
        lon = as.numeric(lon),
        lat = as.numeric(lat)
      ) %>%
      dplyr::select(-coordinates) %>%
      mutate(site = site)
  }
  if (site == "SJ") {
    trees_df <- read_csv("./data/occurrence/StreetTrees/Tree_Inventory_SanJose/Tree_Inventory_SanJose.csv") %>%
      dplyr::select(id = OBJECTID, species = NAMESCIENTIFIC, Y = Y, X = X) # checked that there is no repeated tree id

    # shape <- readOGR(dsn = "/data/ZHULAB/phenology/occurrence/StreetTrees/Tree_Inventory_SanJose/Street_Tree.shp")
    # proj4string(shape)
    projection_sj <- "+proj=lcc +lat_0=36.5 +lon_0=-120.5 +lat_1=38.4333333333333 +lat_2=37.0666666666667 +x_0=2000000.0001016 +y_0=500000.0001016 +datum=NAD83 +units=us-ft +no_defs"
    pts <- SpatialPoints(trees_df[, c("X", "Y")],
      proj4string = CRS(projection_sj)
    )
    pts_reproj <- spTransform(
      pts,
      CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
    )

    trees_df <- cbind(trees_df %>% dplyr::select(-X, -Y), coordinates(pts_reproj)) %>%
      rename(lat = Y, lon = X) %>%
      mutate(site = site)
  }
  if (site == "ST") {
    trees_df <- read_csv("./data/occurrence/StreetTrees/Tree_Inventory_Seattle.csv") %>%
      dplyr::select(id = OBJECTID, species = SCIENTIFIC_NAME, lat = Y, lon = X) %>% # checked that there is no repeated tree id
      mutate(site = site)
  }
  trees_df
}

stopCluster(cl)

trees_df <- bind_rows(trees_df_list) %>%
  rowwise() %>%
  mutate(genus = str_split(species, pattern = " ", simplify = T)[1]) %>% # get genus name from species name
  ungroup() %>%
  distinct(id, site, .keep_all = T) %>% # in case one tree is surveyed repeatedly
  mutate(genus_id = as.integer(as.factor(genus)))

write_rds(trees_df, "./data/occurrence/street_trees_20230327.rds")
