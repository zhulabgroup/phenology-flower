nab_path <- "/data/ZHULAB/phenology/nab/2021-10-04/"

# read in station coordinates
station_df <- read_csv("/data/ZHULAB/phenology/nab/NAB stations.csv") %>% # this csv is manually typed
  mutate(id = row_number())

file_list <- list.files(path = nab_path, pattern = ".xlsx", full.names = T)
nab_df_list <- vector(mode = "list", length = length(file_list))
for (i in 1:length(file_list)) {
  file <- file_list[i]
  
  # read in all data
  dat <- read_excel(
    file,
    col_names = F
  )
  start_n <- which(dat[, 1] == "Date") # excel files have headings of different number of rows. Search for the row(s) that starts with "Date" as the start of data table(s).
  
  # read in meta data
  if (min(start_n) == 1) { # Some excel files have no meta data before data table
    meta_dat <- NA
    station <- NA
  } else { # If there are meta data before data table, read it as a vector
    meta_dat <- read_excel(
      file,
      col_names = F,
      n_max = start_n[1] - 1
    ) %>% pull()
    station <- meta_dat[2] %>%
      strsplit(split = ": ") %>%
      unlist() %>%
      tail(1) # extract station name from meta data vector
  }
  
  location <- file %>%
    strsplit(split = c("/")) %>%
    unlist() %>%
    tail(1) %>%
    strsplit(split = " \\(") %>%
    unlist() %>%
    head(1) # city name extracted from file name
  
  # get coordinates of station
  if (!is.na(station)) { # use both city and station name
    station_info <- data.frame(station = station, location = location) %>%
      stringdist_inner_join(station_df, by = c("location", "station"), max_dist = 20, distance_col = "distance") %>% # join with all entries in station info, measuring dissimilarity in station and location, because their names might be slightly different in both tables
      arrange(station.distance, location.distance) %>%
      head(1) %>% # there might be several close matches. take the closest.
      rename(station = station.y) %>%
      rename(location = location.y) %>%
      dplyr::select(-station.x, -location.x, -station.distance, -location.distance, -distance)
  } else { # use only city name if station name is not available
    station_info <- data.frame(location = location) %>%
      stringdist_inner_join(station_df, by = "location", max_dist = 20, distance_col = "distance") %>%
      arrange(distance) %>%
      head(1) %>%
      rename(location = location.y) %>%
      dplyr::select(-location.x, -distance)
  }
  
  # Extract pollen and spore data table
  if (length(start_n) == 1) { # only pollen, no spores
    colnum <- ncol(read_excel(
      file,
      skip = start_n[1] - 1
    )) # find number of columns
    
    pollen_dat <- read_excel(
      file,
      skip = start_n[1] - 1,
      col_types = c(
        "date",
        rep("numeric", colnum - 1)
      )
    )
    if (is.na(pollen_dat[2, 1])) { # meaning finer taxonomic resolutions available
      genus_names <- read_excel(
        file,
        skip = start_n[1] - 1
      ) %>%
        slice(1) %>% # read the first row with finer taxonomic resolution
        gather(key = "old", value = "new") %>% # coarse and fine taxonomy into long format
        mutate(new = case_when(
          is.na(new) ~ old,
          TRUE ~ new
        )) # sometimes only coarse taxonomy is present, then keep that
      
      pollen_dat <- read_excel(
        file,
        skip = start_n[1] - 1,
        col_types = c(
          "date",
          rep("numeric", colnum - 1)
        ),
        col_names = genus_names$new # change to new colnames with finer taxonomic resolution
      )
    }
    pollen_dat <- pollen_dat %>%
      filter(!is.na(Date)) %>% # filter out rows that are not count data
      gather(key = "taxa", value = "count", -Date) %>% # to long format
      filter(!str_detect(taxa, "Station")) %>% # filter out additional info that are not count data, like "Station Name", "Station Postal Code", "Station State"
      filter(!taxa %in% c("Comment", "WeatherNotes")) %>%
      mutate(group = "pollen") %>%
      rowwise() %>%
      group_by(Date, taxa, group) %>%
      summarize(count = sum(count)) %>%
      ungroup()
    
    # combine with station info
    nab_df_list[[i]] <- pollen_dat %>%
      cbind(station_info)
  }
  
  if (length(start_n) == 2) { # both pollen and spores
    colnum_pollen <- ncol(read_excel(
      file,
      skip = start_n[1] - 1,
      n_max = (start_n[2] - 2) - (start_n[1] + 1)
    ))
    
    pollen_dat <- read_excel(
      file,
      skip = start_n[1] - 1,
      n_max = (start_n[2] - 2) - (start_n[1] + 1),
      col_types = c(
        "date",
        rep("numeric", colnum_pollen - 1)
      )
    )
    
    if (is.na(pollen_dat[2, 1])) { # meaning finer taxonomic resolutions available
      genus_names <- read_excel(
        file,
        skip = start_n[1] - 1,
        n_max = (start_n[2] - 2) - (start_n[1] + 1)
      ) %>%
        slice(1) %>%
        gather(key = "old", value = "new") %>%
        mutate(new = case_when(
          is.na(new) ~ old,
          TRUE ~ new
        ))
      
      pollen_dat <- read_excel(
        file,
        skip = start_n[1] - 1,
        n_max = (start_n[2] - 2) - (start_n[1] + 1),
        col_types = c(
          "date",
          rep("numeric", colnum_pollen - 1)
        ),
        col_names = genus_names$new
      )
    }
    pollen_dat <- pollen_dat %>%
      filter(!is.na(Date)) %>%
      gather(key = "taxa", value = "count", -Date) %>%
      filter(!str_detect(taxa, "Station")) %>%
      filter(!taxa %in% c("Comment", "WeatherNotes")) %>%
      mutate(group = "pollen") %>%
      rowwise() %>%
      group_by(Date, taxa, group) %>%
      summarize(count = sum(count)) %>%
      ungroup()
    
    colname_spore <- ncol(read_excel(
      file,
      skip = start_n[2] - 1
    ))
    spore_dat <- read_excel(
      file,
      skip = start_n[2] - 1,
      col_types = c(
        "date",
        rep("numeric", colname_spore - 1)
      )
    ) %>%
      filter(!is.na(Date)) %>%
      gather(key = "taxa", value = "count", -Date) %>%
      filter(!str_detect(taxa, "Station")) %>%
      filter(!taxa %in% c("Comment", "WeatherNotes")) %>%
      mutate(group = "spore")
    
    if (nrow(spore_dat) != 0) {
      nab_df_list[[i]] <- bind_rows(pollen_dat, spore_dat) %>%
        cbind(station_info)
    } else {
      nab_df_list[[i]] <- pollen_dat %>%
        cbind(station_info)
    }
  }
  print(i)
}

nab_df <- bind_rows(nab_df_list) %>%
  as_tibble() %>%
  rename(date = Date)

write_rds(nab_df, file = "/data/ZHULAB/phenology/nab/nab_dat_20230327.rds")