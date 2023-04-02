path_nab <- "data/nab/raw/2021-10-04/"

# read in station coordinates
df_station <- read_csv("data/nab/raw/NAB stations.csv") %>% # this csv is manually typed
  mutate(id = row_number())

file_nab <- list.files(path = path_nab, pattern = ".xlsx", full.names = T)
ls_df_nab <- vector(mode = "list", length = length(file_nab))
for (i in 1:length(file_nab)) {
  file <- file_nab[i]

  # read in all data
  dat <- readxl::read_excel(
    file,
    col_names = F
  )
  start_n <- which(dat[, 1] == "Date") # excel files have headings of different number of rows. Search for the row(s) that starts with "Date" as the start of data table(s).

  # read in meta data
  if (min(start_n) == 1) { # Some excel files have no meta data before data table
    meta_dat <- NA
    station <- NA
  } else { # If there are meta data before data table, read it as a vector
    meta_dat <- readxl::read_excel(
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
      fuzzyjoin::stringdist_inner_join(df_station, by = c("location", "station"), max_dist = 20, distance_col = "distance") %>% # join with all entries in station info, measuring dissimilarity in station and location, because their names might be slightly different in both tables
      arrange(station.distance, location.distance) %>%
      head(1) %>% # there might be several close matches. take the closest.
      rename(station = station.y) %>%
      rename(location = location.y) %>%
      dplyr::select(-station.x, -location.x, -station.distance, -location.distance, -distance)
  } else { # use only city name if station name is not available
    station_info <- data.frame(location = location) %>%
      fuzzyjoin::stringdist_inner_join(df_station, by = "location", max_dist = 20, distance_col = "distance") %>%
      arrange(distance) %>%
      head(1) %>%
      rename(location = location.y) %>%
      dplyr::select(-location.x, -distance)
  }

  # Extract pollen and spore data table
  if (length(start_n) == 1) { # only pollen, no spores
    colnum <- ncol(readxl::read_excel(
      file,
      skip = start_n[1] - 1
    )) # find number of columns

    pollen_dat <- readxl::read_excel(
      file,
      skip = start_n[1] - 1,
      col_types = c(
        "date",
        rep("numeric", colnum - 1)
      )
    )
    if (is.na(pollen_dat[2, 1])) { # meaning finer taxonomic resolutions available
      genus_names <- readxl::read_excel(
        file,
        skip = start_n[1] - 1
      ) %>%
        slice(1) %>% # read the first row with finer taxonomic resolution
        gather(key = "old", value = "new") %>% # coarse and fine taxonomy into long format
        mutate(new = case_when(
          is.na(new) ~ old,
          TRUE ~ new
        )) # sometimes only coarse taxonomy is present, then keep that

      pollen_dat <- readxl::read_excel(
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
    ls_df_nab[[i]] <- pollen_dat %>%
      cbind(station_info)
  }

  if (length(start_n) == 2) { # both pollen and spores
    colnum_pollen <- ncol(readxl::read_excel(
      file,
      skip = start_n[1] - 1,
      n_max = (start_n[2] - 2) - (start_n[1] + 1)
    ))

    pollen_dat <- readxl::read_excel(
      file,
      skip = start_n[1] - 1,
      n_max = (start_n[2] - 2) - (start_n[1] + 1),
      col_types = c(
        "date",
        rep("numeric", colnum_pollen - 1)
      )
    )

    if (is.na(pollen_dat[2, 1])) { # meaning finer taxonomic resolutions available
      genus_names <- readxl::read_excel(
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

      pollen_dat <- readxl::read_excel(
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

    colname_spore <- ncol(readxl::read_excel(
      file,
      skip = start_n[2] - 1
    ))
    spore_dat <- readxl::read_excel(
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
      ls_df_nab[[i]] <- bind_rows(pollen_dat, spore_dat) %>%
        cbind(station_info)
    } else {
      ls_df_nab[[i]] <- pollen_dat %>%
        cbind(station_info)
    }
  }
  print(i)
}

df_nab <- bind_rows(ls_df_nab) %>%
  as_tibble() %>%
  rename(date = Date) %>%
  mutate(count = abs(count))

write_rds(df_nab, "data/nab/clean/dat_20230327.rds")
