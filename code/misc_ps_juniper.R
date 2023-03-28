library(tidyverse)
library(planetR) # https://github.com/bevingtona/planetR
library(rgdal)
library(lubridate)
library(stringr)
library(httr)
library(raster)
library(parallel)
library(doSNOW)

source("./patch for planetR.R")

# Read in tree file
trees_df <- read_csv("./data/juniper_ashei/tree_dates_coords_220930.csv") %>%
  mutate(id = row_number()) %>%
  distinct(id, species = "Juniper ashei", lon = x, lat = y) %>%
  as_tibble()

write_rds(trees_df, "./data/juniper_ashei/tree_table.rds")
summary(trees_df)


# whole extent is too big so separating into areas
# https://catalog.data.gov/dataset/tiger-line-shapefile-2017-state-texas-current-county-subdivision-state-based
area_sp <- readOGR(dsn = "./data/juniper_ashei/tl_2017_48_cousub/tl_2017_48_cousub.shp")
plot(area_sp)
area_sp_tran <- spTransform(area_sp, CRS("+proj=longlat +datum=WGS84"))

point_sp <- SpatialPoints(trees_df[, c("lon", "lat")],
  proj4string = CRS("+proj=longlat +datum=WGS84")
)

indices <- over(point_sp, area_sp_tran)
trees_df <- trees_df %>%
  mutate(site = indices$NAME)

ggplot(trees_df) +
  geom_path(data = map_data("county"), aes(x = long, y = lat, group = group), color = "grey", alpha = 0.5) +
  geom_point(aes(x = lon, y = lat, col = site), pch = 21, cex = 3) +
  # geom_label(aes(x=lon, y=lat, label=id))+
  theme_classic() +
  ylim(29, 33) +
  xlim(-101, -96.5) +
  coord_equal()

site_list <- trees_df %>%
  pull(site) %>%
  unique() %>%
  sort()
data.frame(site = site_list) %>% View()
# Set API
api_key <- "REMOVED" # nhcarter

# Metadata filters
cloud_lim <- 1 # percent from 0-1
item_name <- "PSScene"
asset <- "ortho_analytic_4b_sr"
product_bundle <- "analytic_sr_udm2"
# (see https://developers.planet.com/docs/data/items-assets/)
# https://developers.planet.com/docs/apis/data/psscene3-4band-deprecation/

ps_path <- "./data/PS/TX_DK/" # change to your path

# order images
for (siteoi in site_list) {
  ps_path_site <- paste0(ps_path, siteoi, "/")
  dir.create(ps_path_site, recursive = T)
  dir.create(paste0(ps_path_site, "orders/"), recursive = T)

  # Set AOI (many ways to set this!) ultimately just need an extent()
  plant_df_site <- trees_df %>%
    filter(site == siteoi) %>%
    drop_na(lon, lat)
  bbox <- extent(min(plant_df_site$lon) - 0.0001, max(plant_df_site$lon) + 0.0001, min(plant_df_site$lat) - 0.0001, max(plant_df_site$lat) + 0.0001)

  for (year_download in 2017:2022) {
    order_df <- data.frame(year = integer(0), month = integer(0), id = character(0), images = integer(0))
    for (month_download in 1:12) {
      # Date range of interest
      start_year <- year_download
      end_year <- year_download
      date_start <- lubridate::floor_date(as.Date(paste0(year_download, "-", str_pad(month_download, 2, pad = "0"), "-01")), unit = "month")
      date_end <- lubridate::ceiling_date(as.Date(paste0(year_download, "-", str_pad(month_download, 2, pad = "0"), "-01")), unit = "month") - 1
      start_doy <- as.numeric(format(date_start, "%j"))
      end_doy <- as.numeric(format(date_end, "%j"))

      # Create order name
      order_name <- paste(siteoi, start_year, start_doy, end_doy, sep = "_")

      # Planet Orders API

      out <- tryCatch(
        {
          images <- planet_search_new(
            api_key = api_key,
            bbox = bbox,
            date_start = date_start,
            date_end = date_end,
            cloud_lim = 1,
            ground_control = T,
            quality = "standard",
            item_name = item_name,
            asset = asset
          )
        },
        error = function(e) {
          return(numeric(0))
        }
      )
      if (length(out) > 0) {
        item_num <- length(images)
        group_num <- ceiling(item_num / 450)
        split_num <- ceiling(31 / group_num)
        date_group <- split(seq(date_start, date_end, by = "day"), floor((seq(1:(end_doy - start_doy + 1)) - 0.0001) / split_num))
        for (g in 1:length(date_group)) {
          orderdone <- F
          while (!orderdone) {
            orderdone <- tryCatch(
              {
                order_id <- planet_order_request_new(
                  api_key = api_key,
                  bbox = bbox,
                  date_start = date_group[[g]] %>% min(),
                  date_end = date_group[[g]] %>% max(),
                  cloud_lim = 1,
                  ground_control = T,
                  quality = "standard",
                  item_name = item_name,
                  product_bundle = product_bundle,
                  asset = asset,
                  order_name = order_name,
                  mostrecent = 0
                )
                orderdone <- T
              },
              error = function(e) {
                Sys.sleep(60)
                print("Sleep for 60 s.")
                return(F)
              }
            )
          }

          if (!is.null(order_id)) {
            order_df <- order_df %>%
              bind_rows(data.frame(year = year_download, month = month_download, id = order_id, images = item_num))
          }
        }
      }




      print(str_c(siteoi, ", ", year_download, ", ", month_download))
    }
    dir.create(paste0(ps_path_site, "orders/"))
    write_rds(order_df, paste0(ps_path_site, "orders/", "order_", year_download, ".rds"))
  }
}

# download
for (siteoi in site_list) {
  ps_path_site <- paste0(ps_path, siteoi, "/")
  for (year_download in 2017:2022) {
    order_df <- read_rds(paste0(ps_path_site, "orders/", "order_", year_download, ".rds"))
    cl <- makeCluster(nrow(order_df), outfile = "")
    registerDoSNOW(cl)
    foreach(
      i = 1:nrow(order_df),
      .packages = c("stringr", "planetR", "httr")
    ) %dopar% {
      # Get order id
      month_download <- order_df$month[i]
      order_id <- order_df$id[i]

      # Date range of interest
      start_year <- year_download
      end_year <- year_download
      date_start <- lubridate::floor_date(as.Date(paste0(year_download, "-", str_pad(month_download, 2, pad = "0"), "-01")), unit = "month")
      date_end <- lubridate::ceiling_date(as.Date(paste0(year_download, "-", str_pad(month_download, 2, pad = "0"), "-01")), unit = "month") - 1
      start_doy <- as.numeric(format(date_start, "%j"))
      end_doy <- as.numeric(format(date_end, "%j"))

      # Set/Create Export Folder
      order_name <- paste(siteoi, start_year, start_doy, end_doy, sep = "_")
      exportfolder <- paste0(ps_path_site, order_name)
      dir.create(exportfolder, recursive = T, showWarnings = F)

      # Download
      Sys.sleep(i * 0.5) # Otherwise sending request to API at the same time may cause error
      orderdone <- F
      while (!orderdone) {
        orderdone <- tryCatch(
          {
            planet_order_download_new(order_id, exportfolder, api_key = api_key, order_num = i, overwrite_opt = FALSE)
            orderdone <- T
          },
          error = function(e) {
            Sys.sleep(10)
            print("Sleep for 10 s.")
            return(F)
          }
        )
      }

      print(str_c(siteoi, ", ", year_download, ", ", month_download))
    }

    stopCluster(cl)
  }
}



cl <- makeCluster(20, outfile = "")
registerDoSNOW(cl)

iscomplete <- F
while (!iscomplete) { # restart when there is error, usually because of cluster connection issues
  iserror <- try(
    for (s in 1:length(site_list)) {
      # get plant locations
      siteoi <- site_list[s]
      trees_site_df <- trees_df %>%
        filter(site == siteoi) %>%
        drop_na(lon, lat)
      id_list <- trees_site_df %>% pull(id)

      # plants as points
      trees_site_sp <- SpatialPoints(trees_site_df[, c("lon", "lat")],
        proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
      )

      if (!file.exists(paste0(ps_path, "analyses/ps_", siteoi, ".rds"))) {
        # read reflectance data
        files <- list.files(path = paste0(ps_path, siteoi), pattern = ".*_SR_clip.tif$", recursive = T, full.names = T) %>% sort()
        nday <- length(files)
        ps_mat <- foreach(
          f = 1:nday,
          .packages = c("raster"),
          .combine = "rbind"
        ) %dopar% {
          file <- files[f]
          ps_st <- stack(file)

          trees_sp_reproj <- spTransform(trees_site_sp, CRSobj = CRS(proj4string(ps_st)))

          ps_values <- cbind(raster::extract(ps_st, trees_sp_reproj), f, id = id_list)
          print(paste0(f, " out of ", nday))
          ps_values[complete.cases(ps_values), ]
        }

        # read quality assessment data
        # 0 - fully usable data
        # other - potentially problematic/unusable data
        #
        # Full description is in Planet's documentation (Page 91, Section 2. UNUSABLE DATA MASK FILE).
        files <- list.files(path = paste0(ps_path, siteoi), pattern = ".*_udm2_clip.tif$", recursive = T, full.names = T) %>% sort()
        nday <- length(files)
        ps_mask_mat <- foreach(
          f = 1:nday,
          .packages = c("raster"),
          .combine = "rbind"
        ) %dopar% {
          file <- files[f]
          ps_ras <- stack(file)

          trees_sp_reproj <- spTransform(trees_site_sp, CRSobj = CRS(proj4string(ps_ras)))

          ps_values <- cbind(qa = raster::extract(ps_ras, trees_sp_reproj), f, id = id_list)

          print(paste0(f, " out of ", nday))
          ps_values[complete.cases(ps_values), ]
        }

        # get corresponding timing from file names
        time_df <- list.files(path = paste0(ps_path, siteoi), pattern = ".*_SR_clip.tif$", recursive = T) %>%
          sort() %>%
          str_split(pattern = "/", simplify = T) %>%
          data.frame() %>%
          dplyr::select(filename = X2) %>%
          rowwise() %>%
          mutate(time = strptime(paste0(str_split(filename, pattern = "_")[[1]][1], str_split(filename, pattern = "_")[[1]][2]), format = "%Y%m%d%H%M%OS")) %>%
          ungroup() %>%
          mutate(f = row_number()) %>%
          dplyr::select(-filename)

        # assign id to each plant
        coord_df <- trees_site_df %>%
          dplyr::select(lon, lat, id)

        # join data
        ps_df <- ps_mat %>%
          as_tibble() %>%
          left_join(time_df, by = "f") %>%
          left_join(coord_df, by = "id") %>%
          mutate(
            red = red * 0.0001, # scaling following Dixon et al's code
            green = green * 0.0001,
            blue = blue * 0.0001,
            nir = nir * 0.0001
          ) %>%
          # mutate(evi=2.5* (nir-red) / (nir + 6*red - 7.5*blue + 1),
          #        gndvi=(nir-green)/(nir+green),
          #        ebi= (red + green + blue) / (green / blue * (red - blue + 1))) %>%
          left_join(ps_mask_mat %>% as_tibble(), by = c("id", "f")) %>%
          dplyr::select(-f)

        # save
        write_rds(ps_df, paste0(ps_path, "analyses/ps_", siteoi, ".rds"))
      }
    }
  )

  if (class(iserror) != "try-error") {
    iscomplete <- T
  } else if (class(iserror) == "try-error") { # restart cluster
    iscomplete <- F
    closeAllConnections()
    cl <- makeCluster(50, outfile = "")
    registerDoSNOW(cl)
  }
}
stopCluster(cl)

ps_site_list <- vector(mode = "list")
for (siteoi in site_list) {
  ps_site_list[[siteoi]] <- read_rds(paste0(ps_path, "analyses/ps_", siteoi, ".rds")) %>%
    mutate(site = siteoi)
}
ps_df <- bind_rows(ps_site_list) %>%
  arrange(id, time)
ps_df %>%
  pull(site) %>%
  unique()
write_rds(ps_df, "./data/juniper_ashei/ps_compiled.rds")

ps_df_proc <- ps_df %>%
  drop_na() %>%
  mutate(date = as.Date(time)) %>%
  mutate(
    year = format(time, "%Y") %>% as.integer(),
    doy = format(time, "%j") %>% as.integer(),
    hour = format(strptime(time, "%Y-%m-%d %H:%M:%S"), "%H") %>% as.integer()
  ) %>%
  # https://developers.planet.com/docs/data/udm-2/
  filter(clear > 0.9, snow < 0.1, shadow < 0.1, haze_light < 0.1, haze_heavy < 0.1, cloud < 0.1, confidence >= 50) %>%
  group_by(id, lon, lat, date, year, doy) %>%
  summarise(
    blue = mean(blue),
    green = mean(green),
    red = mean(red),
    nir = mean(nir)
  ) %>%
  ungroup() %>%
  mutate(evi = 2.5 * (nir - red) / (nir + 6 * red - 7.5 * blue + 1)) %>%
  filter(evi > 0, evi <= 1) %>%
  filter(red > 0, green > 0, blue > 0) %>%
  mutate(r2g = red / green)

set.seed(1)
ggplot(ps_df_proc %>% filter(id %in% (.$id %>% unique() %>% sample(5)))) +
  geom_point(aes(x = date, y = evi, group = as.factor(id), col = as.factor(id)), alpha = 0.25) +
  geom_smooth(aes(x = date, y = evi, group = as.factor(id), col = as.factor(id)), method = "loess", span = 0.1, se = F) +
  geom_vline(xintercept = paste0(2017:2022, "-01-01") %>% as.Date()) +
  theme_classic() +
  facet_wrap(. ~ id, ncol = 1)

ggplot(ps_df_proc %>% filter(id %in% (.$id %>% unique() %>% sample(5)))) +
  # geom_point(aes(x=date, y=r2g, group=as.factor(id), col=as.factor(id)), alpha=0.25)+
  geom_smooth(aes(x = date, y = r2g, group = as.factor(id), col = as.factor(id)), method = "loess", span = 0.1, se = F) +
  geom_vline(xintercept = paste0(2017:2022, "-01-01") %>% as.Date()) +
  theme_classic() +
  facet_wrap(. ~ id, ncol = 1)
