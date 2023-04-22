## get NEON individual tree data from NEON portal
# https://data.neonscience.org/data-products/DP1.10055.001
# All 47 sites
# 2017 Jan to 2023 Mar
# Accessed on Apr 21, 2023

f_neon_meta <- str_c(.path$dat_other, "dat_neon_meta.rds")
f_neon <- str_c(.path$dat_other, "dat_neon.rds")
if (!file.exists(f_neon)) {
  phe <- neonUtilities::stackFromStore(
    filepaths = .path$neon,
    dpID = "DP1.10055.001"
  )

  df_neon_meta <- phe$phe_perindividual %>%
    filter(subtypeSpecification == "primary") %>%
    group_by(site = siteID, plot = plotID) %>%
    summarise(tree_num = individualID %>% unique() %>% length()) %>%
    arrange(desc(tree_num)) %>%
    slice(1) %>%
    ungroup()
  write_rds(df_neon_meta, f_neon_meta)

  ls_df_neon_coord <- vector(mode = "list")
  for (i in 1:nrow(df_neon_meta)) {
    siteoi <- df_neon_meta$site[i]
    plotoi <- df_neon_meta$plot[i]
    try({ # some coords could not be determined
      ls_df_neon_coord[[siteoi]] <- geoNEON::getLocTOS(
        phe$phe_perindividual %>% filter(plotID == plotoi),
        "phe_perindividual"
      ) %>%
        select(
          site = siteID,
          id = individualID,
          lon = adjDecimalLongitude,
          lat = adjDecimalLatitude,
          uncertainty = adjCoordinateUncertainty
        ) %>%
        group_by(id) %>%
        arrange(uncertainty) %>%
        slice(1)
    })
  }
  df_neon_coord <- bind_rows(ls_df_neon_coord)

  ls_df_neon <- list(
    metric = phe$phe_perindividual %>% filter(plotID %in% df_neon_meta$plot),
    intensity = phe$phe_statusintensity %>% filter(plotID %in% df_neon_meta$plot),
    coord = df_neon_coord
  )
  write_rds(ls_df_neon, f_neon)
} else {
  df_neon_meta <- read_rds(f_neon_meta)
  ls_df_neon <- read_rds(f_neon)
}

## get NEON individual phenometrics data from NPN portal
# https://data.usanpn.org/observations/
# All sites
# 2017 Jan to 2023 Mar
# Accessed on Apr 21, 2023
f_neon_npn <- str_c(.path$dat_other, "dat_neon_npn.rds")
if (!file.exists(f_neon_npn)) {
  npn_phenophases <- rnpn::npn_phenophases()

  df_neon_npn_met <- read_csv(str_c(.path$npn, "individual_phenometrics/individual_phenometrics_data.csv")) %>%
    left_join(npn_phenophases, by = c("Phenophase_ID" = "phenophase_id")) %>%
    mutate(event = case_when(
      pheno_class_id == 1 ~ "leaf",
      pheno_class_id == 7 ~ "flower"
    )) %>%
    drop_na(event) %>%
    rowwise() %>%
    mutate(plot = str_split(Site_Name, "\\.", simplify = T)[1]) %>%
    mutate(site = str_split(plot, "_", simplify = T)[1]) %>%
    ungroup() %>%
    select(site, plot,
      genus = Genus, species = Species,
      functype = Species_Functional_Type,
      id = Plant_Nickname,
      event,
      year = First_Yes_Year, doy = First_Yes_DOY
    ) %>%
    filter(plot %in% df_neon_meta$plot)

  df_neon_npn_taxa <- df_neon_npn_met %>%
    distinct(genus, species) %>%
    slice(1) %>%
    rowwise() %>%
    mutate(family = taxize::tax_name(str_c(genus, " ", species), get = "family", db = "ncbi")$family) %>%
    ungroup()

  # df_neon_npn_met %>%
  #   ggplot(aes(x = site, y = doy, col = event))+
  #   geom_point()

  df_neon_npn_int <- read_csv(str_c(.path$npn, "status_intensity/status_intensity_observation_data.csv")) %>%
    filter(Intensity_Value != -9999) %>%
    left_join(npn_phenophases, by = c("Phenophase_ID" = "phenophase_id")) %>%
    mutate(event = case_when(
      pheno_class_id == 1 ~ "leaf",
      pheno_class_id == 7 ~ "flower"
    )) %>%
    drop_na(event) %>%
    rowwise() %>%
    mutate(plot = str_split(Site_Name, "\\.", simplify = T)[1]) %>%
    mutate(site = str_split(plot, "_", simplify = T)[1]) %>%
    ungroup() %>%
    mutate(year = lubridate::year(Observation_Date)) %>%
    select(site, plot,
      genus = Genus, species = Species,
      functype = Species_Functional_Type,
      id = Plant_Nickname, event,
      year, doy = Day_of_Year, intensity = Intensity_Value
    ) %>%
    mutate(value = case_when(
      intensity == "Less than 3" ~ 1,
      intensity == "3 to 10" ~ 2,
      intensity == "11 to 100" ~ 3,
      intensity == "101 to 1,000" ~ 4,
      intensity == "1,001 to 10,000" ~ 5,
      intensity == "More than 10,000" ~ 6,
      intensity == "Less than 5%" ~ 1,
      intensity == "5-24%" ~ 1,
      intensity == "25-49%" ~ 3,
      intensity == "50-74%" ~ 4,
      intensity == "75-94%" ~ 5,
      intensity == "95% or more" ~ 6
    )) %>%
    filter(plot %in% df_neon_meta$plot)

  # df_neon_npn_int %>%
  #   filter(year ==2017,
  #          site == "HARV",
  #          event == "leaf") %>%
  #   ggplot(aes(x=doy, y = value, group = id, col = id))+
  #   geom_line()+
  #   facet_wrap(.~id)+
  #   guides(col = "none")

  ls_df_neon_npn <- list(
    metric = df_neon_npn_met,
    intensity = df_neon_npn_int,
    taxa = df_neon_npn_taxa
  )
  write_rds(ls_df_neon_npn, f_neon_npn)
} else {
  ls_df_neon_npn <- read_rds(f_neon_npn)
}

v_site_neon <- df_neon_meta %>%
  filter(!site %in% c("BARR", "TOOL", "HEAL", "BONA", "DEJU", "PUUM", "GUAN", "LAJA")) %>%
  pull(site)

df_neon_sites <- ls_df_neon$metric %>%
  distinct(site = siteID, lat = decimalLatitude, lon = decimalLongitude) %>%
  filter(site %in% v_site_neon)

p_neon_map <- ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), fill = "white") +
  geom_path(data = map_data("state"), aes(x = long, y = lat, group = group), color = "grey50", alpha = 0.5, lwd = 0.2) +
  geom_point(
    data = df_neon_sites,
    aes(x = lon, y = lat)
  ) +
  ggrepel::geom_label_repel(
    data = df_neon_sites,
    aes(x = lon, y = lat, label = site)
  ) +
  theme_void() +
  coord_map("bonne", lat0 = 50)
