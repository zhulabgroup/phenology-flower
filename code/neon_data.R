## get NEON individual tree data from NEON portal
# https://data.neonscience.org/data-products/DP1.10055.001
# All 47 sites
# 2017 Jan to 2023 Mar
# Accessed on Apr 21, 2023

f_neon_meta <- str_c(.path$dat_other,"dat_neon_meta.rds")
f_neon <- str_c(.path$dat_other,"dat_neon.rds")
if(!file.exists(f_neon)) {
  phe <- neonUtilities::stackFromStore(
    filepaths = .path$neon,
    dpID = "DP1.10055.001") 
  
  df_neon_meta <- phe$phe_perindividual %>% 
    filter(subtypeSpecification == "primary") %>% 
    group_by(site = siteID, plot = plotID) %>% 
    summarise(tree_num = individualID %>% unique() %>% length()) %>% 
    arrange(desc(tree_num)) %>% 
    slice(1) %>% 
    ungroup()
  write_rds(df_neon_meta, f_neon_meta)
  
  ls_df_neon_coord<-vector(mode="list")
  for (i in 1:nrow(df_neon_meta)) {
    siteoi = df_neon_meta$site[i]
    plotoi = df_neon_meta$plot[i]
    ls_df_neon_coord[[siteoi]] <-geoNEON::getLocTOS(phe$phe_perindividual %>% filter(plotID==plotoi),
                                 "phe_perindividual") %>% 
        select(site = siteID,
                 id =  individualID,
               lon = adjDecimalLongitude,
               lat = adjDecimalLatitude,
               uncertainty = adjCoordinateUncertainty) %>% 
        group_by(id) %>% 
        arrange(uncertainty) %>% 
        slice(1)
    
  }
  df_neon_coord <- bind_rows (ls_df_neon_coord)
  
  ls_df_neon <- list (metric = phe$phe_perindividual %>% filter(plotID %in% df_neon_meta$plot),
                       intensity = phe$phe_statusintensity%>% filter(plotID %in% df_neon_meta$plot),
                      coord = df_neon_coord)
  write_rds(ls_df_neon, f_neon)
} else {
  df_neon_meta <-read_rds(f_neon_meta)
  ls_df_neon <-read_rds(f_neon)
}

## get NEON individual phenometrics data from NPN portal
# https://data.usanpn.org/observations/
# All sites
# 2017 Jan to 2023 Mar
# Accessed on Apr 21, 2023
f_neon_npn <- str_c(.path$dat_other, "dat_neon_npn.rds")
if(!file.exists(f_neon_npn)) {
  npn_phenophases <- rnpn::npn_phenophases()
  
  df_neon_npn_met <- read_csv(str_c(.path$npn, "individual_phenometrics/individual_phenometrics_data.csv")) %>% 
    left_join(npn_phenophases, by = c("Phenophase_ID" = "phenophase_id")) %>% 
    mutate(event = case_when (pheno_class_id==1~ "leaf",
                              pheno_class_id==7~ "flower"
    )) %>% 
    drop_na(event) %>% 
    rowwise() %>% 
    mutate(plot = str_split(Site_Name , "\\.", simplify = T)[1]) %>% 
    mutate(site = str_split(plot , "_", simplify = T)[1]) %>% 
    ungroup() %>% 
    select(site, plot, genus = Genus, species = Species, 
           functype = Species_Functional_Type,
           id = Plant_Nickname, 
           event,
           year = First_Yes_Year,doy = First_Yes_DOY ) %>% 
    filter(plot %in% df_neon_meta$plot)
  
  # df_neon_npn_met %>% 
  #   ggplot(aes(x = site, y = doy, col = event))+
  #   geom_point()
  
  df_neon_npn_int <- read_csv(str_c(.path$npn, "status_intensity/status_intensity_observation_data.csv")) %>% 
    filter(Intensity_Value!=-9999) %>% 
    left_join(npn_phenophases, by = c("Phenophase_ID" = "phenophase_id")) %>% 
    mutate(event = case_when (pheno_class_id==1~ "leaf",
                              pheno_class_id==7~ "flower"
    )) %>% 
    drop_na(event) %>% 
    rowwise() %>% 
    mutate(plot = str_split(Site_Name , "\\.", simplify = T)[1]) %>% 
    mutate(site = str_split(plot , "_", simplify = T)[1]) %>% 
    ungroup() %>% 
    mutate(year = lubridate::year(Observation_Date)) %>% 
    select(site, plot, genus = Genus, species = Species, 
           functype = Species_Functional_Type,
           id = Plant_Nickname, event,
           year,doy = Day_of_Year, intensity = Intensity_Value) %>% 
    mutate(value = case_when(intensity == "Less than 3" ~ 1 ,
                             intensity == "3 to 10" ~ 2,
                             intensity == "11 to 100" ~ 3,
                             intensity == "101 to 1,000" ~4,
                             intensity == "1,001 to 10,000" ~5 ,
                             intensity == "More than 10,000" ~6,
                             intensity == "Less than 5%" ~ 1,
                             intensity == "5-24%" ~1,
                             intensity =="25-49%"~3,
                             intensity =="50-74%" ~4,
                             intensity =="75-94%"  ~5,
                             intensity =="95% or more"~6
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
  
  ls_df_neon_npn <- list (metric = df_neon_npn_met,
                          intensity = df_neon_npn_int)
  write_rds(ls_df_neon_npn, f_neon_npn)
} else {
  ls_df_neon_npn <-read_rds(f_neon_npn)
}


## get planetscope data
# read in individual coordinates
df_plant <-ls_df_neon$coord %>% 
  mutate(taxa = "all")

ggplot(df_plant)+
  geom_point(aes(x= lon, y = lat))+
  facet_wrap(.~site, scales="free")+
  theme_classic()

source("code/func_ps_patch.R")
source("code/data_ps_setup.R")
source("code/func_ps_order.R")
source("code/func_ps_down.R")
func_ps_batch_order (dir = str_c(.path$ps,"neon/"), df_plant, v_site = NULL)
func_ps_batch_order (dir = str_c(.path$ps,"neon/"), v_site = NULL)

# source("code/neon_ps_ts.R") 
source("code/func_proc_ps.R")
source("code/prep_hyper.R")
source("code/func_doy.R")
source("code/func_flat.R")
# source("code/neon_doy.R")