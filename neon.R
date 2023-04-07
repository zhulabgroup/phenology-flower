v_site <- c("HARV", "SJER")
v_site_plot <- c("HARV"= "HARV_053", "SJER"= "SJER_066")
v_site_state <- c("HARV"= "MA", "SJER"= "CA")
v_year<-2018:2022
# get npn data
npn_phenophases <- rnpn::npn_phenophases()

spid <- npn_species %>%
  filter(genus == taxaoi_short | family_name == taxaoi_short) %>%
  pull(species_id)

df_npn_neon <- rnpn::npn_download_individual_phenometrics(request_source = "YS", years = c(2013:2023),
                                           states = v_site_state,
                                           pheno_class_ids = c(1, 7),
                                           dataset_ids = 16,
                                           additional_fields = c("site_name","phenophase_category", "plant_nickname"))

df_npn_neon<-df_npn_neon %>% 
  rowwise() %>% 
  mutate(site_plot = str_split(site_name , "\\.", simplify = T)[1]) %>% 
  mutate(site = str_split(site_plot , "_", simplify = T)[1]) %>% 
  ungroup() %>% 
  filter(site_plot %in% v_site_plot) 

write_rds(df_npn_neon, "data/processed/NPN_NEON.rds")

# get neon data
library(neonUtilities)
wd<-getwd()
set.wd("data/NEON/")
phe <- neonUtilities::loadByProduct(dpID = "DP1.10055.001", site=v_site, 
                     startdate = "2013-01", enddate="2022-12", 
                     # token = Sys.getenv("NEON_TOKEN"),
                     check.size = F) 

setwd(wd)

# devtools::install_github("NEONScience/NEON-geolocation/geoNEON")
ls_df_neon<-vector(mode="list")
for (siteoi in v_site) {
  ls_df_neon[[siteoi]] <-list (
    indi= phe$phe_perindividual %>% filter(plotID==v_site_plot[siteoi]),
    status = phe$phe_statusintensity%>% filter(plotID==v_site_plot[siteoi]),
    coord = geoNEON::getLocTOS(phe$phe_perindividual %>% filter(plotID==v_site_plot[siteoi]),  "phe_perindividual") 
  )
}
write_rds(ls_df_neon, "data/processed/NEON.rds")


# read in individual coordinates
ls_df_neon <-read_rds( "data/processed/NEON.rds")
ls_df_plant<-vector(mode = "list")
for (siteoi in v_site) {
  ls_df_plant [[siteoi]] <- ls_df_neon[[siteoi]]$coord %>% 
    select(id = individualID, lon =adjDecimalLongitude , lat = adjDecimalLatitude, growthform= growthForm) %>% 
    mutate(site = siteoi)
}
df_plant <- bind_rows(ls_df_plant) 

df_plant %>% 
  distinct(id, growthform) %>% 
  as_tibble()

ggplot(df_plant)+
  geom_point(aes(x= lon, y = lat))+
  facet_wrap(.~site, scales="free")+
  theme_classic()

# get PS data
source("code/data_ps_patch.R")
source("code/data_ps_setup.R")
# source("code/data_ps_order.R")
# source("code/data_ps_down.R")
# source("code/neon_ps_ts.R") 
source("code/func_proc_ps.R")
source("code/prep_hyper.R")
source("code/func_doy.R")
source("code/func_flat.R")
source("code/neon_doy.R")

# process npn neon doy
df_npn_neon<-read_rds("data/processed/NPN_NEON.rds")

df_neon_leaf <- df_npn_neon %>%
  filter(phenophase_id %in% ( npn_phenophases %>% 
           filter(pheno_class_id==1) %>% 
           pull(phenophase_id))) %>% 
  select(site, genus, species, id=plant_nickname , year = first_yes_year, neon_leaf = first_yes_doy)


df_neon_flower <- df_npn_neon %>%
  filter(phenophase_id %in% ( npn_phenophases %>% 
                                filter(pheno_class_id==7) %>% 
                                pull(phenophase_id))) %>% 
  select(site, genus, species,  id=plant_nickname  , year = first_yes_year, neon_flower = first_yes_doy)

df_neon_flower_leaf<-full_join(df_neon_leaf, df_neon_flower, by =c("site", "genus", "species", "id", "year")) %>% 
  left_join(df_plant, by = c("site", "id"))


df_neon_flower_leaf %>% 
  ggplot()+
  geom_point(aes(x = lat, y = lon, col= species))+
  guides(col = "none")+
  facet_wrap(.~site, scales = "free")

df_neon_flower_leaf %>%
  drop_na() %>%
  ggplot()+
  geom_point(aes(x = neon_leaf, y = neon_flower, col= year))+
  facet_wrap(.~str_c(genus, " ",species))

df_neon_flower_leaf %>% 
  filter(genus =="Quercus") %>% 
  drop_na(leaf) %>% 
  filter(leaf <200) %>% 
  filter(year >= 2018, year <=2022) %>% 
  ggplot()+
  geom_jitter(aes(x = year, y = leaf, col=species))+
  guides(col = "none")+
  facet_wrap(.~site)





# correlation
df_doy_ps<- read_rds( "data/processed/neon_doy.rds") %>% 
  rename(ps_leaf = doy)

df_neon_ps<- inner_join(df_doy_ps,df_neon_flower_leaf, by = c("site", "id", "year") )

ggplot(df_neon_ps %>% 
         # mutate(case_when(neon_leaf))
         # filter(doy>0) %>%
         # filter(site=="SJER") %>% 
         filter(genus %in% c("Quercus"
                             , "Acer"
                             # , "Erodium"  , "Bromus", "Aralia", "Betula"
                             )) %>%
         filter(thres==0.5) 
       )+
  geom_point(aes(x = neon_leaf, y = ps_leaf, col=genus))+
  geom_smooth(aes(x = neon_leaf, y = ps_leaf), method = "lm", se=F)+
  ggpubr::stat_cor(aes(x = neon_leaf, y = ps_leaf))+
  geom_abline(intercept = 0, slope = 1, col="red")+
  # facet_wrap(.~site)+
  theme_classic()
