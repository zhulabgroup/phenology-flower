library(tidyverse)
library(ptw)
library(foreach)
library(doSNOW)
library(EnvCpt)
library(rgdal)
library(mclust)
library(npreg)
library(segmented)

taxa_list <- c("Quercus", "Cupressaceae", "Ambrosia", "Morus", 
               "Pinaceae", "Ulmus early", "Ulmus late", "Fraxinus", "Betula", "Poaceae early", "Poaceae late", "Acer", "Populus")
taxa_short_list <- str_split(taxa_list, pattern = " ", simplify = T) [, 1]

site_list <- c("NY", "SJ", "AT", "ST", "HT", "TP", "DT", "DV", "KC", "SL")
sitename_list <- c("New York", "San Jose", "Austin", "Seattle", "Houston", "Tampa", "Detroit", "Denver", "Kansas City", "St. Louis")

ps_path<-"/data/ZHULAB/phenology/PlanetScope/"

trees_df<-read_rds("./RS4flower/data/street_trees.rds")
grass_df<-read_rds("./RS4flower/data/grass.rds")
ragweed_df<-read_rds("./RS4flower/data/ragweed.rds")
plant_df<-bind_rows(trees_df, grass_df, ragweed_df)

nab_df<-read_rds("/data/ZHULAB/phenology/nab/nab_dat.rds")
nab_taxa_df<-read_rds("/data/ZHULAB/phenology/nab/nab_taxa.rds")

nab_with_taxa_df<-nab_df %>% 
  rename(taxa_raw=taxa) %>% 
  left_join(nab_taxa_df, by="taxa_raw") %>% 
  rename(taxa=taxa_clean) %>% 
  mutate(family=case_when(taxa_raw=="Total Pollen Count"~"Total",
                          TRUE~family)) %>% 
  mutate(genus=case_when(taxa_raw=="Total Pollen Count"~"Total",
                         TRUE~genus)) %>% 
  filter(kingdom=="Viridiplantae"|is.na(kingdom)) %>% 
  group_by(Date, lat, lon, station, id, family,genus, taxa) %>% 
  summarise(count=sum(count)) %>% 
  ungroup() %>% 
  mutate(date=as.Date(Date)) %>% 
  dplyr::select(-Date)

npn_df_all<-read_rds("/data/ZHULAB/phenology/NPN/npn_dat.rds")

flat_better<-function (px_evi_in_sm, k=50) {
  px_doy_new<- (274-365):(365+151)
  px_evi_new<-px_evi_in_sm
  
  fit_list<-vector(mode = "list")
  #fit simple linear regression model
  fit0 <- lm(px_evi_new~px_doy_new,data=data.frame(px_doy_new,px_evi_new))
  fit_list[[1]]<-data.frame(AIC=AIC (fit0, k=k), model="fit0")
  
  #fit piecewise regression model to original model
  try (
    {
      fit1 <- segmented(fit0, seg.Z = ~px_doy_new,npsi=1,it=10, control=seg.control(seed=42, fix.npsi=T))
    fit_list[[2]]<-data.frame(AIC=AIC (fit1, k=k), model="fit1")
    },
    silent = T
  )
  
  try (
    {
      fit2 <- segmented(fit0, seg.Z = ~px_doy_new,npsi=2, it=10, control=seg.control(seed=42, fix.npsi=T))
      fit_list[[3]]<-data.frame(AIC=AIC (fit2, k=k), model="fit2")
    },
    silent = T
  )
  
  try (
    {
      fit3 <- segmented(fit0, seg.Z = ~px_doy_new,npsi=3, it=10, control=seg.control(seed=42, fix.npsi=T))
      fit_list[[4]]<-data.frame(AIC=AIC (fit3, k=k), model="fit3")
    },
    silent = T
  )
  
  aic_df<-bind_rows(fit_list) %>% 
    arrange(AIC, model)
  
  better <- aic_df %>% head(1) %>% pull(model) =="fit0"
  return (better)
}

whitfun<-function (x, lambda) {
  max_id<-0
  done<-F
  while(!done) {
    min_id<-min(which(!is.na(x[(max_id+1):length(x)])))+(max_id)
    if (min_id==Inf) {
      done<-T
    } else {
      max_id<-min(which(is.na(x[min_id:length(x)])))-1+(min_id-1)
      if (max_id==Inf) {
        max_id<-length(x)
        done<-T
      }
      x[min_id:max_id]<-ptw::whit1(x[min_id:max_id],lambda) 
    }
  }
  return(x)
}

cols <- c("EVI (PS)" = "dark green", "G2R (PS)" = "yellow green","EBI (PS)" = "orange", "pollen count (NAB)" = "dark red", "flower observation (USA-NPN)" = "dark orchid", "flowering frequency"="dark blue")

year_list<-2018:2021

thres_list_up<-seq(from=0,to=1, by=0.1) %>% round(1)
thres_list_down<-seq(from=1, to=0.0, by=-0.1) %>% round(1)
thres_df<-bind_rows(data.frame(direction="up", threshold=thres_list_up),
                    data.frame(direction="down", threshold=thres_list_down))

flower_window_df<-read_csv("./RS4flower/data/flower_window.csv")

source("./RS4flower/code/get doy.R")
source("./RS4flower/code/plot for ind tree.R")

cl <- makeCluster(50, outfile = "")
registerDoSNOW(cl)

for (taxaoi in taxa_list) {
  taxaoi_short<-str_split(taxaoi, " ", simplify = T)[1]
  flower_window<-seq(flower_window_df %>% filter(taxa==taxaoi) %>% pull(start),
                     flower_window_df %>% filter(taxa==taxaoi) %>% pull(end),
                     by=1)
  if (taxaoi %in% c("Ambrosia", "Ulmus late")) {
    thres_df_taxa<-thres_df %>% filter(direction=="down")
  } else if (taxaoi== "Poaceae early"  ) {
    thres_df_taxa<-thres_df %>% filter(threshold>=0.5|direction=="up")
  } else if (taxaoi== "Poaceae late"  ) {
    thres_df_taxa<-thres_df %>% filter(threshold>=0.5|direction=="down")
  } else {
    thres_df_taxa<-thres_df %>% filter(direction=="up")
  }
  flower_doy_df_siteyears_list<-flower_freq_df_siteyears_list<-vector(mode="list", length=length(site_list))
  for (s in 1:length(site_list)) {
    siteoi<-site_list[s]
    plant_taxa_df<-plant_df %>% 
      filter(site==siteoi) %>% 
      filter(genus==taxaoi_short|family==taxaoi_short) %>% 
      mutate(id=row_number()) %>% 
      drop_na(lon, lat)
    
    plant_df %>%
      filter(site==siteoi) %>%
      filter(genus==taxaoi_short|family==taxaoi_short) %>%
      group_by(species) %>%
      summarise(n=n()) %>%
      ungroup() %>%
      arrange(desc(n))
    
    if (taxaoi %in% c("Ambrosia","Poaceae")) {
      min_sample_size=10
    } else {
      min_sample_size=20
    }
    
    if (nrow(plant_taxa_df)>=min_sample_size) {
      # plant_taxa_sp<-SpatialPoints(plant_taxa_df[, c("lon","lat")],
      # proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
      
      set.seed(1)
      random_id<<-sample(plant_taxa_df$id, min(2000, length(unique(plant_taxa_df$id)))) %>% sort()
      
      # source("./RS4flower/code/hls data.R") # get hls_df_proc
      
      # preprocess ps data
      ps_df<-read_rds(paste0(ps_path, "analyses/ps_",siteoi,"_",taxaoi_short,".rds")) 
      ps_df_proc<- ps_df%>% 
        drop_na() %>% 
        filter(id%in% random_id) %>%
        mutate(date=as.Date(time)) %>% 
        mutate(year=format(time, "%Y") %>% as.integer(),
               doy=format(time, "%j") %>% as.integer(),
               hour=format(strptime(time,"%Y-%m-%d %H:%M:%S"),'%H') %>% as.integer()) %>% 
        # filter(hour==18|hour==17) %>% 
        filter(qa==0) %>%
        group_by(id, lon, lat, date, year, doy ) %>% 
        summarise(blue=mean(blue),
                  green=mean(green),
                  red=mean(red),
                  nir=mean(nir)) %>% 
        ungroup() %>% 
        mutate(evi=2.5* (nir-red) / (nir + 6*red - 7.5*blue + 1)) %>% 
        # mutate(ebi= ((red+green+blue))/ (green/blue * (red - blue + 1))) %>% 
        # mutate(ndvi=(nir-red)/(nir+red)) %>% 
        # mutate(nirv=(ndvi-0.08)*nir) %>% 
        # mutate(rgb=(red+green+blue)) %>% 
        # mutate(g2r=green/red) %>%
        # mutate(red=red/rgb,
        #        green=green/rgb,
        #        blue=blue/rgb) %>%
        filter(evi>0, evi<=1) %>% 
        # filter(nir<=0.4) %>%
        # filter(rgb<=0.6) %>%
        # filter(abs(g2r-0.9)<=0.4) %>% 
        filter(red>0, green>0, blue>0) #%>%
      # mutate(color=rgb(red, green, blue, 1 )) 
      
      # subset nab data
      pollen_df<- nab_with_taxa_df %>% 
        left_join(meta_df %>% dplyr::select(id, site), by="id") %>% 
        filter(site==siteoi) %>% 
        filter(genus==taxaoi_short|family==taxaoi_short) 
      
      # subset npn data
      npn_df<-npn_df_all %>% 
        filter(site==siteoi) %>% 
        filter(taxa==taxaoi_short)
      
      ts_df<-ps_df_proc %>% 
        left_join(plant_taxa_df, by=c("id", "lon", "lat")) %>% 
        
        dplyr::select(id, date, `EVI (PS)`=evi #, 
                      # `G2R (PS)`=g2r, `EBI (PS)`=ebi
                      ) %>% 
        mutate(id=as.factor(id)) %>% 
        full_join(pollen_df %>% 
                    dplyr::select(date, `pollen count (NAB)`=count) %>% 
                    mutate(id="pollen") , 
                  by=c("date", "id") ) %>% 
        full_join(npn_df %>%
                    dplyr::select(date, `flower observation (USA-NPN)`=count) %>%
                    mutate(id="npn") ,
                  by=c("date", "id") ) %>%
        arrange(id, date) %>% 
        mutate(doy=format(date, "%j") %>% as.numeric()) %>% 
        mutate(year=format(date, "%Y") %>% as.numeric()) 
      
      flower_doy_df_years_list<-flower_freq_df_years_list<-vector(mode="list", length=length(year_list))
      for (y in 1:length(year_list)) {
        yearoi<-year_list[y]
        ts_df_subset<-ts_df %>% 
          filter(doy!=366) %>% 
          # filter(doy>start_doy,doy<=end_doy) %>%
          filter(year==yearoi|year==(yearoi-1)|year==(yearoi+1)) %>% 
          mutate(doy=ifelse(doy>273& year==yearoi-1, doy-365, doy)) %>% 
          mutate(year=ifelse(doy<=0 & year==yearoi-1, year+1, year)) %>% 
          mutate(doy=ifelse(doy<152& year==yearoi+1, doy+365, doy)) %>% 
          mutate(year=ifelse(doy>365 & year==yearoi+1, year-1, year)) %>% 
          filter(year==yearoi) %>% 
          gather(key="var", value="value", -date, -id, -doy, -year ) %>% 
          mutate(var=fct_relevel(var, levels=c( "EVI (PS)", "G2R (PS)","EBI (PS)", "pollen count (NAB)", "flower observation (USA-NPN)"))) %>% 
          mutate(alpha=case_when(var %in% c("EVI", "G2R")~0.05,
                                 TRUE~1)) %>% 
          arrange(doy)
        
        # ts_df_subset %>% filter(year==2021) %>% filter(var=="EVI (PS)")%>% ggplot()+geom_point(aes(x=doy, y=value), alpha=0.1)
        
        ts_df_subset_summary<- ts_df_subset%>%
          drop_na(value) %>%
          group_by(date, var, doy, year) %>%
          summarise(q1=quantile(value, 0.05, na.rm=T),
                    q2=quantile(value, 0.5, na.rm=T),
                    q3=quantile(value, 0.95, na.rm=T),
                    n=n()) %>%
          filter(n>1) %>%
          ungroup()
        
        flower_df_list<-
          foreach (i = 1:length(random_id),
                   .packages = c("tidyverse", "ptw","greenbrown", "EnvCpt","segmented", "RhpcBLASctl"))  %dopar% {
                     blas_set_num_threads(1)
                     omp_set_num_threads(1)
                     
                     debug=F
                     idoi <-  as.character(random_id)[i]
                     
                     if (debug) { 
                       set.seed(NULL)
                       idoi<-sample(random_id,1) %>% as.character()
                     }
                     
                     flower_df<-get_doy(plant_taxa_df, ts_df_subset, idoi)
                     
                     if (debug) {
                       p_1tree<-plot_tree(plant_taxa_df, ts_df_subset, flower_df, idoi)
                     }
                     
                     print(paste0(i , " out of ", length(random_id)))
                     flower_df
                   }
        flower_doy_df<-bind_rows(flower_df_list) %>% 
          left_join(plant_taxa_df %>% dplyr::select(id, species, lat, lon) %>% mutate(id=as.character(id)), by="id") %>% 
          mutate(site=siteoi, year=yearoi)
        
        flower_freq_df_list<-vector(mode="list", length=nrow(thres_df_taxa))
        for (t in 1:nrow(thres_df_taxa)) {
          flower_freq_df_list[[t]]<-flower_doy_df %>% 
            drop_na(start, end) %>% 
            filter(direction==thres_df_taxa$direction[t],
                   thres==thres_df_taxa$threshold[t]) %>% 
            group_by(doy, thres,direction) %>%
            summarise(count=n()) %>%
            ungroup() %>% 
            mutate(freq=count/n())
        }
        flower_freq_df<-bind_rows(flower_freq_df_list)
        
        
        flower_freq_df<-flower_freq_df %>% 
          mutate(doy=factor(doy, levels=c((274-365):(365+151))) ) %>% 
          complete(doy, thres,direction,fill=list(count=0,freq=0)) %>%
          mutate(doy=doy %>% as.character() %>% as.numeric()) %>% 
          arrange(doy) %>% 
          full_join(ts_df_subset %>% 
                      filter(id=="pollen") %>% 
                      filter(var=="pollen count (NAB)") %>% 
                      filter(year==yearoi) %>% 
                      dplyr::select( doy, pollen=value),
                    by="doy") %>% 
          full_join(ts_df_subset %>%
                      filter(id=="npn") %>%
                      filter(var=="flower observation (USA-NPN)") %>%
                      filter(year==yearoi) %>%
                      dplyr::select( doy, npn=value),
                    by="doy") %>%
          full_join(ts_df_subset_summary %>% 
                      filter(var=="EVI (PS)") %>% 
                      filter(year==yearoi) %>% 
                      dplyr::select( doy, evi=q2),
                    by="doy"
          ) %>% 
          full_join(ts_df_subset_summary %>% 
                      filter(var=="G2R (PS)") %>% 
                      filter(year==yearoi) %>% 
                      dplyr::select( doy, g2r=q2),
                    by="doy"
          ) %>% 
          mutate(doy=as.numeric(doy)) %>% 
          mutate(site=siteoi, year=yearoi) %>% 
          drop_na(doy, thres)
        
        print(paste0(siteoi, ", ", yearoi))
        
        flower_doy_df_years_list[[y]]<-flower_doy_df 
        flower_freq_df_years_list[[y]]<-flower_freq_df
      }
      flower_doy_df_siteyears_list[[s]]<-bind_rows(flower_doy_df_years_list)
      flower_freq_df_siteyears_list[[s]]<-bind_rows(flower_freq_df_years_list)
    }
  }
  flower_doy_df<-bind_rows(flower_doy_df_siteyears_list) %>% 
    left_join(data.frame(site=site_list, sitename=sitename_list
                         # , region=region_list
                         ), by=c("site"))
  
  flower_freq_df<-bind_rows(flower_freq_df_siteyears_list) %>% 
    group_by(site, year,thres,direction) %>% 
    mutate(freq_sm=whit1(freq, 30)) %>% 
    mutate(pollen_in=na.approx(pollen, doy, na.rm=F, maxgap=14)) %>%  
    mutate(pollen_fill=replace_na(pollen_in, 0)) %>%
    mutate(pollen_sm=whitfun(pollen_fill,30)) %>%  
    ungroup() %>% 
    dplyr::select(-pollen_in, -pollen_fill) %>% 
    mutate(pollen=case_when(doy %in% flower_window~ pollen)) %>% 
    mutate(pollen_sm=case_when(doy %in% flower_window~ pollen_sm,
                               TRUE~0)) %>% 
    mutate(npn=case_when(doy %in% flower_window~ npn))
  
  pollen_clim<-flower_freq_df %>% 
    group_by(site,direction, thres,doy) %>%
    summarise(pollen_clim=mean(pollen_sm, na.rm=T)) 
  
  flower_freq_df<-flower_freq_df%>% 
    left_join(pollen_clim, by=c("site",  "direction","thres", "doy")) %>% 
    left_join(data.frame(site=site_list, sitename=sitename_list
                         # , region=region_list
                         ), by=c("site"))
  
  
  output_path<-paste0("./RS4flower/output/data/", taxaoi,"/")
  dir.create(output_path, recursive = T)
  write_rds(flower_doy_df,paste0(output_path,"flowering day of year.rds" ))
  write_rds(flower_freq_df,paste0(output_path,"flowering frequency.rds" ) )
}
stopCluster(cl)
