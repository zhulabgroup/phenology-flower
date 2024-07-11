library(pracma)
library(foreach)
library(doSNOW)

path_data<-paste0("/data/ZHULAB/phenology/PlanetScope/")

ps_df<-read_rds(paste0(path_data, "analyses/ps_",siteoi,"_",taxaoi_short,".rds")) 
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
  mutate(ebi= ((red+green+blue))/ (green/blue * (red - blue + 1))) %>% 
  mutate(ndvi=(nir-red)/(nir+red)) %>% 
  mutate(nirv=(ndvi-0.08)*nir) %>% 
  mutate(rgb=(red+green+blue)) %>% 
  mutate(g2r=green/red) %>%
  mutate(red=red/rgb,
         green=green/rgb,
         blue=blue/rgb) %>%
  filter(evi>0, evi<=1) %>% 
  # filter(nir<=0.4) %>%
  # filter(rgb<=0.6) %>%
  # filter(abs(g2r-0.9)<=0.4) %>% 
  filter(red>0, green>0, blue>0) #%>%
  # mutate(color=rgb(red, green, blue, 1 )) 
