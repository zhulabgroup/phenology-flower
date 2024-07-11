library(tidyverse)
library(ptw)
library(foreach)
library(doSNOW)
library(EnvCpt)
library(rgdal)
library(mclust)

cl <- makeCluster(50, outfile = "")
registerDoSNOW(cl)

flower_doy_df_siteyears_list<-flower_freq_df_siteyears_list<-vector(mode="list", length=length(site_list))
for (s in 1:length(site_list)) {
  siteoi<-site_list[s]
  trees_genus_df<-trees_df %>% 
    filter(site==siteoi) %>% 
    filter(genus==taxaoi) %>% 
    mutate(id=row_number()) %>% 
    drop_na(lon, lat)
  
  # trees_df %>% 
  #   filter(site=="NY") %>% 
  #   filter(genus==taxaoi) %>% 
  #   group_by(species) %>% 
  #   summarise(n=n()) %>% 
  #   ungroup() %>% 
  #   arrange(desc(n))
  # trees_genus_df<-trees_genus_df %>%
  #   filter(species=="Quercus palustris")
  # trees_genus_df<-trees_genus_df %>%
  #   filter(species=="Quercus rubra")
  
  trees_genus_sp<-SpatialPoints(trees_genus_df[, c("lon","lat")],
                                proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  set.seed(1)
  random_id<<-sample(trees_genus_df$id, min(2000, length(unique(trees_genus_df$id)))) %>% sort()
  
  # source("./RS4flower/code/hls data.R") # get hls_df_proc
  source("./RS4flower/code/planetscope data.R") # get ps_df_proc
  source("./RS4flower/code/pollen data.R") # get pollen_df
  source("./RS4flower/code/npn data.R") # get npn_df
  
  ts_df<-ps_df_proc %>% 
    dplyr::select(id, date, `EVI (PS)`=evi, `G2R (PS)`=g2r) %>% 
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
  
  if (siteoi=="TP") {
    year_list<-2017:2020
  } else {
    year_list<-2017:2021
  }
  # year_list<-2018
  flower_doy_df_years_list<-flower_freq_df_years_list<-vector(mode="list", length=length(year_list))
  for (y in 1:length(year_list)) {
    yearoi<-year_list[y]
    ts_df_subset<-ts_df %>% 
      # filter(doy>start_doy,doy<=end_doy) %>%
      filter(year==yearoi) %>% 
      gather(key="var", value="value", -date, -id, -doy, -year ) %>% 
      mutate(var=fct_relevel(var, levels=c( "EVI (PS)", "G2R (PS)", "pollen count (NAB)", "flower observation (USA-NPN)"))) %>% 
      mutate(alpha=case_when(var %in% c("EVI", "G2R")~0.05,
                             TRUE~1))
    
    ts_df_subset_summary<- ts_df_subset%>% 
      drop_na(value) %>%
      group_by(date, var, doy, year) %>%
      summarise(q1=quantile(value, 0.05, na.rm=T),
                q2=quantile(value, 0.5, na.rm=T),
                q3=quantile(value, 0.95, na.rm=T),
                n=n()) %>%
      filter(n>1|var=="count") %>%
      ungroup()
    
    flower_df_list<-
      foreach (i = 1:length(random_id),
               .packages = c("tidyverse", "ptw","greenbrown", "EnvCpt"))  %dopar% {
                 
                 debug=F
                 idoi <-  as.character(random_id)[i]
                 
                 if (debug) {
                   set.seed(NULL)
                   idoi<-sample(random_id,1) %>% as.character()
                 }
                 px_doy<-ts_df_subset %>% filter(id==idoi) %>% filter(var=="G2R (PS)") %>% pull(doy)
                 min_doy_avail<-min(px_doy)
                 if (length(px_doy)<50) {
                   flower_doy<-sos<-start_doy<-end_doy<-NA
                 }  else {
                   px_evi<-ts_df_subset %>% filter(id==idoi) %>% filter(var=="EVI (PS)") %>% pull(value)
                   px_evi_in<-approx(px_doy, px_evi, min_doy_avail:365, rule=2)$y
                   px_g2r<-ts_df_subset %>% filter(id==idoi) %>% filter(var=="G2R (PS)") %>% pull(value)
                   px_g2r_in<-approx(px_doy, px_g2r, min_doy_avail:365, rule=2)$y
                   
                   px_evi_in_sm<-whit1(px_evi_in,30)
                   px_g2r_in_sm<-whit1(px_g2r_in,30)
                   
                   px_evi_in<-c(rep(NA, min_doy_avail-1), px_evi_in)
                   px_g2r_in<-c(rep(NA, min_doy_avail-1), px_g2r_in)
                   px_evi_in_sm<-c(rep(NA, min_doy_avail-1), px_evi_in_sm)
                   px_g2r_in_sm<-c(rep(NA, min_doy_avail-1), px_g2r_in_sm)
                   
                   max_evi<-quantile(px_evi_in_sm[1:200], 0.99, na.rm = T)
                   end_doy<-which(!is.na(px_evi_in_sm)&px_evi_in_sm>=max_evi)[1]
                   min_evi<-quantile(px_evi_in_sm[1:end_doy], 0.01, na.rm = T)
                   min_doy<-which(!is.na(px_evi_in_sm[1:end_doy])& px_evi_in_sm[1:end_doy]<=min_evi) %>% max()+1
                   start_doy<-min_doy
                   greenup_thres1<-(max_evi-min_evi)*0.5+min_evi
                   greenup_thres2<-(max_evi-min_evi)*0.5+min_evi
                   
                   param_ok2<- (end_doy>start_doy)
                   
                   
                   if (!param_ok2) {
                     flower_doy<-start_doy<-end_doy<-NA
                   } else {
                     
                     sos1<-which(px_evi_in_sm[start_doy:length(px_evi_in_sm)]>=greenup_thres1) %>% min()+start_doy-1
                     sos2<-which(px_evi_in_sm[start_doy:length(px_evi_in_sm)]>=greenup_thres2) %>% min()+start_doy-1

                     data_density<-length(which (px_doy>=start_doy & px_doy<=end_doy))/(end_doy-start_doy+1)
                     if (length(start_doy:end_doy)<=31 | data_density<1/7) {
                       flower_doy<-NA
                     } else {
                       set.seed(42)
                       res1<-envcpt(px_evi_in_sm[start_doy:end_doy], models = c("trendcpt"),verbose = F, minseglen = 5)
                       # plot(res1)
                       
                       if (length(res2$trendcpt@cpts)<3) {
                         flower_doy<-NA
                       } else {
                         slope_df<-data.frame(cpt=res1$trendcpt@cpts,
                                              slope=res1$trendcpt@param.est$beta[,2] ) %>%
                           mutate(diff=lead(slope)-slope) %>%
                           mutate(prevdiff=lag(diff)) %>%
                           mutate(cpt=cpt+start_doy-1) %>%
                           mutate(timediff=(cpt-sos1)) %>% #10 for DT
                           filter(!is.na(diff)&
                                     diff< - 0.00) %>% 
                           arrange(abs(timediff)) %>% 
                           filter(abs(timediff)<=20,
                                  timediff<0) %>% 
                           arrange(diff) %>% 
                           head(1)
                         flower_doy<-slope_df %>% pull(cpt)
                         if (length(flower_doy)==0){
                           flower_doy<-NA
                         }
                         
                         if (!is.na(flower_doy)) {
                           close_left<-px_doy[which(px_doy<=flower_doy) %>% rev() %>% head(1)]
                           close_right<-px_doy[which(px_doy>=flower_doy)%>% head(1)]
                           if ( close_right-close_left > 14) {
                             flower_doy<-NA
                           }
                         }
                         
                       }
                     }
                   }
                   
                 }
                 
                 if (debug) {
                   if (length(px_doy)>0) {
                     p_1tree<-ggplot( )+
                       # geom_vline(xintercept = cpt_doy, alpha=0.5, lty=2) +
                       geom_point(data=ts_df_subset %>% filter(id==idoi|id=="npn"|id=='pollen') %>% 
                                    filter(var=="EVI (PS)"),
                                  aes(doy, value,group=as.factor(id), col=var, alpha=alpha))+
                       # geom_smooth( data=ts_df_subset_summary,
                       #              aes(doy, q1),alpha=1,
                       #              span=0.2, method="loess",se=F)+
                       # geom_smooth( data=ts_df_subset_summary,
                       # aes(doy, q2),alpha=1,
                       # span=0.2, method="loess",se=F)+
                       # geom_smooth( data=ts_df_subset_summary,
                       #              aes(doy, q3),alpha=1,
                       #              span=0.2, method="loess",se=F)+
                       theme_classic()+
                       facet_wrap(.~var, ncol=1, scales = "free_y")+
                       guides(col=F,
                              alpha=F)+
                       scale_color_manual(values=cols)+
                       geom_vline(xintercept = c(start_doy, end_doy), col="blue", alpha=0.5)+
                       xlab("day of year")+
                       ylab("")+
                       ggtitle(paste0("Site: ", siteoi,"  Year: ", yearoi,"  ID: ", idoi))
                     if (is.na(flower_doy)) {
                       p_1tree
                     } else {
                       p_1tree<-p_1tree+
                         geom_vline(xintercept = flower_doy, col="red")
                     }
                     if (is.na(sos2)) {
                       p_1tree
                     } else {
                       p_1tree<-p_1tree+
                         geom_vline(xintercept = sos1, col="blue", lty=2)+
                         geom_vline(xintercept = sos2, col="blue")
                     }
                     if (site=="DT") {
                       p_1tree<-p_1tree+
                         # geom_vline(xintercept = sos1, col="blue", lty=2)+
                         geom_vline(xintercept = detroit_df_annual %>% filter(id==idoi) %>% pull(peak), col="purple")
                     }
                     print(p_1tree+xlim(0, 200))
                     # cairo_pdf("./RS4flower/output/figures/time series for one tree.pdf",
                     #           height = 6, width = 8)
                     # print(p_1tree)
                     # dev.off()
                   }
                 }
                 print(paste0(i , " out of ", length(random_id)))
                 data.frame(id=idoi, start=start_doy, end=end_doy, doy=flower_doy, sos=sos2) 
               }
    flower_doy_df<-bind_rows(flower_df_list)
    
    flower_doy_df<-flower_doy_df %>% 
      # mutate(doy_new=case_when((!doy %in% boxplot.stats(flower_doy_df$doy, coef=0.5)$out)~doy)) %>% 
      mutate(sos_new=case_when((!sos %in% boxplot.stats(flower_doy_df$sos, coef=0.5)$out)~sos)) %>% 
      rename(#doy_ori=doy, doy=doy_new,
             sos_ori=sos, sos=sos_new) 
    
    ts_df_subset2<-ts_df_subset%>%
      group_by(id, doy, var) %>% 
      summarise(value=mean(value)) %>% 
      ungroup() %>% 
      spread(key="var", value="value") %>%
      dplyr::select(id, doy, evi=`EVI (PS)`, g2r=`G2R (PS)`) 
    id_select<-ts_df_subset2 %>% pull(id) %>% unique() %>% as.character()
    id_select<-sample(id_select,20)
    # id_select<-flower_doy_df %>% drop_na(doy) %>% pull(id) %>% sample(12)
    ts_df_subset_select<-ts_df_subset2%>% 
      filter(id %in% id_select)
    ggplot(flower_doy_df %>% filter(id %in% id_select))+
      # geom_vline(aes(xintercept = doy), col="purple")+
      # geom_vline(aes(xintercept =start), col="blue", alpha=0.5)+
      # geom_vline(aes(xintercept =end), col="blue", alpha=0.5)+
      # geom_vline(aes(xintercept = flower_start), col="purple", alpha=0.5)+
      # geom_vline(aes(xintercept = flower_end), col="purple", alpha=0.5)+
      # geom_vline(aes(xintercept = sos), col="blue")+
      geom_point(data=ts_df_subset_select, aes(x=doy, y=evi), col="dark green" )+
      geom_point(data=ts_df_subset_select, aes(x=doy, y=g2r-0.5), col="yellow green" )+
      facet_wrap(.~id)+
      theme_classic()+
      xlim(0, 200)
    
    
    # offset<-sos_center-Mode(flower_doy_df$doy)
    offset<-0
    flower_doy_df<-flower_doy_df %>% 
      mutate(sos=round(sos,0)) %>% 
      mutate(doy=round(doy+offset,0)) %>% 
      left_join(trees_genus_df %>% dplyr::select(id, species, lat, lon) %>% mutate(id=as.character(id)), by="id")
    
    flower_freq_df_sos<-flower_doy_df %>% 
      drop_na(sos) %>% 
      group_by(doy=sos) %>%
      summarise(freq_sos=n()/nrow(flower_doy_df )) %>%
      ungroup() 
    
    flower_freq_df_cpt<-flower_doy_df %>% 
      drop_na(doy) %>% 
      group_by(doy) %>% 
      summarise(freq_cpt=n()/nrow(flower_doy_df )) %>% 
      ungroup() 
    
    flower_freq_df<-full_join(flower_freq_df_sos, flower_freq_df_cpt, by="doy") %>% 
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
      mutate(doy=factor(doy, levels=1:365) ) %>% 
      complete(doy,fill=list(freq_sos=0, freq_cpt=0)) %>%
      mutate(doy=as.numeric(doy)) %>% 
      mutate(siteoi=siteoi, yearoi=yearoi) %>% 
      drop_na(doy)
    
    print(paste0(siteoi, ", ", yearoi))
    
    flower_doy_df_years_list[[y]]<-flower_doy_df %>% mutate(siteoi=siteoi, yearoi=yearoi)
    flower_freq_df_years_list[[y]]<-flower_freq_df
  }
  flower_doy_df_siteyears_list[[s]]<-bind_rows(flower_doy_df_years_list)
  flower_freq_df_siteyears_list[[s]]<-bind_rows(flower_freq_df_years_list)
}
flower_doy_df<-bind_rows(flower_doy_df_siteyears_list) %>% 
  left_join(data.frame(site=site_list, sitename=sitename_list), by=c("siteoi"="site"))
flower_freq_df<-bind_rows(flower_freq_df_siteyears_list)
# write_rds(flower_freq_df,"./RS4flower/data/flowering frequency_0404.rds" )
# 
# flower_freq_df<-read_rds("./RS4flower/data/flowering frequency_0404.rds" )
ggplot(flower_doy_df %>% filter(yearoi>2017) %>% drop_na(doy, sos))+
  geom_point(aes(x=lon, y=lat, col=doy), cex=1, alpha=0.6)+
  scale_color_viridis_c()+
  theme_classic()+
  facet_wrap(.~sitename*yearoi, ncol=8, scales = "free")

flower_freq_df<-flower_freq_df %>% 
  group_by(siteoi, yearoi) %>% 
  mutate(freq_cpt_sm=whit1(freq_cpt, 30)) %>% 
  mutate(freq_sos_sm=whit1(freq_sos, 30)) %>% 
  ungroup() %>% 
  left_join(data.frame(site=site_list, sitename=sitename_list), by=c("siteoi"="site"))

flower_freq_df_standard<-flower_freq_df %>% 
  group_by(siteoi, sitename, yearoi) %>%
  mutate(freq_sos=(freq_sos-min(freq_sos, na.rm = T))/(max(freq_sos, na.rm = T)-min(freq_sos, na.rm = T))) %>% 
  mutate(freq_cpt=(freq_cpt-min(freq_cpt, na.rm = T))/(max(freq_cpt, na.rm = T)-min(freq_cpt, na.rm = T))) %>% 
  mutate(freq_sos_sm=(freq_sos_sm-min(freq_sos_sm, na.rm = T))/(max(freq_sos_sm, na.rm = T)-min(freq_sos_sm, na.rm = T))) %>% 
  mutate(freq_cpt_sm=(freq_cpt_sm-min(freq_cpt_sm, na.rm = T))/(max(freq_cpt_sm, na.rm = T)-min(freq_cpt_sm, na.rm = T))) %>% 
  mutate(pollen=(pollen-min(pollen, na.rm = T))/(max(pollen, na.rm = T)-min(pollen, na.rm = T))) %>% 
  mutate(npn=(npn-min(npn, na.rm = T))/(max(npn, na.rm = T)-min(npn, na.rm = T))) %>% 
  mutate(evi=(evi-min(evi, na.rm = T))/(max(evi, na.rm = T)-min(evi, na.rm = T))) %>% 
  mutate(g2r=(g2r-min(g2r, na.rm = T))/(max(g2r, na.rm = T)-min(g2r, na.rm = T))) %>% 
  ungroup()

cols <- c("EVI (PS)" = "dark green", "G2R (PS)" = "yellow green", "pollen count (NAB)" = "dark red", "flower observation (USA-NPN)" = "dark orchid", "flowering frequency (SOS)"="dark blue","flowering frequency (CPT)"="brown")
flower_freq_comp<-ggplot(flower_freq_df_standard )+
  geom_point(aes(x=doy, y=npn, col="flower observation (USA-NPN)"), alpha=0.5)+
  geom_point(aes(x=doy, y=pollen, col="pollen count (NAB)"))+
  # geom_point(aes(x=doy, y=evi), col="dark green", alpha=0.5)+
  geom_smooth(aes(x=doy, y=evi, col="EVI (PS)"), alpha=0.5, se=F, span=0.2)+
  # geom_point(aes(x=doy, y=g2r), col="yellow green", alpha=0.5)+
  geom_smooth(aes(x=doy, y=g2r, col="G2R (PS)"), alpha=0.5, se=F, span=0.2)+
  geom_line(aes(x=doy, y=freq_sos_sm, col="flowering frequency (SOS)"), lwd=0.5)+
  geom_line(aes(x=doy, y=freq_cpt_sm, col="flowering frequency (CPT)"), lwd=0.5)+
  theme_classic()+
  facet_wrap(.~sitename*yearoi, ncol=8, scale="free_x")+
  scale_color_manual(values=cols)+
  theme(legend.position="bottom")+
  theme(legend.title = element_blank())+
  ylab("")
flower_freq_comp
cairo_pdf("./RS4flower/output/figures/flower frequency compared with other data_0405.pdf",
          height = 16, width = 16)
print(flower_freq_comp)
dev.off()

flower_freq_df_standard<-flower_freq_df_standard %>% 
  filter(siteoi!="SJ")
res<-cor.test(flower_freq_df_standard$freq_cpt,flower_freq_df_standard$pollen , na.rm=T, method="pearson")
res
res$estimate^2
res<-cor.test(flower_freq_df_standard$freq_sos,flower_freq_df_standard$pollen , na.rm=T, method="pearson")
res
res$estimate^2

flower_freq_corr<-ggplot(flower_freq_df %>% 
                           filter(yearoi>2017) %>% 
                           mutate(year=as.factor(yearoi)) )+
  geom_point(aes(x=freq_cpt_sm, y=pollen %>% sqrt(), group=year, col=year))+
  geom_smooth(aes(x=freq_cpt_sm, y=pollen  %>% sqrt(), group=year), method="lm")+
  theme_classic()+
  facet_wrap(.~sitename, scales = c("free"), ncol=4)+
  ylab("pollen count^(1/2)")+
  xlab("flowering frequency")
flower_freq_corr
cairo_pdf("./RS4flower/output/figures/flower frequency and pollen count correlation_0404.pdf",
          height = 6, width = 8)
print(flower_freq_corr)
dev.off()