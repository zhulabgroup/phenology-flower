flower_freq_df_allsites_list<-vector(mode="list")
for (t in 1:length(taxa_list)) {
  taxaoi<-taxa_list[t]
  output_path<-paste0("/raid/users/ysong67/GitHub/phenology/RS4flower/output/data/", taxaoi,"/")
  
  flower_freq_df<-read_rds(paste0(output_path,"flowering frequency.rds" ) )
  
  # standardize all data to be between 0 and 1
  flower_freq_df_standard<-flower_freq_df %>% 
    mutate(pollen=pollen %>% sqrt()) %>%
    mutate(pollen_sm=pollen_sm %>% sqrt()) %>%
    mutate(pollen_clim=pollen_clim %>% sqrt()) %>%
    group_by(site, sitename, thres) %>%
    mutate(freq=(freq-min(freq, na.rm = T))/(max(freq, na.rm = T)-min(freq, na.rm = T))) %>% 
    mutate(freq_sm=(freq_sm-min(freq_sm, na.rm = T))/(max(freq_sm, na.rm = T)-min(freq_sm, na.rm = T))) %>% 
    mutate(pollen=(pollen-min(pollen, na.rm = T))/(max(pollen, na.rm = T)-min(pollen, na.rm = T))) %>%
    mutate(pollen_clim=(pollen_clim-min(pollen_sm, na.rm = T))/(max(pollen_sm, na.rm = T)-min(pollen_sm, na.rm = T))) %>% 
    mutate(pollen_sm=(pollen_sm-min(pollen_sm, na.rm = T))/(max(pollen_sm, na.rm = T)-min(pollen_sm, na.rm = T))) %>% 
    mutate(npn=(npn-min(npn, na.rm = T))/(max(npn, na.rm = T)-min(npn, na.rm = T))) %>%
    mutate(evi=(evi-min(evi, na.rm = T))/(max(evi, na.rm = T)-min(evi, na.rm = T))) %>% 
    mutate(g2r=(g2r-min(g2r, na.rm = T))/(max(g2r, na.rm = T)-min(g2r, na.rm = T))) %>% 
    ungroup()
  
  flower_freq_df_check<-read_rds(paste0(output_path,"accuracy check.rds") )
  
  best_thres<-flower_freq_df_check %>% 
    group_by(direction, thres) %>% 
    summarise(rmse=median(rmse)) %>% # median rmse for each threshold
    arrange(rmse) %>% 
    head(1) %>% # keep threshold giving the smallest median rmse
    dplyr::select(direction,thres)
  
  # getting time series with best threshold and lag
  flower_freq_df_standard_best_list<-vector(mode="list", length=length(site_list))
  for (r in 1:length(site_list)) {
    regionoi<-site_list[r]
    lagoi<-flower_freq_df_check %>% filter(direction==best_thres$direction,thres==best_thres$thres, region==regionoi) %>% pull(lag)
    if (length(lagoi)>0) {
      flower_freq_df_standard_best<-flower_freq_df_standard %>% 
        filter(site==regionoi) %>% 
        filter(direction==best_thres$direction,
               thres == best_thres$thres) %>%
        ungroup() %>%
        group_by(site, year)
      
      if (lagoi<0) {
        flower_freq_df_standard_best<- flower_freq_df_standard_best %>% mutate(freq_sm=lead(freq_sm,n=-lagoi)) %>% mutate(freq_sm=replace_na(freq_sm, 0))
      } else if (lagoi==0) {
        flower_freq_df_standard_best<- flower_freq_df_standard_best %>% mutate(freq_sm=replace_na(freq_sm, 0))
      } else if (lagoi>0) {
        flower_freq_df_standard_best<- flower_freq_df_standard_best %>% mutate(freq_sm=lag(freq_sm,n=lagoi)) %>% mutate(freq_sm=replace_na(freq_sm, 0))
      }  
      
      flower_freq_df_standard_best_list[[r]]<-flower_freq_df_standard_best %>% mutate(lag=lagoi)
    }
  }
  flower_freq_df_standard_best<-bind_rows(flower_freq_df_standard_best_list) %>% 
    filter(site=="NY") %>% 
    filter(year==2018) %>% 
    mutate(taxa=taxaoi)
  
  flower_freq_df_allsites_list[[t]]<-flower_freq_df_standard_best
}
flower_freq_df_allsites<-bind_rows(flower_freq_df_allsites_list)

# make plots
flower_freq_comp<-ggplot(flower_freq_df_allsites %>% 
                           mutate(doy=as.Date(doy, origin = "2018-01-01")) %>% 
                           mutate(taxa_p=paste0(taxa," (Threshold: ",thres, " green-",direction, ", Lag: ", lag, ")")) %>% 
                           mutate(taxa_p=fct_relevel(taxa_p, levels=unique(taxa_p))))+
  geom_point(aes(x=doy, y=npn, col="flower observation (USA-NPN)"), alpha=0.5)+
  geom_point(aes(x=doy, y=pollen, col="pollen count (NAB)"))+
  # geom_line(aes(x=doy, y=pollen_clim, col="pollen count (NAB)"),alpha=0.5, lwd=1)+
  geom_point(aes(x=doy, y=evi, col="EVI (PS)"), alpha=0.05)+
  geom_line(aes(x=doy, y=freq_sm, col="flowering frequency"), lwd=1)+
  theme_classic()+
  facet_wrap(.~taxa_p, ncol=2)+
  scale_color_manual(values=cols)+
  xlab("Day of year")+
  ylab("Standardized value")+
  # ggtitle(paste0("Site: ", siteoi,"  Year: ", yearoi,"  ID: ", idoi, "  Species: ", plant_sp))+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line = element_blank(),
        strip.background = element_rect(
          color=NA, fill="grey"
        )
  )+
  scale_x_date(date_labels = "%b",date_breaks  ="3 month")+
  theme(legend.position = "bottom")+
  guides(col=guide_legend(title=""))
  # ggtitle(paste0("Taxa: ",taxaoi," (Threshold: ",best_thres$direction, " ",best_thres$thres, ")"))
flower_freq_comp
# jpeg(paste0(output_path, "flower frequency compared with other data.jpg"),
     # height = 3200, width = 3200, res=300)
print(flower_freq_comp)
# dev.off()
