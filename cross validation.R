for (taxaoi in taxa_list) {
  taxaoi_short<-str_split(taxaoi, " ", simplify = T)[1]
  flower_window<-seq(flower_window_df %>% filter(taxa==taxaoi) %>% pull(start),
                     flower_window_df %>% filter(taxa==taxaoi) %>% pull(end),
                     by=1)
  # if (taxaoi %in% c("Ambrosia", "Ulmus late")) {
  #   thres_df_taxa<-thres_df %>% filter(direction=="down")
  # } else if (taxaoi== "Poaceae early"  ) {
  #   thres_df_taxa<-thres_df %>% filter(threshold>=0.5|direction=="up")
  # } else if (taxaoi== "Poaceae late"  ) {
  #   thres_df_taxa<-thres_df %>% filter(threshold>=0.5|direction=="down")
  # } else {
  #   thres_df_taxa<-thres_df %>% filter(direction=="up")
  # }
  
  output_path<-paste0("/raid/users/ysong67/GitHub/phenology/RS4flower/output/data/", taxaoi,"/")
  
  # read in leafing phenology data
  # flower_doy_df<-read_rds(paste0(output_path,"flowering day of year.rds" ))
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
  
  # flower_freq_df_check
  flower_freq_df_check<-read_rds(paste0(output_path,"accuracy check.rds") )
  
  best_thres<-flower_freq_df_check %>% 
    group_by(direction, thres) %>% 
    summarise(rmse=median(rmse)) %>% # median rmse for each threshold
    arrange(rmse) %>% 
    head(1) %>% # keep threshold giving the smallest median rmse
    dplyr::select(direction,thres)
  
  accuracy_df<-flower_freq_df_check %>% 
    right_join(best_thres, by=c("direction", "thres")) %>% 
    left_join(meta_df %>% dplyr::select(site, sitename), by=c("region"="site")) %>% 
    mutate(rmse_cv=NA)
  
  site_valid_list<-flower_freq_df_check %>% pull(region) %>% unique()
  flower_freq_df_standard_best_list_cv<-vector(mode="list")
  # leave one city out
  for (siteout in site_valid_list) {
    best_thres_cv<-flower_freq_df_check %>% 
      filter(region!=siteout) %>% 
      group_by(direction, thres) %>% 
      summarise(rmse=median(rmse)) %>% # median rmse for each threshold
      arrange(rmse) %>% 
      head(1) %>% # keep threshold giving the smallest median rmse
      dplyr::select(direction,thres)
    
    accuracy_df_cv<-flower_freq_df_check %>% 
      right_join(best_thres_cv, by=c("direction", "thres")) %>% 
      left_join(meta_df %>% dplyr::select(site, sitename), by=c("region"="site"))
    
    lag_clim_df_cv<-accuracy_df_cv %>% 
      dplyr::select(-rmse, -rmse_ps, -rmse_clim) %>% 
      left_join(chelsa_df, by=c("region"="site")) %>% 
      mutate(lag_new=case_when(region!=siteout~lag))
    
    lm.model<-lm(lag_new ~ mat, data = lag_clim_df_cv) 
    
    lag_clim_df_cv<-lag_clim_df_cv %>% 
      mutate(lag_fit=predict(lm.model, lag_clim_df_cv))
    
    lag_fit<-lag_clim_df_cv %>% filter(region==siteout) %>% pull(lag_fit) %>% round(0)
    # getting time series with new lag
    flower_freq_df_standard_best_cv<-flower_freq_df_standard %>% 
      filter(site==siteout) %>% 
      filter(direction==best_thres_cv$direction,
             thres == best_thres_cv$thres) %>%
      ungroup() %>%
      group_by(site, year)
    
    if (lag_fit<0) {
      flower_freq_df_standard_best_cv<- flower_freq_df_standard_best_cv %>% mutate(freq_sm=lead(freq_sm,n=-lag_fit)) %>% mutate(freq_sm=replace_na(freq_sm, 0))
    } else if (lag_fit==0) {
      flower_freq_df_standard_best_cv<- flower_freq_df_standard_best_cv %>% mutate(freq_sm=replace_na(freq_sm, 0))
    } else if (lag_fit>0) {
      flower_freq_df_standard_best_cv<- flower_freq_df_standard_best_cv %>% mutate(freq_sm=lag(freq_sm,n=lag_fit)) %>% mutate(freq_sm=replace_na(freq_sm, 0))
    }  
    
    pollen_ts_cv<- flower_freq_df_standard_best_cv %>% pull (pollen) # pollen count, for calculating accuracy
    freq_ts_lag_cv<- flower_freq_df_standard_best_cv %>% pull (freq_sm)
    rmse_cv<-sqrt(mean((freq_ts_lag_cv-pollen_ts_cv)^2, na.rm=T)) # rmse between remotely-sensed flowering phenology and pollen count
    
    accuracy_df[accuracy_df$region==siteout,]$rmse_cv<-rmse_cv
    flower_freq_df_standard_best_list_cv[[siteout]]<-flower_freq_df_standard_best_cv
  }
  flower_freq_df_standard_best_cv<-bind_rows(flower_freq_df_standard_best_list_cv)
  
  # make plots
  flower_freq_comp<-ggplot(flower_freq_df_standard_best_cv)+
    geom_point(aes(x=doy, y=npn, col="flower observation (USA-NPN)"), alpha=0.5)+
    geom_point(aes(x=doy, y=pollen, col="pollen count (NAB)"))+
    geom_line(aes(x=doy, y=pollen_clim, col="pollen count (NAB)"),alpha=0.5, lwd=1)+
    geom_point(aes(x=doy, y=evi, col="EVI (PS)"), alpha=0.2)+
    geom_line(aes(x=doy, y=freq_sm, col="flowering frequency"), lwd=1)+
    theme_classic()+
    facet_wrap(.~sitename*year, ncol=4)+
    scale_color_manual(values=cols)+
    theme(legend.position="bottom")+
    theme(legend.title = element_blank())+
    ylab("")+
    ggtitle(paste0("Taxa: ",taxaoi))
  flower_freq_comp
  
  jpeg(paste0(output_path, "flower frequency compared with other data_cv.jpg"),
       height = 3200, width = 3200, res=300)
  print(flower_freq_comp)
  dev.off()
  
  flower_freq_corr<-ggplot(flower_freq_df_standard_best_cv %>% mutate(year=as.factor(year)))+
    geom_point(aes(x=freq_sm, y=pollen , group=year, col=year))+
    geom_smooth(aes(x=freq_sm, y=pollen , group=year, col=year), method="lm", se=F, lwd=0.5)+
    geom_smooth(aes(x=freq_sm, y=pollen), method="lm")+
    theme_classic()+
    facet_wrap(.~sitename, ncol=4)+
    coord_equal()+
    xlim(0, 1)+
    ylim(0, 1)+
    ylab("pollen count^(1/2)")+
    xlab("flowering frequency")
  flower_freq_corr
  jpeg(paste0(output_path, "flower frequency and pollen count correlation_cv.jpg"),
       height = 2400, width = 3200, res=300)
  print(flower_freq_corr)
  dev.off()
  
  across_site_and_year<-ggplot(flower_freq_df_standard_best_cv %>%  
                                 mutate(year=as.factor(year)))+
    geom_point(aes(x=doy, y=pollen , group=year, col=year))+
    geom_line(aes(x=doy, y=freq_sm, group=year, col=year))+
    facet_wrap(.~sitename, ncol=1)+
    theme_classic()+
    ylab("")+
    ggtitle(paste0("Taxa: ",taxaoi))
  across_site_and_year
  
  jpeg(paste0(output_path, "phenology across site and year_cv.jpg"),
       height = 3200, width = 3200, res=300)
  print(across_site_and_year)
  dev.off()
  
  write_rds(accuracy_df,paste0(output_path,"accuracy check_cv.rds") )
}
