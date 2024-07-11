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
  
  output_path<-paste0("./RS4flower/output/data/", taxaoi,"/")
  
  flower_doy_df<-read_rds(paste0(output_path,"flowering day of year.rds" ))
  flower_freq_df<-read_rds(paste0(output_path,"flowering frequency.rds" ) )
  
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
  
  flower_freq_df_check_list<-vector(mode="list", length=length(site_list))
  # best_thres<-rep(NA, length(regions))
  # names(best_thres)<-regions
  # best_lag<-rep(NA, length(regions))
  # names(best_lag)<-regions
  
  for (r in 1:length(site_list)) {
    regionoi<-site_list[r]
    flower_freq_df_check_region_list<-vector(mode="list", length=nrow(thres_df_taxa))
    
    for (t in 1:nrow(thres_df_taxa)) {
      flower_freq_df_standard_subset<-flower_freq_df_standard %>% 
        filter(site==regionoi) %>%
        filter(direction==thres_df_taxa$direction[t],
               thres==thres_df_taxa$threshold[t]) %>% 
        group_by(site, year) %>% 
        ungroup()
      sample_size<-flower_freq_df_standard_subset %>% filter(!is.na(pollen), pollen>0) %>% nrow()
      if (sample_size>=5*4) {
        pollen_sm_ts<- flower_freq_df_standard_subset %>% pull (pollen_sm)
        pollen_ts<- flower_freq_df_standard_subset %>% pull (pollen)
        
        pollen_clim_ts<- flower_freq_df_standard_subset %>% pull (pollen_clim)
        rmse_clim<-sqrt(mean((pollen_clim_ts-pollen_ts)^2, na.rm=T))
        
        lags_list<- -200:200
        flower_freq_df_check_thres_list<-
          foreach (l = 1:length(lags_list),
                   .packages = c("tidyverse")) %dopar% {
                     lag=lags_list[l]
                     if (lag<0) {
                       flower_freq_df_standard_subset_lag<- flower_freq_df_standard_subset %>% mutate(freq_sm=lead(freq_sm,n=-lag))  %>% mutate(freq_sm=replace_na(freq_sm, 0))
                     } else if (lag==0) {
                       flower_freq_df_standard_subset_lag<-flower_freq_df_standard_subset  %>% mutate(freq_sm=replace_na(freq_sm, 0))
                     } else if (lag>0) {
                       flower_freq_df_standard_subset_lag<-flower_freq_df_standard_subset %>% mutate(freq_sm=lag(freq_sm,n=lag)) %>% mutate(freq_sm=replace_na(freq_sm, 0))
                     }
                     freq_ts_lag<- flower_freq_df_standard_subset_lag %>% pull (freq_sm)
                     # ggplot(flower_freq_df_standard_subset_lag)+
                     #   geom_point(aes(x=doy, y=pollen), col="dark red")+
                     #   geom_line(aes(x=doy, y=freq_sm), col="blue")+
                     #   facet_wrap(.~siteoi*yearoi)+
                     #   theme_classic()
                     rmse<-sqrt(mean((freq_ts_lag-pollen_sm_ts)^2, na.rm=T))
                     # rmse_list[[l]]<-rmse
                     # freq_ts_lag_list[[l]]<-freq_ts_lag
                     rmse_ps<-sqrt(mean((freq_ts_lag-pollen_ts)^2, na.rm=T))
                     
                     print(paste0("region ", r, ", year ", y, ", threshold ", t, ", lag ", l))
                     
                     data.frame(direction=thres_df_taxa$direction[t],thres=thres_df_taxa$threshold[t],lag=lag, rmse=rmse,rmse_ps=rmse_ps, rmse_clim=rmse_clim)
                     # res<-cor.test(freq_ts_lag,pollen_ts , na.rm=T, method="spearman")
                     # spearman<-res$estimate^2
                     # res<-cor.test(freq_ts_lag,pollen_ts, na.rm=T, method="pearson")
                     # pearson<-res$estimate^2
                     # rmse<-sqrt(mean((freq_ts_lag-pollen_ts)^2, na.rm=T))
                   }
        flower_freq_df_check_region_list[[t]]<-bind_rows(flower_freq_df_check_thres_list) %>% 
          arrange(rmse) %>% 
          head(1)
      } else {
        flower_freq_df_check_region_list[[t]]<-data.frame(thres=numeric(0),lag=numeric(0), rmse=numeric(0), rmse_ps=numeric(0),rmse_clim=numeric(0))
      }
    }
    flower_freq_df_check_list[[r]]<-bind_rows(flower_freq_df_check_region_list) %>% 
      mutate(region=regionoi)  %>%
      arrange((rmse)) 
    # if (nrow(flower_freq_df_check_list[[r]])>0) {
    #   best_thres[r]<-flower_freq_df_check_list[[r]] %>% arrange((rmse)) %>% head(1) %>% pull(thres)
    #   best_lag[r]<-flower_freq_df_check_list[[r]] %>% arrange((rmse)) %>% head(1) %>% pull(lag)
    # }
  }
  flower_freq_df_check<-bind_rows(flower_freq_df_check_list)
  flower_freq_df_check
  write_rds(flower_freq_df_check,paste0(output_path,"accuracy check.rds") )
  
  best_thres<-flower_freq_df_check %>% group_by(direction, thres) %>% summarise(rmse=median(rmse)) %>% arrange(rmse) %>% head(1) %>% dplyr::select(direction,thres)
  
  flower_freq_df_check %>% filter(direction==best_thres$direction[1],
                                  thres==best_thres$thres[1])
  # flower_doy_map<-vector(mode="list")
  # for (s in 1:length(site_list)) {
  #   flower_doy_df_subset<-flower_doy_df %>% 
  #     filter(yearoi>2017) %>%
  #     drop_na(start, end) %>% 
  #     filter(doy<200)%>% 
  #     filter(siteoi==site_list[s]) %>% 
  #     filter(thres %in% c(best_thres[region]))
  #   if (nrow(flower_doy_df_subset)>0) {
  #     flower_doy_map[[s]]<-ggplot(flower_doy_df_subset )+
  #       geom_point(aes(x=lon, y=lat, col=doy), cex=1, alpha=0.6)+
  #       scale_color_viridis_c()+
  #       theme_classic()+
  #       facet_wrap(.~sitename*yearoi, nrow=1)+
  #       coord_equal()
  #   }
  #   
  # } 
  # pdf(paste0(output_path, "flower day of year map.pdf"),
  #     height = 6, width = 16)
  # print(flower_doy_map)
  # dev.off()
  
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
  flower_freq_df_standard_best<-bind_rows(flower_freq_df_standard_best_list)
  
  flower_freq_comp<-ggplot(flower_freq_df_standard_best)+
    geom_point(aes(x=doy, y=npn, col="flower observation (USA-NPN)"), alpha=0.5)+
    geom_point(aes(x=doy, y=pollen, col="pollen count (NAB)"))+
    geom_line(aes(x=doy, y=pollen_clim, col="pollen count (NAB)"),alpha=0.5, lwd=1)+
    geom_point(aes(x=doy, y=evi, col="EVI (PS)"), alpha=0.2)+
    geom_line(aes(x=doy, y=freq_sm, col="flowering frequency"), lwd=1)+
    theme_classic()+
    facet_wrap(.~paste0(sitename, " (Lag: ", lag, ")")*year, ncol=4)+
    scale_color_manual(values=cols)+
    theme(legend.position="bottom")+
    theme(legend.title = element_blank())+
    ylab("")+
    ggtitle(paste0("Taxa: ",taxaoi," (Threshold: ",best_thres$direction, " ",best_thres$thres, ")"))
  flower_freq_comp
  cairo_pdf(paste0(output_path, "flower frequency compared with other data.pdf"),
            height = 16, width = 16)
  print(flower_freq_comp)
  dev.off()
  
  flower_freq_corr<-ggplot(flower_freq_df_standard_best %>% mutate(year=as.factor(year)))+
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
  cairo_pdf(paste0(output_path, "flower frequency and pollen count correlation.pdf"),
            height = 6, width = 8)
  print(flower_freq_corr)
  dev.off()
  
  across_site_and_year<-ggplot(flower_freq_df_standard_best %>%  
                                 mutate(year=as.factor(year)))+
    geom_point(aes(x=doy, y=pollen , group=year, col=year))+
    geom_line(aes(x=doy, y=freq_sm, group=year, col=year))+
    facet_wrap(.~paste0(sitename, " (Lag: ", lag, ")"), ncol=1)+
    theme_classic()+
    ylab("")+
    ggtitle(paste0("Taxa: ",taxaoi," (Threshold: ",best_thres$direction, " ",best_thres$thres, ")"))
  across_site_and_year
  
  cairo_pdf(paste0(output_path, "phenology across site and year.pdf"),
            height = 6, width = 8)
  print(across_site_and_year)
  dev.off()
}
