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
  
  # set.seed(42)
  # fit <- greenbrown::FitDoubleLogBeck(px_evi_in_sm,  tout=1:365, plot=T, weighting = T,ninit = 50 )
  # param_ok1<- (fit$params["sos"]>=1 & fit$params["sos"]<=365 & fit$params["sos"]<fit$params["eos"]&fit$params["rsp"]>0  )
  # 
  # if (!param_ok1) {
  #   sos<-NA 
  # } else {
  #   sos<-fit$params["sos"]
  # }
  
  max_evi<-quantile(px_evi_in_sm[1:200], 0.99, na.rm = T)
  end_doy<-which(!is.na(px_evi_in_sm)&px_evi_in_sm>=max_evi)[1]
  min_evi<-quantile(px_evi_in_sm[1:end_doy], 0.01, na.rm = T)
  min_doy<-which(!is.na(px_evi_in_sm[1:end_doy])& px_evi_in_sm[1:end_doy]<=min_evi) %>% max()+1
  start_doy<-min_doy
  greenup_thres1<-(max_evi-min_evi)*0.4+min_evi
  greenup_thres2<-(max_evi-min_evi)*0.5+min_evi
  
  param_ok2<- (end_doy>start_doy)
  
  
  if (!param_ok2) {
    flower_doy<-start_doy<-end_doy<-NA
  } else {
    # if (length(start_doy:end_doy)>31) {
    #   set.seed(42)
    #   res1<-envcpt(px_evi_in_sm[start_doy:end_doy], models = c("trendcpt"),verbose = F, minseglen = 15)
    #   # plot(res1)
    #   slope_df_evi<-data.frame(cpt=res1$trendcpt@cpts,
    #                            slope=res1$trendcpt@param.est$beta[,2]) %>%
    #     mutate(diff=lead(slope)-slope) %>%
    #     mutate(cpt=cpt+start_doy-1) %>%
    #     mutate(grasspeak=(!is.na(diff)&diff< -0.001)& (slope>0) ) %>%
    #     mutate(aftergrasspeak=lag(grasspeak)) %>%
    #     filter(aftergrasspeak) %>%
    #     head(1)
    #   if (nrow(slope_df_evi)>0) {
    #     if (slope_df_evi$cpt<((end_doy-start_doy)/3+start_doy)) {
    #       start_doy<-slope_df_evi$cpt
    #     }
    #   }
    # }
    
    # if (length(start_doy:end_doy)>41) {
    #     set.seed(30)
    #     res1<-envcpt(px_evi_in_sm[start_doy:end_doy], models = c("trendcpt"),verbose = F, minseglen = 20)
    #     # plot(res1)
    #     slope_df_evi<-data.frame(cpt=res1$trendcpt@cpts,
    #                              slope=res1$trendcpt@param.est$beta[,2]) %>%
    #       mutate(diff=lead(slope)-slope) %>%
    #       # mutate(prev=lag(slope)) %>%
    #       mutate(cpt=cpt+start_doy-1) %>%
    #       # arrange(desc(diff)) %>%
    #       filter((!is.na(diff)&diff< -0.003)& (slope>0)) %>%
    #       # filter((!is.na(prev))& prev>0) %>%
    #       tail(1)
    #     if (nrow(slope_df_evi)>0) {
    #       # if (slope_df_evi$cpt>((end_doy-start_doy)*2/3+start_doy)) {
    #         end_doy<-slope_df_evi$cpt
    #       # }
    #     }
    #   }
    
    sos1<-which(px_evi_in_sm[start_doy:length(px_evi_in_sm)]>=greenup_thres1) %>% min()+start_doy-1
    sos2<-which(px_evi_in_sm[start_doy:length(px_evi_in_sm)]>=greenup_thres2) %>% min()+start_doy-1
    # start_doy=max((start_doy-10),1)
    # end_doy=end_doy+10
    data_density<-length(which (px_doy>=start_doy & px_doy<=end_doy))/(end_doy-start_doy+1)
    if (length(start_doy:end_doy)<=31 | data_density<1/7) {
      flower_doy<-NA
    } else {
      set.seed(42)
      res2<-envcpt(px_g2r_in_sm[start_doy:end_doy], models = c("trendcpt"),verbose = F, minseglen = 15)
      # plot(res2)
      
      if (length(res2$trendcpt@cpts)<3) {
        flower_doy<-NA
      } else {
        slope_df<-data.frame(cpt=res2$trendcpt@cpts,
                             slope=res2$trendcpt@param.est$beta[,2] ) %>%
          mutate(diff=lead(slope)-slope) %>%
          mutate(prevdiff=lag(diff)) %>%
          mutate(cpt=cpt+start_doy-1) %>%
          mutate(timediff=(cpt-sos1)) %>% #10 for DT
          # arrange(desc(diff)) %>%
          filter((!is.na(diff)&
                    diff< - 0.00
                  # diff>0.003
          )
          # & (!is.na(prevdiff)&prevdiff<0)
          )  
        
        flower_start<-slope_df %>%
          filter(timediff<0) %>%
          arrange(desc(timediff)) %>%
          head(1) %>%
          pull(cpt) 
        
        flower_end<-slope_df %>%
          filter(timediff>0) %>%
          arrange(timediff)%>%
          head(1) %>%
          pull(cpt) 
        if (length(flower_start)==0){
          flower_start<-NA
        }
        if (length(flower_end)==0){
          flower_end<-NA
        }
        flower_doy<-(flower_start+flower_end)/2
        
        # close_left<-px_doy[which(px_doy<=flower_doy) %>% rev() %>% head(1)]
        # close_right<-px_doy[which(px_doy>=flower_doy)%>% head(1)]
        # if ( close_right-close_left > 14) {
        #   flower_doy<-NA
        # }
      }
    }
  }
  
}

if (debug) {
  if (length(px_doy)>0) {
    p_1tree<-ggplot( )+
      # geom_vline(xintercept = cpt_doy, alpha=0.5, lty=2) +
      geom_point(data=ts_df_subset %>% filter(id==idoi|id=="npn"|id=='pollen'),
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
    # if (is.na(flower_doy)) {
    #   p_1tree
    # } else {
    #   p_1tree<-p_1tree+
    #     geom_vline(xintercept = flower_doy, col="red")
    # }
    if (is.na(flower_start)) {
      p_1tree
    } else {
      p_1tree<-p_1tree+
        geom_vline(xintercept = flower_start, col="red", alpha=0.5)
    }
    if (is.na(flower_end)) {
      p_1tree
    } else {
      p_1tree<-p_1tree+
        geom_vline(xintercept = flower_end, col="red", alpha=0.5)
    }
    if (is.na(sos)) {
      p_1tree
    } else {
      p_1tree<-p_1tree+
        geom_vline(xintercept = sos1, col="blue", lty=2)+
        geom_vline(xintercept = sos2, col="blue")
    }
    print(p_1tree)
    # cairo_pdf("./RS4flower/output/figures/time series for one tree.pdf",
    #           height = 6, width = 8)
    # print(p_1tree)
    # dev.off()
  }
}