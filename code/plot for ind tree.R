plot_tree<-function (plant_df, ts_df, flower_df, idoi)  {
  plant_sp<-plant_df %>% filter(id==idoi) %>% pull(species)
  
    p_1tree<-ggplot( )+
      geom_point(data=ts_df_subset %>% filter(id==idoi|id=="npn"|id=='pollen') %>% 
                   filter(var %in% c("EVI (PS)","pollen count (NAB)", "flower observation (USA-NPN)", "flower observation (DT)")),
                 aes(doy, value,group=as.factor(id), col=var))+
      theme_classic()+
      guides(col=F,
             alpha=F)+
      scale_color_manual(values=cols)+
      facet_wrap(.~var, ncol=1, scales = "free_y")+
      xlab("day of year")+
      ylab("")+
      ggtitle(paste0("Site: ", siteoi,"  Year: ", yearoi,"  ID: ", idoi, "  Species: ", plant_sp))
    if (nrow(flower_df)==0) {
      p_1tree
    } else {
      p_1tree<-p_1tree+
        geom_vline(data=flower_df, aes(xintercept = start), col="red", alpha=0.8)+
        geom_vline(data=flower_df, aes(xintercept = end), col="red", alpha=0.8)+
        geom_vline(data=flower_df, aes(xintercept = doy), col="red", alpha=0.3)
    }
    
    # print(p_1tree)
    # cairo_pdf("./RS4flower/output/figures/time series for one tree.pdf",
    #           height = 6, width = 8)
    # print(p_1tree)
    # dev.off()
  
  return (p_1tree)
}

 