idoi <- "1"
flower_df <- get_doy(plant_taxa_df, thres_df_taxa,ts_df_subset, idoi)
# plant_sp<-plant_df %>% filter(id==idoi) %>% pull(species)
plant_df<-plant_taxa_df
ts_df<-ts_df_subset

p_1tree<-ggplot( )+
  geom_point(data=ts_df_subset %>% filter(id==idoi|id=="npn"|id=='pollen') %>% 
               filter(var %in% c("EVI (PS)","pollen count (NAB)", "flower observation (USA-NPN)", "flower observation (DT)")),
             aes(date, value,group=as.factor(id), col=var))+
  theme_classic()+
  guides(col=F,
         alpha=F)+
  scale_color_manual(values=cols)+
  facet_wrap(.~var, ncol=1, scales = "free_y")+
  xlab("Day of year")+
  ylab("Standardized value")+
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.line = element_blank(),
        strip.background = element_rect(
          color=NA, fill="grey"
        )
  )+
  scale_x_date(date_labels = "%b %d",date_breaks  ="3 month")
  # ggtitle(paste0("Site: ", siteoi,"  Year: ", yearoi,"  ID: ", idoi, "  Species: ", plant_sp))
if (nrow(flower_df)==0) {
  p_1tree
} else {
  p_1tree<-p_1tree+
    geom_vline(data=flower_df %>% 
                 mutate(start=as.Date(start, origin = "2017-01-01")), aes(xintercept = start), col="red", alpha=0.5)+
    geom_vline(data=flower_df%>% 
                 mutate(end=as.Date(end, origin = "2017-01-01")), aes(xintercept = end), col="red", alpha=0.5)+
    geom_vline(data=flower_df%>% 
                 mutate(doy=as.Date(doy, origin = "2017-01-01")), aes(xintercept = doy), col="red", alpha=0.1)
}

cairo_pdf("./agu.pdf", width = 6, height = 6)
jpeg("./agu.jpg",width = 6, height = 6, units = 'in', res = 300)
p_1tree
dev.off()
