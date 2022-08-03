set.seed(3)
idoi<-ts_df_subset %>% pull(id) %>% unique() %>% sample(4)

flower_df<-vector(mode="list")
for (i in 1:length(idoi)) {
  flower_df[[i]]<- get_doy(plant_taxa_df, thres_df_taxa,ts_df_subset, idoi[i])
}
flower_df<-bind_rows(flower_df)


p_dt_tree<-ggplot() +
  geom_point(data=ts_df_subset %>%
               filter(var=="EVI (PS)"|var=="flower observation (DT)") %>% 
               group_by(id, var) %>% 
               mutate(value_st=(value-min(value, na.rm = T))/(max(value, na.rm = T)-min(value, na.rm = T))) %>%
               ungroup() %>% 
               filter(id %in% idoi ) ,
             aes(x = date, y = value_st, col = var)) +
  # geom_vline(data=flower_df %>% 
  #              mutate(start=as.Date(start, origin = "2017-01-01")), aes(xintercept = start), col="red", alpha=0.5)+
  # geom_vline(data=flower_df%>% 
  #              mutate(end=as.Date(end, origin = "2017-01-01")), aes(xintercept = end), col="red", alpha=0.5)+
  geom_vline(data=flower_df%>% 
               mutate(doy=as.Date(doy, origin = "2017-01-01")), aes(xintercept = doy), col="dark green", alpha=0.2)+
  geom_vline(data=flower_df%>%
               filter(thres==0.5) %>% 
               mutate(doy=as.Date(doy, origin = "2017-01-01")), aes(xintercept = doy), col="dark green", alpha=0.8)+
  facet_wrap(. ~ id, ncol = 1, scales = "free_y") +
  scale_color_manual(values = cols) +
  xlab("Day of year")+
  ylab("Standardized value")+
  # ggtitle(paste0("Site: ", siteoi,"  Year: ", yearoi,"  ID: ", idoi, "  Species: ", plant_sp))+
  theme_classic()+
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
  


cairo_pdf("./dt.pdf", width = 10, height = 8)
grid.arrange(annotate_figure(p_dt_tree, fig.lab = "A"),
             annotate_figure(p_dt_corr_2017, fig.lab = "B"),
             annotate_figure(p_dt_corr_years, fig.lab = "C"),
             layout_matrix=rbind(c(1,2),
                                 c(1,3)),
             widths = 3:2
)
dev.off()
