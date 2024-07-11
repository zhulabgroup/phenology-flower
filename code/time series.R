get_time_series_plot<-function (siteoi) {
  siteoi<<-siteoi
  trees_genus_df<-trees_df %>% 
    filter(site==siteoi) %>% 
    filter(genus==taxaoi) %>% 
    mutate(id=row_number())
  
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
  
  if (siteoi=="NY") {
    start_doy<- 80
    end_doy<- 160
    yearoi<-2018
  }
  if (siteoi=="SJ") {
    start_doy<- 30
    end_doy<- 180
    yearoi<-2018
  }
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
  
  if (siteoi=="NY") {
    cpt_doy<-c(106, 119, 125, 135)
  }
  if (siteoi=="SJ") {
    cpt_doy<-c(77, 100, 138)
  }
  
  cols <- c("EVI (PS)" = "dark green", "G2R (PS)" = "yellow green", "pollen count (NAB)" = "dark red", "flower observation (USA-NPN)" = "dark orchid")
  p_time_series<-ggplot( )+
    geom_vline(xintercept = cpt_doy, alpha=0.5, lty=2) +
    geom_point(data=ts_df_subset,
               aes(doy, value,group=as.factor(id), col=var, alpha=alpha))+
    # geom_smooth( data=ts_df_subset_summary,
    #              aes(doy, q1),alpha=1,
    #              span=0.2, method="loess",se=F)+
    geom_smooth( data=ts_df_subset_summary,
                 aes(doy, q2),alpha=1,
                 span=0.2, method="loess",se=F)+
    # geom_smooth( data=ts_df_subset_summary,
    #              aes(doy, q3),alpha=1,
    #              span=0.2, method="loess",se=F)+
    theme_classic()+
    facet_wrap(.~var, ncol=1, scales = "free_y")+
    guides(col=F,
           alpha=F)+
    scale_color_manual(values=cols)+
    xlab("day of year")+
    ylab("")
  
  return(p_time_series)
}


p_time_series_ny<-get_time_series_plot(siteoi="NY")+ggtitle("New York")
p_time_series_sj<-get_time_series_plot(siteoi="SJ")+ggtitle("San Jose")

library(gridExtra)
library(ggpubr)

cairo_pdf("./RS4flower/output/figures/map conceptual and time series.pdf")
print (
  grid.arrange(
    annotate_figure(p_pollen_map, fig.lab = "a", fig.lab.face = "bold"),
    p_tree_genus_ny,
    p_tree_genus_sj,
    annotate_figure(p_conceptual, fig.lab = "b", fig.lab.face = "bold"),
    annotate_figure(p_time_series_ny, fig.lab = "c", fig.lab.face = "bold"),
    p_time_series_sj,
    layout_matrix = rbind(c(1, 1, 4, 4),
                          c(2, 3, 4, 4),
                          c(5, 5, 6, 6),
                          c(5, 5, 6, 6))
  )
)
dev.off()