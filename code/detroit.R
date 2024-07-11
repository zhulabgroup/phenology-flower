library(tidyverse)

detroit_df<-read_csv("/raid/users/ysong67/GitHub/phenology/RS4flower/data/Detroit_oak_pheno_obs_spring_2017.csv") 

id_table<-detroit_df %>% 
  distinct(tree) %>% 
  mutate(id=row_number())

detroit_df_ts<-detroit_df %>% 
  left_join(id_table, by="tree")%>% 
  left_join(trees_df %>% filter(site=="DV") %>% dplyr::select(id, lon, lat), by="id") %>% 
  left_join(detroit_df %>% 
              group_by(tree) %>% 
              arrange(julian_day) %>% 
              summarize(
                mindoy=min(julian_day),
                # peak=findpeaks(flowering_interp, sortstr = T)[1,2] %>% as.numeric(),
                thres=which(flowering_interp>=95) %>% min()
              ) %>% 
              # mutate(peak=peak+mindoy-1) %>%
              mutate(peak=thres+mindoy-1) %>%
              ungroup() %>%
              dplyr::select(tree, peak), by=c("tree")) %>% 
  # left_join(flower_doy_df %>% dplyr::select(id, start, end, flower_start, flower_end, doy, sos) %>% mutate(id=as.numeric(id)), by=c("id")) %>% 
  arrange(peak) %>% 
  mutate(id=as.character(id)) %>% 
  mutate(id=fct_inorder(id))

detroit_df_ts %>% 
  filter(id %in% ((.) %>% distinct(id) %>% pull(id) %>% sample(6))) %>% 
  ggplot()+
  geom_line(aes(x=julian_day, y=flowering_interp))+
  geom_vline(aes(xintercept=peak), col="purple")+
  facet_wrap(.~id, ncol=1)+
  theme_classic()
  

#####
detroit_df_ts2<-ts_df_subset%>%
  group_by(id, doy, var) %>% 
  summarise(value=mean(value)) %>% 
  ungroup() %>% 
  spread(key="var", value="value") %>%
  dplyr::select(id, julian_day=doy, evi=`EVI (PS)`, g2r=`G2R (PS)`) %>% 
  mutate(id=fct_relevel(id, levels=levels(detroit_df_ts$id)))

id_select<-id_table %>% pull(id) %>% unique() %>% as.character()
id_select<-flower_doy_df %>% drop_na(doy) %>% pull(id)
id_select<-sample(id_select,20)
ggplot(detroit_df %>% left_join(id_table, by="tree") %>% filter(id %in% id_select))+
  geom_line(aes(x=julian_day, y=flowering_interp, group=tree, col=tree))+
  theme_classic()

ggplot(detroit_df_ts%>% 
         filter(id %in% id_select))+
  geom_line(aes(x=julian_day, y=flowering_interp/100, col=peak))+
  facet_wrap(.~id)+
  # geom_vline(aes(xintercept = doy), col="purple")+
  # geom_vline(aes(xintercept =start), col="blue", alpha=0.5)+
  # geom_vline(aes(xintercept =end), col="blue", alpha=0.5)+
  # geom_vline(aes(xintercept = flower_start), col="purple", alpha=0.5)+
  # geom_vline(aes(xintercept = flower_end), col="purple", alpha=0.5)+
  geom_vline(aes(xintercept = sos), col="blue")+
  geom_point(data=detroit_df_ts2%>% 
              filter(id %in% id_select), aes(x=julian_day, y=evi), col="dark green" )+
  geom_point(data=detroit_df_ts2%>% 
              filter(id %in% id_select), aes(x=julian_day, y=g2r), col="yellow green" )+
  # geom_point(data=ts_df_subset %>% filter(var=="G2R (PS)")%>% filter(id %in% id_select),aes(x=doy, y=value), col="yellow green" )+
  theme_classic()+
  scale_color_viridis_c()+
  guides(col=F)+
  xlim(40, 200)

library(pracma)
detroit_df_annual<-detroit_df %>% 
  group_by(tree, Species) %>% 
  arrange(julian_day) %>% 
  summarize(
            mindoy=min(julian_day),
            peak=findpeaks(flowering_interp, sortstr = T)[1,2] %>% as.numeric(),
            thres=which(flowering_interp>=90) %>% min()
  ) %>% 
  mutate(peak=peak+mindoy-1) %>%
  mutate(thres=thres+mindoy-1) %>%
  # dplyr::select(-mindoy) %>% 
  ungroup()%>%
  left_join(id_table, by="tree") #%>% 
  # left_join(flower_doy_df%>% mutate(id=as.numeric(id)),by="id") %>%
  # drop_na(peak, doy, sos) %>%
  # mutate(doy=doy)
detroit_df_annual %>% nrow()

offset=median(flower_doy_df$sos-flower_doy_df$doy, na.rm = T)
# offset=Mode(flower_doy_df$sos-flower_doy_df$doy, na.rm = T)
ggplot(flower_doy_df)+
  geom_jitter(aes(x=sos-offset, y=doy), alpha=0.5)+
  theme_classic()+
  geom_abline(slope=1, intercept = 0, alpha=0.5, col="red")+
  coord_equal()

# ggplot(detroit_df_annual %>% 
#          dplyr::select(tree, id, lat, lon, g2r=doy, peak, sos) %>%  
#          drop_na(g2r, peak, sos) %>% 
#          rowwise() %>% 
#          mutate(lat=lat+rnorm(1, sd=0.005),
#                 lon=lon+rnorm(1, sd=0.005)) %>% 
#          ungroup() %>% 
#          gather(key="group", value="value", -tree, -id, -lat, -lon))+
#   geom_point(aes(x=lon, y=lat, col=value), cex=3, alpha=0.6)+
#   scale_color_viridis_c()+
#   theme_classic()+
#   facet_wrap(.~group, nrow=1)

ggplot(detroit_df_annual %>% mutate(sos=sos-offset) %>% dplyr::select(id, peak,g2r=doy,sos) %>% gather(key = "method", value="doy", -id, -peak))+
  geom_jitter(aes(x=peak, y=doy), alpha=0.5)+
  # geom_label_repel(aes(x=peak, y=doy, label=id))+
  geom_smooth(aes(x=peak, y=doy), method = "lm")+
  theme_classic()+
  geom_abline(slope=1, intercept = 0, alpha=0.5, col="red")+
  coord_equal()+
  facet_wrap(.~method, nrow=1)

cor.test(detroit_df_annual$doy, detroit_df_annual$peak,method = "spearman")
cor.test(detroit_df_annual$doy, detroit_df_annual$peak,method = "pearson")

cor.test(detroit_df_annual$sos, detroit_df_annual$peak,method = "spearman")
cor.test(detroit_df_annual$sos, detroit_df_annual$peak,method = "pearson")

t.test(detroit_df_annual$doy, detroit_df_annual$peak, paired = T)
t.test(detroit_df_annual$sos, detroit_df_annual$peak, paired = T)

flower_doy_df %>% mutate(flower_dur=flower_end-flower_start) %>% pull(flower_dur) %>% median(na.rm=T)







library(tidyverse)

detroit_df<-read_csv("/raid/users/ysong67/GitHub/phenology/RS4flower/data/Detroit_oak_pheno_obs_spring_2017.csv") 

id_table<-detroit_df %>% 
  distinct(tree) %>% 
  mutate(id=row_number())

id_select<-c("6","50","81", "108")
id_select<-unique(id_table$id)
ggplot(detroit_df %>% left_join(id_table, by="tree") %>% filter(id %in% id_select))+
  geom_line(aes(x=julian_day, y=flowering_interp, group=tree, col=tree))+
  theme_classic()

# flower_freq_df_standard %>% filter(siteoi=="DT")%>% filter(yearoi==2017)

flower_doy<-flower_doy_df %>% filter(id==idoi) %>% pull(doy)

if (!is.na(flower_doy)) {
  p_detroit+
    geom_vline(xintercept = flower_doy)
}

ggplot()+
  geom_line(data=detroit_df %>% left_join(id_table, by="tree")%>% filter(id %in% id_select),
            aes(x=julian_day, y=flowering_interp/100, col=as.factor(id)))+
  geom_vline(data=flower_doy_df%>% filter(id %in% id_select),aes(xintercept = doy, col=as.factor(id)))+
  # geom_vline(data=flower_doy_df,aes(xintercept = end))+
  # geom_vline(data=flower_doy_df,aes(xintercept = doy))+
  geom_line(data=ts_df_subset %>% filter(var=="EVI (PS)")%>% filter(id %in% id_select),aes(x=doy, y=value), col="dark green" )+
  geom_line(data=ts_df_subset %>% filter(var=="G2R (PS)")%>% filter(id %in% id_select),aes(x=doy, y=value), col="yellow green" )+
  # geom_point(data=ts_df_subset %>% filter(var=="G2R (PS)")%>% filter(id %in% id_select),aes(x=doy, y=value), col="yellow green" )+
  theme_classic()+
  guides(col=F)+
  facet_wrap(.~as.numeric(id))+
  xlim(50, 200)
