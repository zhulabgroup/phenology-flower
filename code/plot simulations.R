waves<-function (t, t_start,
                 intercept, slope, 
                 amplitude1, phase1, period1,
                 amplitude2, phase2, period2,
                 sd=0.05) {
  # d<-as.integer(format(t, "%j"))
  t_diff<-as.numeric(t-t_start)
  if (leap_year(t)) {d_all<-366} else {d_all<-365}
  v <- intercept + slope* t_diff+
    amplitude1 * sin(2*pi/(period1*d_all) *(t_diff+phase1)) + 
    amplitude2 * sin(2*pi/(period2*d_all) *(t_diff+phase2)) + 
    rnorm (1,0,sd)
  
  return (v)
}

env_to_param<-function (env, lower, upper, steepness, midpoint) {
  param<-(upper-lower)/(1+exp(-steepness*(env-midpoint)))+lower
  return(param)
}

temp_sd<-0.01
pheno_sd <- 0.02


param_list<-c("m8", "m2","m3","m4")

date_list<-seq(as.Date("2021-01-01"),as.Date("2040-12-31"), by=1)
midyear<-floor(mean(c(as.numeric(format(min(date_list), "%Y")),as.numeric(format(max(date_list), "%Y")))))


param_name<-list("m2"="summer-winter difference",
                 "m3"="timing of spring onset",
                 "m4"="slope of curve in spring",
                 "m8"="pace of life cycles")
pheno_name<-list("m2"="net primary productivity",
                 "m3"="bird breeding activity",
                 "m4"="enhanced vegetation index",
                 "m8"="adult insect abundance")
env_name<-list("m2"=expression('T'['first 90 days in same year']),
               "m3"=expression('T'['last 90 days in previous year']),
               "m4"=expression('T'['first 40 days in same year']),
               "m8"=expression('T'['all days in same year']))

cl <- makeCluster(5, outfile = "")
registerDoSNOW(cl)

for (p in 1:length(param_list)) {
  param<-param_list[p]
  
  set.seed(42)
  
  date_list<-seq(as.Date("2021-01-01"),as.Date("2040-12-31"), by=1)
  midyear<-floor(mean(c(as.numeric(format(min(date_list), "%Y")),as.numeric(format(max(date_list), "%Y")))))
  coord_df <- data.frame(lon=0, lat=1:5)
  
  distMat<-matrix(c(0,1,2,3,4,
                    1,0,1,2,3,
                    2,1,0,1,2,
                    3,2,1,0,1,
                    4,3,2,1,0), 
                  nrow=5)
  
  ts_all<-
    foreach (s = 1:nrow(coord_df),
             .packages = c("lubridate", "tidyverse")
    ) %dopar% {
      # library(lubridate, lib.loc = "/usr/lib64/R/library")
      # library(tidyverse, lib.loc = "/usr/lib64/R/library")
      temp<-rep(NA, length(date_list))
      for (i in 1:length(date_list)) {
        date<-date_list[i]
        temp[i]<-waves(t=date,
                       t_start=date_list[1],
                       intercept=0.3+0.2*coord_df$lat[s],
                       slope = 0.0001,
                       amplitude1 =0.8,
                       phase1 =0,
                       period1=1,
                       amplitude2 = 0.5,
                       phase2 = 0,
                       period2 = 5,
                       sd = temp_sd
        )
        # print(i)
      }
      ts_site<-data.frame(date=date_list, temp=temp, temp_sd=temp_sd)%>% 
        mutate(year=as.numeric(format(date, "%Y"))) %>% 
        mutate(site=s)
      ts_site
    }
  ts_all<-bind_rows(ts_all)
  
  param_all<-
    foreach (s = 1:nrow(coord_df)) %dopar% {
      if (param == "m2") {
        param_site<-ts_all %>% 
          filter(site==s) %>% 
          dplyr::select(temp,year) %>% 
          group_by(year) %>% 
          slice_head(n=90) %>% 
          summarize(temp_summ=mean(temp)) %>% 
          ungroup() %>% 
          mutate(param=env_to_param (env=temp_summ,
                                     lower=0.2,
                                     upper=1,
                                     steepness=-3,
                                     midpoint=1.5)) %>% 
          mutate(param_mis = case_when((year>midyear)~param+0.1)) %>%
          mutate(site=s)
      }
      if (param == "m3") {
        param_site<-ts_all %>% 
          filter(site==s) %>% 
          dplyr::select(temp,year) %>% 
          group_by(year) %>% 
          slice_tail(n=90) %>% # last 90 days
          summarize(temp_summ=mean(temp)) %>% 
          ungroup() %>% 
          mutate(temp_summ=lag(temp_summ)) %>% 
          mutate(param=env_to_param (env=temp_summ,
                                     lower=40,
                                     upper=120,
                                     steepness=-2,
                                     midpoint=1)) %>% 
          mutate(param_mis = case_when((year>midyear)~param+20)) %>%
          mutate(site=s)
      }
      if (param == "m4") {
        param_site<-ts_all %>% 
          filter(site==s) %>% 
          dplyr::select(temp,year) %>% 
          group_by(year) %>% 
          slice_head(n=14)  %>% 
          summarize(temp_summ=mean(temp)) %>% 
          ungroup() %>% 
          # mutate(temp_summ=lag(temp_summ)) %>%
          mutate(param=env_to_param (env=temp_summ,
                                     lower=10,
                                     upper=80,
                                     steepness=3.5,
                                     midpoint=1.2)) %>% 
          mutate(param_mis = case_when((year>midyear)~param/2)) %>%
          mutate(site=s)
      }
      if (param == "m5") {
        param_site<-ts_all %>% 
          filter(site==s) %>% 
          dplyr::select(temp,year) %>% 
          group_by(year) %>% 
          slice_head(n=120) %>% 
          summarize(temp_summ=mean(temp)) %>% 
          ungroup() %>% 
          mutate(param=env_to_param (env=temp_summ,
                                     lower=180,
                                     upper=280,
                                     steepness=-1,
                                     midpoint=0.8)) %>% 
          mutate(param_mis = case_when((year>midyear)~param+0.2)) %>%
          mutate(site=s)
      }
      if (param =="m8") {
        param_site<-ts_all %>% 
          filter(site==s) %>% 
          dplyr::select(temp,year) %>% 
          group_by(year) %>% 
          # slice_head(n=180) %>% 
          summarize(temp_summ=mean(temp)) %>% 
          ungroup() %>% 
          mutate(param=env_to_param (env=temp_summ,
                                     lower=1,
                                     upper=1.5,
                                     steepness=2,
                                     midpoint=1)) %>% 
          mutate(param_mis = case_when((year>midyear)~param-0.1)) %>%
          mutate(site=s)
      }
      param_site
    }
  param_all<-bind_rows(param_all)
  
  ts_df<-read_csv(paste0("/raid/users/ysong67/GitHub/pheno_review/simulations/output/",param,".csv"),
                            col_types=list(pheno_mis=col_double(),
                                           mismatch_actual=col_double(),
                                           mismatch_model=col_double(),
                                           mismatch_model_upper=col_double(),
                                           mismatch_model_lower=col_double())) %>% 
    mutate(param=param) %>% 
    rowwise() %>% 
    mutate(param_v=param_name[[param]])
  
  pheno_doy<-ts_df %>% 
    dplyr::select(date, site, simulated=pheno, predicted=value) %>% 
    gather(key="cat", value="value", -date, -site) %>% 
    mutate(year=as.numeric(format(date, "%Y")))%>%
    mutate(doy=as.numeric(format(date, "%j"))) %>% 
    left_join(param_all %>% dplyr::select(-param_mis), by=c("year","site")) %>% 
    mutate(cat=factor(cat, levels=c("simulated", "predicted"))) %>% 
    arrange(cat)
  
  p<-ggplot(pheno_doy %>%
           filter(site==3) 
  )+
    geom_line(aes(x=doy, y=value, group=year, col=temp_summ), alpha=0.5)+
    theme_classic()+
    labs(x="day of year",
         y=pheno_name[[param]],
         color=env_name[[param]])+
    theme(legend.position="bottom")+
    facet_wrap(.~cat, ncol=1)+
    scale_color_viridis_c(direction=-1,
                          guide=guide_colorbar(title.position = "bottom"))
  
  cairo_pdf(paste0("./RS4flower/output/figures/simulations_", param,".pdf"), height = 4, width = 2)
  print(p)
  dev.off()
}


