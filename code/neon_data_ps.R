if (FALSE) {
  df_plant <-ls_df_neon$coord %>% 
    group_by(site) %>% 
    arrange(id) %>% 
    mutate(ps_id = row_number()) %>% 
    ungroup() %>% 
    mutate(taxa = "all")
  
  # ggplot(df_plant)+
  #   geom_point(aes(x= lon, y = lat))+
  #   facet_wrap(.~site, scales="free")+
  #   theme_classic()
  
  source("code/func_ps_patch.R")
  source("code/func_ps_order.R")
  func_ps_batch_order (dir = str_c(.path$ps,"neon/"), df_plant, v_site = NULL)
  source("code/func_ps_down.R")
  func_ps_batch_download (dir = str_c(.path$ps,"neon/"), v_site = NULL)
  source("code/func_ps_ts.R")
  func_ps_batch_ts (dir = str_c(.path$ps,"neon/"), tsdir = str_c(.path$ps,"ts_neon/"), v_taxa = "all", v_site =NULL)
  source("code/func_proc_ps.R")
  source("code/prep_hyper.R")
  source("code/func_doy.R")
  source("code/func_flat.R")
  source("code/neon_doy.R")
}
