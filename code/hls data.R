##### read hls data

path_data<-"/data/ZHULAB/phenology/HLS_L30/"

if (!file.exists(paste0(path_data, "analyses/hls_",siteoi,"_",taxaoi,".rds"))) {
  path_data_siteoi<-paste0(path_data, siteoi, "/")
  
  coord_df<-coordinates(trees_genus_sp) %>% 
    as_tibble() %>% 
    mutate(id=row_number())
  
  cl <- makeCluster(20, outfile = "")
  registerDoSNOW(cl)
  
  # Fmask
  files<-list.files(path_data_siteoi, "*Fmask.tif", recursive = T, full.names = T)
  time_df<-list.files(path_data_siteoi, "*Fmask.tif", recursive = T) %>% 
    str_split(pattern="\\.", simplify = T) %>% 
    data.frame() %>% 
    dplyr::select(filename=X4) %>% 
    mutate(year=substr(filename,1,4)%>% as.integer(),
           doy=substr(filename,5,7) %>% as.integer()) %>% 
    mutate(date=as.Date(doy, origin = paste0(year,"-01-01"))) %>% 
    mutate(f=row_number()) %>% 
    dplyr::select(-filename)
  
  nday<-length(files)
  nloc<-length(trees_genus_sp)
  Fmask_mat<-foreach (f = 1:nday,
                      .packages = c("raster"),
                      .combine="rbind") %dopar%{
                        file<-files[f]
                        hls_ras<-raster(file)
                        
                        trees_sp_reproj<-spTransform(trees_genus_sp,CRSobj=CRS(proj4string(hls_ras)))
                        
                        hls_values<-cbind(Fmask=raster::extract(hls_ras,trees_sp_reproj) ,f, id=1:nloc)
                        print(paste0( f, " out of ", nday))
                        hls_values[complete.cases(hls_values),]
                      }
  
  Fmask_df<-Fmask_mat %>% 
    as_tibble() %>% 
    left_join(time_df, by="f") %>% 
    left_join(coord_df, by="id") %>% 
    dplyr::select(-f)
  
  
  # blue
  files<-list.files(path_data_siteoi, "*B02.tif", recursive = T, full.names = T)
  time_df<-list.files(path_data_siteoi, "*B02.tif", recursive = T) %>% 
    str_split(pattern="\\.", simplify = T) %>% 
    data.frame() %>% 
    dplyr::select(filename=X4) %>% 
    mutate(year=substr(filename,1,4)%>% as.integer(),
           doy=substr(filename,5,7) %>% as.integer()) %>% 
    mutate(date=as.Date(doy, origin = paste0(year,"-01-01"))) %>% 
    mutate(f=row_number()) %>% 
    dplyr::select(-filename)
  
  nday<-length(files)
  nloc<-length(trees_genus_sp)
  blue_mat<-foreach (f = 1:nday,
                     .packages = c("raster"),
                     .combine="rbind") %dopar%{
                       file<-files[f]
                       hls_ras<-raster(file)
                       
                       trees_sp_reproj<-spTransform(trees_genus_sp,CRSobj=CRS(proj4string(hls_ras)))
                       
                       hls_values<-cbind(blue=raster::extract(hls_ras,trees_sp_reproj) ,f, id=1:nloc)
                       print(paste0( f, " out of ", nday))
                       hls_values[complete.cases(hls_values),]
                     }
  
  blue_df<-blue_mat %>% 
    as_tibble() %>% 
    left_join(time_df, by="f") %>% 
    left_join(coord_df, by="id") %>% 
    dplyr::select(-f)
  
  # green
  files<-list.files(path_data_siteoi, "*B03.tif", recursive = T, full.names = T)
  time_df<-list.files(path_data_siteoi, "*B03.tif", recursive = T) %>% 
    str_split(pattern="\\.", simplify = T) %>% 
    data.frame() %>% 
    dplyr::select(filename=X4) %>% 
    mutate(year=substr(filename,1,4)%>% as.integer(),
           doy=substr(filename,5,7) %>% as.integer()) %>% 
    mutate(date=as.Date(doy, origin = paste0(year,"-01-01"))) %>% 
    mutate(f=row_number()) %>% 
    dplyr::select(-filename)
  
  nday<-length(files)
  nloc<-length(trees_genus_sp)
  green_mat<-foreach (f = 1:nday,
                      .packages = c("raster"),
                      .combine="rbind") %dopar%{
                        file<-files[f]
                        hls_ras<-raster(file)
                        
                        trees_sp_reproj<-spTransform(trees_genus_sp,CRSobj=CRS(proj4string(hls_ras)))
                        
                        hls_values<-cbind(green=raster::extract(hls_ras,trees_sp_reproj) ,f, id=1:nloc)
                        print(paste0( f, " out of ", nday))
                        hls_values[complete.cases(hls_values),]
                      }
  
  green_df<-green_mat %>% 
    as_tibble() %>% 
    left_join(time_df, by="f") %>% 
    left_join(coord_df, by="id") %>% 
    dplyr::select(-f)
  
  # red
  files<-list.files(path_data_siteoi, "*B04.tif", recursive = T, full.names = T)
  time_df<-list.files(path_data_siteoi, "*B04.tif", recursive = T) %>% 
    str_split(pattern="\\.", simplify = T) %>% 
    data.frame() %>% 
    dplyr::select(filename=X4) %>% 
    mutate(year=substr(filename,1,4)%>% as.integer(),
           doy=substr(filename,5,7) %>% as.integer()) %>% 
    mutate(date=as.Date(doy, origin = paste0(year,"-01-01"))) %>% 
    mutate(f=row_number()) %>% 
    dplyr::select(-filename)
  
  nday<-length(files)
  nloc<-length(trees_genus_sp)
  red_mat<-foreach (f = 1:nday,
                    .packages = c("raster"),
                    .combine="rbind") %dopar%{
                      file<-files[f]
                      hls_ras<-raster(file)
                      
                      trees_sp_reproj<-spTransform(trees_genus_sp,CRSobj=CRS(proj4string(hls_ras)))
                      
                      hls_values<-cbind(red=raster::extract(hls_ras,trees_sp_reproj) ,f, id=1:nloc)
                      print(paste0( f, " out of ", nday))
                      hls_values[complete.cases(hls_values),]
                    }
  
  red_df<-red_mat %>% 
    as_tibble() %>% 
    left_join(time_df, by="f") %>% 
    left_join(coord_df, by="id") %>% 
    dplyr::select(-f)
  
  # nir
  files<-list.files(path_data_siteoi, "*B05.tif", recursive = T, full.names = T)
  time_df<-list.files(path_data_siteoi, "*B05.tif", recursive = T) %>% 
    str_split(pattern="\\.", simplify = T) %>% 
    data.frame() %>% 
    dplyr::select(filename=X4) %>% 
    mutate(year=substr(filename,1,4)%>% as.integer(),
           doy=substr(filename,5,7) %>% as.integer()) %>% 
    mutate(date=as.Date(doy, origin = paste0(year,"-01-01"))) %>% 
    mutate(f=row_number()) %>% 
    dplyr::select(-filename)
  
  nday<-length(files)
  nloc<-length(trees_genus_sp)
  nir_mat<-foreach (f = 1:nday,
                    .packages = c("raster"),
                    .combine="rbind") %dopar%{
                      file<-files[f]
                      hls_ras<-raster(file)
                      
                      trees_sp_reproj<-spTransform(trees_genus_sp,CRSobj=CRS(proj4string(hls_ras)))
                      
                      hls_values<-cbind(nir=raster::extract(hls_ras,trees_sp_reproj) ,f, id=1:nloc)
                      print(paste0( f, " out of ", nday))
                      hls_values[complete.cases(hls_values),]
                    }
  
  nir_df<-nir_mat %>% 
    as_tibble() %>% 
    left_join(time_df, by="f") %>% 
    left_join(coord_df, by="id") %>% 
    dplyr::select(-f)
  
  stopCluster(cl)
  
  
  hls_df<-Fmask_df %>% 
    full_join(blue_df,by=c("id", "year", "doy", "date", "lon", "lat")) %>% 
    full_join(green_df,by=c("id", "year", "doy", "date", "lon", "lat")) %>% 
    full_join(red_df,by=c("id", "year", "doy", "date", "lon", "lat")) %>% 
    full_join(nir_df,by=c("id", "year", "doy", "date", "lon", "lat"))
  
  write_rds(hls_df, paste0(path_data, "analyses/hls_",siteoi,"_",taxaoi,".rds"))
}

hls_df<-read_rds(paste0(path_data, "analyses/hls_",siteoi,"_",taxaoi,".rds"))
hls_df_proc<-hls_df %>% 
  filter(id%in% random_id) %>% 
  rowwise() %>% 
  mutate(aerosol=(intToBits(Fmask) [1:8] %>% as.integer()) [7:8] %>% str_flatten() %>% as.integer(),
         water=(intToBits(Fmask) [1:8] %>% as.integer()) [6] %>% as.integer(),
         snowice=(intToBits(Fmask) [1:8] %>% as.integer()) [5] %>% as.integer(),
         cloudshadow=(intToBits(Fmask) [1:8]  %>% as.integer()) [3:4] %>% str_flatten() %>% as.integer(),
         cloud=(intToBits(Fmask) [1:8]  %>% as.integer()) [2] %>% as.integer()) %>%  
  ungroup() %>% 
  # mutate(qa=case_when(Fmask==0|Fmask==64 ~2,
  #                     TRUE~1)) %>%
  # filter(qa==2|is.na(qa)) %>%
  filter(aerosol<11,
         water==0,
         snowice==0,
         cloudshadow==0,
         cloud==0) %>%
  dplyr::select(-Fmask, -aerosol, -water, -snowice, -cloudshadow, -cloud) %>%
  group_by(id, lon, lat, date, year, doy ) %>% 
  summarize(blue=mean(blue),
            green=mean(green),
            red=mean(red),
            nir=mean(nir)) %>% 
  ungroup() %>% 
  mutate(evi=2.5* (nir-red) / (nir + 6*red - 7.5*blue + 1)) %>% 
  mutate(ebi= ((red+green+blue))/ (green/blue * (red - blue + 1))) %>% 
  mutate(ndvi=(nir-red)/(nir+red)) %>% 
  mutate(nirv=(ndvi-0.08)*nir) %>% 
  mutate(rgb=(red+green+blue)) %>% 
  mutate(g2r=green/red) %>%
  mutate(red=red/rgb,
         green=green/rgb,
         blue=blue/rgb) %>%
  filter(evi>0, evi<=0.6) %>% 
  # filter(nir<=0.4) %>% 
  # filter(rgb<=0.6) %>%
  filter(abs(g2r-0.9)<=0.4) %>% 
  filter(red>0, green>0, blue>0) %>%
  mutate(color=rgb(red, green, blue, 1 )) 
