if (FALSE) {
  library(tidyverse)
  library(ggrepel)
  library(cowplot)
  library(gridExtra)
  library(spocc)
  library(sf)
  library(rnpn)
  # library(devtools)
  # devtools::install_github("bevingtona/planetR")
  unloadNamespace("RJSONIO") # conflict with jsonlite
  library(planetR)
  library(httr)
  library(jsonlite)
  library(raster)
  library(stringr)
  library(pracma)
  library(foreach)
  library(doSNOW)
  library(ptw)
  library(EnvCpt)
  library(rgdal)
  library(mclust)
  library(npreg)
  library(nlme)
  library(readxl)
  library(fuzzyjoin)
  library(taxize)
  library(ptw)
  library(segmented)
  library(RhpcBLASctl)
  library(geosphere)
  library(nlme)
  library(shiny)
  library(rsconnect)
  library(ggpubr)
  library(scales)
  library(neonUtilities)
  # devtools::install_github("NEONScience/NEON-geolocation/geoNEON")
  library(geoNEON)
}

# pacman::p_unload("all")
pacman::p_load("tidyverse")
pacman::p_load("sf")
pacman::p_load("parallel")
pacman::p_load("doSNOW")
pacman::p_load("patchwork")

source("code/func_whit.R")
source("code/util_extend_ts.R")

.deploy_shiny <- T
.fig_save <- T

.path <- list(
  out_fig = "figures/",
  nab = "data/nab/",
  occ = "data/occurrence/",
  neon = "data/neon/",
  dat_other = "data/processed/",
  npn = "data/npn/",
  ps = "data/ps/",
  res = "data/results/"
)
