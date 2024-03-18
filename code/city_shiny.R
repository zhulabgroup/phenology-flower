# devtools::install_github("rstudio/rsconnect", , ref = 'v0.8.29')
# https://community.rstudio.com/t/shinyapps-io-deploy-error-409-application-exists-with-name/170488/13

rsconnect::setAccountInfo(
  name = "yiluansong",
  token = "095CAA728048A2F23867FA44B16C92F8",
  secret = "xZ99vo7in7UIKIcAG0kBoFi8adeeBu0xTNef0dLd"
)
if (.deploy_shiny) {
  for (taxaoi in v_taxa) {
    files <- list.files(str_c(.path$res, taxaoi), pattern = "*.jpg", recursive = T, full.names = T)
    path_local <- str_c("./shinyapp/result_figs/", taxaoi)
    dir.create(path_local, recursive = T)
    file.copy(
      from = files, to = path_local,
      overwrite = TRUE, recursive = TRUE,
      copy.mode = TRUE
    )
  }
  rsconnect::deployApp(appDir = "./shinyapp", lint = F, account = "yiluansong", server = "shinyapps.io", appName = "RS4Flower_result", upload = T, forceUpdate = T)
}
