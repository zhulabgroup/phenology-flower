# Function to check and prompt for missing env variables
check_and_prompt_env <- function(var_name, prompt_text) {
  value <- Sys.getenv(var_name, unset = NA) # Get env variable
  if (is.na(value) || value == "") { # If missing, prompt user
    value <- readline(prompt_text)
    write(paste0(var_name, "=", value), file = ".env", append = TRUE) # Save to .env
  }
  return(value)
}

# Load existing .env file (if it exists)
dotenv::load_dot_env(".env")

# Check each required variable and prompt if missing
name <- check_and_prompt_env("RSCONNECT_NAME", "Enter shinyapps.io name: ")
token <- check_and_prompt_env("RSCONNECT_TOKEN", "Enter shinyapps.io token: ")
secret <- check_and_prompt_env("RSCONNECT_SECRET", "Enter shinyapps.io secret: ")

# Reload .env file to ensure new values are available
dotenv::load_dot_env(".env")

# Set account info
rsconnect::setAccountInfo(
  name = name,
  token = token,
  secret = secret
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
  rsconnect::deployApp(appDir = "./shinyapp", lint = F, account = name, server = "shinyapps.io", appName = "RS4Flower_result", upload = T, forceUpdate = T)
}
