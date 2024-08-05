# PROJECT:  crossroads
# PURPOSE:  Access data
# AUTHOR:   A.Chafetz | USAID
# REF ID:   34767d59 
# LICENSE:  MIT
# DATE:     2024-08-01
# UPDATED:  2024-08-05

# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(arrow)
  #oha
  library(glamr) ##install.packages('glamr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  library(mindthegap)


# FOLDER SETUP ------------------------------------------------------------

  folder_setup()
  
# STORE DATA LOCALLY ------------------------------------------------------


  df <- pull_unaids(pepfar_only = FALSE)
  
  write_parquet(df, "Data/2023_unaids_est_global.parquet")

  