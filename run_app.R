
# Install necessary packages

  # Define required packages
    list.of.packages <- c("shinydashboard","car","dplyr","tidyr","xts",
                          "rcdimple","DT","dygraphs")
  # Check for and install new packages  
    new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
    if(length(new.packages)) install.packages(new.packages) 
  # Load required libraries
    lapply(list.of.packages, library, character.only = TRUE)
    rm(list.of.packages); rm(new.packages)

# Update data
  source("prep/productivity_model_wmcmh.R")
  
# Test app locally
  library(shiny)
  
  runApp("../prod-dash")
  
# Deploy app 
  library(shinyapps)
  
  deployApp("../prod-dash", account = "joshh")
  deployApp("../prod-dash", account = "tbdsolutions")

# Get info about ShinyApps accounts
  accounts()
  accountInfo("joshh")
  accountInfo("tbdsolutions")

# Get logs for diagnostics
  rsconnect::showLogs("prod-dash", account = "joshh")
  