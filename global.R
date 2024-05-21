# Install and load necessary packages
install_and_load_packages <- function() {
  rm(list = ls())
  # List of packages to install and load
  required_packages <- c("shiny", "ggplot2", "dplyr", "tidygeocoder",
                         "sf", "mapview","tidyverse","leaflet")
  
  # Check if packages are installed, install if not
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
    }
  }
  
  # Load the packages
  lapply(required_packages, require, character.only = TRUE)
}

# Call the function to install and load packages
install_and_load_packages()

# Load the data
setwd("C:/Users/rohit/Downloads/AirbnbShinyDashboard 4/AirbnbShinyDashboard/data")
air_bnb_final <- read.csv("air_bnb_daywise_data.csv")

names(air_bnb_final)[names(air_bnb_final) == "price_y"] <- "price"

air_bnb_final$date <- as.Date(air_bnb_final$date)

min_date <- min(air_bnb_final$date, na.rm = TRUE)
max_date <- max(air_bnb_final$date, na.rm = TRUE)

listings <- read.csv("listings.csv")
print(dim(listings))



