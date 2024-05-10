# # Install and load necessary packages
# install_and_load_packages <- function() {
#   # List of packages to install and load
#   required_packages <- c("ggplot2","plyr", "dplyr","reticulate", "tidygeocoder",
#                          "sf", "mapview","tidyverse","leaflet")
#   
#   # Check if packages are installed, install if not
#   for (pkg in required_packages) {
#     if (!requireNamespace(pkg, quietly = TRUE)) {
#       install.packages(pkg, dependencies = TRUE)
#     }
#   }
#   
#   # Load the packages
#   lapply(required_packages, require, character.only = TRUE)
# }

library(ggplot2)
library(plyr)
library(dplyr)
# library(reticulate)
library(tidygeocoder)
library(sf)
library(mapview)
library(tidyverse)
library(leaflet)

# Call the function to install and load packages
# install_and_load_packages()

# Load the data
#setwd("D:/mps_analytics/q2/ALY6070/final_project/AirbnbShinyDashboard/data")
air_bnb_final <- read.csv("data/air_bnb_daywise_data.csv")

names(air_bnb_final)[names(air_bnb_final) == "price_y"] <- "price"

air_bnb_final$date <- as.Date(air_bnb_final$date)

min_date <- min(air_bnb_final$date, na.rm = TRUE)
max_date <- max(air_bnb_final$date, na.rm = TRUE)


