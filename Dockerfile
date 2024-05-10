# Use an official R runtime as a base image
FROM rocker/r-ver:4.1.0

# Set the working directory inside the container
WORKDIR /usr/src/app

# Copy the current directory contents into the container at /usr/src/app
COPY . .

# Install any needed packages specified in requirements.txt
RUN R -e "install.packages(c('ggplot2', 'plyr', 'dplyr', 'tidygeocoder','sf','mapview', 'tidyverse','leaflet'), repos='http://cran.us.r-project.org')"

# Run your R script
CMD ["global.R", "server.R", "ui.R"]