# Download Chlorophyll-a (derived from ocean colour)
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com)
# Files: https://coastwatch.pfeg.noaa.gov/erddap/files/erdMH1chla8day/

# Dataset title: Chlorophyll-a, Aqua MODIS, NPP, L3SMI, Global, 4km, Science Quality, 2003-
#                present (Monthly Composite)

# Dependencies ------------------------------------------------------------

# Required packages
library(lubridate)
library(RCurl)
library(rvest)
library(tidyverse)

# MODIS 4km 2003-2020 month -------------------------------------------------

oFold <- "" # Set destination folder
oFold <- paste0(oFold, "/")
# Set base URL
url2 <- "https://coastwatch.pfeg.noaa.gov/erddap/files/erdMH1chlamday/"
pg <- read_html(url2) # Read the HTML
sFld <- html_attr(html_nodes(pg, "a"), "href") # Get the links
new <- sFld[grep("4km.nc", sFld)] # Just netcdfs 
f <- dir(oFold, pattern = ".nc")
sFlde <- new[!new %in% f] # Exclude files if already downlaoded
sFlde[197:length(sFlde)] -> sFlde # From 2019-ish onwards

# Download 
for (j in 1:length(sFlde)) {
  urli <- paste0(url2, sFlde[j])
  download.file(urli, paste0(oFold, sFlde[j]))
}
