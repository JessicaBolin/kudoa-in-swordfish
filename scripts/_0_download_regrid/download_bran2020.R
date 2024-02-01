# BRAN2020 Download temp, v, u and SSHa required for EAC algorithm
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)
# Last updated: 2024-02-01

### BRAN2020
# Download via thredds at NCI
# 10km resolution (eddy-resolivng) GCM for Australia. 1993-present 
# Daily averaged fields, realistically represent major ocean current systems
# https://dapds00.nci.org.au/thredds/catalog/gb6/BRAN/BRAN2020/daily/catalog.html
# See Chamberlain et al. 2021 Earth System Science Data for skill assessment 

# Set max time limit to a large number (files are ~500MB).
# A test download = 158 sec elapsed for one file
options(timeout = 20000)


# Dependencies ------------------------------------------------------------

library(lubridate)
library(RCurl)
library(beepr)



# Function -----------------------------------------------------------------

getBRAN <- function(yrstrt, # year start
                    yrend,  # year end
                    var,  #variable name in BRAN .nc file
                    varvar,  #variable name
                    diry = "") { #output directory
  
  yrs <- yrstrt:yrend
  ds <- seq(1:12)
  
  for(i in yrs){
    for(j in 1:length(ds)) {
      
      if (!j %in% c(10, 11, 12)) { #paste a 0 in front of single integers 
        j = paste0("0", j) }
      if (j %in% c("04", "06", "09", "11")) { #number of days gets 30 or 31 
        nodays <- 30 } else {
          nodays <- 31 }
      if (j %in% "02") { 
        nodays <- 28 } 
      if (i %in% c(2008, 2012, 2016) & j %in% "02") { # leap years get 29 
        nodays <- 29 } 
      x = "https://dapds00.nci.org.au/thredds/ncss/gb6/BRAN/BRAN2020/daily/"
      xx = var
      y = "_"
      yy = i
      zz = "_"
      zzz = j
      zd <- ".nc"
      jj <- paste0("?var=", varvar, 
                   "&north=-5.00&west=90.00&east=179.00&south=-50.00&horizStride=1&time_start=")
      as <- paste0(i, "-", j, "-01")
      kk <- "T12%3A00%3A00Z&time_end="
      xa <- paste0(i, "-", j, "-", nodays) 
      lll <- "T12%3A00%3A00Z&timeStride=1&vertCoord=1"
      url <- paste0(x,xx,y,yy,zz, zzz, zd, jj, as, kk, xa, lll)
      
      if (!file.exists(paste0(diry, "/", xx, y, yy, zz, zzz, ".nc"))) { 
        if(url.exists(url)) { #Catch 404 errors
          download.file(url, 
                        paste0(diry, "/", xx, y, yy, zz, zzz, ".nc"),
                        quiet = FALSE)
        }
      }
    }
  }
  beep(3)
}


# Download ----------------------------------------------------------------

getBRAN(yrstrt = 2019, 
        yrend = 2022, 
        var = "ocean_temp",
        varvar = "temp",
        diry = diry)

getBRAN(yrstrt = 2019, 
        yrend = 2022, 
        var = "ocean_u",
        varvar = "u",
        diry = diry)

getBRAN(yrstrt = 2019, 
        yrend = 2022, 
        var = "ocean_v",
        varvar = "v",
        diry = diry)

getBRAN(yrstrt = 2019, 
        yrend = 2022, 
        var = "ocean_eta_t",
        varvar = "eta_t",
        diry = diry)

