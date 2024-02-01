# Download ACCESS-S2 renalaysis from 1981 to present via Thredds
# Jessie Bolin (jessica.anne.bolin@gmail.com)
# Thredds: https://dapds00.nci.org.au/thredds/catalog/ux62/access-s2/reanalysis/ocean/catalog.html

# Dependencies ---------------------------------------------------------------

library(RCurl)

options(timeout = 40000) 


# Function ----------------------------------------------------------------

getACCESS <- function(time, var, diry) {
  
  yrs = 1981:2022 
  
  for(i in yrs) { 
    
    x = "https://dapds00.nci.org.au/thredds/fileServer/ux62/access-s2/reanalysis/ocean/"
    xx = var
    xxx = "/"
    xxxx = time
    y = "_"
    yy = var
    yyy = "_"
    z = i
    zz = ".nc"
    url <- paste0(x,xx, xxx, xxxx, y,yy,yyy, z, zz) 
    
    if (!file.exists(paste0(diry, "/", var, "/", xxxx, y, yy,yyy, z, ".nc"))) { 
      if(url.exists(url)) { 
        
        download.file(url, 
                      paste0(diry, "/", var, "/", xxxx, y, yy,yyy, z, ".nc"),
                      quiet = FALSE)
        
      }
    }
  }
}


# Download  --------------------------------------------------------------

workdir <- "" #path to directory where files will be stored

getACCESS("do", "sst", diry = workdir)
getACCESS("do", "hc300", diry = workdir)
getACCESS("do", "sss", diry = workdir)
getACCESS("do", "mld1", diry = workdir)
getACCESS("do", "temp", diry = workdir)
getACCESS("do", "v", diry = workdir)
getACCESS("do", "u", diry = workdir)
getACCESS("do", "ssh_corrected", diry = workdir)




