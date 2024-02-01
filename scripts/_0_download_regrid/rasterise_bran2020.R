# BRAN2020 Rasterise temp, v, u and SSHa required for EAC algorithm
# Author:       Jessica Bolin (jessica.anne.bolin@gmail.com)


# Dependencies ------------------------------------------------------------

library(terra) 
library(beepr)
library(tidyverse)


# Function  --------------------------------------------------------------------

bran_rast <- function(var, wd, newwd, varname) {
  
  cd <- list.files(wd, pattern = "\\.nc$") #where files are stored
  cd <- cd[grep(pattern = varname, cd)]
  
  for (i in 1:length(cd)) { #for each file in  working directory
    
    j <- rast(paste0(wd, "/", cd[i])) #read in raster
    j <- crop(j, extent(130, 179, -70, 0))
    ind <- grep("st_ocean=2.5_", names(j)) # indices each day surface layer
    new_r <- j[[ind]] #subset so the raster only contains surface layers 
    strsplit(names(new_r), paste0(var, "_st_ocean=2.5_Time=")) %>% 
      unlist() %>%  
      stringi::stri_remove_empty() %>% 
      gsub(paste0(varname, "_st_ocean=2.5_Time="), "", .) %>% 
      as.numeric %>% 
      as.Date(origin = "1979-01-01") -> names(new_r) 
    j <- new_r
    print(paste0(i, "/", length(cd))) 
    
    for (k in 1:nlyr(j)) { #for each layer, write each individual day as raster
      
      if (!file.exists(paste0(newwd, var, "/", varname, "_", names(j[[k]]), ".nc"))) { 
        writeCDF(j[[k]], #individual layer of raster
                           paste0(newwd, "/", varname, "_", names(j[[k]]), ".nc"),
                 overwrite = T) 
      } #if then 
    } #for loop
  } # for loop 
  beep(3) 
} # function 

# Run function ------------------------------------------------------------

bran_rast(var = "ocean_u",
          wd = "oldirectory",
          newwd = "newdirectory",
          varname = "u")

bran_rast(var = "ocean_v",
          wd = "oldirectory",
          newwd = "newdirectory",
          varname = "v")

bran_rast(var = "ocean_temp",
          wd = "oldirectory",
          newwd = "newdirectory",
          varname = "temp")

# SSHa ------------------------------------------------------------------

cd <- list.files(wd, pattern = "\\.nc$")
cd <- cd[grep(pattern = "eta_t", cd)]

for (i in 1:length(cd)) { 
  
  j <- rast(paste0(wd, "/", cd[i])) 
  strsplit(names(j), "eta_t_Time=") %>% 
    unlist() %>%  
    stringi::stri_remove_empty() %>% 
    as.numeric %>% 
    as.Date(origin = "1979-01-01") -> names(j) 
  print(paste0(i, "/", length(cd))) 
  
  for (k in 1:nlyr(j)) { 
    
    if (!file.exists(paste0("oldirectory",
                            names(j[[k]]), 
                            ".nc"))) { 
      terra::writeCDF(j[[k]], 
                         paste0("oldirectory",
                                names(j[[k]]), 
                                ".nc"), 
                         overwrite = T) 
    } 
  }
}

