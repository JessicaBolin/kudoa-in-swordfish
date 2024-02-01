# Regrid daily ACCESS-RA netCDFs
# Author: Jessica Bolin (jessica.anne.bolin@gmail.com) and Jason Hartog 
#         (jason.hartog@csiro.au)

# This script invokes cdo with the the `system()` command. Works fine on my 
# MacOS with cdo installed via homebrew, but will not work on Windows. 

# Steps:
# (i) Subsets surface layers of u and v only and writes to new .nc files
# (ii) Regrids files of all vars from tripolar native ORCA2 grid to 0.25˚ 
#      lat/lon WGS84 (bilinear interpolation) and fills in missing NAs
# (iii) Crops extent to the ETBF
# (iv) After regridding ACCESS vars, creates EKE and speed using u and v


# Dependencies ---------------------------------------------------------------

library(tidyverse)


# Step 1 - subset surface layers of u and v  ---------------------------------

# Reads in nc files for u/v, selects the first level, and writes level as .nc
# file to a new directory.
# Surface layer for ACCESS-S2 = 0.505760014m (`cdo -showlevel file.nc`)

dir = "" # directory to u files
fileys <- list.files(dir) 

for (i in 1:length(fileys)) {
  newfile <- gsub(".nc", "", fileys[i]) 
  system(paste0("cdo -sellevel,0.505760014 ", dir, "/", fileys[i], " ", "/directory_for_u_surface_files/", newfile, "_surface.nc")) 
  print(newfile)
} 

dir = "" # directory to v files
fileys <- list.files(dir) 

for (i in 1:length(fileys)) {
  newfile <- gsub(".nc", "", fileys[i])
  system(paste0("cdo -sellevel,0.505760014 ", dir, "/", fileys[i], " ", "/directory_for_v_surface_files/", newfile, "_surface.nc")) 
  print(newfile)
} 



# 2 - Regrid --------------------------------------------------------------

# For variables with one layer (i.e., SST, SSS, hc300, mld1)

regrid_JH <- function(var) {
  
  setwd(paste0("", var)) # Replace with path to regridded .nc directory
  filefile <- list.files(path = paste0("", var)) # Replace with path to raw .nc files directory
  
  for (i in 1:length(filefile)) { 
    
    file <- filefile[i]
    dir = "" # Replace with path to raw .nc files directory
    system(paste0("cp ", dir, var, "/", file, " ", "tmp_1.nc"))  
    system(paste0('ncatted -a coordinates,', var,',c,c,"nav_lon nav_lat" tmp_1.nc'))
    system(paste0('cdo -s -L -sellonlatbox,100,200,-50,10 -selname,',var,' tmp_1.nc tmp_2.nc  && mv tmp_{2,1}.nc')) 
    system(paste0('cdo -s -L remapbil,r1440x720 -selname,',var,' -setmisstonn tmp_1.nc tmp_2.nc   && mv tmp_{2,1}.nc'))
    system(paste0('cdo -s -L -sellonlatbox,110,190,-45,5 -selname,', var, ' tmp_1.nc tmp_2.nc  && mv tmp_{2,1}.nc'))
    datey <- system(paste0("cdo -showyear tmp_1.nc"), intern = T) %>% str_remove(., " ")
    system(paste0('cdo -s -f nc4 -z zip copy tmp_1.nc ', 'do_' ,var, '_', datey, '_RG.nc')) 
    system("rm tmp_1.nc") 
    print(file)
    
  }
}

# Velocity (surface layer)

regrid_JH_velocity <- function(var, surfvar) {
  
  setwd(paste0("", var)) # Replace with path to regridded .nc directory
  filefile <- list.files(path = paste0("", surfvar)) # Replace with path to raw surface level .nc files directory
  
  for (i in 1:length(filefile)) { 
    
    file <- filefile[i]
    dir = "" # Replace with path to raw .nc files directory
    system(paste0("cp ", dir, surfvar, "/", file, " ", "tmp_1.nc"))  
    system(paste0('ncatted -a coordinates,', var,',c,c,"nav_lon nav_lat" tmp_1.nc'))
    system(paste0('cdo -s -L -sellonlatbox,100,200,-50,10 -selname,',var,' tmp_1.nc tmp_2.nc  && mv tmp_{2,1}.nc')) 
    system(paste0('cdo -s -L remapbil,r1440x720 -selname,',var,' -setmisstonn tmp_1.nc tmp_2.nc   && mv tmp_{2,1}.nc'))
    system(paste0('cdo -s -L -sellonlatbox,110,190,-45,5 -selname,', var, ' tmp_1.nc tmp_2.nc  && mv tmp_{2,1}.nc'))
    datey <- system(paste0("cdo -showyear tmp_1.nc"), intern = T) %>% str_remove(., " ")
    system(paste0('cdo -s -f nc4 -z zip copy tmp_1.nc ', 'do_' ,var, '_', datey, '_RG.nc')) 
    system("rm tmp_1.nc") 
    print(file)
    
  }
}

# SSH

regrid_JH_ssh <- function(var, surfvar) {
  
  setwd(paste0("", var)) # Replace with path to regridded .nc directory
  filefile <- list.files(path = paste0("", var)) # Replace with path to raw .nc files directory
  
  for (i in 1:length(filefile)) { 
    
    file <- filefile[i]
    dir = "" # Replace with path to raw .nc files directory
    
    system(paste0("cp ", dir, var, "/", file, " ", "tmp_1.nc"))  
    system(paste0('ncatted -a coordinates,', var,',c,c,"nav_lon nav_lat" tmp_1.nc'))
    system(paste0('cdo -s -L -sellonlatbox,100,200,-50,10 -selname,',var,' tmp_1.nc tmp_2.nc  && mv tmp_{2,1}.nc')) 
    system(paste0('cdo -s -L remapbil,r1440x720 -selname,',var,' -setmisstonn tmp_1.nc tmp_2.nc   && mv tmp_{2,1}.nc'))
    system(paste0('cdo -s -L -sellonlatbox,110,190,-45,5 -selname,', var, ' tmp_1.nc tmp_2.nc  && mv tmp_{2,1}.nc'))
    datey <- system(paste0("cdo -showyear tmp_1.nc"), intern = T) %>% str_remove(., " ")
    system(paste0('cdo -s -f nc4 -z zip copy tmp_1.nc ', 'do_' ,var, '_', datey, '_RG.nc')) 
    system("rm tmp_1.nc") 
    print(file)
    
  }
}



# 3 - Run functions -----------------------------------------------------------

regrid_JH("sst")
regrid_JH("hc300")
regrid_JH("mld1")
regrid_JH("sss")

regrid_JH_velocity("u", "us")
regrid_JH_velocity("v", "vs")

regrid_JH_ssh("ssh_corrected", "ssh")



# EKE and speed -----------------------------------------------------------

# Gets regridded u and v files and creates .nc files for EKE and current speed

# EKE = (u^2 + v^2) / 2
# Speed = sqrt(u^2 + v^2) 

diri_u <- "" #regridded u directory
allfiles_u <- list.files(diri_u)
diri_v <- "" #regridded v directory
allfiles_v <- list.files(diri_v)

ekedir <- "" #directory for eke files
speeddir <- "" #directory for speed files

for (i in 1:length(allfiles_u)) {
  
  u <- rast(paste0(diri, "/", allfiles_u[i]))
  v <- rast(paste0(diri, "/", allfiles_u[i]))
  eke <- (u^2 + v^2) / 2
  names(eke) <- gsub("u", "eke", names(eke))
  names(eke) <- gsub("depthu", "depth", names(eke))
  speed <- sqrt(u^2 + v^2) 
  names(speed) <- gsub("u_", "speed_", names(speed))
  names(speed) <- gsub("depthu", "depth", names(speed))
  yr <- gsub("[^0-9]+", "", allfiles_u[i]) 
  terra::writeCDF(eke, 
                  paste0(ekedir, "/do_eke_", yr, "_RG.nc"),
                  overwrite = T)
  terra::writeCDF(speed, 
                  paste0(speeddir, "/do_speed_", yr, "_RG.nc"),
                  overwrite = T)
  
}
