## A series of functions allow us to calculate and export the vegetation indices as a layer
memory.limit(8e+6)
rm(list=ls())

####################
## All functions are contained in this script
####################
source("make_GLiHT_indices_stacks")

wd_location <- "GLiHT"
setwd(wd_location)

####################
## Start by creating a list of all the hyperspectral data and their unique identifiers
####################
short.list <- list.files(paste0(wd_location, "/Reflectance"), full.names = F)
short.list <- short.list[-grep(".hdr",short.list)]

## You just want to isolate the base name, no "DTM", "CHM", or "_at_sensor" information
base.names <- unlist(lapply(strsplit(short.list, split = "_at"), function(x) return(x[1]))) 

## Loop through and create the vegetation indices stacks
for(i in 1:length(base.names)){
  make_veg_indices_layer(files = base.names[i])
  removeTmpFiles(h = 24)
  gc()
}


#### If you had enough cores/ram to do this in parallel, this is how it would be done:
# library(parallel)
# no_cores <- 3# detectCores() -1
# cl <- makeCluster(no_cores)
# 
# parLapply(cl, base.names, make_veg_indices_layer)
# stopCluster(cl)

