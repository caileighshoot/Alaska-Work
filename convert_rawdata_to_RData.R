rm(list=ls())

###############
## libraries ##
library(rgdal)
memory.limit(8e+6)

####################
## get file names ##
file.list <- list.files("C:/GLiHTdelivery/reflectance", full.names = T)
file.list <- file.list[-grep(".hdr",file.list)]

## Create a list of only the names
short.list <- list.files("C:/GLiHTdelivery/reflectance", full.names = F)
short.list <- short.list[-grep(".hdr",short.list)]

## Grab the unique identifier from the full name, use for creating name list
grab_ID <- function(x){
  if(x[5] == "TIU"){
    return(x[6])
  } else if(x[5] == "tiu"){
    return(x[6])
  } else{
    return(x[5])
  }
}

## Create a list of names that we will assign to each of our data files
## This uses grab_ID to get the unique identifier from the filename, and then pastes 
## it with "dat." to create dat.l0[unique ID]
name.list <- paste("dat.", sapply(strsplit(short.list, split = "_"), FUN = grab_ID), sep = "")


###################################
## Credit for the below code section goes to Chad Babcock
## Comments added by Caileigh Shoot
###################################
for(i in 1:length(file.list)){
  ## Read in the data
  dat <- readGDAL(file.list[i])
  
  ## This converts the hyperspectral data to 8-bit reflectance values (0 to 255)
  dat@data <- dat@data/10000
  
  ## Sum all the rows within our raster data
  sums <- rowSums(dat@data)
  
  ## Get the coordinates of the data where non-zero values are present 
  sub.coords <- coordinates(dat)[!(sums==0),]
  ## Get the values of the data where they are non-zero, assign to new object
  sub.dat <- dat@data[!(sums==0),]
  
  ## Set the coordinates and projection of the new object
  coordinates(sub.dat) <- sub.coords
  proj4string(sub.dat) <- proj4string(dat)
  
  #Set the object as gridded
  gridded(sub.dat) <- TRUE
  
  ## Replace dat with our newly created object containing no zeros
  dat <- sub.dat
  rm(sub.dat)
  
  ## Assign the unique name to "dat"
  assign(name.list[i],dat)
  
  ## Save the newly assigned unique object as an RData file
  ## This ensures that when it is loaded in, the unique ID is present
  ## Thus, if you load in multiple objects they are not all simply named "dat",
  ## But instead have unique identifiers that are preserved when you load() them in
  ## Do call is required so that it can grab the unique ID within the loop, otherwise this command fails
  do.call(save, list(name.list[i], file = paste0("C:/GLiHTdelivery/Rdata/",short.list[i],".RData")))
  print(i)
  gc()
}
