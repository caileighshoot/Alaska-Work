#########################################################
### Make Topo Layers from DTM and Stack into RData File
#########################################################
rm(list=ls())

## Set Data foldername and WD + Chamge memory limit to prevent crashes
data_foldername <- "C:/LiDAR/DTM"
setwd(data_foldername)
memory.limit(8e+6)
###############
## libraries ##
require(raster)
require(sp)
require(gdalUtils)

#########################################
## Credit to Sean Jeronimo for these two functions 
########################################
getCircularKernel <- function(radius)
{
  kernel_side <- 2 * as.integer(radius) + 1
  kernel_y <- matrix(rep(radius:-radius, kernel_side), ncol=kernel_side)
  kernel_x <- -t(kernel_y)
  kernel   <- matrix(as.matrix(dist(cbind(as.vector(kernel_x), 
                                          as.vector(kernel_y))))[as.integer((kernel_side^2) / 2) + 1,], 
                     ncol=kernel_side)
  kernel[kernel <= radius] <- 0
  kernel[kernel > 0]  <- 1
  kernel <- 1 - kernel
  return(kernel)
}

# radius in map units, converted to cell units internally
# TPI exists in raster's terrain() function but does not have a customizable window
# Assumes square pixels from DEM
TPI <- function(dem, window.width)
{
  conv.dem <- focal(dem, w=getCircularKernel(window.width), 
                    mean, na.rm=TRUE, pad=FALSE)
  return(dem - conv.dem)
}

#########################################
## Create Topometric Layers and Stack to RData File
########################################
# Grab a list of all the files so we can reference each of the 7 files associated
# with each acquisition
files <- list.files(data_foldername, pattern = ".tif")

## Get the unique names of flights from the DTM files for each
names <- gsub("\\_DTM_aa83.tif$","", list.files(data_foldername, 
                                                pattern="\\_DTM_aa83.tif$"))
# names <- gsub("\\_DTM.tif$","", list.files(data_foldername, pattern="\\_DTM.tif$"))

## Create a list of flight identifiers - NOTE: This needs dates eventually
ids <- strsplit(names, split = "_")
grab_ID <- function(x){
  if(x[3] == "TIU"){
    return(x[4])
  } else if(x[3] == "tiu"){
    return(x[4])
  } else{
    return(x[3])
  }
}

## Create a list of unique file names
ids <- unlist(lapply(ids, grab_ID))

name.list <- paste0("dat.", ids)

## Create Alaska Albers CRS Reference
ref <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

for(i in 1:length(name.list)){  
  ### If you want the data later
  ### slope <- raster(files[grep(paste0(names[i], "_slope"), files)])
  
  ## DTM
  dtm  <- raster(files[grep(paste0(names[i], "_DTM_aa83"), files)])
  ## We must reproject the DTMs if they are not already in AK Albers
  ## Tanana data is already in AK Albers, so this line should be removed
  dtm <- projectRaster(dtm, crs = ref) ## Remove this on Tananna Stuff
  dtm[dtm == 0] <- NA
  writeRaster(dtm, filename = paste0("reprocessed/", names[i], "_DTM_aa83.tif"))
  
  ## Slope
  slope <- terrain(x = dtm, opt = "slope", 
                   unit = 'tangent', neighbors = 8,
                   filename = paste0(names[i], "_slope_aa83.tif"))
  slope_smoothed <- focal(x = slope, w=getCircularKernel(6), 
                          mean, na.rm=TRUE, pad=FALSE)
  writeRaster(slope_smoothed, paste0(names[i], "_slope_Focal_aa83.tif"))
  
  ## Aspect
  aspect <- terrain(x = dtm, opt = "aspect", neighbors = 8,
                    filename = paste0(names[i], "_aspect_aa83.tif"))
  
  ## TPI
  tpi <- TPI(dtm, 6) ### A radius of 6 gives a 13 x 13 cell circular window
  writeRaster(tpi, paste0(names[i], "_TPI_Focal_aa83.tif"))  
  
  ## TRI
  tri <- terrain(x = dtm, opt = "tri", neighbors = 8,
                 filename = paste0(names[i], "_TRI_aa83.tif"))
  
  ## Roughness
  roughness <- terrain(x = dtm, opt = "roughness", neighbors = 8,
                       filename = paste0(names[i], "_roughness_aa83.tif"))
  
  ## Stack them all
  stk <- stack(aspect, dtm, roughness, slope_smoothed, tpi, tri) # Create the stack
  
  ## Convert to SP Pixels DF
  dat <- as(stk, "SpatialPixelsDataFrame")
  rm(stk)
  
  ## Export into .Rdata File
  assign(name.list[i],dat)
  rm(dat)
  
  ## Save the Rdata file with the unique ID as the name
  do.call(save, list(name.list[i], file = paste0("Rdata/",names[i],"_topo_aa83.RData")))
  print(i)
  
  gc()
  removeTmpFiles(h = 0.1)
}