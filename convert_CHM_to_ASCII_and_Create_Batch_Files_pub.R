## This 

wd_location <- "GLiHT"
setwd(wd_location)

####################
## Start by creating a list of all the hyperspectral data and their unique identifiers
####################
short.list <- list.files(paste0(wd_location, "/Reflectance"), full.names = F)
short.list <- short.list[-grep(".hdr",short.list)]

base.names <- unlist(lapply(strsplit(short.list, split = "_at"), function(x) return(x[1])))

all_chm <- list.files("GLiHT/CHM/", pattern = ".tif")
tifFiles <- paste0(base.names, "_CHM.tif")

data_presence <- tifFiles %in% all_chm
table(data_presence)
tifFiles[!data_presence]

####################
## Convert the tif's to ascii files so they can be read into FUSION - do this in parallel
####################
tifToAscii <- function(files){
  library(raster)
  names <- unlist(strsplit(files, ".tif"))
  data_foldername <- "GLiHT/CHM/"
  setwd(data_foldername)
  
  r <- raster(files)
  NAvalue(r) <- -9999
  writeRaster(r, paste0(data_foldername, "/ascii/", names, ".asc"), format = "ascii", datatype = 'INT4S')
}

library(parallel)
no_cores <- detectCores() -1
cl <- makeCluster(no_cores)

parLapply(cl, tifFiles, tifToAscii)
stopCluster(cl)

asciiFiles <- list.files(paste0("GLiHT/CHM/ascii"), pattern = ".asc")
BaseNames <- unlist(strsplit(asciiFiles, ".asc"))

####################
## Run fusion commands to convert CHM ascii files to .dtm files, and then 
## use GridSurfaceStats to get CHM metrics
####################

## If you wanted to create a batch file, you could do it this way, or you could use the system calls below...
# sink("GLiHT/CHM/asc_to_dtm.bat")
# for(i in 1:length(asciiFiles)){
#   line <- paste0("ASCII2DTM ", "dtm/", BaseNames[i], ".dtm M M 0 0 0 0 ", "ascii/", BaseNames[i],".asc")
#   cat(line)
#   cat("\n")
#   
#   line2 <- paste0("GridSurfaceStats /ascii ", "dtm/", BaseNames[i], ".dtm ", BaseNames[i],"_13m.dtm 13")
#   cat(line2)
#   cat("\n")
#   
# }
# sink()

## These system calls require that you have FUSION installed and added as an environment variable:
## Download FUSION: http://forsys.cfr.washington.edu/fusion/fusionlatest.html
## Add environment variable: https://www.fs.fed.us/eng/rsac/lidar_training/Advanced_Lidar_Processing/story_content/external_files/Exer01_setup.pdf
distribute_CHMMetric <- function(x){
  loc <- "GLiHT/CHM"  ## Location of the GLiHT CHM Data
  system(paste0("ASCII2DTM ", loc,"/dtm/", x, ".dtm M M 0 0 0 0 ", loc, "/ascii/", x,".asc"), wait = T)
  system(paste0("GridSurfaceStats /ascii ", loc,"/dtm/", x, ".dtm ", loc, "/ascii/", x,"_13m.dtm 13"))
}

library(parallel)
no_cores <- detectCores() -2
cl <- makeCluster(no_cores)

parLapply(cl, BaseNames[1:length(BaseNames)], distribute_CHMMetric)
stopCluster(cl)

asciiFiles <- list.files(paste0("IGLiHT/CHM/ascii"), pattern = ".asc")
BaseNames <- unlist(strsplit(asciiFiles, ".asc"))

