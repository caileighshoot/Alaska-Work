## Make Stacks of G-LiHT Data
## GLiHT Functions
library(rgdal)
library(raster)
require(sp)
require(gdalUtils)
require(randomForest) #randomforest
require(e1071) #svm
require(nnet) #multinom
require(RColorBrewer)
require(Rfast)

#########################################
## Calculate Vegetation Indices 
########################################

## This function grabs the band closest to a specified wavelength
R <- function(band, ## Specify wavelength
              d = data, ## Specify the data frame of hyperspectral data
              nc = 0){ ## Specify the number of columns to include as offset (are there columns at the beginning of the df that you want to omit?)
  ## These are all the wavelengths for G-LiHT
  wavelength <- c(418.384277,422.810944,427.237701,431.664429,436.091156,440.517853,444.944580,449.371338,453.798004,458.224762,
                  462.651489,467.078217,471.504913,475.931641,480.358368,484.785065,489.211792,493.638550,498.065277,502.491943,
                  506.918701,511.345367,515.772156,520.198853,524.625549,529.052307,533.479004,537.905762,542.332458,546.759155,
                  551.185913,555.612610,560.039368,564.466064,568.892822,573.319519,577.746216,582.172974,586.599670,591.026367,
                  595.453125,599.879883,604.306580,608.733276,613.159973,617.586731,622.013428,626.440186,630.866882,635.293579,
                  639.720337,644.147034,648.573792,653.000488,657.427246,661.853943,666.280640,670.707397,675.134094,679.560791,
                  683.987549,688.414307,692.841003,697.267761,701.694519,706.121155,710.547852,714.974609,719.401367,723.827942,
                  728.254700,732.681458,737.108215,741.534912,745.961670,750.388367,754.815125,759.241882,763.668518,768.095215,
                  772.521973,776.948730,781.375427,785.802063,790.228821,794.655579,799.082275,803.509033,807.935791,812.362488,
                  816.789246,821.215881,825.642578,830.069336,834.496094,838.922791,843.349426,847.776184,852.202942,856.629639,
                  861.056396,865.483154,869.909851,874.336609,878.763367,883.190002,887.616699,892.043457,896.470215,900.896790,
                  905.323547,909.750305,914.177063,918.603760)
  if(length(band) == 1){
    out <- which(abs(wavelength - band) == min(abs(wavelength - band)))
    d.out <- d[,out + nc]
  } else {
    out <- c()
    for(i in 1:length(band)){
      out <- c(out, which(abs(wavelength - band[i]) == min(abs(wavelength - band[i]))))
      out <- unique(out)
    }
    d.out <- rowMeans(d[,out + nc])
  }
  return(d.out)
}


calculate_veg_indices <- function(data = raw.hyp@data, ## Specify the dataframe of hyperspectral data
                                  n_col = 0){
  
  # Define the wavelengths that are of interest...
  NIR   <- 800
  REDEDGE <- 710
  RED   <- 650
  GREEN <- 550
  BLUE  <- 475
  
  ## Basic 5 bands
  NIR.w <- R(NIR)
  REDEDGE.w <- R(REDEDGE)
  RED.w <- R(RED)
  GREEN.w <- R(GREEN)
  BLUE.w <- R(BLUE)
  
  ## Multispectral Agricultural Indices from Auburn University
  NDVI <- (R(NIR) - R(RED))/(R(NIR) + R(RED))
  NDRE <- (R(NIR) - R(REDEDGE))/(R(NIR) + R(REDEDGE))
  SR <- R(NIR)/R(RED)
  SR_RE <- R(NIR)/R(REDEDGE)
  ISR <- (1-NDVI)/(1+NDVI)
  ISR_NDRE <- (1-NDRE)/(1+NDRE)
  CSM <- R(RED)/R(NIR)
  CSM_RE <- R(REDEDGE)/R(NIR)
  Datt <-(R(NIR) - R(REDEDGE))/(R(NIR) - R(RED))
  
  ## Additional Hyperspectral Indices
  ### Structure
  EVI <- ((2.5*R(NIR))-R(RED))/((R(NIR))+(6*R(RED))-(7.5*R(BLUE))+1)
  VARI <- (R(GREEN)-R(RED))/(R(GREEN)+R(RED)-R(BLUE))
  VIgreen <- (R(GREEN)-R(RED))/(R(GREEN)+R(RED))
  
  ### Biochemical
  ##### Pigments
  SIPI <- (R(NIR)-R(445))/(R(NIR)-R(680))
  PSSR_a <- R(NIR)/R(675)
  PSSR_b <- R(NIR)/R(650)
  PSND_a <- (R(NIR)-R(675))/(R(NIR)+R(675))
  PSND_b <- (R(NIR)-R(650))/(R(NIR)+R(650))
  PSRI <- (R(680)-R(500))/R(750)
  
  ##### Chlorophyll
  CARI <- (R(700)-R(670))- #ERROR: THIS WAS / INSTEAD OF - 
    (.2*(R(700)-R(550)))
  
  MCARI <- (R(700)-R(670))- #ERROR: THIS WAS / INSTEAD OF - 
    (.2*(R(700)-R(550))*(R(700)/R(670)))
  
  CI_RE <- (R(NIR)/R(REDEDGE))-1
  
  ##### Anthrocyanins
  ARI <- (1/R(GREEN))-(1/R(REDEDGE))
  mARI <- ((1/R(GREEN))-(1/R(REDEDGE)))*R(NIR)
  RGRI <- R(RED)/R(GREEN)                              
  ACI  <- R(GREEN)/R(NIR) 
  
  ##### Carotenoids 
  CRI1 <- (1/R(510))-(1/R(550))
  CRI2 <- (1/R(510))-(1/R(700))
  ### Bind together into matrix of predictors
  h.predictors <- data.frame(cbind(NIR.w, REDEDGE.w, RED.w, GREEN.w, BLUE.w, ## Basic Spectra
                                   NDVI, NDRE, SR, SR_RE, ISR, ISR_NDRE, CSM, CSM_RE, Datt, ## Ag Indices
                                   EVI, VARI, VIgreen, ## Structure
                                   SIPI, PSSR_a, PSSR_b, PSND_a, PSND_b, PSRI, ## Pigments
                                   CARI, MCARI, CI_RE, ## Chlopophyll
                                   ARI, mARI, RGRI, ACI, ## Anthrocyanins
                                   CRI1, CRI2)) ## Carotenoids
  
  return(h.predictors)
}

#########################################
## Credit to Sean Jeronimo for these two functions, used in 
## the topometrics function
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
## Calculate Cover Above 4.5 Feet
########################################
calc_cover <- function(x, ...){
  num <- length(na.omit(x))
  
  feet = 4.5
  meters = feet * 0.3048
  num_over <- length(na.omit(x)[na.omit(x) > meters])
  
  percent <- (num_over/num)*100
  return(percent)
}


#########################################
## Grab CHM Layers and Stack Them
########################################
### CHM layers were made using FUSION:
## http://forsys.cfr.washington.edu/fusion/fusionlatest.html
## This grabs the resulting files and makes them into a stacked and resampled spatial data frame
make_CHMMetric_Stack <- function(chm = raw.chm, BaseNames = files){
  r.chm <- raster(chm)
  
  ## Surface Volume
  sv <- readGDAL(paste0("CHM/ascii/", BaseNames, "_CHM_13m_surface_volume.asc"))
  sv <- resample(raster(sv), r.chm, method = "bilinear")
  sv <- as(sv, "SpatialGridDataFrame")
  
  ## Surface Volume Ratio
  svr <- readGDAL(paste0("CHM/ascii/", BaseNames, "_CHM_13m_surface_volume_ratio.asc"))
  svr <- resample(raster(svr), r.chm, method = "bilinear")
  svr <- as(svr, "SpatialGridDataFrame")
  
  ## Surface Area Ratio
  sar <- readGDAL(paste0("CHM/ascii/", BaseNames, "_CHM_13m_surface_area_ratio.asc"))
  sar <- resample(raster(sar), r.chm, method = "bilinear")
  sar <- as(sar, "SpatialGridDataFrame")
  
  ## Stddev Height
  sdH <- readGDAL(paste0("CHM/ascii/", BaseNames, "_CHM_13m_stddev_height.asc"))
  sdH <- resample(raster(sdH), r.chm, method = "bilinear")
  sdH <- as(sdH, "SpatialGridDataFrame")
  
  ## Potential Volume
  pv <- readGDAL(paste0("CHM/ascii/", BaseNames, "_CHM_13m_potential_volume.asc"))
  pv <- resample(raster(pv), r.chm, method = "bilinear")
  pv <- as(pv, "SpatialGridDataFrame")
  
  ## Mean Height
  meanH <- readGDAL(paste0("CHM/ascii/", BaseNames, "_CHM_13m_mean_height.asc"))
  meanH <- resample(raster(meanH), r.chm, method = "bilinear")
  meanH <- as(meanH, "SpatialGridDataFrame")
  
  ## Max Height
  maxH <- readGDAL(paste0("CHM/ascii/", BaseNames, "_CHM_13m_max_height.asc"))
  maxH <- resample(raster(maxH), r.chm, method = "bilinear")
  maxH <- as(maxH, "SpatialGridDataFrame")
  
  ## Cover above 4.5 ft
  cover <- aggregate(x = r.chm, fact = 13, fun = calc_cover, na.rm = F)  
  cover <- resample(cover, r.chm, method = "bilinear")  ## THERE WAS AN ERROR HERE: COVER WAS SWITCHED WITH MEANH :(
  cover <- as(cover, "SpatialGridDataFrame")
  
  ## Stack them all
  chm@data <- cbind(chm@data, sv@data, svr@data, sar@data, 
                    sdH@data, pv@data, meanH@data, maxH@data, 
                    cover@data) # Create the stack
  
  ## Convert to SP Pixels DF
  colnames(chm@data) <- c("chm", "surface_volume", "surface_volume_ratio", 
                          "surface_area_ratio", "stddev_height", "potential_volume",
                          "mean_height", "max_height", "cover_above_4.5ft")
  
  return(chm)
}


#########################################
## Create Topometric Layers and Stack Them
########################################
## This creates topography metrics from a raw dtm
make_topoMetric_Stack <- function(dtm = raw.dtm){
  r.dtm <- raster(dtm)
  
  ## Slope
  slope <- terrain(x = r.dtm, opt = "slope", 
                   unit = 'tangent', neighbors = 8)
  slope_smoothed <- focal(x = slope, w=getCircularKernel(6), 
                          mean, na.rm=TRUE, pad=FALSE)
  slope_smoothed <- as(slope_smoothed, "SpatialGridDataFrame");nrow(slope_smoothed)
  
  ## Aspect
  aspect <- terrain(x = r.dtm, opt = "aspect", neighbors = 8)
  aspect <- as(aspect, "SpatialGridDataFrame");nrow(aspect)
  
  ## TPI
  tpi <- TPI(r.dtm, 6) ### A radius of 6 gives a 13 x 13 cell circular window
  tpi <- as(tpi, "SpatialGridDataFrame");nrow(tpi)
  
  ## TRI
  tri <- terrain(x = r.dtm, opt = "tri", neighbors = 8)
  tri <- as(tri, "SpatialGridDataFrame");nrow(tri)  
  
  ## Roughness
  roughness <- terrain(x = r.dtm, opt = "roughness", neighbors = 8)
  roughness <- as(roughness, "SpatialGridDataFrame");nrow(roughness)
  
  nrow(dtm)
  
  ## Stack them all
  dtm@data <- cbind(aspect@data, dtm@data, roughness@data, 
                    slope_smoothed@data, tpi@data, tri@data) # Create the stack
  
  ## Convert to SP Pixels DF
  colnames(dtm@data) <- c("aspect", "dtm", "roughness",
                          "slope_smoothed", "tpi", "tri")
  
  ## Convert to SP Pixels DF
  return(dtm)
}


#########################################
## Put it all together to make the layers and stacks
########################################
make_veg_indices_layer <- function(files, 
                                   wd = "GLiHT"){
  print("Loading Files")
  
  ## Read in the file
  raw.hyp <- readGDAL(paste0(wd, "/Reflectance/", files, "_at-sensor_refl_L1G"))
  
  raw.dtm <- readGDAL(paste0(wd, "/DTM/", files, "_DTM.tif"))
  raw.dtm@data[is.na(raw.dtm@data)] <- 0
  dtm.m <- as.matrix(raw.dtm@data)
  dtm.m[is.na(dtm.m)] <- 0
  raw.dtm@data <- data.frame(dtm.m)
  
  raw.chm <- readGDAL(paste0(wd, "/CHM/", files, "_CHM.tif"))
  chm.m <- as.matrix(raw.chm@data)
  chm.m[is.na(chm.m)] <- 0
  raw.chm@data <- data.frame(chm.m)
  
  gc()
  
  
  #####################
  ## CHM
  #####################
  print("CHM Metrics...")
  chm.metrics <- make_CHMMetric_Stack(chm = raw.chm, BaseNames = files)
  chm.metrics@data[sapply(chm.metrics@data, is.infinite)] <- 0
  
  
  gc()
  #####################
  ## Hyperspectral
  #####################
  print("Hyperspectral...")
  raw.hyp@data <- raw.hyp@data/10000
  
  ## Make the vegetation Indices
  print("Hyperspectral Veg Indices")
  h.indices <- raw.hyp
  h.indices@data <- calculate_veg_indices(data = h.indices@data)
  
  gc()
  
  #####################
  ## DTM
  #####################
  print("DTM Metrics...")
  dtm.indices <- make_topoMetric_Stack(raw.dtm)
  
  
  #####################
  ## Remove Bordering 0's
  #####################
  ## We take the sums of all datasets, in order to identify pixels outside of our data area.
  print("Remove Bordering 0's")
  all.dat <- h.indices
  all.dat@data <- cbind(dtm.indices@data, chm.metrics@data, h.indices@data, raw.hyp@data)
  all.dat@data[is.na(all.dat@data)] <- 0
  
  
  all.sums <- as.matrix(rowsums(as.matrix(all.dat@data), parallel = T))
  all.sums[is.na(all.sums)] <- 0 ## files the NA's need to be assigned as 0's
  all.sums[is.infinite(all.sums)] <- 0
  nonzerosums <- !(all.sums == 0)
  
  ## This process removes the 0's around the border of the raw hyperspectral data
  sub.coords <- coordinates(all.dat)[nonzerosums,]
  sub.dat <- data.frame(all.dat@data[nonzerosums,])
  
  coordinates(sub.dat) <- sub.coords
  proj4string(sub.dat) <- proj4string(all.dat)
  gridded(sub.dat) <- TRUE
  
  all.dat <- sub.dat
  rm(sub.dat); rm(sub.coords)
  
  #####################
  ## Get Object Names -- CHECK THIS WORKS!!!
  #####################
  ## CHECK THIS PART WORKS BEFORE RUNNING!!
  funtimes <- function(x){
    if(x[3] == "TIU"){
      return(x[4])
    } else if(x[3] == "tiu"){
      return(x[4])
    } else if(x[3] == "Tanana"){
      return(x[5]) ## CHECK THIS PART WORKS BEFORE RUNNING!!
    } else if(x[1] == "Bonanza"){
      return(x[5])
    } else if(x[3] == "CPC"){
      return(x[5])
    } else if(x[3] == "AIRIS"){
      return(x[4])
    } else if(x[3] == "DoD2"){
      return(x[4])
    } else if(x[3] == "am"){
      return(x[4])
    } else if(x[3] == "pm"){
      return(x[4])
    }else {
      return(x[3])
    }
  }
  
  ## CHECK THIS PART WORKS BEFORE RUNNING!!
  name <- paste("dat.", sapply(strsplit(files, split = "_"), FUN = funtimes), sep = "") 
  gc()
  
  #####################
  ## Remove Extras
  #####################
  rm(chm.metrics, dtm.indices, h.indices, raw.chm, 
     nonzerosums, raw.dtm)
  gc()
  
  #####################
  ## Export as 1m Rdata File
  #####################
  print("Export as 1m Rdata File")
  assign(name,all.dat)
  
  do.call(save, list(name, file = paste0(paste0(wd, "/RData_1m/"),files,".RData")))
  print(paste0("Exported to: ", wd, "/RData_1m/",files,".RData"))
  do.call(rm, list(name))
  
  gc()
  
  #####################
  ## Resample to 13 meters
  #####################
  print("Resample to 13 meters")
  all.dat_13m <- aggregate(x = brick(all.dat), fact = 13, FUN = mean)
  all.dat_13m <- as(all.dat_13m,"SpatialPixelsDataFrame")
  
  all.sums_13m <- as.matrix(rowsums(as.matrix(all.dat_13m@data[,2:ncol(all.dat_13m@data)]), parallel = T))
  all.sums_13m[is.na(all.sums_13m)] <- 0 ## files the NA's need to be assigned as 0's
  all.sums_13m[is.infinite(all.sums_13m)] <- 0
  nonzerosums_13m <- !(all.sums_13m == 0)
  
  ## This process removes the 0's around the border of the raw hyperspectral data
  sub.coords <- coordinates(all.dat_13m)[nonzerosums_13m,]
  sub.dat <- data.frame(all.dat_13m@data[nonzerosums_13m,])
  
  coordinates(sub.dat) <- sub.coords
  proj4string(sub.dat) <- proj4string(all.dat)
  gridded(sub.dat) <- TRUE
  
  all.dat_13m <- sub.dat
  rm(sub.dat); rm(sub.coords)
  
  #####################
  ## Export 13 meter data to RData File
  #####################
  print("Export 13 meter data to RData File")
  assign(name,all.dat_13m)
  
  do.call(save, list(name, file = paste0(wd,"/RData_13m/",files,".RData")))
  print(paste0("Exported to: ", wd, "/RData_13m/",files,".RData"))
  do.call(rm, list(name))
  
  gc()
}