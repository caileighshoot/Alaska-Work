### Setup Model inputs

# Data Setup
# Bring in data...
data <- read.csv(file = "allPlots_hyp_CHM_DTM_rfdata.csv",
                 header = T, row.names = 1)

### Remove all NAs
data <- data[complete.cases(data), ]
complete.cases(data)

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

h.predictors = calculate_veg_indices(data = data)

predictors <- cbind(data[,c(which(names(data) == "aspect"):n_col)], h.predictors)

# Set up response dataset
response <- as.matrix(data[,"FLDTYPCD"]) ## This is the column containing forest type classes 
table(response)

#### LOOK AT THE TABLE BEFORE RUNNING THIS!! MAKE SURE ALL VALUES ARE ACCOUNTED FOR!!
response[response == 0] <- 1   # Non-forest
response[response == 122] <- 2 # White Spruce
response[response == 125] <- 3 # Black Spruce
response[response == 126] <- 4 # Tamarack
response[response == 703] <- 5 # Cotonwood 
response[response == 901] <- 6 # Aspen
response[response == 902] <- 7 # Paper Birch
response[response == 904] <- 8 # Balsam Popla

rm(list=ls()[! ls() %in% c("predictors", "response", "data")])

