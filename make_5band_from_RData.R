## Make 5-band previews

rm(list=ls())
###############
## libraries ##
library(rgdal)

###########################################
## set wavelengths for reflactance bands ##
## These are the unique wavelengths found in the GLiHT hyperspectral data we recieved
wavelength <- c(418.384277,422.810944,427.237701,431.664429,436.091156,440.517853,444.944580,449.371338,453.798004,458.224762,462.651489,467.078217,
                471.504913,475.931641,480.358368,484.785065,489.211792,493.638550,498.065277,502.491943,506.918701,511.345367,515.772156,520.198853,
                524.625549,529.052307,533.479004,537.905762,542.332458,546.759155,551.185913,555.612610,560.039368,564.466064,568.892822,573.319519,
                577.746216,582.172974,586.599670,591.026367,595.453125,599.879883,604.306580,608.733276,613.159973,617.586731,622.013428,626.440186,
                630.866882,635.293579,639.720337,644.147034,648.573792,653.000488,657.427246,661.853943,666.280640,670.707397,675.134094,679.560791,
                683.987549,688.414307,692.841003,697.267761,701.694519,706.121155,710.547852,714.974609,719.401367,723.827942,728.254700,732.681458,
                737.108215,741.534912,745.961670,750.388367,754.815125,759.241882,763.668518,768.095215,772.521973,776.948730,781.375427,785.802063,
                790.228821,794.655579,799.082275,803.509033,807.935791,812.362488,816.789246,821.215881,825.642578,830.069336,834.496094,838.922791,
                843.349426,847.776184,852.202942,856.629639,861.056396,865.483154,869.909851,874.336609,878.763367,883.190002,887.616699,892.043457,
                896.470215,900.896790,905.323547,909.750305,914.177063,918.603760)


## set red green blue and near-infrared bands ##
NIR   <- which(abs(wavelength - 800) == min(abs(wavelength - 800)))
RE    <- which(abs(wavelength - 710) == min(abs(wavelength - 710)))
RED   <- which(abs(wavelength - 650) == min(abs(wavelength - 650)))
GREEN <- which(abs(wavelength - 550) == min(abs(wavelength - 550)))
BLUE  <- which(abs(wavelength - 475) == min(abs(wavelength - 475)))

file.list <- list.files("C:/GLiHTdelivery/Rdata", full.names = TRUE)

file.short.list <- list.files("C:/GLiHTdelivery/Rdata")
file.short.list <- unlist(strsplit(file.short.list, ".RData"))

for(i in 1:length(file.list)){
  load(file.list[i])
  
  Name <- ls()[grep("dat.l",ls())]
  Name = Name[1]
  dat <- get(Name)
  eval(parse(text=paste('rm(', Name, ')')))
  
  dat@data <- dat@data[,c(BLUE,GREEN,RED,RE,NIR)]
  
  writeGDAL(dat, paste0("C:/GLiHTdelivery/GLiHT_5band/",file.short.list[i],".tif"))
  rm(dat);gc()
  print(i)
}   