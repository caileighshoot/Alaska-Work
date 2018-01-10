## Make 13m scaled hyperspectral data

library(rgdal) ## this will automatically load the sp package.
library(raster)

aggregate_to_13m <- function(wd = "C:/GLiHTdelivery5", 
                             rdata_foldername = "C:/GLiHTdelivery5/GLiHT5_Rdata", 
                             resolution = 13, 
                             template_foldername = "C:/GLiHTdelivery5/Template_Rasters_13m", 
                             valuematrices_foldername = "C:/GLiHTdelivery5/Value_Matrices_13m",
                             nbands = 114){
  setwd(wd)
  
  file.list <- list.files(rdata_foldername) 
  
  
  for(j in 1:length(file.list)){
    load(paste0(rdata_foldername, "/",file.list[j]))
    print(paste0("I am currently on File #", j))  
    
    Name <- ls()[grep("dat.l",ls())]
    Name = Name[1]
    h.dat <- get(Name)
    eval(parse(text=paste('rm(', Name, ')')))
    
    
    template_raster <- aggregate(x = raster(h.dat[1]), fact = resolution, FUN = mean)
    values.matrix <- as.matrix((values(template_raster))[!is.na((values(template_raster)))])
    
    print("Beginning creation of data matrix")
    for(k in 2:nbands){
      h.dat13 <- aggregate(x = raster(h.dat[k]), fact = resolution, FUN = mean)
      values.matrix <- cbind(values.matrix, as.matrix((values(h.dat13))[!is.na((values(h.dat13)))]))
    }
    print("Data matrix complete, now exporting data!")
    
    #do.call(write.csv, args = list(values.matrix, paste0("Value_Matrices_13m/", Name, "_13m.csv")))
    write.csv(values.matrix, file = paste0("Value_Matrices_13m/", Name, "_13m.csv"))
    writeRaster(template_raster, filename = paste0("Template_Rasters_13m/", Name, "_13m.tif"), overwrite = T)
    print("All done! Onto the next file! :-)")
    
    rm(h.dat13)
    rm(h.dat)
    rm(Name)
    
    gc()
  }
}

aggregate_to_13m()

# This is how you convert from the value matrices to the template rasters for visualizing a specific band
matrix_to_raster <- function(template_raster, values_matrix){
  template_values <- as.matrix(values(template_raster))
  na.vals <- complete.cases(as.matrix(values(template_raster)))
  template_values[na.vals] <- values.matrix[,1]
  new.raster <- raster(template_values, template = template_raster)
  image(new.raster)
  return(new.raster)
}
