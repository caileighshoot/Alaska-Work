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
## Make Predictions with Models
########################################
best_columns = 1:10 ## Change this based on model inputs/columns that worked best

makePredictions <- function(n = files, r, columns = best_columns){
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
  load("final_models.RData") ## Load in the models you wish to use
  
  r <- readGDAL(r)
  
  ### Predictions 
  rf_prediction <- predict(object = rf_final, r@data[])
  svm_prediction <- predict(object = svm_final, r@data)
  multi_prediction <- predict(object = multi_final, r@data)
  
  predictions <- r
  predictions@data <- cbind(data.frame(rf_prediction), 
                            svm_prediction, 
                            multi_prediction)
  
  ## Make Tiff Files
  writeRaster(raster(predictions["rf_prediction"]), 
              filename = paste0("Model_Predictions/RandomForest/", n, ".tif"))
  writeRaster(raster(predictions["svm_prediction"]), 
              filename = paste0("Model_Predictions/SVM/", n, ".tif"))
  writeRaster(raster(predictions["multi_prediction"]),
              filename = paste0("Model_Predictions/MultinomLogisticRegression/", n, ".tif"))
}

#########################################
## Make the predictions in parallel
########################################
library(parallel)
no_cores <- detectCores() -1
cl <- makeCluster(no_cores)

parLapply(cl, list_of_13mRdata_Files, makePredictions)
stopCluster(cl)

