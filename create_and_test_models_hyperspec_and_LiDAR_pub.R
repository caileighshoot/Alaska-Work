rm(list = ls())

## RF Model
require(randomForest)
require(raster)
require(e1071)
require(caret)
require(class)
require(nnet)
require(vegan)

source("setup_predictor_and_response_hyperspec_and_LiDAR.R")

predictors <- cbind(predictors,
                    data[,which(names(data) == "X418.384277"):which(names(data) == "X918.60376")])

###################################################################
## K-fold cross validation and Bootstrapping Functions
###################################################################

ksample <- function(x, r = response, len = 1, bootstrap = F){
  r1 <- which(r == x)
  N <- sample(1:3)
  
  if(!bootstrap){
    if(length(r1) > 3){
      samp1 <- split(sample(r1),
                     c(rep(N[1], times = round(length(r1)/3)),
                       rep(N[2], times = round(length(r1)/3)),
                       rep(N[3], times = (length(r1) - (round(length(r1)/3) * 2)))))
    } else if (length(r1) == 2) {
      samp1 <- split(sample(c(r1, sample(r1, 
                                         size = 1))),
                     c(rep(N[1], times = 1),
                       rep(N[2], times = 1),
                       rep(N[3], times = 1)))
      samp1 <- list(r1[1], r1[2], r1[sample(1:2, 1)])
    } else {
      samp1 <- list(r1[1], r1[1], r1[1])
    }
  } else {
    if(length(r1) >= len){
      samp1 <- split(sample(r1, size = len),
                     c(rep(N[1], times = round(len/3)),
                       rep(N[2], times = round(len/3)),
                       rep(N[3], times = (len - (round(len/3) * 2)))))
    } else if (length(r1) < len && length(r1) > 6) {
      samp1.initial <- split(sample(r1),
                             c(rep(N[1], times = round(length(r1)/3)),
                               rep(N[2], times = round(length(r1)/3)),
                               rep(N[3], times = (length(r1) - (round(length(r1)/3) * 2)))))
      
      samp1.n1 <- sample(samp1.initial[[1]], size = round(len/3), replace = T)
      samp1.n2 <- sample(samp1.initial[[2]], size = round(len/3), replace = T)
      samp1.n3 <- sample(samp1.initial[[3]], size = (len - (round(len/3) * 2)), replace = T)
      
      samp1 <- list('1' = samp1.n1, '3' = samp1.n2, '3' =  samp1.n3)
      
    } else if (length(r1) < 6 && length(r1) != 1) {
      samp1.initial <- split(sample(r1),
                             c(rep(N[1], times = round(length(r1)/3)),
                               rep(N[2], times = round(length(r1)/3)),
                               rep(N[3], times = (length(r1) - (round(length(r1)/3) * 2)))))
      
      if(length(samp1.initial[[1]]) > 1){
        samp1.n1 <- sample(samp1.initial[[1]], size = round(len/3), replace = T)
      } else {
        samp1.n1 <- rep(samp1.initial[[1]], times = round(len/3))
      }
      
      if(length(samp1.initial[[2]]) > 1){
        samp1.n2 <- sample(samp1.initial[[2]], size = round(len/3), replace = T)
      } else {
        samp1.n2 <- rep(samp1.initial[[2]], times = round(len/3))
      }
      
      if(length(samp1.initial[[3]]) > 1){
        samp1.n3 <- sample(samp1.initial[[3]], size = (len - (round(len/3) * 2)), replace = T)
      } else {
        samp1.n3 <- rep(samp1.initial[[3]], times = (len - (round(len/3) * 2)))
      }
      
      samp1 <- list('1' = samp1.n1, '3' = samp1.n2, '3' =  samp1.n3)
      
    } else {
      samp1 <- split(rep(r1, times = len),
                     c(rep(N[1], times = round(len/3)),
                       rep(N[2], times = round(len/3)),
                       rep(N[3], times = (len - (round(len/3) * 2)))))
    }
  }
  return(samp1)
}

grab_testing <- function(x, s1 = samp1, s2 = samp2,
                         s3 = samp3, s4 = samp4, 
                         s5 = samp5, s6 = samp6,
                         s7 = samp7, s8 = samp8){
  t <- c(s1[[x]], s2[[x]], s3[[x]], 
         s4[[x]], s5[[x]], s6[[x]], 
         s7[[x]], s8[[x]])
  return(t)
}

grab_training <- function(a, b, s1 = samp1, s2 = samp2,
                          s3 = samp3, s4 = samp4, 
                          s5 = samp5, s6 = samp6,
                          s7 = samp7, s8 = samp8){
  t <- c(c(s1[[a]], s1[[b]]),
         c(s2[[a]], s2[[b]]),
         c(s3[[a]], s3[[b]]),
         c(s4[[a]], s4[[b]]),
         c(s5[[a]], s5[[b]]),
         c(s6[[a]], s6[[b]]),
         c(s7[[a]], s7[[b]]),
         c(s8[[a]], s8[[b]]))
  return(t)
}

grab_testing.b <- function(x, s1 = samp1.b, s2 = samp2.b,
                           s3 = samp3.b, s4 = samp4.b, 
                           s5 = samp5.b, s6 = samp6.b,
                           s7 = samp7.b, s8 = samp8.b){
  t <- c(s1[[x]], s2[[x]], s3[[x]], 
         s4[[x]], s5[[x]], s6[[x]], 
         s7[[x]], s8[[x]])
  return(t)
}

grab_training.b <- function(a, b, s1 = samp1.b, s2 = samp2.b,
                            s3 = samp3.b, s4 = samp4.b, 
                            s5 = samp5.b, s6 = samp6.b,
                            s7 = samp7.b, s8 = samp8.b){
  t <- c(c(s1[[a]], s1[[b]]),
         c(s2[[a]], s2[[b]]),
         c(s3[[a]], s3[[b]]),
         c(s4[[a]], s4[[b]]),
         c(s5[[a]], s5[[b]]),
         c(s6[[a]], s6[[b]]),
         c(s7[[a]], s7[[b]]),
         c(s8[[a]], s8[[b]]))
  return(t)
}


###################################################################
## K-fold cross validation
###################################################################
samp1 <- ksample(1)
samp2 <- ksample(2)
samp3 <- ksample(3)
samp4 <- ksample(4)
samp5 <- ksample(5)
samp6 <- ksample(6)
samp7 <- ksample(7)
samp8 <- ksample(8)

testing1 <- grab_testing(1);table(response[testing1])
training1 <- grab_training(a = 2, b = 3);table(response[training1])

testing2 <- grab_testing(2);table(response[testing2])
training2 <- grab_training(a = 1, b = 3);table(response[training2])

testing3 <- grab_testing(3);table(response[testing3])
training3 <- grab_training(a = 1, b = 2);table(response[training3])

###################################################################
## K-fold cross validation with bootstrapping
###################################################################
l <- max(table(response)) ## Length of each sample
samp1.b <- ksample(1, len = l, bootstrap = T)
samp2.b <- ksample(2, len = l, bootstrap = T)
samp3.b <- ksample(3, len = l, bootstrap = T)
samp4.b <- ksample(4, len = l, bootstrap = T)
samp5.b <- ksample(5, len = l, bootstrap = T)
samp6.b <- ksample(6, len = l, bootstrap = T)
samp7.b <- ksample(7, len = l, bootstrap = T)
samp8.b <- ksample(8, len = l, bootstrap = T)

testing1.b <- grab_testing.b(1);table(response[testing1.b])
training1.b <- grab_training.b(a = 2, b = 3);table(response[training1.b])

testing2.b <- grab_testing.b(2);table(response[testing2.b])
training2.b <- grab_training.b(a = 1, b = 3);table(response[training2.b])

testing3.b <- grab_testing.b(3);table(response[testing3.b])
training3.b <- grab_training.b(a = 1, b = 2);table(response[training3.b])

###################################################################
## Columns
###################################################################
topo_columns <- which(names(predictors) == "aspect"):which(names(predictors) == "tri")
chm_columns <- which(names(predictors) == "chm"):which(names(predictors) == "cover_above_4.5ft")
hyp_columns <- which(names(predictors) == "NIR.w"):which(names(predictors) == "CRI2")
hypBands_columns <- which(names(predictors) == "X418.384277"):which(names(predictors) == "X918.60376")
all_columns <- 1:ncol(predictors)
allNoBands_columns <- 1:(which(names(predictors) == "X418.384277")-1)

###########################################
#### Random Forest
##########################################
rf_model <- function(pred  = predictors, resp = response,
                     test1 = testing1, train1 = training1,
                     test2 = testing2, train2 = training2,
                     test3 = testing3, train3 = training3,
                     columns = all_columns,
                     msg = "Hyp Bands and Vegetation Indices with CHM and DTM Metrics"){
  
  #### Create Random Forest Models ####
  rf1 <- randomForest(x = pred[train1, columns],
                          y = as.factor(resp)[train1],
                      ntrees = 5000)
  rf2 <- randomForest(x = pred[train2, columns],
                      y = as.factor(resp)[train2],
                      ntrees = 5000)
  rf3 <- randomForest(x = pred[train3, columns],
                      y = as.factor(resp)[train3],
                      ntrees = 5000)

  #### Predict  over rows dataset ####
  predicted1 <- predict(object = rf1, as.matrix(pred[test1, columns]))
  predicted2 <- predict(object = rf2, as.matrix(pred[test2, columns]))
  predicted3 <- predict(object = rf3, as.matrix(pred[test3, columns]))
  
  #### CM ####
  print("#######################################################")
  print(paste0(msg, " Model Confusion Matrix"))
  print("#######################################################")
  all_response <- as.factor(c(resp[test1],
                              resp[test2], 
                              resp[test3]))
  all_predicted <- as.factor(c(predicted1, predicted2, predicted3))
  levels(all_predicted) <- levels(all_response)
  
  cm <- confusionMatrix(all_predicted, all_response)
  print(cm)
  
  return(list(rf1, rf2, rf3, cm))
}

#### Only k-fold cross-validation ####
rf_all <- rf_model()
rf_allNoBands <- rf_model(columns = allNoBands_columns,
                          msg = "Hyp Vegetation Indices with CHM and DTM Metrics")
rf_hyp <- rf_model(columns = hyp_columns,
                   msg = "Vegetation Indices")
rf_hypBands <- rf_model(columns = hypBands_columns,
                   msg = "Hyperspectral Bands")
rf_topo <- rf_model(columns = topo_columns,
                   msg = "DTM Metrics")
rf_chm <- rf_model(columns = chm_columns,
                    msg = "CHM Metrics")

#### K-fold cross-validation and Bootstrapping ####
rf_all.b <- rf_model(test1 = testing1.b, train1 = training1.b,
                   test2 = testing2.b, train2 = training2.b,
                   test3 = testing3.b, train3 = training3.b)
rf_allNoBands.b <- rf_model(test1 = testing1.b, train1 = training1.b,
                          test2 = testing2.b, train2 = training2.b,
                          test3 = testing3.b, train3 = training3.b,
                          columns = allNoBands_columns,
                          msg = "Hyp Vegetation Indices with CHM and DTM Metrics")
rf_hyp.b <- rf_model(columns = hyp_columns,
                   msg = "Vegetation Indices",
                   test1 = testing1.b, train1 = training1.b,
                   test2 = testing2.b, train2 = training2.b,
                   test3 = testing3.b, train3 = training3.b)
rf_hypBands.b <- rf_model(columns = hypBands_columns,
                     msg = "Hyperspectral Bands",
                     test1 = testing1.b, train1 = training1.b,
                     test2 = testing2.b, train2 = training2.b,
                     test3 = testing3.b, train3 = training3.b)
rf_topo.b <- rf_model(columns = topo_columns,
                    msg = "DTM Metrics",
                    test1 = testing1.b, train1 = training1.b,
                    test2 = testing2.b, train2 = training2.b,
                    test3 = testing3.b, train3 = training3.b)
rf_chm.b <- rf_model(columns = chm_columns,
                   msg = "CHM Metrics",
                   test1 = testing1.b, train1 = training1.b,
                   test2 = testing2.b, train2 = training2.b,
                   test3 = testing3.b, train3 = training3.b)

###########################################
#### Support Vector Machine
##########################################
## Try bootstrapping here, essentially for each class sample with replacement and replicate each
## sample until you get to the value you want. 
## Class weights may also have a similar effect. 

svm_model <- function(pred  = predictors, resp = response,
                      test1 = testing1, train1 = training1,
                      test2 = testing2, train2 = training2,
                      test3 = testing3, train3 = training3,
                      columns = 1:ncol(predictors),
                      tune = F, msg = "Hyp Bands and Vegetation Indices with CHM and DTM Metrics"){
  
  all_svm1 <- svm(x = pred[train1, columns],
                 y = as.factor(resp)[train1])
  all_svm2 <- svm(x = pred[train2, columns],
                 y = as.factor(resp)[train2])
  all_svm3 <- svm(x = pred[train3, columns],
                 y = as.factor(resp)[train3])
  
  # Predict  over rows dataset
  predicted1 <- predict(object = all_svm1,
                       as.matrix(pred[test1, columns]))
  predicted2 <- predict(object = all_svm2,
                        as.matrix(pred[test2, columns]))
  predicted3 <- predict(object = all_svm3,
                        as.matrix(pred[test3, columns]))
  

  print("#######################################################")
  print(paste0(msg, " Initial Model Confusion Matrix"))
  print("#######################################################")
  all_response <- as.factor(c(response[test1],
                              response[test2], 
                              response[test3]))
  all_predicted <- as.factor(c(predicted1, predicted2, predicted3))
  levels(all_predicted) <- levels(all_response)
  
  cm <- confusionMatrix(all_predicted, all_response)
  
  print(cm)
  
  if(tune){
    ### Tune the model
    ### Try with a few different ranges, and plot the accuracies with the results. 
    print("Tuning the Model...")
    print("...1")
    svm_tune1 <- tune.svm(x= pred[train1, columns],
                         y= as.factor(resp)[train1],
                         kernel="radial",
                         cost=10^(-1:2),#(-4:4),
                         gamma= c(.5,1,2)) #c(10^(-6:4), 0.5, 1, 2))
    print("...2")
    svm_tune2 <- tune.svm(x= pred[train2, columns],
                         y= as.factor(resp)[train2],
                         kernel="radial",
                         cost=10^(-1:2), #(-4:4),
                         gamma=c(.5,1,2))
    print("...3")
    svm_tune3 <- tune.svm(x= pred[train3, columns],
                         y= as.factor(resp)[train3],
                         kernel="radial",
                         cost=10^(-1:2), #(-4:4),
                         gamma=c(.5,1,2))
    
    ### Redo model after tune
    print("Running Tuned models...")
    print("1...")
    svm_model_after_tune1 <- svm(x = pred[train1, columns],
                                y = as.factor(resp)[train1],
                                kernel="radial",
                                cost=svm_tune1$best.parameters$cost,
                                gamma=svm_tune1$best.parameters$gamma)
    print("2...")
    svm_model_after_tune2 <- svm(x = pred[train2, columns],
                                y = as.factor(resp)[train2],
                                kernel="radial",
                                cost=svm_tune2$best.parameters$cost,
                                gamma=svm_tune2$best.parameters$gamma)
    print("3...")
    svm_model_after_tune3 <- svm(x = pred[train3, columns],
                                y = as.factor(resp)[train3],
                                kernel="radial",
                                cost=svm_tune3$best.parameters$cost,
                                gamma=svm_tune3$best.parameters$gamma)
    
    # Predict  over rows dataset
    print("Making predictions...")
    print("1...")
    predicted_tune1 <- predict(object = svm_model_after_tune1,
                              as.matrix(pred[test1, columns]))
    print("2...")
    predicted_tune2 <- predict(object = svm_model_after_tune2,
                              as.matrix(pred[test2, columns]))
    print("3...")
    predicted_tune3 <- predict(object = svm_model_after_tune3,
                              as.matrix(pred[test3, columns]))

    print("#######################################################")
    print(paste0(msg, " Tuned Model Confusion Matrix"))
    print("#######################################################")
    all_response <- as.factor(c(response[test1],
                                response[test2], 
                                response[test3]))
    all_predicted_tune <- as.factor(c(predicted_tune1, predicted_tune2, predicted_tune3))
    levels(all_predicted_tune) <- levels(all_response)
    
    cm2 <- confusionMatrix(all_predicted_tune, all_response)
    
    print(cm2)
    
    return(list(all_svm1, all_svm2, all_svm3, cm, svm_model_after_tune1, svm_model_after_tune2, svm_model_after_tune3, cm2))
  } else {
    return(list(all_svm1, all_svm2, all_svm3, cm))
  }
}

#### Only k-fold cross-validation
svm_all_tune <- svm_model(tune = T)
svm_allNoBands <- svm_model(tune = T,
                            columns = allNoBands_columns,
                            msg = "Hyp Vegetation Indices with CHM and DTM Metrics")
svm_hyp_tune <- svm_model(columns = hyp_columns,
                           tune = T, msg = "Vegetation Indices")
svm_hypBands_tune <- svm_model(columns = hypBands_columns,
                          tune = T, msg = "Hyperspectral Bands")
svm_topo_tune <- svm_model(columns = topo_columns,
                          tune = T, msg = "DTM Metrics")
svm_chm_tune <- svm_model(columns = topo_columns,
                           tune = T, msg = "CHM Metrics")
#### K-fold cross-validation and Bootstrapping
svm_all_tune.b <- svm_model(test1 = testing1.b, train1 = training1.b,
                     test2 = testing2.b, train2 = training2.b,
                     test3 = testing3.b, train3 = training3.b,
                     tune = T)
svm_allNoBands.b <- svm_model(test1 = testing1.b, train1 = training1.b,
                            test2 = testing2.b, train2 = training2.b,
                            test3 = testing3.b, train3 = training3.b,
                            columns = allNoBands_columns,
                            msg = "Hyp Vegetation Indices with CHM and DTM Metrics",
                            tune = T)
svm_hyp_tune.b <- svm_model(columns = hyp_columns,
                     msg = "Vegetation Indices",
                     test1 = testing1.b, train1 = training1.b,
                     test2 = testing2.b, train2 = training2.b,
                     test3 = testing3.b, train3 = training3.b,
                     tune = T)
svm_hypBands_tune.b <- svm_model(columns = hypBands_columns,
                            msg = "Hyperspectral Bands",
                            test1 = testing1.b, train1 = training1.b,
                            test2 = testing2.b, train2 = training2.b,
                            test3 = testing3.b, train3 = training3.b,
                            tune = T)
svm_topo_tune.b <- svm_model(columns = topo_columns,
                      msg = "DTM Metrics",
                      test1 = testing1.b, train1 = training1.b,
                      test2 = testing2.b, train2 = training2.b,
                      test3 = testing3.b, train3 = training3.b,
                      tune = T)
svm_chm_tune.b <- svm_model(columns = chm_columns,
                             msg = "CHM Metrics",
                             test1 = testing1.b, train1 = training1.b,
                             test2 = testing2.b, train2 = training2.b,
                             test3 = testing3.b, train3 = training3.b,
                             tune = T)

###########################################
#### Gaussian Maximum Likelihood -- Bayes Classifier
##########################################
## This is actually a maximum likelihood bayes classifier...
## it follows the format laid out on p. 280 of Modern Photogrammetry,
## You start with a gaussian maximum likelihood estimator function for the data (x) given the class
## You want the class given the data, so you need to use bayes to do that
## The function naiveBayes does all that in one...
## https://desktop.arcgis.com/en/arcmap/10.3/tools/spatial-analyst-toolbox/how-maximum-likelihood-classification-works.htm
mlc_model <- function(pred  = predictors, resp = response,
                     test1 = testing1, train1 = training1,
                     test2 = testing2, train2 = training2,
                     test3 = testing3, train3 = training3,
                     columns = all_columns,
                     msg = "Hyp Bands and Vegetation Indices with CHM and DTM Metrics"){

  #### Create Random Forest Models ####
  all.dat <- cbind(resp, pred[columns])
  bayes1 <- naiveBayes(as.factor(resp) ~ ., data = all.dat[train1,])
  bayes2 <- naiveBayes(as.factor(resp) ~ ., data = all.dat[train2,])
  bayes3 <- naiveBayes(as.factor(resp) ~ ., data = all.dat[train3,])
  
  #### Predict  over rows dataset ####
  predicted1 <- predict(object = bayes1, pred[test1, columns])
  predicted2 <- predict(object = bayes2, pred[test2, columns])
  predicted3 <- predict(object = bayes3, pred[test3, columns])
  
  #### CM ####
  print("#######################################################")
  print(paste0(msg, " Model Confusion Matrix"))
  print("#######################################################")
  all_response <- as.factor(c(resp[test1],
                              resp[test2], 
                              resp[test3]))
  all_predicted <- as.factor(c(predicted1, predicted2, predicted3))
  levels(all_predicted) <- levels(all_response)
  
  cm <- confusionMatrix(all_predicted, all_response)
  
  print(cm)
  
  return(list(bayes1, bayes2, bayes3, cm))
}

#### Only k-fold cross-validation
mlc_all <- mlc_model()
mlc_allNoBands <- mlc_model(columns = allNoBands_columns,
                              msg = "Hyp Vegetation Indices with CHM and DTM Metrics")
mlc_hyp <- mlc_model(columns = hyp_columns,
                     msg = "Vegetation Indices")
mlc_hypBands <- mlc_model(columns = hypBands_columns,
                     msg = "Hyperspectral Bands")
mlc_topo <- mlc_model(columns = topo_columns,
                      msg = "DTM Metrics")
mlc_chm <- mlc_model(columns = chm_columns,
                     msg = "CHM Metrics")

#### K-fold cross-validation and Bootstrapping
mlc_all.b <- mlc_model(test1 = testing1.b, train1 = training1.b,
                        test2 = testing2.b, train2 = training2.b,
                        test3 = testing3.b, train3 = training3.b)
mlc_allNoBands.b <- mlc_model(test1 = testing1.b, train1 = training1.b,
                              test2 = testing2.b, train2 = training2.b,
                              test3 = testing3.b, train3 = training3.b,
                              columns = allNoBands_columns,
                              msg = "Hyp Vegetation Indices with CHM and DTM Metrics")
mlc_hyp.b <- mlc_model(columns = hyp_columns,
                        msg = "Vegetation Indices",
                        test1 = testing1.b, train1 = training1.b,
                        test2 = testing2.b, train2 = training2.b,
                        test3 = testing3.b, train3 = training3.b)
mlc_hypBands.b <- mlc_model(columns = hypBands_columns,
                            msg = "Hyperspectral Bands",
                            test1 = testing1.b, train1 = training1.b,
                            test2 = testing2.b, train2 = training2.b,
                            test3 = testing3.b, train3 = training3.b)
mlc_topo.b <- mlc_model(columns = topo_columns,
                         msg = "DTM Metrics",
                         test1 = testing1.b, train1 = training1.b,
                         test2 = testing2.b, train2 = training2.b,
                         test3 = testing3.b, train3 = training3.b)
mlc_chm.b <- mlc_model(columns = chm_columns,
                        msg = "CHM Metrics",
                        test1 = testing1.b, train1 = training1.b,
                        test2 = testing2.b, train2 = training2.b,
                        test3 = testing3.b, train3 = training3.b)


## the conditional probabilities table tells you that y given aspect, dtm, etc is....
## https://stackoverflow.com/questions/37874222/interpreting-conditional-probabilities-returned-by-naivebayes-classifier-in-e107

###########################################
#### KNN
###########################################

knn_model <- function(pred  = predictors, resp = response,
                     test1 = testing1, train1 = training1,
                     test2 = testing2, train2 = training2,
                     test3 = testing3, train3 = training3,
                     columns = all_columns,
                     msg = "Hyp Bands and Vegetation Indices with CHM and DTM Metrics"){

  accuracy1 <- rep(0, 100)
  accuracy2 <- rep(0, 100)
  accuracy3 <- rep(0, 100)
  
  k <- 1:100
  for(x in k){
    prediction1 <- knn(train = pred[train1,], 
                      test = pred[test1,], 
                      cl = resp[train1], k = x)
    accuracy1[x] <- mean(prediction1 == resp[test1])
    
    prediction2 <- knn(train = pred[train2,], 
                      test = pred[test2,], 
                      cl = resp[train2], k = x)
    accuracy2[x] <- mean(prediction2 == resp[test2])
    
    prediction3 <- knn(train = pred[train1,], 
                      test = pred[test1,], 
                      cl = resp[train1], k = x)
    accuracy3[x] <- mean(prediction3 == resp[test1])
  }
  
  plot(k, accuracy1, type = 'b', ylab = "Accuracy",
       main = paste0("Accuracy Given K with ",msg))
  lines(accuracy2, type = "b", col = "RED")
  lines(accuracy3, type = "b", col = "BLUE")
  
  predicted1 <- knn(train = pred[train1,], 
                   test = pred[test1,], cl = resp[train1], 
                   k = which(accuracy1 == max(accuracy1))[1], prob = F)
  predicted2 <- knn(train = pred[train2,], 
                    test = pred[test2,], cl = resp[train2], 
                    k = which(accuracy2 == max(accuracy2))[1], prob = F)
  predicted3 <- knn(train = pred[train3,], 
                    test = pred[test3,], cl = resp[train3], 
                    k = which(accuracy3 == max(accuracy3))[1], prob = F)
  ## Cross validation here may change this value!
  
  #### CM ####
  print("#######################################################")
  print(paste0(msg, " Model Confusion Matrix"))
  print("#######################################################")
  all_response <- as.factor(c(resp[test1],
                              resp[test2], 
                              resp[test3]))
  all_predicted <- as.factor(c(predicted1, predicted2, predicted3))
  levels(all_predicted) <- levels(all_response)
  
  cm <- confusionMatrix(all_predicted, all_response)
  
  print(cm)
  
  return(list(predicted1, predicted2, predicted3, cm))
}

#### Only k-fold cross-validation
knn_all <- knn_model()
knn_allNoBands <- knn_model(columns = allNoBands_columns,
                            msg = "Hyp Vegetation Indices with CHM and DTM Metrics")
knn_hyp <- knn_model(columns = hyp_columns,
                   msg = "Vegetation Indices")
knn_hypBands <- knn_model(columns = hypBands_columns,
                        msg = "Hyperspectral Bands")
knn_topo <- knn_model(columns = topo_columns,
                    msg = "DTM Metrics")
knn_chm <- knn_model(columns = chm_columns,
                   msg = "CHM Metrics")

#### K-fold cross-validation and Bootstrapping
knn_all.b <- knn_model(test1 = testing1.b, train1 = training1.b,
                     test2 = testing2.b, train2 = training2.b,
                     test3 = testing3.b, train3 = training3.b)
knn_allNoBands.b <- knn_model(test1 = testing1.b, train1 = training1.b,
                              test2 = testing2.b, train2 = training2.b,
                              test3 = testing3.b, train3 = training3.b,
                              columns = allNoBands_columns,
                              msg = "Hyp Vegetation Indices with CHM and DTM Metrics")
knn_hyp.b <- knn_model(columns = hyp_columns,
                     msg = "Vegetation Indices",
                     test1 = testing1.b, train1 = training1.b,
                     test2 = testing2.b, train2 = training2.b,
                     test3 = testing3.b, train3 = training3.b)
knn_hypBands.b <- knn_model(columns = hypBands_columns,
                          msg = "Hyperspectral Bands",
                          test1 = testing1.b, train1 = training1.b,
                          test2 = testing2.b, train2 = training2.b,
                          test3 = testing3.b, train3 = training3.b)
knn_topo.b <- knn_model(columns = topo_columns,
                      msg = "DTM Metrics",
                      test1 = testing1.b, train1 = training1.b,
                      test2 = testing2.b, train2 = training2.b,
                      test3 = testing3.b, train3 = training3.b)
knn_chm.b <- knn_model(columns = chm_columns,
                     msg = "CHM Metrics",
                     test1 = testing1.b, train1 = training1.b,
                     test2 = testing2.b, train2 = training2.b,
                     test3 = testing3.b, train3 = training3.b)


###########################################
#### Multinomial Logistic Regression
##########################################
multi_model <- function(pred  = predictors, resp = response,
                        test1 = testing1, train1 = training1,
                        test2 = testing2, train2 = training2,
                        test3 = testing3, train3 = training3,
                        columns = allNoBands_columns,
                        msg = "Vegetation Indices with CHM and DTM Metrics") {
  
  #### Create Random Forest Models ####
  all.dat <- cbind(resp, pred[columns])
  multi1 <- multinom(as.factor(resp) ~ ., data = all.dat[train1,])
  multi2 <- multinom(as.factor(resp) ~ ., data = all.dat[train2,])
  multi3 <- multinom(as.factor(resp) ~ ., data = all.dat[train3,])
  
  #### Predict  over rows dataset ####
  predicted1 <- predict(object = multi1, pred[test1, columns])
  predicted2 <- predict(object = multi2, pred[test2, columns])
  predicted3 <- predict(object = multi3, pred[test3, columns])
  
  #### CM ####
  print("#######################################################")
  print(paste0(msg, " Model Confusion Matrix"))
  print("#######################################################")
  all_response <- as.factor(c(resp[test1],
                              resp[test2], 
                              resp[test3]))
  all_predicted <- as.factor(c(predicted1, predicted2, predicted3))
  levels(all_predicted) <- levels(all_response)
  
  cm <- confusionMatrix(all_predicted, all_response)
  
  print(cm)
  
  return(list(multi1, multi2, multi3, cm))
}

#### Only k-fold cross-validation
## This won't work because it's too many predictors:
## multi_all <- multi_model(columns = all_columns)
multi_allNoBands <- multi_model(columns = allNoBands_columns)
multi_hyp <- multi_model(columns = hyp_columns,
                     msg = "Vegetation Indices")
multi_hypBands <- multi_model(columns = hypBands_columns,
                          msg = "Hyperspectral Bands")
multi_topo <- multi_model(columns = topo_columns,
                      msg = "DTM Metrics")
multi_chm <- multi_model(columns = chm_columns,
                     msg = "CHM Metrics")

#### K-fold cross-validation and Bootstrapping
multi_allNoBands.b <- multi_model(test1 = testing1.b, train1 = training1.b,
                       test2 = testing2.b, train2 = training2.b,
                       test3 = testing3.b, train3 = training3.b)
multi_hyp.b <- multi_model(columns = hyp_columns,
                       msg = "Vegetation Indices",
                       test1 = testing1.b, train1 = training1.b,
                       test2 = testing2.b, train2 = training2.b,
                       test3 = testing3.b, train3 = training3.b)
multi_hypBands.b <- multi_model(columns = hypBands_columns,
                            msg = "Hyperspectral Bands",
                            test1 = testing1.b, train1 = training1.b,
                            test2 = testing2.b, train2 = training2.b,
                            test3 = testing3.b, train3 = training3.b)
multi_topo.b <- multi_model(columns = topo_columns,
                        msg = "DTM Metrics",
                        test1 = testing1.b, train1 = training1.b,
                        test2 = testing2.b, train2 = training2.b,
                        test3 = testing3.b, train3 = training3.b)
multi_chm.b <- multi_model(columns = chm_columns,
                       msg = "CHM Metrics",
                       test1 = testing1.b, train1 = training1.b,
                       test2 = testing2.b, train2 = training2.b,
                       test3 = testing3.b, train3 = training3.b)

###########################################
#### Make Summary Tables
##########################################
#### Random Forest ####
rm("rf_obs"); rm("rf_tab"); rm("rf_obs.b")
rf_obs <- ls()[grep(pattern = "rf", x = ls())]
rf_obs[rf_obs == "rf_model"] <- NA
rf_obs <- rf_obs[!is.na(rf_obs)]

rf_obs.b <- rf_obs[grep(pattern = ".b", x = rf_obs)]
rf_obs <-  rf_obs[-grep(pattern = ".b", x = rf_obs)]

rf_tab <- data.frame("model" = rf_obs, "model.b" = rf_obs.b, 
                     "accuracy" = NA, "kappa" = NA,  
                     "bootstrap_accuracy" = NA, "bootstrap_kappa" = NA)

for(i in 1:length(rf_obs)) {
  rf_tab[i, 3] <- get(rf_obs[i])[[4]]$overall["Accuracy"]
  rf_tab[i, 4] <- get(rf_obs[i])[[4]]$overall["Kappa"]
  
  rf_tab[i, 5] <- get(rf_obs.b[i])[[4]]$overall["Accuracy"]
  rf_tab[i, 6] <- get(rf_obs.b[i])[[4]]$overall["Kappa"]
}

rf_tab

#### SVM ####
rm("svm_obs"); rm("svm_tab"); rm("svm_obs.b"); rm("svm_tab")
svm_obs <- ls()[grep(pattern = "svm", x = ls())]
svm_obs[svm_obs == "svm_model"] <- NA
svm_obs <- svm_obs[!is.na(svm_obs)]

svm_obs.b <- svm_obs[grep(pattern = ".b", x = svm_obs)]
svm_obs <-  svm_obs[-grep(pattern = ".b", x = svm_obs)]

svm_tab <- data.frame("model" = svm_obs, "model.b" = svm_obs.b, 
                     "untuned_accuracy" = NA, "untuned_kappa" = NA,  
                     "untuned_bootstrap_accuracy" = NA, "untuned_bootstrap_kappa" = NA, 
                     "tuned_accuracy" = NA, "tuned_kappa" = NA,  
                     "tuned_bootstrap_accuracy" = NA, "tuned_bootstrap_kappa" = NA)

for(i in 1:length(svm_obs)) {
  svm_tab[i, 3] <- get(svm_obs[i])[[4]]$overall["Accuracy"]
  svm_tab[i, 4] <- get(svm_obs[i])[[4]]$overall["Kappa"]
  
  svm_tab[i, 5] <- get(svm_obs.b[i])[[4]]$overall["Accuracy"]
  svm_tab[i, 6] <- get(svm_obs.b[i])[[4]]$overall["Kappa"]
  
  svm_tab[i, 7] <- get(svm_obs[i])[[8]]$overall["Accuracy"]
  svm_tab[i, 8] <- get(svm_obs[i])[[8]]$overall["Kappa"]
  
  svm_tab[i, 9] <- get(svm_obs.b[i])[[8]]$overall["Accuracy"]
  svm_tab[i, 10] <- get(svm_obs.b[i])[[8]]$overall["Kappa"]
}

svm_tab

#### KNN ####
rm("knn_obs"); rm("knn_tab"); rm("knn_obs.b")
knn_obs <- ls()[grep(pattern = "knn", x = ls())]
knn_obs[knn_obs == "knn_model"] <- NA
knn_obs <- knn_obs[!is.na(knn_obs)]

knn_obs.b <- knn_obs[grep(pattern = ".b", x = knn_obs)]
knn_obs <-  knn_obs[-grep(pattern = ".b", x = knn_obs)]

knn_tab <- data.frame("model" = knn_obs, "model.b" = knn_obs.b, 
                     "accuracy" = NA, "kappa" = NA,  
                     "bootstrap_accuracy" = NA, "bootstrap_kappa" = NA)

for(i in 1:length(knn_obs)) {
  knn_tab[i, 3] <- get(knn_obs[i])[[4]]$overall["Accuracy"]
  knn_tab[i, 4] <- get(knn_obs[i])[[4]]$overall["Kappa"]
  
  knn_tab[i, 5] <- get(knn_obs.b[i])[[4]]$overall["Accuracy"]
  knn_tab[i, 6] <- get(knn_obs.b[i])[[4]]$overall["Kappa"]
}

knn_tab

#### MLC ####
rm("mlc_obs"); rm("mlc_tab"); rm("mlc_obs.b")
mlc_obs <- ls()[grep(pattern = "mlc", x = ls())]
mlc_obs[mlc_obs == "mlc_model"] <- NA
mlc_obs <- mlc_obs[!is.na(mlc_obs)]

mlc_obs.b <- mlc_obs[grep(pattern = ".b", x = mlc_obs)]
mlc_obs <-  mlc_obs[-grep(pattern = ".b", x = mlc_obs)]

mlc_tab <- data.frame("model" = mlc_obs, "model.b" = mlc_obs.b, 
                     "accuracy" = NA, "kappa" = NA,  
                     "bootstrap_accuracy" = NA, "bootstrap_kappa" = NA)

for(i in 1:length(mlc_obs)) {
  mlc_tab[i, 3] <- get(mlc_obs[i])[[4]]$overall["Accuracy"]
  mlc_tab[i, 4] <- get(mlc_obs[i])[[4]]$overall["Kappa"]
  
  mlc_tab[i, 5] <- get(mlc_obs.b[i])[[4]]$overall["Accuracy"]
  mlc_tab[i, 6] <- get(mlc_obs.b[i])[[4]]$overall["Kappa"]
}

mlc_tab

#### Multi ####
rm("multi_obs"); rm("multi_tab"); rm("multi_obs.b")
multi_obs <- ls()[grep(pattern = "multi", x = ls())]
multi_obs[multi_obs == "multi_model"] <- NA
multi_obs <- multi_obs[!is.na(multi_obs)]

multi_obs.b <- multi_obs[grep(pattern = ".b", x = multi_obs)]
multi_obs <-  multi_obs[-grep(pattern = ".b", x = multi_obs)]

multi_tab <- data.frame("model" = multi_obs, "model.b" = multi_obs.b, 
                     "accuracy" = NA, "kappa" = NA,  
                     "bootstrap_accuracy" = NA, "bootstrap_kappa" = NA)

for(i in 1:length(multi_obs)) {
  multi_tab[i, 3] <- get(multi_obs[i])[[4]]$overall["Accuracy"]
  multi_tab[i, 4] <- get(multi_obs[i])[[4]]$overall["Kappa"]
  
  multi_tab[i, 5] <- get(multi_obs.b[i])[[4]]$overall["Accuracy"]
  multi_tab[i, 6] <- get(multi_obs.b[i])[[4]]$overall["Kappa"]
}

multi_tab


all_tab <- rbind(rf_tab, knn_tab, mlc_tab, multi_tab)
write.csv(all_tab, "Model_accuracy_tabs/all_tab_v4.12.18.csv")
write.csv(svm_tab, "Model_accuracy_tabs/svm_tab_v4.12.18.csv")



save.image(file = "R Files/Final_Models.RData")
