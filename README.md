# Overview
This is a variety of code that I wrote for my MS Thesis work using hyperspectral and LiDAR data to predict Forest Type in the Tanana Valley of Alaska. I also wrote a series of scripts for using this data to predict forest type over GLiHT Swaths and compare the values to that of NLCD, LandFire, and Photo Interpreted stand maps in the region. 

## Funding
This work was funded by: 
- [University of Washington School of Environmental and Forest Sciences](http://www.cfr.washington.edu/)
- [Precision Forestry Cooperative (PFC)](https://sites.uw.edu/uwpfc/)
- [USDA Forest Service](https://www.fs.fed.us/)
- [NASA Arctic Boreal Vunerability Experiment](https://above.nasa.gov/)

## Contact Caileigh Shoot
- A current version of my resume and CV can be found at: http://sites.uw.edu/shootc
- All other questions can be answered by emailing me at: shootc@uw.edu 
- Feel free to connect with me on linkedIn: https://www.linkedin.com/in/caileighshoot

## Classifying FIA Forest Type from a Fusion of Hyperspectral and LiDAR Data
### Abstract
In this study, we develop a methodology for classifying FIA defined forest type across the Tanana Inventory Unit (TIU) using a fusion of hyperspectral and LiDAR data. The hyperspectral and LiDAR data used in this study were collected as part of the 2014 acquisition with the NASA Goddard's LiDAR, Hyperspectral & Thermal Imager (G-LiHT). In order to determine the best classification method, we tested 5 classification algorithms: Naive Bayes Classifier, K-Nearest Neighbor, Multinomial Logistic Regression, Support Vector Machine, and Random Forests.

Each model was trained and validated using the forest type corresponding to each FIA subplot, alongside raw hyperspectral data (114 spectral bands in total), hyperspectral vegetation indices, and selected LiDAR-derived canopy height and topography metrics. Six different combinations of this input data were tested to determine the most accurate classification algorithm and model inputs. A 3-fold cross validation was performed in order to ensure that all data was included in both training and validation, but never within the same model. Of the five models and six model input combinations tested, we found Random Forest with hyperspectral vegetation indices as well as topography and canopy height metrics as model inputs had the highest accuracy at 77.53% overall. With the completion of this work, we hope to use this “best” model to classify forest types across the Tanana Inventory Unit in central inland Alaska where there is G-LiHT coverage.

### Data 
NASA Goddard's LiDAR, Hyperspectral & Thermal Imager (G-LiHT) data can be downloaded at: https://gliht.gsfc.nasa.gov/

### Code
If you wish to repeat this work, this is the order with which the code is run: 
1. Convert CHM to ASCII files, then .dtm files, then run the gridsurfacestats function in FUSION using: *convert_CHM_to_ASCII_and_Create_Batch_Files_pub.R*
2. Make stacks of hyperspectral, DTM, and CHM metrics and export them as .Rdata files using: *make_GLiHT_indices_stacks_pub.R* and *process_GLiHT_pub.R*
3. Extract the data at each circular plot (USDA Forest Service Forest Inventory and Analysis, or FIA, plot in this case) using: *extract_plot_data.R*
4. Make models to assess which algorithm and data input works best given your data using: *create_and_test_models_hyperspec_and_LiDAR_pub.R* and *setup_predictor_and_response_hyperspec_and_LiDAR_pub.R*
5. When you decide which moedls work best, train your models using the entire dataset and save them all to one .Rdata file.
6. Then, make predictions over your 13m data using: *make_predictions_pub.R*

## Detecting Aspen Leaf Miner and Evaluating Its Impacts in Interior Alaska

