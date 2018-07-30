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
7. All of these predictions can be mosaiced together in ArcMap using the "Mosaic to New Raster" tool (https://desktop.arcgis.com/en/arcmap/10.3/tools/data-management-toolbox/mosaic-to-new-raster.htm)

## Detecting Aspen Leaf Miner and Evaluating Its Impacts in Interior Alaska
### Abstract
This study uses hyperspectral and LiDAR data to identify and evaluate aspen leaf miner (ALM) damage in interior Alaska. Quaking Aspen (Populus tremuloides) are deciduous trees that are native to Alaska. Since the early 2000’s, an outbreak of aspen leaf miner (ALM) has been occurring throughout the state. The full extent of the impact of the ALM is not well studied with modern remote sensing technology.

This work builds upon a previous study which developed a methodology for classifying USDA Forest Service Forest Inventory and Analysis (FIA) defined forest type across a portion of interior Alaska using a fusion of hyperspectral and LiDAR data in 2014 by NASA Goddard's LiDAR, Hyperspectral & Thermal Imager (G-LiHT). The “best” model from this study was able to predict Aspen presence at 83% overall accuracy. Thus, this model was used in this continuation of that work. The G-LiHT model forest type classification was validated by comparing it with three different well-known vegetation/land cover classification maps. Next, this study investigated the use of Airborne Visible-Infrared Imaging Spectrometer - Next Generation (AVIRIS-NG) to identify ALM infection in validated aspen stands.

The classification of FIA-defined forest type using the G-LiHT forest type model showed that the model is able to differentiate changes in forest type, but may not always be selecting the correct forest type for that area. When the classification values were compared to the three other vegetation/land cover maps, our model had relatively low agreement for predicting aspen presence, ranging between 14 and 23%. This comparison showed that it is extremely challenging to precisely predict vegetation types at a given location. Future work should consider including field identification of pure aspen stands and their ALM infection status for use in model training and validation, and focusing study to a smaller portion of the landscape that is relatively close/similar to field plots. This additional data would allow for higher accuracy identification of aspen stands and ALM infection. Following acquisition of additional data, the next steps are to identify stands infected by ALM and use AVIRIS-NG hyperspectral vegetation indices to quantify the spectral changes that the plant undergoes when infected by ALM. This research will help forest land managers and rural Alaskan communities to better understand and manage the impacts of this invasive pest. 

### Data 
- All 2017 ABoVE Data (including AVIRIS-NG) can be downloaded at: https://above.nasa.gov/airborne_2017.html#data
- The 2011 National Land Cover Database products can be downloaded here: https://www.mrlc.gov/nlcd11_data.php
- The 2014 LandFire Existing Vegetation Type data can be downloaded here: https://www.landfire.gov/version_comparison.php
- The Photo-Interpreted Stand Map for Alaska is not available online to my knowledge. To gain access, please reach out to the Alaska Department of Natural Resources Division of Forestry: http://forestry.alaska.gov/

### Code
Note: Steps 2-4 can be done with open source tools (i.e. python, qgis, R, etc.), but I did not do so in this case. If you do write scripts to do this easily, please let me know! :-)
1. Rasterize the Photo-Interpreted Stand map in ArcMap by the vegetation type column using the Polygon to Raster tool: https://pro.arcgis.com/en/pro-app/tool-reference/conversion/polygon-to-raster.htm
1. Clip each raster dataset (NLCD and LandFire) to your region of interest (this would preferably be a polygon layer), then reproject the layers as needed so that they are all the same projection. This was done in ArcMap, but can be scripted as well. Your region of interest may also be a polygon outline of your mosaiced forest type prediction layers (created in step 7 above). 
3. With your region of interest polygon, create a grid of points with the Create Fishnet (Data Management) tool in ArcMap (https://pro.arcgis.com/en/pro-app/tool-reference/data-management/create-fishnet.htm). I did it along a 13m grid, but this can be changed depending on your goals.
4. Ensure your fishnet of points, mosaiced predictions (from step 7 above), NLCD, photo-interpreted vegetation type raster, and LandFire layers are all in the same projection. If not, reproject as needed so that they all match. 
5. Use the "Extract Multi Values to Points" tool in Arcmap to extract the values for all raster layers (NLCD, LandFire, Prediction Mosaics, and Photo-interpreted layers) at each of your fishnet points: https://desktop.arcgis.com/en/arcmap/10.3/tools/spatial-analyst-toolbox/extract-multi-values-to-points.htm
6. Use the *make_confusion_matrices_pub.R* script to make confusion matrices comparing the values for each reference landcover layer (NLCD, LandFire, and Photo-Interp) to your model's predictions. 
Note 2: This is where my analysis concluded. But had I continued, I would have used the script *AVIRIS_Hyp_indices.R* to make many hyperspectral vegetation indices from the AVIRIS-NG data. 
