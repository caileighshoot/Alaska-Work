

# Alaska_Work
This is a variety of code that I wrote for my MS Thesis work using hyperspectral and LiDAR data to predict Forest Type in the Tanana Valley of Alaska. I also wrote a series of scripts for using this data to predict forest type over GLiHT Swaths and compare the values to that of NLCD, LandFire, and Photo Interpreted stand maps in the region. 

## Funding
This work was funded by: 
[University of Washington School of Environmental and Forest Sciences](http://www.cfr.washington.edu/)
[Precision Forestry Cooperative (PFC)](https://sites.uw.edu/uwpfc/)
[USDA Forest Service](https://www.fs.fed.us/)
[NASA Arctic Boreal Vunerability Experiment](https://above.nasa.gov/)

## Contact Caileigh Shoot
A current version of my resume and CV can be found at: http://sites.uw.edu/shootc

All other questions can be answered by emailing me at: shootc@uw.edu 

Feel free to connect with me on linkedIn: https://www.linkedin.com/in/caileighshoot

## Classifying FIA Forest Type from a Fusion of Hyperspectral and LiDAR Data
If you wish to repeat this work, this is the order with which the code is run: 
1. Convert CHM to ASCII files, then .dtm files, then run the gridsurfacestats function in FUSION using: *convert_CHM_to_ASCII_and_Create_Batch_Files_pub.R*
2. Make stacks of hyperspectral, DTM, and CHM metrics and export them as .Rdata files using: *make_GLiHT_indices_stacks_pub.R* and *process_GLiHT_pub.R*
3. Extract the data at each circular plot (USDA Forest Service Forest Inventory and Analysis, or FIA, plot in this case) using: *extract_plot_data.R*
4. Make models to assess which algorithm and data input works best given your data using: *create_and_test_models_hyperspec_and_LiDAR_pub.R* and *setup_predictor_and_response_hyperspec_and_LiDAR_pub.R*
5. When you decide which moedls work best, train your models using the entire dataset and save them all to one .Rdata file.
6. Then, make predictions over your 13m data using: *make_predictions_pub.R*


## Detecting Aspen Leaf Miner and Evaluating Its Impacts in Interior Alaska

