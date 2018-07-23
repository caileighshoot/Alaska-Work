# Alaska_Work
This is a variety of code that I wrote for my MS Thesis work using hyperspectral and LiDAR data to predict Forest Type in the Tanana Valley of Alaska. I also wrote a series of scripts for using this data to predict forest type over GLiHT Swaths and compare the values to that of NLCD, LandFire, and Photo Interpreted stand maps in the region. 

If you wish to repeat this work, this is the order with which the code is run: 
1. Convert CHM to ASCII files, then .dtm files, then run the gridsurfacestats function in FUSION using: *convert_CHM_to_ASCII_and_Create_Batch_Files_pub.R*
2. Make stacks of hyperspectral, DTM, and CHM metrics and export them as .Rdata files using: *make_GLiHT_indices_stacks_pub.R* and *process_GLiHT_pub.R*

