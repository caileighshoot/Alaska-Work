###############
## libraries ##
library(sp)
library(spBayes)
library(raster)

## Set options
memory.limit(8e+6)

# Load in Field Data and define inputs into function
#####################
## load field data ##
f.dat = shapefile("C:/Data/Plots.shp")

## Define inputs
rdata_foldername = "C:/Data_Products/Rdata"
subp.radius = 7.3
output_csv_filename = "plot_aggregated_signal.csv"

#####################
## grab file names ##
# Get the file names for the RData files we will be using. 
# These are the files containing the hyperspectral data.
# **This is a possible spot where things could go wrong**
file.list <- list.files(rdata_foldername, pattern = ".RData", full.names = TRUE)

# Create a matrix where we can store the aggregated signatures for each plot
######################################################
## make container for storing aggregated signatures ##
subp.hp.dat <- matrix(nrow = length(f.dat), ncol = 9)

# Start a progress bar (named pb) so that we can track how far into our for loop we are
pb <- winProgressBar(title = "Progress Bar - Overall Percent Completed", min = 0,
                     max = length(file.list), width = 300)


# Using a for loop, go through each file within our previously created file list
for(f in 1:length(file.list)){
  ## ###############
  ## read in data ##
  print(paste0("loading field data ", f))
  load(file.list[f])
  
  ## #################################
  ## change name of object to 'dat' ##
  Name <- ls()[grep("dat.l",ls())]
  dat <- get(Name)
  eval(parse(text=paste('rm(', Name, ')')))
  
  ## ###############################
  ## reproject hyperspectral data ##
  
  suppressWarnings(f.dat <- spTransform(f.dat, dat@proj4string))
  ## all hp files get coerced to points.
  ## 'suppressWarnings' stops this
  ## message from being printed to the console.
  
  ## ###########################
  ## Get the coordinates for the plot locations that are
  ## within the bounding box of our hyperspectral data.
  ## This ensures that we are not looping over thousands of plots, getting
  ## distance information for plots that are outside our current scope of interest.
  
  # Here we are grabbing the x and y coordinates, as well as the column index so that we can put everything
  # back into our distance matrix in order later
  f.coords = as.matrix(cbind(1:nrow(coordinates(f.dat)), coordinates(f.dat)))
  
  # Get the coordinates that are within the bounding box of the hyperspectral data
  f.dat.subcoords = cbind(f.coords[,1][f.coords[,2] <= bbox(dat)[1,2] &
                                         f.coords[,2] >= bbox(dat)[1,1] &
                                         f.coords[,3] >= bbox(dat)[2,1] &
                                         f.coords[,3] <= bbox(dat)[2,2]],
                          f.coords[,2][f.coords[,2] <= bbox(dat)[1,2] &
                                         f.coords[,2] >= bbox(dat)[1,1] &
                                         f.coords[,3] >= bbox(dat)[2,1] &
                                         f.coords[,3] <= bbox(dat)[2,2]],
                          f.coords[,3][f.coords[,2] <= bbox(dat)[1,2] &
                                         f.coords[,2] >= bbox(dat)[1,1] &
                                         f.coords[,3] >= bbox(dat)[2,1] &
                                         f.coords[,3] <= bbox(dat)[2,2]])
  ## ###########################
  ## Generate distance matrix for plots within our hyperspectral data bounding box
  if(length(f.dat.subcoords) > 0){
    print(paste0("iDist-int out plots ", f))
    dists <- iDist(f.dat.subcoords[,2:3],
                   coordinates(dat))
    
    dists.idx <- which(apply(dists, 1, min) <= subp.radius)  #This is the index of the "in" plots 1:numplots
    
    # This is the index of the "in" plots relative to all the plots in our list
    df.idx <- f.dat.subcoords[dists.idx,1]
    
    if(length(dists.idx) != 0){
      ## This is our counter, making sure we grab the correct index from df.idx
      n = 1
      for(i in dists.idx){
        ## This figures out what cells of the hyperspectral data are within the
        ## 7.3 meter radius around the plot center and then grabs the hyperspectral
        ## data values (dat) for those cells.
        sub.dat <- dat[dists[i,] <= subp.radius,,drop = T]
        
        if(!is.na(colMeans(sub.dat@data)[1])){
          # This grabs the index of the plot within the larger all-plot dataset
          idx = df.idx[n]
          
          ### Error check to see if multiple files are  pulling the same plot
          if(idx == 3360){
            print(Name)
          }
          ## Assign the mean to the appropriate columb within the hyperspectral
          ## data dataframe we created earlier (subp.hp.dat)
          subp.hp.dat[idx,] <- as.vector(colMeans(sub.dat@data))
        }
        n = n + 1
      }
    }else{
    }
  }
  
  gc()
  rm(dat)
  print(paste0("Done with ", f, "!"))
  
  Sys.sleep(0.1)
  setWinProgressBar(pb, f, title=paste( round(f/length(file.list)*100, 0), "% done"))
}

close(pb)

print("Writing CSV file...")
write.csv(subp.hp.dat, output_csv_filename, row.names = FALSE)

print("ALL DONE! :-)")

