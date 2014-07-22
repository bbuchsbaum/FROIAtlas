#Fit gaussians to froi data and save means and vars of gaussians (uses mclust).
#Creates 4 lists: uniqFroi, compNums, means, covars with aligned indices representing
#different frois.

library(mclust)


#######
## first update neuroim package. 
## to do so, run te following commands. You only need to do this onc, or whenver you want to update neuroim.

# install devtools poackage. This is needed to install a package from github.
install.packages("devtools")
library(devtools)

## install development version stored on github.
install_github("neuroim", "bbuchsbaum")
######


library(neuroim)

#compFroiTable = read.table("froi_atlas.txt")
## BB: header=TRUE option reads in column headers. Without this the first row will be the column header.
compFroiTable = read.table("froi_atlas.txt", header=TRUE)

#remove outliers
#noOutlierRows = which(compFroiTable[,10] == FALSE)
noOutlierRows = which(compFroiTable$out95 == FALSE)
compFroiTable = compFroiTable[noOutlierRows,]

locFroiTable = compFroiTable[,c("X","Y","Z","Hemisphere", "FROI")]

# convert relevant info to non-factors
## BB: these were read as factors because the first row was a character vector. 
## BB: With the header=TRUE argument the coordinate columns are read in as plain numeric vectors, so no need to convert from factors.
x = locFroiTable$X
y = locFroiTable$Y
z = locFroiTable$Z
xyz = cbind(x, y, z) 
froi = locFroiTable$FROI

uniqFroi = unique(froi)

#initialize data structures
compNums = list() #number of components (gaussians)
means = list()
covars = list()

## load MNI_SPACE_1MM object from neuroim. This object is part of the neuroim package and can be loaded with the 'data' command.
data("MNI_SPACE_1MM")

## rename to something shorter
MNI <- MNI_SPACE_1MM

## MNI is a "BrainSpace" object. This is just a represention of the geometry of an MNI image with 1mm gird spacing. 
## It has no associated image data.
## help(BrainSpace) for more information



localCoordinates <- function(coords) {
  res <- lapply(1:nrow(coords), function(i) {
    ## convert coordinate from MNI space to voxel space
    grid.loc <- coordToGrid(MNI, as.matrix(coords[i,,drop=FALSE]))   
    
    ## construct a region of interest around the coordinate
    ## this region has 5 voxels (surround=5) on either side of the central coordinate
    cube <- RegionCube(MNI, grid.loc, surround=10)
    
    ## extract the 1-dimensional array of indices representing the grid coordinates in MNI space.
    indices(cube)
  })
  
  ## get only the unique indices
  idx <- sort(unique(unlist(res)))
  list(coords=indexToCoord(MNI, idx), indices=idx)
}


fitMClust <- function(rois, hemi) {
  out <- list()
  for (roi in rois) {
    print(roi)
    coords <- subset(compFroiTable, FROI == roi & Hemisphere == hemi)[, c("X", "Y", "Z")]
    
    ## not all regions have left and right ROIs.
    if (nrow(coords) > 1) {
      
      ## we use densityMclust instead of Mclust as this allows us to predict density values from new observations.
      dens <- densityMclust(coords, 1:5)   
      
      ## note: plot (dens) will show the model fit as a contour map
      
      lcoords <- localCoordinates(coords)
      
      ## the critical step. This will evaluate the fitted mixture model on the coordinates supplied as 'lcoords'.
      ## we could have evaluated every coordinate in MNI space, but that would be very inefficient and would take a long time.
      ## hence the "localCoordinates" function above.
      estvals <- predict(dens, lcoords$coords)
      
      ## normalize so that the region of highest density is 1.
      normvals <- estvals/max(estvals)
      
      ## create a new image volume. We use a "SparseBrainVolume" because most values are 0.
      svol <- SparseBrainVolume(normvals, MNI, indices=lcoords$indices)
      
      ## to write this image volume to disk
      ## writeVolume(svol, "nameofimage.nii") 
      ## important to add ".nii" extension to output file name
      
      ## we store model fit, outvol, hemi in list
      out[[roi]] <- list(mfit=dens, outvol=svol, hemi=hemi, G=dens$G, )
           
    }
  }
  out
}

## process all left hemi ROIs
leftClust <- fitMClust(uniqFroi, "Left")

## process all right hemi ROIs
rightClust <- fitMClust(uniqFroi, "Right")

## remaining work:
## save the output images to disk with approprate names, e.g. "mclust_left_STS_PolySensory.nii", "mclust_right_STS_PolySensory.nii"
## save cluster parameters as tables (these are in mfit$parameters)





