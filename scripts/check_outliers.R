#! /usr/bin/env Rscript

options(warn=-1)
args <- commandArgs(trailingOnly=TRUE)
roiname <- args[1]
hemi <- args[2]

if (!suppressMessages(require("FROIAtlas"))) {
  library(devtools)
  install_github("FROIAtlas", user="bbuchsbaum")
}

trap <- capture.output( suppressMessages( library("FROIAtlas")))
#suppressMessages(library("FROIAtlas"))

conn <- connect_atlas_db("~/Dropbox/fROI Atlas/FROIAtlas.sqlite")
foci <- get_roi_foci(conn, roiname, hemi)
coords <- as.matrix(foci[,2:4])

if (nrow(coords) == 0) {
  stop(paste("could not fid any coordinates for ROI", roiname, " on the ", hemi))
}

cmass <- colMeans(coords)
cmed <- apply(coords,2,median)
cat(nrow(coords), " total coordinates for ROI ", roiname, " found in database \n")
cat("***   Center of MASS: ", cmass, "\n")
cat("***   Median Coordinate: ", cmed, "\n")


outs <- check_outliers(conn, roiname, hemi)
N1 <- sum(outs$prob > .94)
cat("***   ", N1, " outliers found", "\n")
N2 <- sum(outs$prob > .99)
cat("***   ", N2, " severe outliers found", "\n")

idx <- which(outs$prob > .94)
if (length(idx) > 0) {
  cat("***   Outliers    ***** \n\n")
  print(outs[idx,c(1,2,3,4,5,10,11)])
}


