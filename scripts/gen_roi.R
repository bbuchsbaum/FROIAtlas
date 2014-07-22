#! /usr/bin/env Rscript

library(cluster)
library(neuroim)


options(warn=-1)
args <- commandArgs(trailingOnly=TRUE)
roiname <- args[1]
hemi <- args[2]

if (length(args) == 3) {
  method <- args[3]
} else {
  method = "boot"
}

if (!suppressMessages(require("FROIAtlas"))) {
  library(devtools)
  install_github("FROIAtlas", user="bbuchsbaum")
}

trap <- capture.output( suppressMessages( library("FROIAtlas")))
#suppressMessages(library("FROIAtlas"))

datestr <- format(Sys.time(), "%d_%m_%y")

conn <- connect_atlas_db("~/Dropbox/fROI Atlas/FROIAtlas.sqlite")
foci <- as.matrix(get_roi_foci(conn, roiname, hemi)[,2:4])

if (nrow(foci) == 0) {
  cat("no coordinates found in database for ROI: ", roiname, "and hemi: ", hemi, "\n")
  stop()
}

template <- readRDS(system.file("data/MNI_SPACE.RDS", package="FROIAtlas"))

outs <- check_outliers(conn, roiname, hemi)
keep <- outs$prob < .99
foci <- foci[keep,]

if (method == "boot") { 
  bootvol <- FROIAtlas:::boot_foci(foci, N=50, centroidWeighted=FALSE)
  outname <- paste0(roiname, "_", method, "_", hemi, "_", datestr, ".nii")
  writeVolume(bootvol, outname)
} else if (method == "points") { 
  blurvol <- blur_foci(as.matrix(foci), template, kerndim=c(8,8,8), sd=3)
  outname <- paste0(roiname, "_", method, "_", hemi, "_", datestr, ".nii")
  writeVolume(blurvol, outname)
} else if (method == "density") {
  out <- coord_density(as.matrix(foci), template)
  outname <- paste0(roiname, "_", method, "_", hemi, "_", datestr, ".nii")
  writeVolume(out, outname)
} else {
  stop(paste("unrecognized method: ", method, "legal values are: boot, points, density"))
}
  
