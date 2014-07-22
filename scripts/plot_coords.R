#! /usr/bin/env Rscript

library(cluster)

options(warn=-1)
args <- commandArgs(trailingOnly=TRUE)
roiname <- args[1]
hemi <- args[2]
method <- args[3]

if (!suppressMessages(require("FROIAtlas"))) {
  library(devtools)
  install_github("FROIAtlas", user="bbuchsbaum")
}

trap <- capture.output( suppressMessages( library("FROIAtlas")))
#suppressMessages(library("FROIAtlas"))

conn <- connect_atlas_db("~/Dropbox/fROI Atlas/FROIAtlas.sqlite")
foci <- as.matrix(get_roi_foci(conn, roiname, hemi)[,2:4])

cres <- clusterCoords(foci, method)

outname <- paste0(roiname, "_points.pdf")
pdf(outname)
pairs(foci, col=cres@clusters)
dev.off()
cat("open ", outname, "to view clustering \n")

