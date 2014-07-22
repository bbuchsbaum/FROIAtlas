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
foci <- get_roi_foci(conn, roiname, hemi)[,c(1,2,3,4,5)]
print(foci)

dbDisconnect(conn)
