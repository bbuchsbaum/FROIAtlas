#! /usr/bin/env Rscript

library(cluster)

options(warn=-1)


if (!suppressMessages(require("FROIAtlas"))) {
  library(devtools)
  install_github("FROIAtlas", user="bbuchsbaum")
}

trap <- capture.output( suppressMessages( library("FROIAtlas")))
#suppressMessages(library("FROIAtlas"))

conn <- connect_atlas_db("~/Dropbox/fROI Atlas/FROIAtlas.sqlite")

rois <- sort(roi_names(conn))
out <- paste(rois, collapse="\n")
cat(out, "\n")
dbDisconnect(conn)
