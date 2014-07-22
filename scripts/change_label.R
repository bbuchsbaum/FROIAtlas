#! /usr/bin/env Rscript


if (!suppressMessages(require("FROIAtlas"))) {
  library(devtools)
  install_github("FROIAtlas", user="bbuchsbaum")
}

trap <- capture.output( suppressMessages( library("FROIAtlas")))
#suppressMessages(library("FROIAtlas"))

args <- commandArgs(trailingOnly=TRUE)
old <- args[1]
new <- args[2]

cat("changing: ", old, "\n")
cat("to:", new, "\n\n")

conn <- connect_atlas_db("~/Dropbox/fROI Atlas/FROIAtlas.sqlite")
changeLabel(conn, old, new)
dbDisconnect(conn)