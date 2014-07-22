#! /usr/bin/env Rscript

library(neuroim)

options(warn=-1)
args <- commandArgs(trailingOnly=TRUE)

outname <- args[[1]]

vols <- lapply(args[2:length(args)], loadVolume)

dfout <- as.data.frame(do.call(rbind, lapply(vols, function(x) {
  i <- which.max(x)
  vox <- as.integer(indexToCoord(x,i))
  
})))


names(dfout) <- c("X", "Y", "Z")
dfout$image <- args[2:length(args)]
write.table(dfout, outname, row.names=FALSE, quote=FALSE)
