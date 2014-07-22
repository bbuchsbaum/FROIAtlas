#! /usr/bin/env Rscript
source("~/rsource/drive_suma.R")

library(optparse)
library(RColorBrewer)
library(colorspace)

suppressPackageStartupMessages(library("optparse"))

option_list <- list(make_option(c("-s", "--surface"), type="character", help="the name of the SUMA surface data set"),
                    make_option(c("-b", "--bins"), type="character", help="the name of the file containing color bins -- or a quoted string of cutoff values, e.g. '1,2,3,4'"),
		    make_option(c("-n", "--column"), type="character", help="the column number of the SUMA dataset (between 1 and N)"),
		    make_option(c("-o", "--out"), type="character", help="the name of the output colored surface"),
                    make_option(c("-c", "--colorscale"), default="Spectral",                    
                                type="character", help=paste("the name of the colorbrewer color scale, one of"), paste(row.names(brewer.pal.info), collapse=", ")))

oparser <- OptionParser(usage = "map_colors.R [options]", option_list=option_list, prog="map_colors.R")
opt <- parse_args(oparser, positional_arguments=TRUE)

print(opt)
if (is.null(opt$options$surface) || !file.exists(opt$options$surface)) {
	cat("could not find surface file: ", opt$options$surface, "\n")
	print_help(oparser)
	stop()
}



dframe <- read.table(opt$options$surface)

if (file.exists(opt$options$bins)) {
	bins <- read.table(opt$options$bins, stringsAsFactors=FALSE)
} else if (is.character(opt$options$bins)) {
	bins <- as.numeric(strsplit(opt$options$bins, ",")[[1]])
} else {
	cat("error reading 'bins' argument: ", opt$options$bins, "\n")
	print_help(oparser)
	stop()	
}

as.rgb <- function(x) {
	if (substr(x,1,1) == "#") {
		ret <- hex2RGB(x)@coords * 255
		colnames(ret) <- c("red", "green", "blue")
		ret
	} else {
		ret <- t(col2rgb(x))
		colnames(ret) <- c("red", "green", "blue")
		ret
	}
}

if (is.data.frame(bins) && ncol(bins) == 2) {
	print(bins)
	## colors are supplied in bins data.frame
	cols <- do.call(rbind, lapply(bins[,2], as.rgb))
	bins <- bins[,1]
} else if (is.data.frame(bins) && ncol(bins) == 1) {
	cols <- t(col2rgb(brewer.pal(1, opt$options$colorscale)[nrow(bins)]))
	bins <- bins[,1]
} 

outname <- opt$options$out

vals <- dframe[, as.integer(opt$options$column)]

print(bins)
print(cols)		
makeColorMap <- function(vals, cmap, bins) {
	cmap <- rbind(c(0,0,0), cmap)
	colmap <- suma_as_rgb(as.numeric(vals), cmap, as.numeric(bins))
	keep.idx <- apply(colmap, 1, function(vals) any(vals != 0))
	list(nodes=keep.idx, cols=colmap[keep.idx,])
}

out <- makeColorMap(vals, cols, bins)
suma_save_colset(dframe[out$nodes,1], out$cols, outname)			
				
                   