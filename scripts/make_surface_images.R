#! /usr/bin/env Rscript

args <- commandArgs(trailingOnly=TRUE)

surf <- args[[1]]
hemi <- args[[2]]

source("~/.voltosurf")

for (view in c("lateral", "medial", "ventral", "posterior")) {
	oname <- gsub(".dset", paste0("_", view, "_", hemi, ".jpg"), surf)
	print(oname)
	
	cmd1 <- paste("~/bin/snap_surface.R -s",
	               surf, "-n 4 -d", hemi, "-t .1 -i \".1,.5\" -v", view, "-o",  oname)
	
	print(cmd1)
	system(cmd1)
}

## clus command
##SurfClust -spec /Users/brad/data1/freesurfer/subjects/fsaverage/SUMA/fsaverage_RD_7_lh_std.spec -surf_A smoothwm -input sm_PPA_density_left_27_05_13_lh.1D.dset 4 -rmm -2 -out_roidset -out_clusterdset -no_cent -thresh_col 4 -thresh .1 -amm2 75
