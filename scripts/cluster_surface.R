#! /usr/bin/env Rscript

args <- commandArgs(trailingOnly=TRUE)


surf <- args[[1]]
hemi <- args[[2]]
area <- args[[3]]

source("~/.voltosurf")

if (hemi == "lh") {
  cmd <- paste("SurfClust -spec",
               LH_STD_SPEC,
               "-surf_A smoothwm -input", surf,  
               "4 -rmm -2 -no_cent -thresh_col 4 -out_clusterdset -thresh .1 -amm2", area)
  system(cmd)
} else {
  cmd <- paste("SurfClust -spec",
               RH_STD_SPEC,
               "-surf_A smoothwm -input", surf,  
               "4 -rmm -2 -no_cent -thresh_col 4 out_clusterdset -thresh .1 -amm2", area)
  system(cmd)
}