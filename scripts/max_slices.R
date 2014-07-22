#! /usr/bin/env Rscript

options(warn=-1)

library(neuroim)

args <- commandArgs(trailingOnly=TRUE)
roiImage <- args[1]
roiName <- args[2]


template <- loadVolume("MNI152_T1_1mm_brain.nii.gz")
roivol <- loadVolume(roiImage)

layer1 <- Layer(template)