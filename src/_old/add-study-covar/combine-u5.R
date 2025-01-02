################################################################################
#' @description Combine neonate and postneonate study databases
#' @return
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/set-inputs.R")
neo <- read.csv(paste("./gen/add-study-covar/output/Studies2023_00to28d.csv", sep = ""))
post <- read.csv(paste("./gen/add-study-covar/output/Studies2023_01to59m.csv", sep = ""))
################################################################################

dat <- rbind(neo, post)
