################################################################################
#' @description Merge on premvslbw covariate
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
if(ageSexSuffix == "00to28d"){
  ## Study data with too small and too large data points excluded
  studies <- read.csv(paste0("./gen/process-new-studies/temp/studies_exc_", ageSexSuffix, ".csv", sep = ""))
  ## Strata_id for neonates with premvslbw binary indicator
  dat <- read.csv(paste0("./gen/process-new-studies/temp/dat_premvslbw_", ageSexSuffix, ".csv", sep = ""))
}
################################################################################

dat <- merge(studies, dat, by = "strata_id", all.x = TRUE)
dat$premvslbw_source <- "Generated from study data"

# Save output -------------------------------------------------------------

write.csv(dat, paste("./gen/process-new-studies/temp/studies_premvslbw_", ageSexSuffix,".csv", sep = ""), row.names = FALSE)
