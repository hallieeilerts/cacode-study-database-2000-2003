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
## Study data with too small and too large data points excluded
if(ageSexSuffix == "00to28d"){
  studies <- read.csv(paste0("./gen/update-old-studies/output/studies-wide_upd-covar_",ageSexSuffix,".csv"))
  ## Strata_id for neonates with premvslbw binary indicator
  dat <- read.csv(paste0("./gen/update-old-studies/temp/dat_premvslbw_", ageSexSuffix, ".csv", sep = ""))
}
################################################################################

dat <- merge(studies, dat, by = "strata_id", all.x = TRUE)
dat$premvslbw_source <- "Generated from study data"

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/update-old-studies/output/Studies2019-UpdatedCovar_",ageSexSuffix,".csv"), row.names = FALSE)
