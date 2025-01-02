################################################################################
#' @description Adjust total deaths when don't match sum of CODs
#' @return Study deaths with adjusted total or other category
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Study data with aggregated causes or redistributed tb deaths for younger age groups
# Aggregated causes
if(ageSexSuffix %in% c("00to28d","01to59m","15to19yF", "15to19yM")){
  dat <- read.csv(paste0("./gen/process-new-studies/temp/studies_cod-agg_", ageSexSuffix, ".csv", sep = ""))
}
# Redistributed tb deaths
if(ageSexSuffix %in% c("05to09y", "10to14y")){
  dat <- read.csv(paste0("./gen/process-new-studies/temp/studies_tb-redist_", ageSexSuffix, ".csv", sep = ""))
}
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Reclassified CODs for this age group
# (includes Other and Undetermined)
v_cod_reclass <- unique(subset(key, !is.na(cod_reclass))$cod_reclass)
# Except for "TB" which has been redistributed (only present in 5-9y and 10-14y reclass vector)
v_cod_reclass <- v_cod_reclass[!(v_cod_reclass %in% "TB")]

# Increase total deaths when they are smaller than sum of COD
totSmall <- which(dat$totdeaths - apply(dat[, paste0(v_cod_reclass)], 1, sum, na.rm = T) < -.01)
length(totSmall)
dat$totdeaths[totSmall] <- apply(dat[totSmall, paste0(v_cod_reclass)], 1, sum, na.rm = T)
# Check that length is now zerp
totSmall <- which(dat$totdeaths - apply(dat[, paste0(v_cod_reclass)], 1, sum, na.rm = T) < -.01)
length(totSmall) == 0

# # !!!! I think this step is unnecessary.
# # RECLASSIFY CAUSES OF DEATH
# # Other dumpster
# dat$OtherCMPN[which(dat$OtherCMPN == 0)] <- NA
# dat$OtherNCD[which(dat$OtherNCD == 0)] <- NA
# dat$OtherInj[which(dat$OtherInj == 0)] <- NA
# dat$Other[which(dat$Other == 0)] <- NA

# Adjust OTHER when total deaths are bigger than sum of COD
totLarge <- which(dat$totdeaths - apply(dat[, paste0(v_cod_reclass)], 1, sum, na.rm = T) > .01)
length(totLarge)
v_cod_other <- c("OtherCMPN", "OtherNCD", "OtherInj", "Other")
# Subset to other categories present in data
v_cod_other <- v_cod_other[v_cod_other %in% names(dat)]
# Update OTHER
if (length(totLarge) > 1) {
  for (i in totLarge) {
    # Difference in deaths
    diffDeath <- dat$totdeaths[i] - sum(dat[i, paste0(v_cod_reclass)], na.rm = T)
    # Proportion in "other" categories
    propOther <- dat[i, paste(v_cod_other)] / sum(dat[i, paste(v_cod_other)], na.rm = T)
    
    # If all "other" categories are NA, propOther vector will be all NAs and nothing will be redistributed
    # replace the proportion of non-specific "other" as 1
    # and assign it as 0 if diffDeath needs to be added to it
    if(!any(!is.na(propOther))){
      propOther[length(propOther)] <- 1
      if(diffDeath > 0){
        dat[i, paste(v_cod_other[length(v_cod_other)])] <- 0
      }
    }
    
    # Redistribute excess of deaths among 'other' categories
    dat[i, paste(v_cod_other)] <- dat[i, paste(v_cod_other)] + diffDeath * propOther
    
  }
}
# Check that length is now zero
totLarge <- which(dat$totdeaths - apply(dat[, paste0(v_cod_reclass)], 1, sum, na.rm = T) > .01)
length(totLarge) == 0

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/process-new-studies/temp/studies_adj-tot_",ageSexSuffix,".csv",sep =""), row.names = FALSE)



