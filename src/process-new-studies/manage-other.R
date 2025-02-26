################################################################################
#' @description Exclude studies with only "Other" causes reported, redistribute "Other" to other-specific
#' Round deaths to integers and recalculate totdeaths
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Study data with adjusted totals
dat <- read.csv(paste0("./gen/process-new-studies/temp/studies_adj-tot_", ageSexSuffix, ".csv", sep = ""))
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Reclassified CODs for this age group (includes Other and Undetermined)
v_cod_reclass <- unique(subset(key, !is.na(cod_reclass))$cod_reclass)
# Except for "TB" which has been redistributed (only present in 5-9y and 10-14y reclass vector)
v_cod_reclass <- v_cod_reclass[!(v_cod_reclass %in% "TB")]

# Create vector for "Other" causes
v_cod_other1 <- c("Other", "OtherCMPN", "OtherNCD", "OtherInj")

# If only OTHER categories reported, exclude data point
v_nonOther <- v_cod_reclass[!(v_cod_reclass %in% v_cod_other1)]
idExclude <- apply(dat[, paste(v_nonOther)], 1,
                   function(x) {
                     if ( all(is.na(x)) |
                          sum(x, na.rm=T) == 0) {
                       return(1)
                     } else return(0)
                   })
sum(idExclude) # Number of excluded data points

# idExclude will be a vector of 0 or 1's for each study data point
dat_exc1 <- dat[which(idExclude == 1), ]
if(nrow(dat_exc1) > 0 ){
  dat_exc1$exclude_reason <- "Only other dumpster reported"
}
dat <- dat[which(idExclude == 0), ]

# If only < 2 causes reported, exclude data point
# Note 2024-12-20: inclusion criteria remembered by Jamie. Not sure if should apply to 5-19 as well
# Or scrap this and apply 5-19y criteria to under-5.
if(ageSexSuffix %in% c("00to28d", "01to59m")){
  v_nonUndt <- v_cod_reclass[!(v_cod_reclass %in% "Undetermined")]
  idExclude <- apply(dat[, paste(v_nonUndt)], 1,
                     function(x) {
                       if ( sum(!is.na(x)) < 2) {
                         return(1)
                       } else return(0)
                     })
  dat_exc2 <- dat[which(idExclude == 1), ]
  if(nrow(dat_exc2) > 0 ){
    dat_exc2$exclude_reason <- "Less than 2 CODs reported"
  }
  dat <- dat[which(idExclude == 0), ]
}else{
  dat_exc2 <- dat[0,]
}

# 5-19 have multiple "Other" categories
# If Other = 0 and other-specific were reported, assign NA to Other
# If other-specific reported and > 0, re-distribute deaths reported as "Other"
# If Other and other-specific are NA, assign Other a 0
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF","15to19yM")){
  # If Other = 0 and other-specific were reported, assign NA to Other
  idOther <- which(dat$Other == 0 & !is.na(dat$OtherCMPN) & !is.na(dat$OtherNCD) & !is.na(dat$OtherInj))
  length(idOther)
  if (length(idOther) > 0) dat[idOther, "Other"] <- NA
  
  # Re-distribute deaths reported as "Other" when other-specific reported and > 0
  idOther <- which(!is.na(dat$Other) & dat$Other > 0 &
                     dat$OtherCMPN >= 0 & dat$OtherNCD >= 0 & dat$OtherInj >= 0)
  if (length(idOther) > 0) {
    v_cod_other2 <- c("OtherCMPN", "OtherNCD", "OtherInj")
    for (i in idOther) {
      # Proportion of deaths in each "other-specific" category
      propOther <- prop.table(dat[i, v_cod_other2])
      # Re-distribute deaths in "Other" proportionally
      dat[i, v_cod_other2] <- dat[i, v_cod_other2] + propOther * dat[i, "Other"]
      # Assign NA to Other
      dat[i, "Other"] <- NA
    }
  }
  
  # If Other and any other-specific are NA, other should be assigned a 0
  # This is for the reclassification matrix. 
  # Need to have Other equal to 0 to include it as a row in the reclassification matrix.
  # This way any reported cause (column in reclassification matrix) which is not found in the study can be pulled out of Other.
  idOther <- which(is.na(dat$Other) &
                     (is.na(dat$OtherCMPN) | is.na(dat$OtherNCD) | is.na(dat$OtherInj)))
  if(length(idOther) > 0){dat$Other[idOther] <- 0}
}

# For under-5, if Other is NA, assign Other a 0
# This is for the reclassification matrix. 
# Need to have Other equal to 0 to include it as a row in the reclassification matrix.
# This way any reported cause (column in reclassification matrix) which is not found in the study can be pulled out of Other.
if(ageSexSuffix %in% c("00to28d", "01to59m")){
  idOther <- which(is.na(dat$Other))
  if(length(idOther) > 0){dat$Other[idOther] <- 0}
}

# ROUND TO INTEGERS and Re-calculate TOTAL DEATHS
dat[, paste0(v_cod_reclass)] <- round(dat[, paste0(v_cod_reclass)])
dat$totdeaths <- apply(dat[, paste0(v_cod_reclass)], 1, sum, na.rm = T)

# Check there are no negative values
if(length(which(dat[, paste(v_cod_reclass)] < 0)) > 0 ){
  warning("negative values in CODs")
}

# Combine excluded data
dat_exc <- rbind(dat_exc1, dat_exc2)

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/process-new-studies/temp/studies_mng-other_",ageSexSuffix,".csv",sep =""), row.names = FALSE)
write.csv(dat_exc, paste0("./gen/process-new-studies/audit/dat_exc1_",ageSexSuffix,".csv",sep =""), row.names = FALSE)

