################################################################################
#' @description Merge on cod mapping for studies
#' @return Study deaths with mapped causes
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Study data with cleaned columns
dat <- read.csv(paste0("./gen/process-new-studies/temp/study-causes_clean-col_", ageSexSuffix, ".csv", sep = ""))
## Key with cod mapping for studies
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("studycausemapping", dat_filename, ignore.case = TRUE)]
key <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
#' Functions 
################################################################################

# Select appropriate age group for study cod mapping key
if(ageSexSuffix == "00to28d"){
  key <- key[,c("cause_of_death","Neonates")]
}
if(ageSexSuffix == "01to59m"){
  key <- key[,c("cause_of_death","X1.59m")]
}
if(ageSexSuffix %in% c("05to09y","10to14y","15to19yF","15to19yM")){
  key <- key[,c("cause_of_death","X5.19y")]
}
names(key) <- c("cod_study", "cod_mapped")

# Merge on cause mapping
dat <- merge(dat, key, by.x = "cause_of_death", by.y = "cod_study")

# Drop causes that were not mapped to anything
# These should be only for causes that are typos or exclusively for neonates
# Everything else will get reclassified into the appropriate age-specific category
dat <- subset(dat, !is.na(cod_mapped))
dat <- subset(dat, !(cod_mapped == "NA"))

# Tidy
# Move id to the front
dat <- dat[,c("id", names(dat)[!(names(dat) %in% "id")])]
# Delete unnecessary columns
v_delete <- c("cause_of_death")
dat <- dat[!(names(dat) %in% v_delete)]
dat <- dat[order(dat$id),]

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/process-new-studies/temp/study-causes_cod-map_",ageSexSuffix,".csv",sep =""), row.names = FALSE)
