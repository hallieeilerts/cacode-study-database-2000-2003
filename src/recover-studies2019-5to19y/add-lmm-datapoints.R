################################################################################
#' @description Add LMM data points. These were in the 2019 model input but not the study database.
#' Add idvars that are defined in process-new-studies/combine-inputs-set-idvars and update-old-studies/set-idvars
#' @return Age-specific study database with all covariates and id variable that matches model input. Includes LMM data points.
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
#' Inputs
source("./src/set-inputs.R")
## Old age-specific study database
studydb <- read.csv(paste0("./gen/recover-studies2019-5to19y/temp/studydb-modinput-match_", ageSexSuffix, ".csv", sep = ""))
## Old model inputs
if(ageSexSuffix == "05to09y"){load("./data/model-inputs-old/20201217-Data5to9-VAMCM009-Test3.RData")}
if(ageSexSuffix == "10to14y"){load("./data/model-inputs-old/20201222-Data10to14-VAMCM009-Test8j.RData")}
if(ageSexSuffix == "15to19yF"){load("./data/model-inputs-old/20210207-Data15to19Fem-VAMCM009-Test9.RData")}
if(ageSexSuffix == "15to19yM"){load("./data/model-inputs-old/20210212-Data15to19Men-VAMCM009-Test9e.RData")}
################################################################################

# Find LMM data points that are in model input but not study database
v_lmm <- studies$id[!(studies$id %in% studydb$id)]
df_lmm <- subset(studies, id %in% v_lmm)

if(ageSexSuffix %in% c("05to09y")){
  df_lmm$sex <- substr(sexLabels[1],1,1)
  df_lmm$age_lb <- 5
  df_lmm$age_ub <- 9
}
if(ageSexSuffix %in% c("10to14y")){
  df_lmm$sex <- substr(sexLabels[1],1,1)
  df_lmm$age_lb <- 10
  df_lmm$age_ub <- 14
}
if(ageSexSuffix %in% c("15to19yF")){
  df_lmm$sex <- substr(sexLabels[2],1,1)
  df_lmm$age_lb <- 15
  df_lmm$age_ub <- 19
}
if(ageSexSuffix %in% c("15to19yM")){
  df_lmm$sex <- substr(sexLabels[3],1,1)
  df_lmm$age_lb <- 15
  df_lmm$age_ub <- 19
}

# Add variables to match with study database
df_lmm$iso3 <- df_lmm$reterm
df_lmm$year <- as.numeric(substr(df_lmm$id, 5, 8))
df_lmm$Refid <- "LMM"
df_lmm$sid <- paste0("VR", 1:nrow(df_lmm))

# Add location_fac value which is equivalent to "nationwide"
# To be used in creation of informative id in /update-covar-for-old-studies/set-idvars
df_lmm$location_fac <- 1

# Delete all covariate columns. All will be replaced.
df_lmm <- df_lmm[,c("sid","id","Refid","reterm","iso3","year","location_fac","totdeaths", "sex", "age_lb", "age_ub")]

# Add to study database
studydb$sid <- as.character(studydb$sid)
dat <- dplyr::bind_rows(studydb, df_lmm)

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste0("./gen/recover-studies2019-5to19y/output/Studies2019_", ageSexSuffix, ".csv", sep = ""), row.names = FALSE)
