################################################################################
#' @description 
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readstata13)
library(tidyverse)
library(data.table)
#' Inputs
source("./src/update-natl-covar-for-old-studies/set-inputs.R")

if(ageSexSuffix == "05to09y"){load("./data/model-inputs/20201217-Data5to9-VAMCM009-Test3.RData")}
if(ageSexSuffix == "10to14y"){load("./data/model-inputs/20201222-Data10to14-VAMCM009-Test8j.RData")}
if(ageSexSuffix == "15to19yF"){load("./data/model-inputs/20210207-Data15to19Fem-VAMCM009-Test9.RData")}
if(ageSexSuffix == "15to19yM"){load("./data/model-inputs/20210212-Data15to19Men-VAMCM009-Test9e.RData")}

# Key matching old study and new pred covariate names
key_covarnames <- read.csv("./gen/update-natl-covar-for-old-studies/temp/key_study-pred-covarnames.csv")

################################################################################

if(ageSexSuffix == "05to09y"){
  names(studies)[which(names(studies) == "dtp3_mf")] <- "vac_dtp3"
  names(studies)[which(names(studies) == "edu_mean")] <- "edu_mean_mf"
  names(studies)[which(names(studies) == "height")] <- "height_mf"
  names(studies)[which(names(studies) == "lowest_wealth")] <- "wealth_lowest"
  names(studies)[which(names(studies) == "q5to19")] <- "mr05to19"
  names(studies)[which(names(studies) == "thinness")] <- "thinness_mf" 
  names(studies)[which(names(studies) == "unemployment_neet")] <- "unemployment_neet_mf"
}
if(ageSexSuffix == "10to14y"){
  names(studies)[which(names(studies) == "alcohol")] <- "alcohol_mf"
  names(studies)[which(names(studies) == "depression")] <- "depression_mf"
  names(studies)[which(names(studies) == "dtp3_mf")] <- "vac_dtp3"
  names(studies)[which(names(studies) == "lowest_wealth")] <- "wealth_lowest"
  names(studies)[which(names(studies) == "ors")] <- "ors_mf"
  names(studies)[which(names(studies) == "q5to19")] <- "mr05to19"
  names(studies)[which(names(studies) == "sex_age_15")] <- "sex_age15_mf"
  names(studies)[which(names(studies) == "thinness")] <- "thinness_mf"
}
if(ageSexSuffix == "15to19yF"){
  names(studies)[which(names(studies) == "alcohol")] <- "alcohol_mf"
  names(studies)[which(names(studies) == "birth_healthfacility_3")] <- "birth_healthfacility3"
  names(studies)[which(names(studies) == "contraception_unmet" )] <- "contraception_met"
  names(studies)[which(names(studies) == "depression")] <- "depression_f"
  names(studies)[which(names(studies) == "literacy")] <- "literacy_f"
  names(studies)[which(names(studies) == "lowest_wealth")] <- "wealth_lowest"
  names(studies)[which(names(studies) == "q5to19")] <- "mr05to19"
  names(studies)[which(names(studies) == "sex_age_15")] <- "sex_age15_f"
  names(studies)[which(names(studies) == "thinness")] <- "thinness_f"
}
if(ageSexSuffix == "15to19yM"){
  names(studies)[which(names(studies) == "alcohol")] <- "alcohol_mf"
  names(studies)[which(names(studies) == "depression")] <- "depression_m"
  names(studies)[which(names(studies) == "labor_participation")] <- "labor_participation_m"
  names(studies)[which(names(studies) == "literacy")] <- "literacy_m"
  names(studies)[which(names(studies) == "lowest_wealth")] <- "wealth_lowest"
  names(studies)[which(names(studies) == "q5to19")] <- "mr05to19"
  names(studies)[which(names(studies) == "unemployment_neet")] <- "unemployment_neet_m"
}

# Harmonize scales
# Old study database uses zero_hundred whereas new prediction database uses zero_one
# Subset variables which are zero_one in new prediction database
v_zeroone <- unique(subset(key_covarnames, pred_scale == "zero_one"))$pred
names(studies)[names(studies) %in% v_zeroone]
# After checking scales plot. Make some manual adjustments.
# Do not divide by 100 for study covar that are already zero_one.
v_zeroone <- v_zeroone[!(v_zeroone %in% c("pop_male_15_29", "sab"))]
studies[,names(studies) %in% v_zeroone] <- studies[,names(studies) %in% v_zeroone]/100

# Harmonize values
if("contraception_met" %in% names(studies)){
  studies$contraception_met <- 1 - studies$contraception_met
}
 
# Save output -------------------------------------------------------------

# Study database that now has same covariate names and scales as pred database
# Some covariates removed if they weren't in pred database
write.csv(studies, paste0("./gen/update-natl-covar-for-old-studies/output/ModInput2020-UpdatedNamesScales_", ageSexSuffix, ".csv"), row.names = FALSE)



