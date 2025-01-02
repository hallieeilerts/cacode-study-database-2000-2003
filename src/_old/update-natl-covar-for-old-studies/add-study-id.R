################################################################################
#' @description Match study covariate names in 2000-2020 model objects to 2023 prediction database
#' @return Key with HMM-StudyDatabase study/covariate rows and whether replacement with prediction database called for
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readstata13)
library(tidyverse)
library(data.table)
#' Inputs
source("./src/update-natl-covar-for-old-studies/set-inputs.R")

# 2020 study database
study <- readstata13::read.dta13("./data/previous-database/20200930-HMM-StudyDatabase.dta")

# Key matching old study and new pred covariate names
key_studyid <- read.csv(paste0("./gen/update-natl-covar-for-old-studies/temp/key_studyid_",ageSexSuffix,".csv", sep = ""))

# Old model inputs
if(ageSexSuffix == "05to09y"){load("./data/model-inputs/20201217-Data5to9-VAMCM009-Test3.RData")}
if(ageSexSuffix == "10to14y"){load("./data/model-inputs/20201222-Data10to14-VAMCM009-Test8j.RData")}
if(ageSexSuffix == "15to19yF"){load("./data/model-inputs/20210207-Data15to19Fem-VAMCM009-Test9.RData")}
if(ageSexSuffix == "15to19yM"){load("./data/model-inputs/20210212-Data15to19Men-VAMCM009-Test9e.RData")}

################################################################################

# Match on id variable that Pancho makes in data cleaning file
# This is essential to matching to study objects in model input
dat <- merge(study, key_studyid[,c("id","Refid","study_id","study_location","iso3","year","age_lb","strata_gender","study_location","strata_ur")], 
             by = c("Refid","study_id","study_location","iso3","year","age_lb","strata_gender","study_location","strata_ur"))
nrow(dat) == nrow(key_studyid)
key_studyid$id[!(key_studyid$id %in% dat$id)]
# Used to have age_ub in the merge, but then a handful of studies didn't match
# maybe because age group was collapsed in some way that affected the id
# e.g. "IDN-2001-24135-44-5-19-B" and "PNG-1983-36645-48-5-19-M" for 5-9

# Only keep id and covariates
dat <- dat[,c("id", "iso3","year", names(dat)[!(names(dat) %in% c("id", "iso3","year"))] )]
nrow(dat) == length(unique(dat$id))

# # LOAD MODEL INPUT AND ONLY KEEP STUDIES IN MODEL INPUT!!!!
# nrow(dat)
# dat <- subset(dat, id %in% studies$id)
# nrow(dat$id)
# dat$id[!(dat$id %in% studies$id)]


write.csv(dat, paste0("./gen/update-natl-covar-for-old-studies/temp/studies-with-id_", ageSexSuffix, ".csv",sep=""), row.names = FALSE)


