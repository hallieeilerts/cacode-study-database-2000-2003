################################################################################
#' @description Check that recovered study IDs match IDs in model inputs
#' Make sure that study database has same number of data points for model inputs (except for LMM data points that are added later)
#' @return Age-specific study database with all covariates and id variable that matches model input. No LMM data points.
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
library(readstata13)
#' Inputs
source("./src/set-inputs.R")
## Old age-specific study database
studydb <- read.csv(paste0("./gen/recover-studies2019-5to19y/temp/studydb-old-allcovar_", ageSexSuffix, ".csv", sep = ""))
## Old model inputs
if(ageSexSuffix == "05to09y"){load("./data/model-inputs-old/20201217-Data5to9-VAMCM009-Test3.RData")}
if(ageSexSuffix == "10to14y"){load("./data/model-inputs-old/20201222-Data10to14-VAMCM009-Test8j.RData")}
if(ageSexSuffix == "15to19yF"){load("./data/model-inputs-old/20210207-Data15to19Fem-VAMCM009-Test9.RData")}
if(ageSexSuffix == "15to19yM"){load("./data/model-inputs-old/20210212-Data15to19Men-VAMCM009-Test9e.RData")}
################################################################################

df_studydb_id <- studydb
df_studydb_id$source <- "key"
df_studydb_id <- df_studydb_id[,c("id","source")]

# Merge model input with study database
check <- merge(studies, df_studydb_id, by = c("id"), all = TRUE)

# Data points in model input that do not have an id in the studydb
nrow(subset(check, is.na(source)))
subset(check, is.na(source))
# This should only data points from LMM
subset(check, is.na(source))$reterm
# Missing for 5-9
# "COL" "ECU" "GUY" "NIC" "PER" "PRY" "SLV" "VEN"
# Missing for 10-14
# "COL" "ECU" "GUY" "NIC" "PER" "PRY" "SLV" "VEN"
# Missing for 15-19f
#"COL" "ECU" "GUY" "NIC" "PER" "PRY" "SLV" "VEN"
# Missing for 15-19m
# "ECU" "GUY" "NIC" "PER" "PRY" "SLV"

# Data points in studydb that do not have an id in the model input
# There should be none
subset(check, is.na(sid))
# 15-19m has these two extra data points
# ZAF-2003-37423-10-15-19-M
# ZAF-2012-37423-10-15-19-M 

if(nrow(subset(check, is.na(sid))) > 0){
  warning("Study database has more data points than model input")
}

# Remove any extra data points from studydb
# These should have been dropped.
# Perhaps were not because Pancho's script not replicated exactly
dat <- subset(studydb, id %in% studies$id)

# Save output(s) ----------------------------------------------------------

write.csv(dat, paste0("./gen/recover-studies2019-5to19y/temp/studydb-modinput-match_", ageSexSuffix,".csv", sep =""), row.names = FALSE)


