################################################################################
#' @description Assess number of new studies added for each age group
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readxl)
require(tidyverse)
require(data.table)
#' Inputs
source("./src/process-new-studies/set-inputs.R")

new05to09y <- read.csv(paste0("./gen/process-new-studies/audit/NewStudyDataPoints_05to09y.csv"))
new10to14y <- read.csv(paste0("./gen/process-new-studies/audit/NewStudyDataPoints_10to14y.csv"))
new15to19yF <- read.csv(paste0("./gen/process-new-studies/audit/NewStudyDataPoints_15to19yF.csv"))
new15to19yM <- read.csv(paste0("./gen/process-new-studies/audit/NewStudyDataPoints_15to19yM.csv"))

load("./data/model-inputs/20201217-Data5to9-VAMCM009-Test3.RData")
old05to09y <- studies
load("./data/model-inputs/20201222-Data10to14-VAMCM009-Test8j.RData")
old10to14y <- studies
load("./data/model-inputs/20210207-Data15to19Fem-VAMCM009-Test9.RData")
old15to19yF <- studies
load("./data/model-inputs/20210212-Data15to19Men-VAMCM009-Test9e.RData")
old15to19yM <- studies

################################################################################

# Data frame with number of new studies added to each age group
# Columns for number of new studies excluded and number of old studies
new05to09y$AgeSexSuffix <- "05to09y" 
new10to14y$AgeSexSuffix <- "10to14y" 
new15to19yF$AgeSexSuffix <- "15to19yF" 
new15to19yM$AgeSexSuffix <- "15to19yM" 
new <- rbind(new05to09y, new10to14y, new15to19yF, new15to19yM)
setDT(new)[,n_exclude := sum(exclude), by=AgeSexSuffix]
setDT(new)[,n_new := .N,by=AgeSexSuffix]
new$n_new <- new$n_new - new$n_exclude
new$n_old <- nrow(old05to09y)
new$n_old <- ifelse(new$AgeSexSuffix == "10to14y", nrow(old10to14y), new$n_old)
new$n_old <- ifelse(new$AgeSexSuffix == "10to14y", nrow(old10to14y), new$n_old)
new$n_old <- ifelse(new$AgeSexSuffix == "15to19yF", nrow(old15to19yF), new$n_old)
new$n_old <- ifelse(new$AgeSexSuffix == "15to19yM", nrow(old15to19yM), new$n_old)

new$n_ctry <- 1 - new$exclude
new[,n_ctry:=sum(n_ctry),by=list(ISO3,AgeSexSuffix)]

new$perincrease <- (new$n_new)/new$n_old

#' New inputs ###########
#' single cause - HIV
#' single cause - malaria
#' new prediction database
#' a few new studies
#' coefficients
#' 
#' Extended from simple update ############
#' single cause - crisis
#' single cause - measles
#' single cause - tb
#' envelopes - igme

write.csv(new, paste0("./gen/process-new-studies/audit/StudyDatabaseChanges.csv"), row.names = FALSE)




