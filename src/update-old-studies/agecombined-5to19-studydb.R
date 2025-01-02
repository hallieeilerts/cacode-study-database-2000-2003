################################################################################
#' @description Combines data points from all age-specific study databases for 2000-2019 with columns for study id and study location 
#' @return Data frame with study id and location for all old studies
#' Used to get subnational pfpr for MAP
#' Astha already sharing a list of locations for all studies from 2000-2023 systematic review.
#' Need subnational pfpr for old 5-19y studies as well. Already have subnational pfpr for old u5 studies.
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
library(readstata13)
#' Inputs
source("./src/set-inputs.R")
## Old age-specific study database with location character variable
old5 <- read.csv(paste0("./gen/update-old-studies/temp/studydb-old-allcovar_05to09y.csv", sep = ""))
old10 <- read.csv(paste0("./gen/update-old-studies/temp/studydb-old-allcovar_10to14y.csv", sep = ""))
old15f <- read.csv(paste0("./gen/update-old-studies/temp/studydb-old-allcovar_15to19yF.csv", sep = ""))
old15m <- read.csv(paste0("./gen/update-old-studies/temp/studydb-old-allcovar_15to19yM.csv", sep = ""))
################################################################################

#' RecNr is 1:nrow of the study databse
#' id is the id created by Pancho's code in update-old-studies/recover-study-id.R that matches the model inputs
#' idAux uses a factor for location that was created before dropping studies. 
#' idAux should match better across age-specific study databases
#' It makes it easier to see if the same data points are used in multiple models, which would otherwise be impossible because the location
#' variable was converted to a factor after running code dropping/combining studies which varied by age.
#' The id and idAux variables are updated when data points are merged/collapsed. 
#' Thus, these ids will sometimes not match between age groups if they were collapsed by sex for one model and not the other.
# View(studies[,c("RecNr","id","idAux")])

old5 <- old5[,c("RecNr","id", "idAux","Refid","year","iso3","location_char")]
old10 <- old10[,c("RecNr","id", "idAux","Refid","year","iso3","location_char")]
old15f <- old15f[,c("RecNr","id", "idAux","Refid","year","iso3","location_char")]
old15m <- old15m[,c("RecNr","id", "idAux","Refid","year","iso3","location_char")]

old <- rbind(old5, old10, old15f, old15m)
nrow(old) # 730 data points in all age-specific study DBs
length(unique(old$id)) # 663 unique Ids for Pancho's id variable where some location factors don't match even if they had the same text
# (location factor was created after some data points were dropped for certain age groups)
length(unique(old$idAux)) # 604 unique Ids for my id variable where location factor was generated right at the beginning
length(unique(old$RecNr)) # 603 unique record numbers which is 1:nrow
# (this is maybe one less than the unique id Aux because a collapse/merge of data points would change age or sex of idAux, but it would be the same row number)

old <- old[!duplicated(old),]
old <- old[order(old$RecNr),]
nrow(old) # 663

# For Astha to send to MAP
datMAP <- old[,c("id", "Refid","year","iso3","location_char")]
nrow(datMAP)

write.csv(datMAP, "./gen/update-old-studies/temp/MAPregions_2020studies05to19y_20241127.csv", row.names = FALSE)

#' Once we receive the subnational pfpr from MAP, will be able to merge it to the appropriate study using id
