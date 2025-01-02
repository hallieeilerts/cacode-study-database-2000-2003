
#' Clear environment
rm(list = ls())
library(stringr)

#' Inputs
source("./src/update-covar-for-old-studies/set-inputs.R")

## Old model input
if(ageSexSuffix == "05to09y"){load("./data/model-inputs-old/20201217-Data5to9-VAMCM009-Test3.RData")}
modinput <- studies
modinput$iso3 <- substr(modinput$id, 1, 3)
modinput$year <- substr(modinput$id, 5, 8)
modinput$age_lb <- sapply(strsplit(as.character(modinput$id), "-"), `[`, 5)
modinput$age_ub <- sapply(strsplit(as.character(modinput$id), "-"), `[`, 6)
modinput <- modinput[,c("id","iso3","year","totdeaths", "age_lb", "age_ub","dtp3_mf")]
# id is unique

## Old study database
dat_filename <- list.files("./data/study-data-old")
studydb <- read.dta13(paste0("./data/study-data-old/", dat_filename, sep = ""))
studydb <- studydb[,c("Refid","study_id","study_location","age_lb","age_ub","strata_gender","iso3","year","totdeaths_orig", "dtp3_mf")]
studydb$totdeaths <- round(studydb$totdeaths_orig)
# study_id is unique

# Key for matching old model input to old studydb
# key <- read.csv(paste0("./gen/update-covar-for-old-studies/temp/key_studyid_",ageSexSuffix,".csv",sep =""))

key <- read.csv(paste0("./gen/update-covar-for-old-studies/temp/studydb-old-allcovar_",ageSexSuffix,".csv",sep =""))
key$source <- "key"
key <- key[,c("id","source")]

# Merge modinput with key -------------------------------------------------

m1 <- merge(modinput, key, by = c("id"), all.x = TRUE)
head(m1)
# Match
match <- subset(m1, !is.na(source))
# Number of matches
setDT(match)[,n:=.N,by=id]
match1 <- subset(match, n == 1)
match1[,-c("study_location")]
# modinput id is matched to multiple studydb id
matchmult <- subset(match, n > 1)
matchmult <- matchmult[order(matchmult$id)]
matchmult[,-c("study_location")]
# No match
nomatch <- subset(m1, is.na(source))

subset(modinput, iso3 == "AGO" & year == 2011)
subset(studydb, iso3 == "AGO" & year == 2011)
subset(key, iso3 == "AGO" & year == 2011)

# Merge on iso3, year, age_lb, age_ub, and a covariate with subnational info ------------------------------------------

# Merge
head(modinput)
head(studydb)
m1 <- merge(modinput, studydb, by = c("iso3","year", "age_lb", "age_ub", "dtp3_mf"), all.x = TRUE)
head(m1)
# Match
match <- subset(m1, !is.na(study_id))
# Number of matches
setDT(match)[,n:=.N,by=id]
match1 <- subset(match, n == 1)
match1[,-c("study_location")]
# modinput id is matched to multiple studydb id
matchmult <- subset(match, n > 1)
matchmult <- matchmult[order(matchmult$id)]
matchmult[,-c("study_location")]
# No match
nomatch <- subset(m1, is.na(study_id))
# THIS NOW IS JUST THE LMM COUNTRIES THAT ARE IN THE MODEL INPUT BUT NOT THE STUDY DATABASE

# De-duplicate multiple matches
# Go to script recover-study-id
# Check actual characters of location factor in id variable

matchmult
subset(dat, ISO3 == "IND" & year == 2012 & age_lb == 5 & age_ub == 9)[,c("location","location_fac", "dtp3_mf")]



