################################################################################
#' @description Create a match key for covariate names and scales between master 2000-2020 study db, 2021 India pred db, and 2000-2023 pred db
#' @return Key with covariate names, scales in all databases
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
library(readstata13)
#' Inputs
source("./src/set-inputs.R")
## Old study database
dat_filename <- list.files("./data/study-data-old")
dat_filename <- dat_filename[grepl("neonates", dat_filename)]
dat00to28d <- read.dta13(paste0("./data/study-data-old/", dat_filename, sep = ""), nonint.factors = T)
dat_filename <- list.files("./data/study-data-old")
dat_filename <- dat_filename[grepl("combined", dat_filename)]
dat01to59m <- read.dta13(paste0("./data/study-data-old/", dat_filename, sep = ""), nonint.factors = T)
dat_filename <- list.files("./data/study-data-old")
dat_filename <- dat_filename[grepl("HMM", dat_filename)]
dat05to19y <- read.dta13(paste0("./data/study-data-old/", dat_filename, sep = ""), nonint.factors = T)
# ## Old age-specific study database
# study <- read.csv(paste0("./gen/update-old-studies/temp/studydb-modinput-match_", ageSexSuffix, ".csv", sep = ""))
## Prediction database
dat_filename <- list.files("./data/prediction-database")
dat_filename <- dat_filename[grepl("wide", dat_filename, ignore.case = TRUE)]
pred <- read.csv(paste0("./data/prediction-database/", dat_filename, sep = ""))
dat_filename <- list.files("./data/prediction-database")
dat_filename <- dat_filename[grepl("codebookbyvar", dat_filename, ignore.case = TRUE)]
pred_cb <- read.csv(paste0("./data/prediction-database/", dat_filename, sep = ""))
## India prediction database
dat_filename <- list.files("./data/prediction-database-india")
predInd <- read.csv(paste0("./data/prediction-database-india/", dat_filename, sep = ""))
################################################################################

# Study database: take covariate names from source column
scovar05to19y <- dat05to19y %>% select(ends_with("_source")) %>% 
  rename_with(function(c) str_replace(c, "_source", "")) %>%
  names()
scovar01to59m <- dat01to59m %>% select(ends_with("_source")) %>% 
  rename_with(function(c) str_replace(c, "_source", "")) %>%
  names()
scovar00to28d <- dat00to28d %>% 
  select(c(u5mr, nmr, dpt, gfr, lbwrate, sba, bcg, pab, femlit, premvslbw)) %>%
  names()

scovar <- c(scovar05to19y, scovar01to59m, scovar00to28d)
scovar <- unique(sort(scovar))
# # Old study database had both ors and ors_mf
# # Drop ors
# scovar <- scovar[(!scovar %in% c("ors"))]

# Prediction database: take covariate names from source column
pcovar <- pred %>% select(ends_with("_source")) %>% 
  rename_with(function(c) str_replace(c, "_source", "")) %>%
  names()
# India prediction database: take covariate names from source column
pcovar_ind <- predInd %>% select(ends_with("_source")) %>% 
  rename_with(function(c) str_replace(c, "_source", "")) %>%
  names()

# Merge study and prediction database covariate names
key <- data.frame(study = scovar) %>%
  full_join(data.frame(pred = pcovar), by = join_by(study == pred), keep = TRUE) %>% 
  full_join(data.frame(predInd = pcovar_ind), by = join_by(study == predInd), keep = TRUE) %>%
  arrange(study, pred)

# Add study covariates for mortality covariates
# These aren't in the original study database file, but added in the age-specific cleaning process
key$study[key$pred == "mr05to19"] <- "q5to19"
key$study[key$pred == "mr15to19_mf"] <-  "MR15_19"

# Create column to manually match study names to pred
key$predFillIn <- key$pred
subset(key, is.na(pred))$study
subset(key, is.na(study))$pred
# Assign name of study covariates that are NA in pred, but they are actually there under a different name
# Didn't match up because name has changed
key$predFillIn[key$study == "_5q0"] <- "u5mr"
key$predFillIn[key$study == "alcohol"] <- "alcohol_mf"
key$predFillIn[key$study == "bcg"] <- "vac_bcg"
#key$predFillIn[key$study == "bcg_f"] <- 
#key$predFillIn[key$study == "bcg_m"] <- 
key$predFillIn[key$study == "bcg_mf"] <- "vac_bcg"
key$predFillIn[key$study == "birth_healthfacility_3"] <- "birth_healthfacility3"
key$predFillIn[key$study == "birth_healthfacility_5"] <- "birth_healthfacility5" 
#key$predFillIn[key$study == "cannabis_f"] <- 
#key$predFillIn[key$study == "cannabis_m"] <- 
#key$predFillIn[key$study == "cannabis_mf"] <- 
#key$predFillIn[key$study == "clean_fuel"] <- 
#key$predFillIn[key$study == "condom_risk_f"] <- 
#key$predFillIn[key$study == "condom_risk_m"] <- 
#key$predFillIn[key$study == "condom_risk_mf"] <- 
#key$predFillIn[key$study == "contraception_now"] <- 
key$predFillIn[key$study == "contraception_unmet"] <- "contraception_met"
key$predFillIn[key$study == "dpt"] <- "vac_dtp3" 
key$predFillIn[key$study == "dtp3_mf"] <- "vac_dtp3" 
#key$predFillIn[key$study == "edu_completion_f"] <- 
#key$predFillIn[key$study == "edu_completion_m"] <- 
#key$predFillIn[key$study == "edu_completion_mf"] <-
#key$predFillIn[key$study == "edu_expect_f"] <- 
#key$predFillIn[key$study == "edu_expect_m"] <- 
#key$predFillIn[key$study == "edu_expect_mf"] <- 
#key$predFillIn[key$study == "edu_index"] <- 
#key$predFillIn[key$study == "exbf"] <- 
key$predFillIn[key$study == "femlit"] <- "literacy_f"
key$predFillIn[key$study == "hib3"] <- "vac_hib3" 
key$predFillIn[key$study == "hib3_mf"] <- "vac_hib3" 
key$predFillIn[key$study == "imr"] <- "imr_mf" 
key$predFillIn[key$study == "lbwrate"] <-  "lbw"
key$predFillIn[key$study == "lowest_wealth"] <- "wealth_lowest" 
#key$predFillIn[key$study == "mcv_f"] <- 
#key$predFillIn[key$study == "mcv_m"] <- 
key$predFillIn[key$study == "mcv_mf"] <- "vac_mcv1" 
key$predFillIn[key$study == "mcv"] <- "vac_mcv1" 
key$predFillIn[key$study == "ors"] <- "ors_mf" 
key$predFillIn[key$study == "pcv3_mf"] <- "vac_pcv3" 
key$predFillIn[key$study == "pcv3"] <- "vac_pcv3" 
#key$predFillIn[key$study == "pop_over15_f"] <- 
#key$predFillIn[key$study == "pop_over15_m"] <- 
#key$predFillIn[key$study == "pop_over15_mf"] <- 
#key$predFillIn[key$study == "premvslbw"] <- 
key$predFillIn[key$study == "rota_last_mf" ] <- "vac_rota_last" 
key$predFillIn[key$study == "sanitation" ] <- "wash_sanitation_improved"
key$predFillIn[key$study == "sba" ] <- "sab"
key$predFillIn[key$study == "sex_age_f_15"] <- "sex_age15_f"
key$predFillIn[key$study == "sex_age_f_18"] <- "sex_age18_f"
key$predFillIn[key$study == "sex_age_f_20"] <- "sex_age20_f"
key$predFillIn[key$study == "sex_age_m_15"] <- "sex_age15_m"
key$predFillIn[key$study == "sex_age_m_18"] <- "sex_age18_m"
key$predFillIn[key$study == "sex_age_m_20"] <- "sex_age20_m"                                                 
key$predFillIn[key$study == "sex_age_mf_15"] <- "sex_age15_mf"
key$predFillIn[key$study == "sex_age_mf_18"] <- "sex_age18_mf"
key$predFillIn[key$study == "sex_age_mf_20"] <- "sex_age20_mf"  
#key$predFillIn[key$study == "stunt_f"] <-  
#key$predFillIn[key$study == "stunt_m"] <- 
key$predFillIn[key$study == "stunt_mf"] <- "stunting"
#key$predFillIn[key$study == "tobacco_f"] <-
#key$predFillIn[key$study == "tobacco_m"] <-
#key$predFillIn[key$study == "tobacco_mf"] <-
key$predFillIn[key$study == "u5pop"] <- "u5pop_mf"
#key$predFillIn[key$study == "underwt_f"] <-   
#key$predFillIn[key$study == "underwt_m"] <-   
key$predFillIn[key$study == "underwt_mf"] <- "underweight"
key$predFillIn[key$study == "underwt"] <- "underweight"
#key$predFillIn[key$study == "vehicles"] <-
key$predFillIn[key$study == "water"] <- "wash_water_improved"

# Check work: pred and predfillin should always match
subset(key, !is.na(pred) & pred != predFillIn)
# Check NAs in predfillin and make sure there is in fact no corresponding covariate in the prediction database for that study covariate

# Count appearance of pred fill-in covariates
setDT(key)[,n:=.N,by=predFillIn]
# Recode N of NA values as NA
key$n[is.na(key$predFillIn)] <- NA
# Discard those that were missing but now assigned to a study covar (and thus appear twice)
key <- subset(key, !(is.na(study) & !is.na(n) & n >= 2))

# Replace pred column with predfillin
key$n <- key$pred <- NULL
names(key)[which(names(key) == "predFillIn")] <- "pred"

# Create column to fill in manually
# Match study names to predInd
key$predIndFillIn <- key$predInd
subset(key, is.na(predInd))$study
subset(key, is.na(study))$predInd
key$predIndFillIn[key$study == "bcg_mf"] <- "bcg"
key$predIndFillIn[key$study == "dtp3_mf"] <- "dpt3" 
#key$predIndFillIn[key$study == "hib3_mf"] <- "hib3"
key$predIndFillIn[key$study == "literacy_f"] <- "literacy_fem" 
#key$predIndFillIn[key$study == "mcv_mf"] <- "mcv" 
#key$predIndFillIn[key$study == "ors_mf"] <- "ors" 
#key$predIndFillIn[key$study == "pcv3_mf"] <- "pcv" 
key$predIndFillIn[key$study == "rota_last_mf" ] <- "rota_last" 
#key$predIndFillIn[key$study == "stunt_mf"] <- "stunting"
key$predIndFillIn[key$study == "underwt_mf"] <- "underweight"

# Check work: predInd and predIndfillin should always match
subset(key, !is.na(predInd) & predInd != predIndFillIn)
# Check NAs in predfillin and make sure there is in fact no corresponding covariate in the prediction database for that study covariate

# Count appearance of pred fill-in covariates
setDT(key)[,n:=.N,by=predIndFillIn]
# Recode N of NA values as NA
key$n[is.na(key$predIndFillIn)] <- NA
# Discard those that were missing but now assigned to a study covar (and thus appear twice)
key <- subset(key, !(is.na(study) & !is.na(predIndFillIn) & n == 2 & !is.na(n) & n >= 2))

# Replace predInd column with predIndfillin
key$n <- key$predInd <- NULL
names(key)[which(names(key) == "predIndFillIn")] <- "predInd"

# Merge on predication variable scale
key <- merge(key, pred_cb[,c("variable","scale")], by.x = "pred", by.y = "variable", all = TRUE)
names(key)[which(names(key) == "scale")] <- "pred_scale"

# Create column which contains all covar
key$all <- key$pred
key$all[is.na(key$all)] <- key$study[is.na(key$all)]
key$all[is.na(key$all)& is.na(key$study)] <- key$predInd[is.na(key$all) & is.na(key$study)]

# Check if "all" has any repetition
setDT(key)[,n:=.N,by=all]
subset(key, n > 1)
# Yes
# This is because under-5 study database didnt use sex suffixes for certain covariates
# And the 5-19 database did
# Make "all" into a unique variable
setDT(key)[,n:=1:.N,by=all]
key$all <- paste0(key$all, "-", key$n, sep = "")
length(unique(key$all)) == nrow(key)
key$n <- NULL

# Scale correction for old study database ---------------------------------

# # Drop India?
# key <- subset(key, !(is.na(study) & is.na(pred)))
# key$predInd <- NULL

key$study_adjustment <- NA
key$study_adjustment[!is.na(key$study)] <- "none"
key$study_adjustment[!is.na(key$study) & key$pred_scale == "zero_one"] <- "divide by 100"
key$study_adjustment[key$pred %in% c("pop_male_15_29", "sab")] <- "none"
key$study_adjustment[key$pred %in% c("contraception_met")] <- "subtract from 100 and divide by 100"

# Tidy
key <- key[,c("all","study","pred", "predInd","pred_scale","study_adjustment")]
key <- key[order(key$all),]

# Save output -------------------------------------------------------------

warning("Covariate key saving turned off on May 13, 2025. Turn back on saving function if update made to covariates.")
# April 30th update: adding mr05to09, mr05to14
# May 13th update: adding premvslbw
#write.csv(key, paste0("./gen/update-old-studies/output/CovariateKey_Study2019Pred2023_",format(Sys.Date(), format="%Y%m%d"),".csv", sep = ""), row.names = FALSE)


