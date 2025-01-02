################################################################
# Matching studies to DHS regions
# (Not final - did quickly for Barcelona 10/2024 annual project meeting)
################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Study data
if(ageSexSuffix == "00to28d"){dat <- read.csv("./data/intermediate-study-db/neonatal_upd.csv")}
if(ageSexSuffix == "01to59m"){dat <- read.csv("./data/intermediate-study-db/1to59mo_data_upd.csv")}
## Prediction database
dat_filename <- list.files("./data/prediction-database")
dat_filename <- dat_filename[grepl("long", dat_filename, ignore.case = TRUE)]
pred <- read.csv(paste0("./data/prediction-database/", dat_filename, sep = ""))
## Covariate data extraction from DHS
dat_filename <- list.files("./data/dhs")
dat_filename <- dat_filename[grepl("long", dat_filename, ignore.case = TRUE)]
dhs <- read.csv(paste0("./data/dhs/", dat_filename, sep = ""))
## Keys
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("manually-added", dat_filename, ignore.case = TRUE)]
key_studydhs_byreg <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("studydhsregionmatching", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[!grepl("manually-added", dat_filename, ignore.case = TRUE)]
key_studydhs_byvar <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

## Patch - add meningitis_epi to replacement key
# I have already rerun studydata-dhs matching so that the key includes men_epi
# Need to have Astha add the manual regions
# Then add the latest DHS keys to the Data Warehouse.
menepi <- key_studydhs_byvar[,c("strata_id","iso3")]
menepi <- menepi[!duplicated(menepi),]
menepi$variable <- "meningitis_epi"
menepi$dhs_match <- NA
key_studydhs_byvar <- rbind(key_studydhs_byvar, menepi)
###

# Create clean strata_id variable
dat$strata_id <- sapply(dat$strata_id, function(x) gsub("[^[:alnum:]\\s]", "-", x))

### Add identifying columns to match old study database

# Recode
names(dat)[names(dat) == 'strata_gender'] <- 'sex'
names(dat)[names(dat) == 'study_location'] <- 'location'
names(dat)[names(dat) == 'totdeaths_orig'] <- 'totdeaths'
names(dat)[names(dat) == "VA.algorithm"] <- 'VA_algorithm'

# Re-label sexs
dat$sex[dat$sex == 1] <- sexLabels[3]
dat$sex[dat$sex == 2] <- sexLabels[2]
dat$sex[dat$sex == 3] <- sexLabels[1]

dat$sid <- 1:nrow(dat)
dat$year <- floor(dat$year_start + (dat$year_end - dat$year_start)/2)

# Create id variable
dat$id <- paste(dat$iso3, dat$year, dat$Refid, dat$strata_id,
                round(dat$age_lb), round(dat$age_ub), substr(dat$sex,1,1), sep = '-')

# REMOVE ONCE ASTHA HAS ADDED
if(!("nationalrep" %in% names(dat))){
  dat$nationalrep <- NA  #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! DELETE AFTER ASTHA HAS ADDED THIS
  dat$reterm <- NA
}

#####

# Keep dat info columns
studyinfo <- dat[,c("strata_id", "iso3", "year")]
nrow(subset(studyinfo, is.na(iso3))) # 0

# Merge studies to DHS survey key for each covariate
key <- merge(studyinfo, key_studydhs_byvar[,c("strata_id","variable","dhs_match")], by = "strata_id", all.x = TRUE) # 
# Check if some countries never match to a dhs survey
key %>% 
  mutate(hasdhs = ifelse(!is.na(dhs_match),1,0)) %>%
  group_by(iso3) %>%
  mutate(hasdhs = sum(hasdhs)) %>%
  filter(hasdhs == 0) %>%
  pull(iso3) %>% unique()
# Drop studies with "Mutliple" countries. Can't add covariates
key <- subset(key, iso3 != "Mutliple")
# !!!! Checked and didn't have these countries included in dhs data extraction
# need to do again

# Merge to DHS key with regions
key <- merge(key, key_studydhs_byreg[,c("strata_id","dhs_match","admin_level","region_name")], by = c("strata_id","dhs_match"), all.x = TRUE)
# Recode missing national region_name for merge with DHS data
key$region_name[key$admin_level == "National"] <- "XXNATIONALXX"
# Rename dhs_match
names(key)[which(names(key) == "dhs_match")] <- "survey_id"

# DHS data extraction
dhs <- dhs
dhs <- dhs[,c("variable","survey_id","admin_level","region_name","value")]
# Recode missing national region_name for merge with key
dhs$region_name[is.na(dhs$region_name) & dhs$admin_level == "National"] <- "XXNATIONALXX"

# Merge study/dhs region match key to dhs data extraction
dhs <- merge(key, dhs, by = c("survey_id","admin_level","region_name","variable"), all.x = TRUE)

# Subset covariates that need national prediction database values
df_match <- subset(dhs, !is.na(value))
df_miss <- subset(dhs, is.na(value))
nrow(df_miss) + nrow(df_match) == nrow(dhs)

# Subset columns of interest in prediction database
pred <- pred[,c("variable","iso3","year","value_main")]
v_cov_pred <- unique(pred$variable)

# Match studies missing covariates with prediction database
df_miss <- merge(df_miss, pred, by = c("iso3", "year","variable"), all.x = TRUE)
df_miss$value <- df_miss$value_main
df_miss$value_main <- NULL
# Make source column
df_miss$source <- paste("PredDB", df_miss$iso3, df_miss$year, sep = "-")

# Make source column for match
df_match$region_name[df_match$region_name == "XXNATIONALXX"] <- ""
df_match$source <- paste(df_match$survey_id, df_match$admin_level, df_match$region_name, sep = "-")
df_match$source <- gsub("\\-$", "", df_match$source)

# Recombine covariates that got values from DHS and those that got from prediction database
df_all <- rbind(df_miss, df_match)

# Only keep covariates in prediction database
# A few more were extracted from DHS
df_all <- subset(df_all, variable %in% v_cov_pred)
df_all <- df_all[,c("strata_id","variable","value","source")]
df_all <- df_all[order(df_all$strata_id, df_all$variable),]

# Reshape wide
wideVal <- df_all %>%
  pivot_wider(
    id_cols = strata_id,
    names_from = variable,
    values_from = value,
  )
v_valNames <- names(wideVal)[-1]
wideSrc <- df_all %>%
  pivot_wider(
    id_cols = strata_id,
    names_from = variable,
    values_from = source,
  )
v_srcnames <- paste(names(wideSrc),"_source",sep="")
v_srcnames[1] <- "strata_id"
names(wideSrc) <- v_srcnames
v_srcnames <- v_srcnames[-1]

# Merge covariates and source values
results <- merge(wideVal, wideSrc, by = "strata_id")
results <- results[,c("strata_id", sort(c(v_valNames, v_srcnames)))]

# Merge back onto original study data
final <- merge(dat, results, by = "strata_id", all.x = TRUE)

# Order columns
v_all_col <- names(final)
v_remaining <- v_all_col[!(v_all_col %in% c(idVarsAux, idVarsAuxNew))]
final <- final[,c(idVarsAux, idVarsAuxNew, v_remaining)]

# See if any covariate values are missing
nrow(subset(final, is.na(alcohol_f)))
# Missing for studies with multiple countries.
subset(final, is.na(alcohol_f))$iso3
# 5 missing for neonates
# 5 missing for post
# Drop these.
final <- subset(final, !is.na(alcohol_f))

# Tidy
final <- final[order(final$sid),]
row.names(final) <- NULL

# Save output(s) ----------------------------------------------------------

write.csv(final, paste0("./gen/add-study-covar/output/Studies2023_",ageSexSuffix,".csv",sep = ""), row.names=FALSE)


