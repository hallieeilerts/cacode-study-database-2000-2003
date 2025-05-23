################################################################################
#' @description Merge on subnational covariates from DHS
#' @return Long data where each row is study/covar with covar value from DHS if applicable, and NA otherwise
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Study data with too small and too large data points excluded
studies <- read.csv(paste0("./gen/process-new-studies/temp/studies_exc_", ageSexSuffix, ".csv", sep = ""))
## Covariate data extraction from DHS
dat_filename <- list.files("./data/dhs")
dat_filename <- dat_filename[grepl("long", dat_filename, ignore.case = TRUE)]
dhs <- read.csv(paste0("./data/dhs/", dat_filename, sep = ""))
dat_filename <- list.files("./data/dhs")
dat_filename <- dat_filename[grepl("wide", dat_filename, ignore.case = TRUE)]
dhsWide <- read.csv(paste0("./data/dhs/", dat_filename, sep = ""))
## Keys 
# Study-covariate matched with DHS survey
dat_filename <- list.files("./data/dhs")
dat_filename <- dat_filename[grepl("studydhsregionmatching", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[!grepl("manually-added", dat_filename, ignore.case = TRUE)]
key_dhs_var <- read.csv(paste0("./data/dhs/", dat_filename, sep = ""))
# Studies matched with DHS survey (sometimes multiple) and survey-specific region
dat_filename <- list.files("./data/dhs")
dat_filename <- dat_filename[grepl("manually-added", dat_filename, ignore.case = TRUE)]
key_dhs_reg <- read.csv(paste0("./data/dhs/", dat_filename, sep = ""))
################################################################################

# We would like to first have any covariates reported alongside the CODs in the studies.
# In the 2000-2023 round it seems that there were barely any.
# So we start with DHS subnational values and progress to national.

# Keep id and iso3/year for merging on covariates
studyinfo <- studies[,c("strata_id","iso3", "year_mid")]
nrow(subset(studyinfo, is.na(iso3))) # 0

# Create data frame with empty value for every covariate
covarinfo <- data.frame(strata_id = rep(studies$strata_id, each = length(unique(key_dhs_var$variable))), variable = unique(key_dhs_var$variable))
studyinfo <- merge(studyinfo, covarinfo, by = "strata_id")

# Merge studies to key with study id/covar/DHS survey match
df_studies <- merge(studyinfo, key_dhs_var[,c("strata_id","variable","dhs_match")], by = c("strata_id","variable"), all.x = TRUE) # 

# Check if some countries never match to a dhs survey
if(length(df_studies %>% 
  mutate(hasdhs = ifelse(!is.na(dhs_match),1,0)) %>%
  group_by(iso3) %>%
  mutate(hasdhs = sum(hasdhs)) %>%
  filter(hasdhs == 0) %>%
  pull(iso3) %>% unique()) > 0){
  warning("Check that countries match to a DHS survey where possible")
}
df_studies %>% 
  mutate(hasdhs = ifelse(!is.na(dhs_match),1,0)) %>%
  group_by(iso3) %>%
  mutate(hasdhs = sum(hasdhs)) %>%
  filter(hasdhs == 0) %>%
  pull(iso3) %>% unique()
# 0-28d: BOL. 
## There are DHS for Bolivia but it is not in the DHS extraction because the extraction was done before there was adHoc data (and no study data points were from Bolivia). No need to add to the DHS data extraction though because this particular data point is national.
# 1-59m: BOL, CHN, IRN
## There are no DHS for China or Iran
# 5-9y: all match
# 10-14y: all match

# Merge studies to key with study id/DHS survey match/DHS region match
df_studies <- merge(df_studies, key_dhs_reg[,c("strata_id","dhs_match","admin_level","region_name")], by = c("strata_id","dhs_match"), all.x = TRUE)
# If region_name is missing for the DHS survey match, recode as "XXNATIONALXX"
# This is because either (i) we haven't identified the studies dhs region yet, (ii) the study was at the national level
df_studies$region_name[df_studies$admin_level == "National"] <- "XXNATIONALXX"
# Rename dhs_match column
names(df_studies)[which(names(df_studies) == "dhs_match")] <- "survey_id"

# DHS data extraction with values
df_dhs <- dhs[,c("variable","survey_id","admin_level","region_name","value")]
# Recode missing national region_name
df_dhs$region_name[is.na(df_dhs$region_name) & df_dhs$admin_level == "National"] <- "XXNATIONALXX"

# Merge studies with matched dhs survey id, region, covar with actual values from dhs data extraction
df_covar <- merge(df_studies, df_dhs, by = c("survey_id","admin_level","region_name","variable"), all.x = TRUE)

# Check if any DHS regions didn't merge
# View(subset(df_covar, !is.na(region_name) & is.na(value)))
# Sometimes it might be the case that they merged, but there was an NA value in the DHS data
# Check that the region manually assigned in the region key does exist in that DHS
if(df_covar %>%
   filter(!is.na(region_name) & is.na(value)) %>%
   select(survey_id, region_name) %>%
   unique() %>%
   left_join(dhsWide[,c("survey_id", "region_name","iso3")], by = join_by(survey_id, region_name)) %>% nrow > 0){
  warning("Check that studies matched to DHS regions")
}
# First subsetting those with missing values and merging on DHS region key
# If the iso3 from the DHS region key is missing, there was an error in manually assigned region
df_covar %>%
  filter(!is.na(region_name) & is.na(value)) %>% 
  select(survey_id, region_name) %>%
  unique() %>%
  left_join(dhsWide[,c("survey_id", "region_name","iso3")]) #%>% View

# If the iso3 from the DHS region key is not NA it means the region did merge but the value was missing for that region
# Check covariates that are missing
df_covar %>%
  filter(!is.na(region_name) & is.na(value)) %>%
  select(survey_id, region_name, variable) %>%
  unique() %>%
  left_join(dhsWide[,c("survey_id", "region_name","iso3")]) %>%
  filter(!is.na(iso3))
# 5-9y: edu_mean_m, sex_age15_m, sex_age15_mf, ors_f, ors_m, ors_mf, obese_mf
# 10-14y: edu_mean_m, sex_age15_m, sex_age15_mf, ors_f, ors_m, ors_mf

# If region_name is national, it means that the value was missing for that nation
# Check national covar that are missing
df_covar %>%
  filter(!is.na(region_name) & is.na(value)) %>%
  filter(region_name == "XXNATIONALXX")
# 1-59m: edu_mean_f, edu_mean_mf
# 5-9y: none
# 10-14y: edu_mean_mf for GH2014DHS

# Decision: if it is a national-level study, the value should come from the prediction database and not a national-level DHS data point.
# Comment out the below line of code in order to use DHS national value (if decision changes)
df_covar$value[df_covar$region_name == "XXNATIONALXX"] <- NA

# Subset study covariates that were matched to DHS
df_match <- subset(df_covar, !is.na(value))
# Subset covariates that need national prediction database values
df_miss <- subset(df_covar, is.na(value))
nrow(df_miss) + nrow(df_match) == nrow(df_covar)

# Make source column for match
df_match$region_name[df_match$region_name == "XXNATIONALXX"] <- ""
df_match$source <- paste(df_match$survey_id, df_match$admin_level, sub(" ", "-",df_match$region_name), sep = "_")
df_match$source <- gsub("\\-$", "", df_match$source)

# Merge back with study covariates that were not matched to a dhs value
df_miss$source <-NA
df_studies_dhscovar <- rbind(df_match, df_miss)

# Delete unnecessary columns
df_studies_dhscovar <- df_studies_dhscovar[!(names(df_studies_dhscovar) %in% c("survey_id","admin_level","region_name"))]

# Save output(s) -------------------------------------------------------------

write.csv(df_studies_dhscovar, paste("./gen/process-new-studies/temp/studycovar_subnat_", ageSexSuffix,".csv", sep = ""), row.names = FALSE)

