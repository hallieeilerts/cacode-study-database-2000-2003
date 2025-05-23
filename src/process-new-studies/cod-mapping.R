################################################################################
#' @description Apply age-specific COD mapping
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Study CODs in long format
dat <- read.csv(paste0("./gen/process-new-studies/temp/studies_long_", ageSexSuffix, ".csv", sep = ""))
## Key with mapped study CODs
dat_filename <- list.files("./data/study-data")
dat_filename <- dat_filename[grepl("studycausemapping", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1) # Most recent
key <- read.csv(paste0("./data/study-data/", dat_filename, sep = ""))
## Key with model classification for hmm/lmm countries
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("modelclass", dat_filename, ignore.case = TRUE)]
key_modclass <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Discussed at objective 1 meeting in early March, we don't remove China and others for under-5
if(ageSexSuffix %in% c("05to09y", "10to14y", "15to19yF", "15to19yM")){
  # Remove data points from COUNTRIES NOT VA
  v_keep <- subset(key_modclass, Group %in% c('HMM/LMM', 'HMM'))$ISO3
  dat <- subset(dat, iso3 %in% v_keep)
  nrow(dat)
}

length(grepl("adhoc", unique(dat$ref_id), ignore.case = TRUE)) # 70

# Subset data points applicable to age group being processed
if(ageSexSuffix %in% "00to28d"){
  dat <- subset(dat, age_ub_m <= 2) # 0-1 low-low, 0-2 low-high, 1-2 high-high
}
if(ageSexSuffix %in% "01to59m"){
  dat <- subset(dat, (age_lb_m >= 1 & age_ub_m > 2 & age_ub_m <= 60) | 
                                  # 1-3 low-low, 1-60 low-high, 59-60 high-high
                     (age_lb_m < 1  & age_ub_m > 2 & age_ub_m <= 60))  
                                  # 0-3 low-low, 0-60 low-high, 0-60 high-high (lb is always 0)
}
if(ageSexSuffix %in% "05to09y"){
  dat <- subset(dat, (age_lb_m >= 61 & age_ub_m <=120) | # 61-62 low-low, 61-120 low-high, 120-120 high-high
                  (age_lb_m < 61 & age_ub_m >= 61) |     # 0-61 low-low, 0-inf low-high, 60-inf high-high
                  (age_lb_m < 120 & age_ub_m >= 120))    # 0-120 low-low, 0-inf low-high, 119-inf high-high
}
# 60 = 5y
# 120 = 10y

if(ageSexSuffix %in% "10to14y"){
  dat <- subset(dat, (age_lb_m >= 121 & age_ub_m <= 180) | # 121-121 low-low, 121-180 low-high, 180-180 high-high
                  (age_lb_m < 121 & age_ub_m >= 121) |     # 0-121 low-low, 0-inf low-high, 120-inf high-high
                  (age_lb_m < 180 & age_ub_m >= 180))      # 0-180 low-low, 0-inf low-high, 179-inf high-high
}
# 120 = 10y
# 180 = 15y

if(ageSexSuffix %in% c("15to19yF", "15to19yM")){
  dat <- subset(dat, (age_lb_m >= 181 & age_ub_m <=228) | 
                  (age_lb_m < 181 & age_ub_m >= 181) | 
                  (age_lb_m < 228 & age_ub_m >= 228))
}

# length(unique(dat$article_id)[grepl("adhoc", unique(dat$article_id), ignore.case = TRUE)]) # 54 adhoc studies in 1-59m
# sort(sub("adHoc2022","",unique(dat$article_id)[grepl("adhoc", unique(dat$article_id), ignore.case = TRUE)]))


# View unique age groups
dat %>%
  select(age_lb_m, age_ub_m) %>%
  unique
# Check that lb is always less than ub
if(nrow(subset(dat, age_lb_m > age_ub_m)) > 0 ){
  warning("Age group lower bound is larger than upper bound.")
}

# Merge on cause mapping
dat <- merge(dat, key, by.x = "cause_of_death", by.y = "cod_study", all.x = TRUE)

# Note March 10, 2025
# not sure why these cod aren't matching even though they are in the study cause mapped key
# just doing a quick fix here
# must be something with special character for space
nrow(dat) # 1271 for 0-28d, 2769 for 1-59m
problems <- subset(dat, is.na(cod_mapped) & article_id == "2022MDS_IND2014")
problems$cod_mapped[grepl("asphyxia", problems$cause_of_death) & is.na(problems$cod_mapped)] <- "birth_asphyxia"
problems$cod_mapped[grepl("newborn", problems$cause_of_death) & is.na(problems$cod_mapped)] <- "neonatal_cond"
problems$cod_mapped[grepl("prematurity", problems$cause_of_death) & is.na(problems$cod_mapped)] <- "preterm"
other <- subset(dat, !(is.na(cod_mapped) & article_id == "2022MDS_IND2014"))
dat <- rbind(other, problems)
nrow(dat) # 1271

# Check that all causes are mapped
if(nrow(subset(dat, is.na(cod_mapped))) > 0){
  warning("Not all CODs found in studies and/or ad-hoc data were mapped.")
}

# Delete unnecessary columns
dat <- dat %>%
  select(-c(cause_of_death))

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/process-new-studies/temp/studies_cod-map_",ageSexSuffix,".csv",sep =""), row.names = FALSE)

