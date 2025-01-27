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
#dat <- read.csv(paste0("./gen/process-new-studies/temp/studies_long.csv", sep = ""))
dat <- read.csv(paste0("./gen/process-new-studies/temp/studies_long_", ageSexSuffix, ".csv", sep = ""))
## Key with mapped study CODs
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("studycausemapping", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1) # Most recent
key <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Subset data points applicable to age group being processed
if(ageSexSuffix %in% "00to28d"){
  dat <- subset(dat, age_ub_m <= 2) # 0-1 low-low, 0-2 low-high, 1-2 high-high
}
if(ageSexSuffix %in% "01to59m"){
  dat <- subset(dat, (age_lb_m >= 1 & age_ub_m > 2 & age_ub_m <= 60) | 
                                  # 1-3 low-low, 1-60 low-high, 59-60 high-high (assuming lb is always less than ub)
                     (age_lb_m < 1  & age_ub_m > 2 & age_ub_m <= 60))  
                                  # 0-3 low-low, 0-60 low-high, 0-60 high-high (lb is always 0)
}
if(ageSexSuffix %in% "05to09y"){
  dat <- subset(dat, (age_lb_m >= 61 & age_ub_m <=120) | # 61-62 low-low, 61-120 low-high, 119-120 high-low, 120-120 high-high
                  (age_lb_m < 61 & age_ub_m >= 61) |     # 0-61 low-low, 0-inf low-high, 59-61 high-low, 60-inf high-high
                  (age_lb_m < 120 & age_ub_m >= 120))    # 0-120 low-low, 0-inf low-high, 119-120 high-low, 119-inf high-high
}
# 60 = 5y
# 120 = 10y

if(ageSexSuffix %in% "10to14y"){
  dat <- subset(dat, (age_lb_m >= 121 & age_ub_m <= 180) | (age_lb_m < 121 & age_ub_m >= 121) | (age_lb_m < 180 & age_ub_m >= 180))
}
if(ageSexSuffix %in% c("15to19yF", "15to19yM")){
  dat <- subset(dat, (age_lb_m >= 181 & age_ub_m <=228) | 
                  (age_lb_m < 181 & age_ub_m >= 181) | 
                  (age_lb_m < 228 & age_ub_m >= 228))
}

# View unique age groups
dat %>%
  select(age_lb_m, age_ub_m) %>%
  unique
# Check that lb is always less than ub
if(nrow(subset(dat, age_lb_m > age_ub_m)) > 0 ){
  warning("Age group lower bound is larger than upper bound.")
}

# Merge on cause mapping
dat <- merge(dat, key, by.x = "cause_of_death", by.y = "cod_study")

# Delete unnecessary columns
dat <- dat %>%
  select(-c(cause_number, cause_of_death, cod_p, cod_mr, cod_mro))%>%
  mutate(cod_n = as.numeric(cod_n))

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/process-new-studies/temp/studies_cod-map_",ageSexSuffix,".csv",sep =""), row.names = FALSE)

