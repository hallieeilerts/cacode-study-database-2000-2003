################################################################################
#' @description Add new information that was extracted for old studies
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
library(stringr)
library(readxl)
#' Inputs
source("./src/set-inputs.R")
## Old study database
if(ageSexSuffix %in% c("01to59m")){
  dat_filename <- list.files("./data/study-data-old")
  dat_filename <- dat_filename[grepl("combined", dat_filename)]
  studies <- read.dta13(paste0("./data/study-data-old/", dat_filename, sep = ""), nonint.factors = T)
}
## Newly extracted variables for old studies
extract <- read_excel(paste0("./data/study-data-old-augmented/old 1to59_20250203.xlsx", sep = ""))
################################################################################

# Rename columns
df_extract <- extract %>%
  rename(va_alg_src = va_alg_stat) %>%
  select(study_id, VA.algorithm, va_alg_src, age_lb_m, age_ub_m)
  
df_studies <- studies %>%
  mutate(study_id = trimws(study_id))

dat <- merge(df_studies, df_extract, by = "study_id", all.x = TRUE)

# Check if newly extracted information is available for all data points
#View(dat[,c("study_id", "iso3","citation","VA.algorithm","va_alg_src","age_lb_m.x","age_ub_m.x","age_lb_m.y","age_ub_m.y")])
#View(subset(dat, is.na(age_ub_m.y))[,c("study_id", "iso3","citation","VA.algorithm","va_alg_src","age_lb_m.x","age_ub_m.x","age_lb_m.y","age_ub_m.y")])

# Studies with missing extracted information
# ***I think Astha identified these as duplicates and therefore didn't bother extracting new information
#View(subset(dat_other, is.na(VA.algorithm))[,c("study_id", "iso3","citation","VA.algorithm","va_alg_src","age_lb_m.x","age_ub_m.x","age_lb_m.y","age_ub_m.y")])
#View(dat[,c("study_id", "iso3","citation","VA.algorithm","va_alg_src","age_lb_m.x","age_ub_m.x","age_lb_m.y","age_ub_m.y")])
# Use old information when missing
dat$age_lb_m <- dat$age_lb_m.y
dat$age_lb_m[is.na(dat$age_lb_m)] <- dat$age_lb_m.x[is.na(dat$age_lb_m)] 
dat$age_ub_m <- dat$age_ub_m.y
dat$age_ub_m[is.na(dat$age_ub_m)] <- dat$age_ub_m.x[is.na(dat$age_ub_m)] 
dat$va_alg_src[is.na(dat$VA.algorithm)] <- "study"
dat$VA.algorithm[is.na(dat$VA.algorithm)] <- "Not reported"

nrow(dat) == nrow(studies)

dat <- dat %>%
  select(-c(age_lb_m.x, age_lb_m.y, age_ub_m.x, age_ub_m.y))

# Save output -------------------------------------------------------------

# Old model input that now has covariate names and scales as pred database
write.csv(dat, paste0("./gen/update-old-studies/temp/studies_add-extracted-vars_", ageSexSuffix, ".csv"), row.names = FALSE)
