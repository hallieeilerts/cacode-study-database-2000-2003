################################################################################
#' @description
#' @return
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readxl)
require(tidyverse)
require(countrycode)
#' Inputs
source("./src/set-inputs.R")
## Ad-hoc data
dat_filename <- list.files("./data/ad-hoc")
dat_filename <- dat_filename[grepl("dhs_adhoc studies", dat_filename, ignore.case = TRUE)]
adhoc <- read_excel(paste0("./data/ad-hoc/", dat_filename, sep = ""))
## Study CODs with cleaned columns
dat <- read.csv(paste0("./gen/combine-studies-adhoc/temp/studies_convert-dths.csv", sep = ""))
################################################################################

# adhoc column names that are not in studies
names(adhoc)[!(names(adhoc) %in% names(dat))]

# studies column names that are not in ad-hoc
names(dat)[!(names(dat) %in% names(adhoc))]

# Create age group in months variable

# Rename in adhoc to match studies
adhoc <- adhoc %>%
  mutate(ref_id = study_id,
         strata_id = paste0("Adhoc",study_id),
         article_id = paste0("Adhoc",study_id),
         location_long = NA,
         strata_other1 = NA,
         strata_other2 = NA) %>%
  rename(va_alg = VA.algorithm,
         year_mid = yr_mid, 
         countryname = country,
         location_short = study_loc
  ) %>%
  select(-c(study_id, totdeaths_orig))

# Check that both datasets now have all the same columns
names(adhoc)[!(names(adhoc) %in% names(dat))] # 0
names(dat)[!(names(dat) %in% names(adhoc))] # 0

dat <- rbind(dat, adhoc)

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/combine-studies-adhoc/temp/studies_add-ad-hoc.csv",sep =""), row.names = FALSE)

