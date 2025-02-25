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
# For ref_id, remove the last underscore which is effectively the strata number
adhoc <- adhoc %>%
  # if has underscore and trailing numbers, remove all
  # if does not have underscore, remove numbers at end of string
  # if has lower-case letter at end of string, remove last three characters
  mutate(hasUnderscoreNum = grepl("_[0-9]+$", study_id),
         article_id = sub("_(\\d+)$", "", study_id),
         hasNum = grepl("[0-9]+$", study_id),
         article_id = ifelse(hasUnderscoreNum == FALSE & hasNum == TRUE, sub("[0-9]+$", "", study_id), article_id),
         article_id = ifelse(hasUnderscoreNum == FALSE & hasNum == FALSE & grepl("[a-z]$", study_id), 
                         substr(study_id, 1, nchar(study_id) - 3), article_id)) %>%
  mutate(strata_id = study_id,
         ref_id = "AdHoc",
         location_long = NA,
         strata_other1 = NA,
         strata_other2 = NA,
         citation = NA,
         author = NA,
         totdeaths = totdeaths_orig) %>%
  rename(va_alg = VA.algorithm,
         year_mid = yr_mid, 
         countryname = country,
         location_short = study_loc
  ) %>%
  select(-c(study_id, totdeaths_orig, hasUnderscoreNum, hasNum)) %>%
  # Step 1: Calculate cod_n using cod_p (percentage) and totdeaths_orig
  mutate(across(starts_with("cod_n"), ~ if_else(
    # Condition: cod_n is NA, cod_p is not NA, and totdeaths_orig is not NA
    is.na(.x) & !is.na(as.numeric(get(paste0("cod_p", sub("cod_n", "", cur_column()))))) & !is.na(as.numeric(totdeaths)),
    # True: Calculate cod_n as (cod_p / 100) * totdeaths_orig (convert percentage to proportion)
    (as.numeric(get(paste0("cod_p", sub("cod_n", "", cur_column())))) / 100) * as.numeric(totdeaths),
    # False: Retain the original value of cod_n
    .x
  ))) 

# Check that both datasets now have all the same columns
names(adhoc)[!(names(adhoc) %in% names(dat))] # 0
names(dat)[!(names(dat) %in% names(adhoc))] # 0

dat <- rbind(dat, adhoc)

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/combine-studies-adhoc/temp/studies_add-ad-hoc.csv",sep =""), row.names = FALSE)

