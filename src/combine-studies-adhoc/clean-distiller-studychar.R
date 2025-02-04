################################################################################
#' @description Extract necessary columns from study characteristics
#' @return Study info columns for countryname, iso3, location, VA algorithm
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readxl)
require(tidyverse)
require(countrycode)
#' Inputs
source("./src/set-inputs.R")
## Studies and their identifying columns from 2000-2023 systematic review
dat_filename <- list.files("./data/study-data")
dat_filename <- dat_filename[grepl("Distiller_StudyCharacteristics", dat_filename, ignore.case = TRUE)]
dat <- read_excel(paste0("./data/study-data/", dat_filename, sep = ""))
################################################################################

# Correct incorrect iso3 codes and add country name column
dat$country[dat$country == "Mutliple"] <- "Multiple"
dat$iso3 <- dat$country
dat$countryname <- countrycode(dat$iso3, origin = "iso3c", destination = "country.name")
dat$iso3[dat$country == "Multiple"] <- NA
dat$iso3[dat$country == "SIL"] <- "SLE"
dat$iso3[dat$country == "TAN"] <- "TZA"
dat$iso3[dat$country == "TMP"] <- "TLS"
dat$iso3[dat$country == "ZAM"] <- "ZMB"
dat$countryname <- countrycode(dat$iso3, origin = "iso3c", destination = "country.name")
# Delete original country column
dat <- dat[!(names(dat) %in% "country")]

# Rename columns
dat <- dat %>%
  rename(ref_id = Refid,
         va_alg = `Verbal Autopsy Algorithm`,
         location_long = study_location) 

# Keep necessary columns
dat <- dat[,c("ref_id", "article_id", "iso3", "countryname", "location_long", "va_alg")]

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/combine-studies-adhoc/temp/study-char_clean-col.csv",sep =""), row.names = FALSE)
