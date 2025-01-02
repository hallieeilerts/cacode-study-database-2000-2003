################################################################################
#' @description Create codebook for StudyDatabase2023
#' @return
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/set-inputs.R")
## Study db
dat_filename <- list.files("./gen/create-studydb/output")
dat_filename <- dat_filename[grepl("studydatabase2023", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[!grepl("codebook", dat_filename, ignore.case = TRUE)] 
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
dat <- read.csv(paste0("./gen/create-studydb/output/", dat_filename, sep = ""))
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_cod <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
## Prediction database codebook
dat_filename <- list.files("./data/prediction-database")
dat_filename <- dat_filename[grepl("codebookbyvar", dat_filename, ignore.case = TRUE)]
pred_cb <- read.csv(paste0("./data/prediction-database/", dat_filename, sep = ""))
################################################################################

dat <- data.frame(recnr = 1:length(names(dat)),
                  variable = names(dat))
dat <- merge(dat, pred_cb[,c("variable","label","scale")], by = "variable", all.x = TRUE)
names(dat)[which(names(dat) == "label")] <- "definition"
dat <- dat[order(dat$recnr),]

dat$definition[dat$variable %in% key_cod$cod_reclass] <- "Cause of death"
dat$scale[dat$variable %in% key_cod$cod_reclass] <- "integer"
dat$definition[grepl("source", dat$variable)] <- "Source of covariate"
dat$scale[grepl("source", dat$variable)] <- "free_text"

# Manually define id variables
dat$definition[dat$variable == "round"] <- "Denotes whether the data point is from the study database originally used to produce the 2000-2019 estimates or the recently conducted 2000-2023 systematic review."
dat$scale[dat$variable == "round"] <- "string"

dat$definition[dat$variable == "recnr"] <- "Count of data points in file. Unique identifier."
dat$scale[dat$variable == "recnr"] <- "integer"

dat$definition[dat$variable == "id"] <- "Informative ID created from iso3, strata_id, location factor, age, sex. 5-19 data points from 2000-2019 have a slightly different format."
dat$scale[dat$variable == "id"] <- "string"

dat$definition[dat$variable == "ref_id"] <- "In new studies, this is an integer that identifies each article. In old studies for 5-19, it was a integer to denote a study, or INDEPTH, MDS, MDG, COMSA, PAKVASA, LMM for ad-hoc/VR data. It did not exist in old under-5 studies and was recoded with study_id. "
dat$scale[dat$variable == "ref_id"] <- "string"

dat$definition[dat$variable == "article_id"] <- "In new studies, begins with R2022. In old studies it was recoded from study_id, and begins with R for articles, integers, or an abbreviation to identify ad-hoc (VASA, HDSS CentreId)."
dat$scale[dat$variable == "article_id"] <- "string "

dat$definition[dat$variable == "citation"] <- "Article citation for data point. Does not exist in the new studies."
dat$scale[dat$variable == "citation"] <- "free_text"

dat$definition[dat$variable == "author"] <- "Author citation for data point. Does not exist in the new studies."
dat$scale[dat$variable == "author"] <- "free_text"

dat$definition[dat$variable == "strata_id"] <- "Unique identifier for each data point. In the new studies, this is the article_id plus a number denoting the strata. It did not exist in the old studies but was created from study_id and sid."
dat$scale[dat$variable == "strata_id"] <- "numeric"

dat$definition[dat$variable == "strata_other1"] <- "Additional strata information about the data point."
dat$scale[dat$variable == "strata_other1"] <- "free_text"

dat$definition[dat$variable == "location_long"] <- "Long location name from study data extraction."
dat$scale[dat$variable == "location_long"] <- "free_text"

dat$definition[dat$variable == "location_short"] <- "Short location name - no more than 5 words"
dat$scale[dat$variable == "location_short"] <- "free_text"

dat$definition[dat$variable == "location_fac"] <- "Factor conversion of location_short with first alphabetical name being reference category and value of 1. For the old 5-19y years, nationwide was the reference category, and the factor conversion took place after dropping different studies for different age groups. So the same study in difference age-specific study databases will not necessarily have the same factor value."
dat$scale[dat$variable == "location_fac"] <- "integer"

dat$definition[dat$variable == "nationalrep"] <- "Indicator for if the study is nationally representative."
dat$scale[dat$variable == "nationalrep"] <- "binary"

dat$definition[dat$variable == "reterm"] <- "If the study is nationally representative, reterm will be the iso3 code. Otherwise, it has additional characters."
dat$scale[dat$variable == "reterm"] <- "string"

dat$definition[dat$variable == "va_alg"] <- "Verbal autopsy algorithm used in the study."
dat$scale[dat$variable == "va_alg"] <- "string"

dat$definition[dat$variable == "va_mult_ind"] <- "Indicator for whether the study data point is reported multiple times with different VA algorithms."
dat$scale[dat$variable == "va_mult_ind"] <- "binary"

dat$definition[dat$variable == "va_mult_id"] <- "ID for study data points that are reported multiple times with different VA algorithms. Created from article_id, iso3, year_start, year_end, sex, total deaths."
dat$scale[dat$variable == "va_mult_id"] <- "string"

dat$definition[dat$variable == "va_mult_n"] <- "Count of records by va_mult_ind"
dat$scale[dat$variable == "va_mult_n"] <- "integer"

dat$definition[dat$variable == "iso3"] <- "ISO3 code for country in which study took place"
dat$scale[dat$variable == "iso3"] <- "string"

dat$definition[dat$variable == "countryname"] <- "Name of country in which study took place"
dat$scale[dat$variable == "countryname"] <- "string"

dat$definition[dat$variable == "sex"] <- "Sex of population of study data point"
dat$scale[dat$variable == "sex"] <- "string"

dat$definition[dat$variable == "age_lb_m"] <- "Age lower bound in months of study datapoint"
dat$scale[dat$variable == "age_lb_m"] <- "numeric"

dat$definition[dat$variable == "age_ub_m"] <- "Age upper bound in months of study datapoint"
dat$scale[dat$variable == "age_ub_m"] <- "numeric"

dat$definition[dat$variable == "year_start"] <- "Starting year of study data point."
dat$scale[dat$variable == "year_start"] <- "integer"

dat$definition[dat$variable == "year_end"] <- "Ending year of study data point."
dat$scale[dat$variable == "year_end"] <- "integer"

dat$definition[dat$variable == "year_mid"] <- "Middle year of study data point. floor(year_start + (year_end - year_start)/2)"
dat$scale[dat$variable == "year_mid"] <- "integer"

dat$definition[dat$variable == "totdeaths"] <- "Total deaths reported in the study data point"
dat$scale[dat$variable == "totdeaths"] <- "integer"

dat <- dat[,c("recnr","variable","scale","definition")]

# Check if any definitions missing
if(nrow(subset(dat, is.na(definition))) != 0){
  warning("codebook incomplete")
}

# Save outputs ------------------------------------------------------------

write.csv(dat, paste0("./gen/create-studydb/output/StudyDatabase2023_Codebook_",ageSexSuffix,"_",format(Sys.Date(), format="%Y%m%d"),".csv"), row.names = FALSE)

