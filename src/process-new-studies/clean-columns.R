################################################################################
#' @description Clean column names, create id var, drop non-modeled countries, keep age-specific studies
#' @return Age-specific studies
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## New studies from 2000-2023 systematic review in long format
dat_filename <- list.files("./data/study-data")
dat_filename <- dat_filename[grepl("cod long_before exclude", dat_filename, ignore.case = TRUE)]
dat <- read.csv(paste0("./data/study-data/", dat_filename, sep = ""))
## Key with model classification for hmm/lmm countries
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("modelclass", dat_filename, ignore.case = TRUE)]
key <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Remove special characters from strata id
dat$strata_id <- sapply(dat$strata_id, function(x) gsub("[^[:alnum:]\\s]", "-", x))

# Update variable names
names(dat)[names(dat) == 'strata_gender'] <- 'sex'
names(dat)[names(dat) == 'study_location'] <- 'location'
names(dat)[names(dat) == 'totdeaths_orig'] <- 'totdeaths'
rownames(dat) <- NULL

# Re-label sexes
dat$sex[dat$sex == 1] <- sexLabels[3]
dat$sex[dat$sex == 2] <- sexLabels[2]
dat$sex[dat$sex == 3] <- sexLabels[1]

# Create year variable is middle year of study
dat$year <- floor(dat$year_start + (dat$year_end - dat$year_start)/2)

# Recode "mutliple"
dat$iso3[dat$iso3 == "Mutliple"] <- "Multiple"

# Create id
dat$id <- paste(dat$iso3, dat$year, dat$Refid, dat$strata_id,
                round(dat$age_lb), round(dat$age_ub), substr(dat$sex,1,1), sep = '-')
dat <- dat[,c("id", names(dat)[!(names(dat) %in% "id")])]
length(unique(dat$id))
nrow(dat)

# Drop rows that are (1) missing cod_n due to missing denominators or (2) facility studies
dat <- subset(dat, !(remove == 1))

# Drop study still missing cod_n
dat <- subset(dat, !is.na(cod_n))

# REMOVE data points from COUNTRIES NOT VA
v_keep <- subset(key, Group %in% c('HMM/LMM', 'HMM'))$ISO3
dat <- subset(dat, iso3 %in% v_keep)

# Keep age/sex group of interest
if(ageSexSuffix == "00to28d"){dat <- subset(dat, inc_neo == 1)}
if(ageSexSuffix == "01to59m"){dat <- subset(dat, inc_1to59 == 1)}
if(ageSexSuffix == "05to09y"){dat <- subset(dat, inc_5to9y == 1)}
if(ageSexSuffix == "10to14y"){dat <- subset(dat, inc_10to14y == 1)}
if(ageSexSuffix == "15to19yF"){dat <- subset(dat, inc_15to19y.f == 1)}
if(ageSexSuffix == "15to19yM"){dat <- subset(dat, inc_15to19y.m == 1)}

# Tidy
# Remove unnecessary columns
v_delete <- c("Facility", "remove", "country","inc_neo", "inc_1to59","inc_5to9y", "inc_10to14y", "inc_15to19y.f", "inc_15to19y.m",
              "cause_number", "cod_p", "cod_mr", "cod_mro")
dat <- dat[!(names(dat) %in% v_delete)]
dat <- dat[order(dat$id),]

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/process-new-studies/temp/study-causes_clean-col_",ageSexSuffix,".csv",sep =""), row.names = FALSE)

