################################################################################
#' @description Create covariate replacement key
#' @return Key with study data point id and covariate that should be updated with a new national prediction database value
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readstata13)
library(tidyverse)
library(data.table)
#' Inputs
source("./src/set-inputs.R")
## Old age-specific study database in long format that now has covariate names and scales as pred database
dat <- read.csv(paste0("./gen/update-old-studies/temp/studies-long_upd-names-scales_", ageSexSuffix, ".csv"))
## Covariate data extraction from DHS
dat_filename <- list.files("./data/dhs")
dat_filename <- dat_filename[grepl("long", dat_filename, ignore.case = TRUE)]
dhs <- read.csv(paste0("./data/dhs/", dat_filename, sep = ""))
## Key with matched study and prediction covariate names and scales
dat_filename <- list.files("./gen/update-old-studies/output")
dat_filename <- dat_filename[grepl("covariatekey", dat_filename, ignore.case = TRUE)] 
dat_filename <- tail(sort(dat_filename), 1) # most recent
key_covar <- read.csv(paste0("./gen/update-old-studies/output/", dat_filename, sep = ""))
################################################################################

# Identify covariates that came from DHS survey and are subnational
v_dhs_covar <- unique(dhs$variable)
# If needed, manually add covariates that have different names in dhs extraction
# Make dhs extraction match prediction database. Prediction database should take precedent.
# No additions needed for now.

# Manually add covariates that aren't in dhs extraction this time (because not included in any model),
# but likely were pulled from dhs subnational in the past
v_dhs_covar <- c(v_dhs_covar, "imr_mf")

# Covariates from India prediction database that are likely subnational
v_predInd_covar <- subset(key_covar, !is.na(predInd))$pred
v_predInd_covar <- v_predInd_covar[!is.na(v_predInd_covar)]
v_predInd_covar <- v_predInd_covar[v_predInd_covar %in% v_dhs_covar]

# Assign source to study covariates 
# Article and survey data points do not need to be updated
df_src <- dat %>%
  mutate(source_type = ifelse(grepl("article", source, ignore.case = TRUE), "Article", NA)) %>%
  mutate(source_type = ifelse(grepl("author", source, ignore.case = TRUE), "Article", source_type)) %>%
  mutate(source_type = ifelse(grepl("same study", source, ignore.case = TRUE), "Article", source_type)) %>%
  mutate(source_type = ifelse(grepl("INDEPTH", source), "Article", source_type)) %>%
  mutate(source_type = ifelse(grepl("HDSS", source), "Article", source_type)) %>%
  mutate(source_type = ifelse(grepl("China Statistics year book", source), "Article", source_type)) %>%
  mutate(source_type = ifelse(grepl("China Health Statisitcs Year Book", source), "Article", source_type)) %>%
  mutate(source_type = ifelse(grepl("Statistics Year Book", source), "Article", source_type)) %>%
  mutate(source_type = ifelse(grepl("China Health statistics year book", source), "Article", source_type)) %>%
  mutate(source_type = ifelse(grepl("National Health Services Survey in China", source), "Article", source_type)) %>%
  mutate(source_type = ifelse(grepl("China health statistics year book", source), "Article", source_type)) %>%
  mutate(source_type = ifelse(grepl("Chinese Statistical Yearbook", source), "Article", source_type)) %>%
  mutate(source_type = ifelse(grepl("Saul's database", source), "Article", source_type)) %>%
  mutate(source_type = ifelse(grepl("MICS", source), "Survey", source_type)) %>%
  mutate(source_type = ifelse(grepl("Multiple Indicator Cluster Survey", source), "Survey", source_type)) %>%
  # If the source_type is not already assigned, the covariate is available in DHS surveys, it's not a Low-mortality modeled (which should come from predDB), and it's not a very old data point (don't want to use very old survey data points. prob better to use prediction database for those); then say it is a survey value that will not get replaced.
  mutate(source_type = ifelse( is.na(source_type) &  variable %in% v_dhs_covar & !(ref_id == "LMM") & year_mid >= 1990, "Survey", source_type)) %>% 
  mutate(source_type = ifelse( is.na(source_type) &  ref_id == "LMM", "Prediction Database", source_type)) %>%
  mutate(source_type = ifelse( grepl("who|unicef|unpd|unesco|owid|barro|wpp|undp|ncdrisc|ncd-risc|ilo|igme|map|noaa|transparency|imputed|interpolation|extrapolation|average", 
                                     source, ignore.case = TRUE), "Prediction Database", source_type)) %>%
  # Get all covariates for nationally representative studies from prediction database
  mutate(source_type = ifelse( source_type == "Survey" &  nationalrep == 1, "Prediction Database", source_type)) %>%  #filter(iso3 == "BFA") %>% View
  mutate(source_type = ifelse( is.na(value) & variable %in% key_covar$pred, "Prediction Database", source_type )) %>%
  mutate(source_type = ifelse( is.na(source_type) & variable %in% key_covar$pred, "Prediction Database", source_type )) %>%
  mutate(source_type = ifelse( source_type != "Article" & 
                                 !grepl("national|jmp|igme|who", source, ignore.case = TRUE) & 
                                 iso3 %in% "IND" &  variable %in% v_predInd_covar & !is.na(value),
                               "India Subnational Prediction Database", source_type )) %>%
  mutate(replace = ifelse(source_type %in% c("Prediction Database"), TRUE, FALSE))

key_replace <- df_src %>%
  filter(replace == TRUE) %>%
  select(id, strata_id, variable)

# Save output -------------------------------------------------------------

write.csv(key_replace, paste0("./gen/update-old-studies/temp/key_covar-replacement_",ageSexSuffix,".csv",sep =""), row.names = FALSE)

