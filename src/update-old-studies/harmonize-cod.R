################################################################################
#' @description Harmonize COD names, exclude Undetermined
#' @return Study CODs aggregated by reclassified categories in long format 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
#' Inputs
source("./src/set-inputs.R")
## Old study databases with under-5 duplicates dropped
if(ageSexSuffix %in% c("00to28d","01to59m")){
  dat <- read.csv(paste0("./gen/update-old-studies/temp/studies_exc_", ageSexSuffix, ".csv", sep = ""))
}
## Old study databases with updated id variables to match new studies
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF", "15to19yM")){
  dat <- read.csv(paste0("./gen/update-old-studies/temp/studies_set-id_", ageSexSuffix, ".csv", sep = ""))
}
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_cod <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Reclassified CODs for this age group (includes Other and Undetermined)
v_cod_reclass <- unique(subset(key_cod, !is.na(cod_reclass))$cod_reclass)
# Exclude "TB" which has been redistributed (only present in 5-9y and 10-14y reclass vector)
v_cod_reclass <- v_cod_reclass[!(v_cod_reclass %in% "TB")]
# Exclude "Undetermined" for certain checks
v_cod_noundt <- v_cod_reclass[!(v_cod_reclass %in% "Undetermined")]

# Rename causes
datCOD <- dat %>% 
  rename_with(
    ~ case_when(
      . == "OtherCD" ~ "OtherCMPN",
      TRUE ~ .)
  ) %>%
  select(all_of(idVars), "totdeaths", all_of(v_cod_reclass))

# Check that all reclassified COD are reported and add as columns if not
v_missing <- v_cod_reclass[!(v_cod_reclass %in% names(datCOD))]
if(length(v_missing) > 0){
  warning("Missing some expected CODs in old study database.")
  for(i in 1:length(v_missing)){
    datCOD$missingCOD <- NA
    names(datCOD)[which(names(datCOD) == "missingCOD")] <- v_missing[i]
  }
}

# Recalculate totdeaths
datCOD$totdeaths <- apply(datCOD[, paste0(v_cod_noundt)], 1, sum, na.rm = T)

# Check that totdeaths is equal to sum of causes
totDif <- which(datCOD$totdeaths != apply(datCOD[, paste0(v_cod_noundt)], 1, sum, na.rm = T))
if(length(totDif)>0){
  warning("Sum of causes does not equal totdeaths.")
  #View(dat[totDif,])
}

# Check no negative CODs
codNeg <- datCOD %>%
  ungroup() %>%
  select(any_of(v_cod_reclass)) %>%
  filter(if_any(everything(), ~ . < 0))
if(nrow(codNeg)>0){
  warning("Negative value for COD.")
}

# Save output -------------------------------------------------------------

# Old model input that now has covariate names and scales as pred database
write.csv(datCOD , paste0("./gen/update-old-studies/temp/cod-wide_harmonized_", ageSexSuffix, ".csv"), row.names = FALSE)

