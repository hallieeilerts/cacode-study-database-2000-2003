################################################################################
#' @description Old and new studies with no duplicates
#' @return Final version of study database
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/set-inputs.R")
# Combined old and new studies
dat <- read.csv(paste0("./gen/create-studydb/temp/studies-combined_dup-drop_", ageSexSuffix,".csv", sep = ""))
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_cod <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Reclassified CODs for this age group (includes Other and Undetermined)
df_reclass <- subset(key_cod, !is.na(cod_reclass))
# Exclude "TB" which has been redistributed (only present in key for 5-9y and 10-14y)
df_reclass <- subset(df_reclass, cod_reclass != "TB")
# Exclude "Undetermined" which is used to eliminate studies in cleaning phase
df_reclass <- subset(df_reclass, cod_reclass != "Undetermined")
v_cod <- sort(unique(df_reclass$cod_reclass))

# Check that totdeaths is equal to sum of causes
totDif <- which(dat$totdeaths != apply(dat[, paste0(v_cod)], 1, sum, na.rm = T))
if(length(totDif)>0){
  warning("Sum of causes does not equal totdeaths.")
}

# Check no negative CODs
codNeg <- apply(dat[, paste0(v_cod)], 2, function(x) x < 0)
codNeg <- as.data.frame(cbind(dat[,c("strata_id")], codNeg))
codNeg <- codNeg %>%
  filter(if_any(everything(), ~ . == TRUE))
if(nrow(codNeg)>0){
  warning("Negative value for COD.")
}

# Check if all COD values are either NA or whole numbers
codWhole <- dat %>%
  mutate(any_not_whole = if_any(all_of(v_cod), ~ !is.na(.) & (. %% 1 != 0))) %>%
  filter(any_not_whole == TRUE)
if(nrow(codWhole)>0){
  warning("Non-whole number COD values.")
}

# Save outputs ------------------------------------------------------------

write.csv(dat, paste0("./gen/create-studydb/output/StudyDatabase2023_",ageSexSuffix,"_",format(Sys.Date(), format="%Y%m%d"),".csv"), row.names = FALSE)

# Notes:
# -CODs should be all whole numbers
# -CODs can have NA in them
# -Some study data points are for the same population with different VA algorithms applied.
# subset(dat, va_mult_ind == 1)
