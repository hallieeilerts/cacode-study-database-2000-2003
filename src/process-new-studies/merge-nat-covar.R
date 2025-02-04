################################################################################
#' @description Merge on national-level covariates for new studies and save
#' @return New studies with all covariates
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Study data with too small and too large data points excluded
studies <- read.csv(paste0("./gen/process-new-studies/temp/studies_exc-size_", ageSexSuffix, ".csv", sep = ""))
## Long form data with identifying columns for studies and DHS subnational covar merged where possible
studycovar <- read.csv(paste0("./gen/process-new-studies/temp/studycovar_subnat_", ageSexSuffix, ".csv", sep = ""))
## Prediction Database
dat_filename <- list.files("./data/prediction-database")
dat_filename <- dat_filename[grepl("long", dat_filename, ignore.case = TRUE)]
pred <- read.csv(paste0("./data/prediction-database/", dat_filename, sep = ""))
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_cod <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Reclassified CODs for this age group (includes Other and Undetermined)
df_reclass <- subset(key_cod, !is.na(cod_reclass))
# Exclude "TB" which has been redistributed (only present in 5-9y and 10-14y)
df_reclass <- subset(df_reclass, cod_reclass != "TB")
# Exclude "Undetermined" which is used to eliminate studies in cleaning phase
df_reclass <- subset(df_reclass, cod_reclass != "Undetermined")
v_cod <- sort(unique(df_reclass$cod_reclass))

# Subset columns of interest in prediction database
df_pred <- pred[,c("variable","iso3","year","value_main")]
v_cov_pred <- unique(pred$variable)

# Subset study/covar for which there were study provided values, or DHS subnational values
df_match <- subset(studycovar, !is.na(value))
# Subset those missing covar values
df_miss <- subset(studycovar, is.na(value))

# Match studies missing covariates with prediction database
df_miss <- merge(df_miss, df_pred, by.x = c("iso3","year_mid","variable"), by.y = c("iso3","year","variable"), all.x = TRUE)
df_miss$value <- df_miss$value_main
df_miss$value_main <- NULL
# Make source column
df_miss$source <- paste("PredDB2023", df_miss$iso3, df_miss$year_mid, sep = "_")

# Recombine study covariates
df_all <- rbind(df_miss, df_match)

# Only keep study covariates if they are in the prediction database
# I included a few extra ones in the DHS data extraction
df_all <- subset(df_all, variable %in% v_cov_pred)
df_all <- df_all[,c("strata_id","variable","value","source")]
df_all <- df_all[order(df_all$strata_id, df_all$variable),]

# Check if any covariate values are missing
if(nrow(subset(df_all, is.na(value))) > 0){
  warning("covariate values are missing")
}
if(nrow(subset(df_all, is.na(source))) > 0){
  warning("covariate sources are missing")
}

# Reshape covariate values wide
wideVal <- df_all %>%
  pivot_wider(
    id_cols = strata_id,
    names_from = variable,
    values_from = value,
  )
# Create value name vector without "strata_id"
v_valNames <- names(wideVal)[-1]
# Reshape covariate source columns wide
wideSrc <- df_all %>%
  pivot_wider(
    id_cols = strata_id,
    names_from = variable,
    values_from = source,
  )
# Add source suffix to names vector
v_srcnames <- paste(names(wideSrc),"_source",sep="")
# Remove source suffix from "strata_id"
v_srcnames[1] <- "strata_id"
# Use names vector to rename the data frame
names(wideSrc) <- v_srcnames
# Remove "strata_id" from names vector
v_srcnames <- v_srcnames[-1]

# Merge covariates and source values
df_covar_wide <- merge(wideVal, wideSrc, by = "strata_id")
df_covar_wide <- df_covar_wide[,c("strata_id", sort(c(v_valNames, v_srcnames)))]

# Merge back all original study data columns
result <- merge(studies, df_covar_wide, by = "strata_id", all.x = TRUE)

# Tidy
result <- result[order(result$recnr),]
v_other_col <- names(result)[!(names(result) %in% c(idVars, v_cod))]
result <- result[,c(idVars, v_cod, v_other_col)]

# Save output(s) ----------------------------------------------------------

write.csv(result, paste("./gen/process-new-studies/output/Studies2023_", ageSexSuffix,".csv", sep = ""), row.names = FALSE)
