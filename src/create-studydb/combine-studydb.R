################################################################################
#' @description Combine study databases
#' @return study database with old and new studies, CODs, and covariates
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/set-inputs.R")
## New study database
new <- read.csv(paste("./gen/process-new-studies/output/Studies2023_", ageSexSuffix,".csv", sep = ""))
## Old study database with updated covariates
old <- read.csv(paste0("./gen/update-old-studies/output/Studies2019-UpdatedCovar_", ageSexSuffix,".csv", sep = ""))
################################################################################

# Add round variable
old$round <- "2000-2019"
new$round <- "2000-2023"

# Check that old and new have the same column names
# Columns in old that are not in new
names(old)[!(names(old) %in% names(new))]
# Columns in new that are not in old
names(new)[!(names(new) %in% names(old))]

# Update recnr in new studies
new$recnr <- max(old$recnr) + new$recnr 

# Combine
dat <- rbind(old, new)
dat <- dat %>%
  select(round, everything())

# Check id is unique
if(!(nrow(dat) == length(unique(dat$id)))){
  warning("Informative ID is not unique")
}

# Check that whatever variable has been re-coded as strata_id is unique
if(!(nrow(dat) == length(unique(dat$strata_id)))){
  warning("Variable recoded as strata_id is not unique")
}

# Save outputs ------------------------------------------------------------

write.csv(dat, paste0("./gen/create-studydb/temp/studies-combined_",ageSexSuffix,".csv"), row.names = FALSE)

