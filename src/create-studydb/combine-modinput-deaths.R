################################################################################
#' @description Combine model inputs for deaths
#' @return deaths model input with old and new study studies
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/set-inputs.R")
## New model input for deaths
load(paste("./gen/process-new-studies/output/ModInput2023-NewDeaths_", ageSexSuffix,".RData", sep = ""))
new <- deaths
## Old model input for deaths
load(paste("./gen/update-old-studies/output/ModInput2019-Deaths_", ageSexSuffix,".RData", sep = ""))
old <- deaths
rm(deaths)
# Updated recnr for old model input for deaths
old_recnr <- read.csv(paste0("./gen/update-old-studies/temp/studies_set-id_", ageSexSuffix, ".csv"))
old_recnr <- old_recnr[,c("recnr","id")]
################################################################################

# Merge on recnr for old studies
# This was created in update-old-studies pipeline
# It's similar to sid, but slightly different due to how lmm data was incoporated
df_old <- merge(old, old_recnr, by = "id")
nrow(old) == nrow(df_old)
df_old$sid <- NULL
old <- df_old

# Update recnr in new studies
new$recnr <- new$recnr + max(old$recnr)

# Combine
deaths <- rbind(old, new)

# Update class
deaths$sex <- as.factor(deaths$sex)

# Tidy
deaths <- deaths %>%
  select(recnr, everything()) %>%
  arrange(recnr, cause)

# Save output(s) ----------------------------------------------------------

save(deaths, file = paste("./gen/create-studydb/output/ModInput2023-HMM-Deaths_",ageSexSuffix,"_",resDate,".RData", sep = ""))

