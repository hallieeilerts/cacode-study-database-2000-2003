################################################################################
#' @description Merge on study-level pfpr
#' @return Study data points with covariates in long format, including study-level pfpr
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Long form data with identifying columns for studies and DHS subnational covar merged where possible
dat <- read.csv(paste0("./gen/process-new-studies/temp/studycovar_subnat_", ageSexSuffix, ".csv"))
## PFPR data from MAP
dat_filename <- list.files("./data/study-pfpr")
dat_filename <- dat_filename[grepl("pfpr_map", dat_filename, ignore.case = TRUE)]
pfpr <- read.csv(paste0("./data/study-pfpr/", dat_filename, sep = ""))
################################################################################

df_pfpr <- pfpr[,c("strata_id", "PfPR_rmean")]
df_pfpr$strata_id <- sapply(df_pfpr$strata_id, function(x) gsub("[^[:alnum:]\\s]", "-", x))
names(df_pfpr)[which(names(df_pfpr) == "PfPR_rmean")] <- "pfpr"
df_pfpr$variable <- "pfpr"

dat <- dat %>%
  left_join(df_pfpr, by = c("strata_id", "variable"))

dat$source[!is.na(dat$pfpr)] <- "MAP20250205_StudyLevel"
dat$value[!is.na(dat$pfpr)] <- dat$pfpr[!is.na(dat$pfpr)]

# Save output -------------------------------------------------------------

write.csv(datLong , paste0("./gen/process-new-studies/temp/studycovar_subnat-pfpr_", ageSexSuffix, ".csv"), row.names = FALSE)

