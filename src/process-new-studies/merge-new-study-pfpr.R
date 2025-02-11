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

df_pfpr <- pfpr
df_pfpr$strata_id <- sapply(df_pfpr$strata_id, function(x) gsub("[^[:alnum:]\\s]", "-", x))

# Check duplicates and take smaller administrative area
df_pfpr <- df_pfpr %>% 
  group_by(strata_id) %>%
  mutate(n = n(),
         nAdmin = uniqueN(ADMIN)) %>% 
  arrange(strata_id, ADMIN) %>% 
  mutate(nrec = 1:n(),
         maxrec = max(nrec),
         exclude = ifelse(n> 1 & nrec != maxrec, 1, 0)) %>% 
  filter(exclude == 0) %>%
  select(strata_id, PfPR_rmean) %>%
  rename(pfpr = PfPR_rmean) %>%
  mutate(variable = "pfpr")

dat <- dat %>%
  left_join(df_pfpr, by = c("strata_id", "variable"))

dat$source[!is.na(dat$pfpr)] <- "MAP20250205_StudyLevel"
dat$value[!is.na(dat$pfpr)] <- dat$pfpr[!is.na(dat$pfpr)]
dat$pfpr <- NULL

# Save output -------------------------------------------------------------

write.csv(dat , paste0("./gen/process-new-studies/temp/studycovar_subnat-pfpr_", ageSexSuffix, ".csv"), row.names = FALSE)

