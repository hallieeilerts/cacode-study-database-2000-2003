################################################################################
#' @description Merge on study-level pfpr for old 5-19y studies. 
#' In previous round, 5-19y studies had national-level pfpr. 
#' We requested study-level pfpr for old 5-19y studies from MAP for 2000-2023 round. 
#' (Also requested study-level pfpr for all new studies 0-19y)
#' Old 1-59m studies already had study-level pfpr. 
#' Old neonate studies didn't, but they don't use pfpr as a covariate. 
#' The pfpr covariate merged onto old neonatal studies is national.
#' @return Study data points for 5-19y with covariates in long format, including study-level pfpr
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
# Old 1-59m already had study-level pfpr. 
# old neonate study database did not include pfpr.
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF","15to19yM")){ 
  dat <- read.csv(paste0("./gen/update-old-studies/temp/studies-long_upd-names-scales_", ageSexSuffix, ".csv"))
}
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

write.csv(dat , paste0("./gen/update-old-studies/temp/studies-long_merge-pfpr_", ageSexSuffix, ".csv"), row.names = FALSE)

