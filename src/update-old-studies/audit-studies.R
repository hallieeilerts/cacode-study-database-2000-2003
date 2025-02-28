################################################################################
#' @description 
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readxl)
require(tidyverse)
require(data.table)
#' Inputs
source("./src/set-inputs.R")
## All studies from 2000-2019
studies <- read.csv(paste0("./gen/update-old-studies/temp/studies_cod-agg_", ageSexSuffix, ".csv"))
## Studies from 2000-2019 with some data points excluded
dat_incl <- read.csv(paste0("./gen/update-old-studies/temp/studies_exc_", ageSexSuffix, ".csv"))
## Excluded studies from 2000-2019 with some data points excluded
dat_filename <- list.files("./gen/update-old-studies/audit")
dat_filename <- dat_filename[grepl("dat_exc", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
dat_exc <- read.csv(paste0("./gen/update-old-studies/audit/", dat_filename, sep = ""))
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
key <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Reclassified CODs for this age group (includes Other and Undetermined)
v_cod_reclass <- unique(subset(key, !is.na(cod_reclass))$cod_reclass)
# Exclude "TB" which has been redistributed (only present in 5-9y and 10-14y reclass vector)
v_cod_reclass <- v_cod_reclass[!(v_cod_reclass %in% "TB")]

v_id <- c("recnr", "id", "ref_id", "article_id","totdeaths","exclude_reason")

dat_incl$exclude_reason <- NA
dat_incl <- dat_incl[c(v_id, v_cod_reclass)]
dat_exc_abbrev <- dat_exc
dat_exc_abbrev <- dat_exc_abbrev[c(v_id, v_cod_reclass )]

all <- bind_rows(dat_incl, dat_exc_abbrev)

all$n_old <- nrow(studies)
all$included <- ifelse(is.na(all$exclude_reason), 1, 0)
all$excluded <- ifelse(!is.na(all$exclude_reason), 1, 0)
all$n_new <- sum(all$included)
all$n_exc <- sum(all$excluded)
all <- all[order(all$id),]

# Save output(s) ----------------------------------------------------------

write.csv(all, paste0("./gen/update-old-studies/audit/StudyDatabaseChanges_",ageSexSuffix,"_", resDate,".csv"), row.names = FALSE)



