################################################################################
#' @description Assess excluded new studies and total number of new studies added for each age group
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
## Final included studies from 2000-2023 systematic review
dat_filename <- list.files("./gen/process-new-studies/output/")
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)]
dat_filename <- dat_filename[grepl("studies", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename),1) # Most recent
dat <- read.csv(paste0("./gen/process-new-studies/output/", dat_filename, sep = ""))
## Excluded studies from 2000-2023 systematic review (does not include duplicates which were already dropped)
list_csv_files <- list.files("./gen/process-new-studies/audit/", pattern = "*.csv")
list_csv_files <- list_csv_files[grepl("dat_exc", list_csv_files, ignore.case =TRUE)]
list_csv_files <- list_csv_files[grepl(ageSexSuffix, list_csv_files)]
l_exc <- lapply(list_csv_files, function(x) read.csv(paste0("./gen/process-new-studies/audit/",x,sep="")))
## Old model inputs
if(ageSexSuffix %in% c("00to28d", "01to59m")){
}
if(ageSexSuffix == "05to09y"){
  load("./data/model-inputs-old/20201217-Data5to9-VAMCM009-Test3.RData")
}
if(ageSexSuffix == "10to14y"){
  load("./data/model-inputs-old/20210207-Data15to19Fem-VAMCM009-Test9.RData")
}
if(ageSexSuffix %in% c("15to19yF","15to19yM" )){
  load("./data/model-inputs-old/20210212-Data15to19Men-VAMCM009-Test9e.RData")
}
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
key <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Excluded data frames
# Only keep columns that are in included data
l_exc <- lapply(l_exc, function(x) x[names(x) %in% c(names(dat), "exclude_reason")])
l_exc <- lapply(l_exc, function(x){ x$ref_id <- as.character(x$ref_id); return(x)})
l_exc <- l_exc[lapply(l_exc, nrow) > 0]
dat_exc <- do.call(bind_rows, l_exc)

# Vector of all CODs in correct order
v_cod_reclass  <- unique(subset(key, !is.na(cod_reclass))$cod_reclass)
# Exclude "TB" which has been redistributed (only present in 5-9y and 10-14y reclass vector)
# Exclude "Undetermined" because any deaths attributed to Undetermined have been excluded in exclude-by-size
v_cod_reclass <- v_cod_reclass[!(v_cod_reclass %in% c("TB", "Undetermined"))]
v_id <- c("id", "strata_id", "article_id","totdeaths","exclude_reason")

dat_incl <- dat
dat_incl$exclude_reason <- NA
dat_incl <- dat_incl[c(v_id, v_cod_reclass)]
dat_exc_abbrev <- dat_exc
dat_exc_abbrev <- dat_exc_abbrev[c(v_id, v_cod_reclass )]

all <- bind_rows(dat_incl, dat_exc_abbrev)

if(ageSexSuffix %in% c("00to28d", "01to59m")){
  all$n_old <- NA # Don't have information on old number of studies loaded in at the moment.
}else{
  all$n_old <- nrow(studies)
}

all$included <- ifelse(is.na(all$exclude_reason), 1, 0)
all$excluded <- ifelse(!is.na(all$exclude_reason), 1, 0)
all$n_new <- sum(all$included)
all$n_exc <- sum(all$excluded)
all$perincrease <- all$n_new/all$n_old
all <- all[order(all$id),]

# Save output(s) ----------------------------------------------------------

write.csv(all, paste0("./gen/process-new-studies/audit/StudyDatabaseChanges_",ageSexSuffix,"_", resDate,".csv"), row.names = FALSE)


