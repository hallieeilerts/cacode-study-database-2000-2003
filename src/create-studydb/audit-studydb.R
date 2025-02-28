################################################################################
#' @description Audit of number of data points in study db
#' @return
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Combined study db
dat_filename <- list.files("./gen/create-studydb/output")
dat_filename <- dat_filename[grepl("studydatabase2023", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[!grepl("codebook", dat_filename, ignore.case = TRUE)] 
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- dat_filename[!(grepl("noMal", dat_filename, ignore.case = TRUE))] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
dat <- read.csv(paste0("./gen/create-studydb/output/", dat_filename, sep = ""))
## Audit for new studies
dat_filename <- list.files("./gen/process-new-studies/audit")
dat_filename <- dat_filename[grepl("studydatabasechanges", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
auditNew <- read.csv(paste0("./gen/process-new-studies/audit/", dat_filename, sep = ""))
## Audit for old studies
if(ageSexSuffix %in% c("00to28d","01to59m")){
  dat_filename <- list.files("./gen/update-old-studies/audit")
  dat_filename <- dat_filename[grepl("studydatabasechanges", dat_filename, ignore.case = TRUE)]
  dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
  dat_filename <- tail(sort(dat_filename),1) # Most recent
  auditOldU5 <- read.csv(paste0("./gen/update-old-studies/audit/", dat_filename, sep = ""))
}
## Audit for duplicates between new and old
dat_filename <- list.files("./gen/create-studydb/temp")
dat_filename <- dat_filename[grepl("dat_exc", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
auditBw <- read.csv(paste0("./gen/create-studydb/temp/", dat_filename, sep = ""))
################################################################################


exctab <- auditNew %>%
  filter(excluded == 1) %>%
  group_by(exclude_reason) %>%
  summarise(n = n())

if(ageSexSuffix %in% c("00to28d","01to59m")){
  
  num2019_exc <- auditOldU5 %>%
    filter(excluded == 1) %>%
    nrow()
  
  exctab2019 <- auditOldU5  %>%
    filter(excluded == 1) %>%
    group_by(exclude_reason) %>%
    summarise(n = n()) %>%
    mutate(round = "2000-2019") %>%
    relocate(round)
  exctab <- exctab %>% 
    mutate(round = "2000-2023") %>%
    relocate(round)
  exctab <- rbind(exctab2019, exctab)
}
if(nrow(auditBw) > 0){
  extabBw <- auditBw %>% 
    group_by(exclude_reason) %>%
    summarise(n = n()) %>% 
    mutate(round = "2000-2023") %>%
    relocate(round)
  exctab <- rbind(exctab, extabBw)
}


num2019 <- dat %>%
  filter(round == "2000-2019") %>%
  nrow

num2023 <- dat %>%
  filter(round == "2000-2023") %>%
  nrow

num2023_exc <- auditNew %>%
  filter(excluded == 1) %>%
  nrow()
if(nrow(auditBw) > 0){
  num2023_exc <- num2023_exc + nrow(auditBw)
}

num_rows <- nrow(dat)


# Create an audit message
audit_message <- paste(
  "Study Database 2023 Audit Report",
  "-----------------",
  paste("Age group:", ageSexSuffix),
  paste("Number of observations:", num_rows),
  paste("Number of 2000-2019 obs:", num2019),
  paste("Number of 2000-2023 obs:", num2023),
  paste("Number of 2000-2023 obs excluded:", num2023_exc),
  sep = "\n"
)
if(ageSexSuffix %in% c("00to28d","01to59m")){
  audit_message <- paste(
    "Study Database 2023 Audit Report",
    "-----------------",
    paste("Age group:", ageSexSuffix),
    paste("Total number of data points:", num_rows),
    paste("Number from 2000-2019:", num2019),
    paste("Number from 2000-2019 excluded:", num2019_exc),
    paste("Number from 2000-2023:", num2023),
    paste("Number from 2000-2023 excluded:", num2023_exc),
    sep = "\n"
  )
}


dat_filename <- list.files("./gen/create-studydb/output")
dat_filename <- dat_filename[grepl("studydatabase2023", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[!grepl("codebook", dat_filename, ignore.case = TRUE)] 
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- dat_filename[!(grepl("noMal", dat_filename, ignore.case = TRUE))] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
studydbver_audited <- sub("^[^_]+_[^_]+_([^.]*)\\.csv$", "\\1", dat_filename)
audit_file <- paste0("./gen/create-studydb/audit/StudyDatabase2023Audit_",ageSexSuffix,"_", studydbver_audited,".txt")

# Save output(s) ----------------------------------------------------------

writeLines(audit_message, audit_file)
write.table(exctab, audit_file, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t", append = TRUE)
