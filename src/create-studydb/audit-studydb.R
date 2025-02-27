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
## Study db
dat_filename <- list.files("./gen/create-studydb/output")
dat_filename <- dat_filename[grepl("studydatabase2023", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[!grepl("codebook", dat_filename, ignore.case = TRUE)] 
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- dat_filename[!(grepl("noMal", dat_filename, ignore.case = TRUE))] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
dat <- read.csv(paste0("./gen/create-studydb/output/", dat_filename, sep = ""))
## Previous audit that shows data points excluded this round
dat_filename <- list.files("./gen/process-new-studies/audit")
dat_filename <- dat_filename[grepl("studydatabasechanges", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
changes <- read.csv(paste0("./gen/process-new-studies/audit/", dat_filename, sep = ""))
################################################################################


num2023_exctab <- changes %>%
  filter(excluded == 1) %>%
  group_by(exclude_reason) %>%
  summarise(n = n())

num2019 <- dat %>%
  filter(round == "2000-2019") %>%
  nrow

num2023 <- dat %>%
  filter(round == "2000-2023") %>%
  nrow

num2023_exc <- changes %>%
  filter(excluded == 1) %>%
  nrow()

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
write.table(num2023_exctab, audit_file, row.names = FALSE, col.names = TRUE, quote = FALSE, sep = "\t", append = TRUE)

