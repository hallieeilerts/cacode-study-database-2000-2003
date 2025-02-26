################################################################################
#' @description Extract necessary columns from study citations
#' @return Study info columns for strata_id, ref_id, citation, author
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readxl)
library(stringi)
#' Inputs
source("./src/set-inputs.R")
## Studies and their citations from 2000-2023 systematic review
dat_filename <- list.files("./data/study-data")
dat_filename <- dat_filename[grepl("bibliography", dat_filename, ignore.case = TRUE)]
dat <- read_excel(paste0("./data/study-data/", dat_filename, sep = ""))
################################################################################

# Rename columns
dat <- dat %>%
  rename(citation = Bibliography,
         ref_id = Refid)

# remove accents/special characters before saving to csv
dat$citation <- stri_trans_general(dat$citation, "Latin-ASCII")

# Create author column
dat$author <- gsub("(.+?)(\\,.*)", "\\1", dat$citation)
dat$author[grepl("population-based", dat$author, ignore.case = TRUE)] <- NA

# Keep necessary columns
dat <- dat[,c("ref_id", "citation", "author")]

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/combine-studies-adhoc/temp/study-citations_clean-col.csv",sep =""), row.names = FALSE)
