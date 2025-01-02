################################################################################
#' @description Clean detailed information on study location and where data point is regionally/nationally representative
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readxl)
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Information on study locations and whether study is nationally representative
# Manually put together by Astha
dat_filename <- list.files("./data/study-data")
dat_filename <- dat_filename[grepl("Studies_RegionsRepresentative", dat_filename, ignore.case = TRUE)]
dat <- read_excel(paste0("./data/study-data/", dat_filename, sep = ""))
################################################################################

# Remove special character from strata_id
dat$strata_id <- sapply(dat$strata_id, function(x) gsub("[^[:alnum:]\\s]", "-", x))

# Rename columns
dat <- dat %>%
  rename(ref_id = Refid,
         location_short = `study loc`, 
  ) %>%
  mutate(nationalrep = ifelse(nationalrep == "National", 1, 0)) %>%
  select(c(strata_id, ref_id, location_short, nationalrep))

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/process-new-studies/temp/study-loc_clean-col.csv",sep =""), row.names = FALSE)
