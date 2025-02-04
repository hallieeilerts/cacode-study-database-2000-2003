################################################################################
#' @description  Clean columns in distiller study COD data
#' @return Study COD columns with age bounds
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readxl)
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Studies and their CODs from 2000-2023 systematic review
dat_filename <- list.files("./data/study-data")
dat_filename <- dat_filename[grepl("Distiller_StudyCOD", dat_filename, ignore.case = TRUE)]
dat <- read_excel(paste0("./data/study-data/", dat_filename, sep = ""))
################################################################################

# Create strata_id and remove special character
v_col <- names(dat)
dat$strata_id <- sapply(dat$deaths_by_cause_k, function(x) gsub("[^[:alnum:]\\s]", "-", x))
dat <- dat[,c("strata_id", v_col)]

# Rename columns
dat <- dat %>%
  rename(ref_id = Refid,
         year_start = strata_year_srt, 
         year_end = strata_year_end, 
         sex = strata_gender,
         age_lb = strata_age_lb,
         age_ub = strata_age_ub
  ) %>%
  mutate(year_mid = floor(year_start + (year_end-year_start)/2),
         age_lb_unit = age_unit,
         age_ub_unit = age_unit)  %>%
  select(-age_unit)


# Delete other unnecessary columns
dat <- dat %>%
  select(-c(User, Level, deaths_by_cause_k, study_id, strata_ur, cod_loc_tab, cod_loc_pag))


# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/combine-studies-adhoc/temp/study-cod_clean-col.csv",sep =""), row.names = FALSE)


