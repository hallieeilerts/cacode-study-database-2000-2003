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
         sex = strata_gender
  ) %>%
  mutate(year_mid = floor(year_start + (year_end-year_start)/2))

# Re-label sexes
dat$sex[dat$sex == 1] <- sexLabels[3]
dat$sex[dat$sex == 2] <- sexLabels[2]
dat$sex[dat$sex == 3] <- sexLabels[1]

# Generate age variables in months
dat <- dat %>%
  rename(age_lb = strata_age_lb, 
         age_ub = strata_age_ub) %>%
  mutate(age_lb = as.numeric(age_lb), 
         age_ub = as.numeric(age_ub)) %>%
  mutate(age_lb_m = case_when(
            age_unit == "months" ~ age_lb,      # Already in months
            age_unit == "days" ~ age_lb / 30,   # Converting days to months
            age_unit == "years" ~ age_lb * 12), # Converting years to months
        age_ub_m = case_when(
            age_unit == "months" ~ age_ub,       # Already in months
            age_unit == "days" ~ age_ub / 30,    # Converting days to months
            age_unit == "years" ~ age_ub * 12)   # Converting years to months
  ) %>%
  select(-c(age_unit, age_lb, age_ub))

# Check that age_lb_m is always less than age_ub_m
if(nrow(subset(dat, age_lb_m > age_ub_m))>0){
  warning("age_lb_m is greater than age_ub_m")
}
# Check no negative ages
if(nrow(subset(dat, age_lb_m < 0))>0){
  warning("negative age_lb_m")
}
if(nrow(subset(dat, age_ub_m < 0))>0){
  warning("negative age_ub_m")
}

# Delete other unnecessary columns
dat <- dat %>%
  select(-c(User, Level, deaths_by_cause_k, study_id, strata_ur, cod_loc_tab, cod_loc_pag))


# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/process-new-studies/temp/study-cod_clean-col.csv",sep =""), row.names = FALSE)


