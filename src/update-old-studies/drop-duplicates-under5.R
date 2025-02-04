################################################################################
#' @description Harmonize names for old study id columns with new study data
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
library(stringr)
library(readstata13)
#' Inputs
source("./src/set-inputs.R")
## Old study databases with updated id variables to match new studies
dat <- read.csv(paste0("./gen/update-old-studies/temp/studies_set-id_", ageSexSuffix, ".csv", sep = ""))
################################################################################

# Manual review of data points with same country, year, sex, age, total deaths
df_batch1 <- dat %>%
  mutate(tempid = paste0(iso3, year_start, year_end, sex, age_lb_m, age_ub_m,  totdeaths)) %>%
  group_by(tempid) %>%
  mutate(N = n()) %>%
  filter(N > 1) %>%
  select(tempid, strata_id, everything()) %>%
  arrange(tempid)
# Look at location_long and strata_other1 to see if different. If appear to be a duplicate, check CODs to make final decision.
#View(df_batch1)

# Manual review of data points with same country, year, sex, age, two CODs
# df_batch2 <- dat %>%
#   filter(!(strata_id %in% v_exclude_strataid)) %>%
#   rowwise() %>%
#   mutate(tempid = ifelse(ageSexSuffix == "01to59m", paste0(iso3, year_start, year_end, sex, age_lb_m, age_ub_m, round(meningitis), round(pneumonia)), NA)) %>%
#   mutate(tempid = ifelse(ageSexSuffix == "00to28d", paste0(iso3, year_start, year_end, sex, age_lb_m, age_ub_m, round(preterm), round(pneumonia)), tempid)) %>%
#   group_by(tempid) %>%
#   mutate(N = n()) %>%
#   filter(N > 1) %>%
#   select(tempid, strata_id, everything()) %>%
#   arrange(tempid)
# View(df_batch2)
# Look at location_long and strata_other1 to see if different. If appear to be a duplicate, check CODs to make final decision.

# Manual review of data points with same country, age, two other CODs
# df_batch3 <- dat %>%
#   filter(!(strata_id %in% v_exclude_strataid)) %>%
#   rowwise() %>%
#   mutate(tempid = ifelse(ageSexSuffix == "01to59m", paste0(iso3, year_start, year_end, sex, age_lb_m, age_ub_m, round(injuries), round(pneumonia)), NA)) %>%
#   mutate(tempid = ifelse(ageSexSuffix == "00to28d", paste0(iso3, year_start, year_end, sex, age_lb_m, age_ub_m, round(congenital), round(intrapartum)), tempid)) %>%
#   #mutate(tempid = paste0(iso3, age_lb_m, age_ub_m, round(injuries), round(pneumonia))) %>%
#   group_by(tempid) %>%
#   mutate(N = n()) %>%
#   filter(N > 1) %>%
#   select(tempid, strata_id, everything()) %>%
#   arrange(tempid)
# View(df_batch3)
# Look at years, totdeaths, and location_long to see if this could be a duplicate with some minor discrepancies in these variables.
# If yes, look at other CODs.
# If CODs are similar as well, drop.

# Manual review of data points with same country, age, three other CODs
# df_batch4 <- dat %>%
#   filter(!(strata_id %in% v_exclude_strataid)) %>%
#   #mutate(tempid = paste0(iso3, age_lb_m, age_ub_m, year_end, round(meningitis), round(injuries), round(measles))) %>%
#   rowwise() %>%
#   mutate(tempid = ifelse(ageSexSuffix == "01to59m", paste0(iso3, year_start, year_end, sex, age_lb_m, age_ub_m, round(injuries), round(meningitis), round(measles)), NA)) %>%
#   mutate(tempid = ifelse(ageSexSuffix == "00to28d", paste0(iso3, year_start, year_end, sex, age_lb_m, age_ub_m, round(diarrhoea), round(tetanus), round(sepsis)), tempid)) %>%
#   group_by(tempid) %>%
#   mutate(N = n()) %>%
#   filter(N > 1) %>%
#   select(tempid, strata_id, everything()) %>%
#   arrange(tempid)
# View(df_batch4)
# Look at years, totdeaths, and location_long to see if this could be a duplicate with some minor discrepancies in these variables.
# If yes, look at other CODs.
# If CODs are similar as well, drop.

# Ad-hoc duplicate dropping
if(ageSexSuffix == "00to28d"){
  v_exclude_strataid <- c()
}
if(ageSexSuffix == "01to59m"){
  v_exclude_strataid <- c("A4101") # duplicate of R2013-6186-18
  v_exclude_strataid <- c(v_exclude_strataid, "ID011") # duplicate of R20176002301
  v_exclude_strataid <- c(v_exclude_strataid, "R2015-DHS13") # duplicate of R2013-DHS08
  # Duplicates manually identified by Astha
  v_exclude_strataid <- c(v_exclude_strataid, "2803") # duplicate of 4001
  v_exclude_strataid <- c(v_exclude_strataid, "R201770004PAKVASA", "R201770005COMSA1") # Duplicates of new ad-hoc data
    # View(subset(dat, strata_id %in% v_exclude_strataid))
  # View(subset(dat, iso3 == "AFG" & year_mid == 2007))
  # View(subset(dat, iso3 == "AFG"))
  # View(subset(dat, iso3 == "IDN" & year_mid == 2001))
  # View(subset(dat, iso3 == "IND" & year_mid == 2005))
  # View(subset(dat, strata_id %in% c("A4101","R2013-6186-18")))
  
}

dat <- subset(dat, !(strata_id %in% v_exclude_strataid))


# Save output -------------------------------------------------------------

# Old model input that now has covariate names and scales as pred database
write.csv(dat, paste0("./gen/update-old-studies/temp/studies_dup-dropped_", ageSexSuffix, ".csv"), row.names = FALSE)

