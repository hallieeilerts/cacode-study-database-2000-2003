################################################################################
#' @description Drop duplicates between old and new studies
#' @return study database with old and new studies, CODs, and covariates
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/set-inputs.R")
# Combined old and new studies
dat <- read.csv(paste0("./gen/create-studydb/temp/studies-combined_", ageSexSuffix,".csv", sep = ""))
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_cod <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Reclassified CODs for this age group (includes Other and Undetermined)
df_reclass <- subset(key_cod, !is.na(cod_reclass))
# Exclude "TB" which has been redistributed (only present in 5-9y and 10-14y)
df_reclass <- subset(df_reclass, cod_reclass != "TB")
# Exclude "Undetermined" which is used to eliminate studies in cleaning phase
df_reclass <- subset(df_reclass, cod_reclass != "Undetermined")
v_cod <- unique(df_reclass$cod_reclass)

### Basic duplicate checks between systematic reviews
# Old neonates do not have age and years to check
# Old studies do not have VA algorithm to check

# Manual review of data points with same country, totdeaths, age
df_batch1 <- dat %>%
  mutate(tempid = paste0(iso3, totdeaths, age_lb_m, age_ub_m)) %>%
  group_by(tempid) %>%
  mutate(N = n(),
         round_N = n_distinct(round)) %>%
  filter(N > 1 & round_N != 1) %>%
  arrange(tempid, iso3, totdeaths)
nrow(df_batch1)
#View(df_batch1[,c("tempid","round","id","strata_id","location_long","age_lb_m","age_ub_m","totdeaths",v_cod)])

# Manual review of data points with same country, totdeaths
# df_batch2 <- dat %>%
#    filter(!(strata_id %in% v_exclude_strataid)) %>%
#    mutate(tempid = paste0(iso3, totdeaths)) %>%
#    group_by(tempid) %>%
#    mutate(N = n(),
#           round_N = n_distinct(round)) %>%
#    filter(N > 1 & round_N != 1) %>%
#    arrange(tempid, iso3, totdeaths)
# nrow(df_batch2)
# View(df_batch2[,c("tempid","round","id","strata_id","location_long","age_lb_m","age_ub_m","totdeaths",v_cod)])

# Manual review of data points with same country, year, age
# df_batch3 <- dat %>%
#   filter(!(strata_id %in% v_exclude_strataid)) %>%
#   mutate(tempid = paste0(iso3, year_mid, age_lb_m, age_ub_m)) %>%
#   group_by(tempid) %>%
#   mutate(N = n(),
#          round_N = n_distinct(round)) %>%
#   filter(N > 1 & round_N != 1) %>%
#   arrange(tempid, iso3, year_mid, age_lb_m, age_ub_m)
# nrow(df_batch3)
# View(df_batch3[,c("tempid","round","id","strata_id","location_long","age_lb_m","age_ub_m","totdeaths",v_cod)])

# Manual review of data points with same country, year and randomly selected CODs
# df_batch4 <- dat %>%
#   filter(!(strata_id %in% v_exclude_strataid)) %>%
#   rowwise() %>%
#   mutate(tempid = ifelse(ageSexSuffix == "01to59m", paste0(iso3, year_start, year_end, sex, age_lb_m, age_ub_m, round(LRI)), NA)) %>%
#   mutate(tempid = ifelse(ageSexSuffix == "00to28d", paste0(iso3, year_start, year_end, sex, age_lb_m, age_ub_m, round(Preterm), round(LRI)), tempid)) %>%
#   mutate(tempid = ifelse(ageSexSuffix == "05to09y", paste0(iso3, year_start, year_end, sex, age_lb_m, age_ub_m, round(Drowning), round(Diarrhoeal)), tempid)) %>%
#   group_by(tempid) %>%
#   mutate(N = n(),
#          round_N = n_distinct(round)) %>%
#   filter(N > 1 & round_N != 1) %>%
#   arrange(tempid, iso3, year_mid)
# nrow(df_batch4)
# View(df_batch4[,c("tempid","round","id","strata_id","location_long","age_lb_m","age_ub_m","totdeaths", v_cod)])
# df_batch5 <- dat %>%
#   filter(!(strata_id %in% v_exclude_strataid)) %>%
#   rowwise() %>%
#   mutate(tempid = ifelse(ageSexSuffix == "01to59m", paste0(iso3, year_start, year_end, sex, age_lb_m, age_ub_m, round(Diarrhoeal)), NA)) %>%
#   mutate(tempid = ifelse(ageSexSuffix == "00to28d", paste0(iso3, year_start, year_end, sex, age_lb_m, age_ub_m, round(Preterm), round(LRI)), tempid)) %>%
#   mutate(tempid = ifelse(ageSexSuffix == "05to09y", paste0(iso3, year_start, year_end, sex, age_lb_m, age_ub_m, round(LRI), round(Congenital)), tempid)) %>%
#   group_by(tempid) %>%
#   mutate(N = n(),
#          round_N = n_distinct(round)) %>%
#   filter(N > 1 & round_N != 1) %>%
#   arrange(tempid, iso3, year_mid)
# nrow(df_batch5)
# View(df_batch5[,c("tempid","round","id","strata_id","location_long","age_lb_m","age_ub_m","totdeaths", v_cod)])

# Duplicate dropping
if(ageSexSuffix == "00to28d"){
  v_exclude_strataid <- c("R2019-62-198") # R2019-62-198 is duplicate of R202222679-09 in 2000-2023
}
if(ageSexSuffix == "01to59m"){
  v_exclude_strataid <- c()
  # Duplicates manually identified by Astha
  v_exclude_strataid <- c(v_exclude_strataid, "VASA1", "VASA2", "VASA3", "VASA4") 
  # duplicates of "R202222679-11", "R202222679-07", "R202222679-03", "R202223055-03"
}
if(ageSexSuffix == "05to09y"){
  v_exclude_strataid <- c()
}
if(ageSexSuffix == "10to14y"){
  v_exclude_strataid <- c()
}
if(ageSexSuffix == "15to19yF"){
  v_exclude_strataid <- c()
}
if(ageSexSuffix == "15to19yM"){
  v_exclude_strataid <- c()
}

# Exclude duplicates
dat <- dat %>%
  filter(!(strata_id %in% v_exclude_strataid))

# Create new recnr
dat$recnr <- 1:nrow(dat)
dat <- dat %>%
  select(recnr, everything())


# Save outputs ------------------------------------------------------------

write.csv(dat, paste0("./gen/create-studydb/output/StudyDatabase2023_",ageSexSuffix,"_",format(Sys.Date(), format="%Y%m%d"),".csv"), row.names = FALSE)

# Notes:
# -CODs can have NA in them
# -Some study data points are for the same population with different VA algorithms applied.
# subset(dat, va_mult_ind == 1)

