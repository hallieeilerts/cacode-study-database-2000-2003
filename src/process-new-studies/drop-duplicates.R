################################################################################
#' @description Detect duplicates and drop, add indicator for multiple VA algorithms for same study
#' @return No duplicates in study data
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Study data with cleaned columns
dat <- read.csv(paste0("./gen/process-new-studies/temp/studies_cod-map_", ageSexSuffix, ".csv", sep = ""))
################################################################################

# Reshape cod_mapped and cod_n wide for quality checks
v_id_col <- names(dat)[!(names(dat) %in% c("cod_mapped", "cod_n"))]
datWide <- dat %>%
  pivot_wider(
    id_cols = all_of(v_id_col),
    names_from = cod_mapped,
    values_from = cod_n,
    values_fn= function(x) sum(x, na.rm = TRUE)
  )

# Manual review of data points with same country, year, sex, and total deaths
# Do not have two different VA algorithms.
# Are not an ad-hoc data point. We compiled ad-hoc data and should not have any duplicates.
# Identify duplicates to drop and add to v_exclude_strataid or v_exclude_article
# df_batch1 <- datWide %>%
#   group_by(iso3, year_start, year_end, sex, totdeaths) %>%
#   mutate(N = n(),
#          va_alg_N = n_distinct(va_alg)) %>%
#   mutate(adhoc = grepl("adhoc",article_id,ignore.case = TRUE)) %>%
#   filter(N > 1 & va_alg_N == 1 & adhoc == FALSE) %>%
#   arrange(iso3, totdeaths)
# View(df_batch1)
# df_batch2 <- datWide %>%
#   filter(!(strata_id %in% v_exclude_strataid)) %>%
#   filter(!(article_id %in% v_exclude_article)) %>%
#   group_by(iso3, year_start, year_end, sex, totdeaths) %>%
#   mutate(N = n(),
#          va_alg_N = n_distinct(va_alg)) %>%
#   mutate(adhoc = grepl("adhoc",article_id,ignore.case = TRUE)) %>%
#   filter(N > 1 & va_alg_N == 1 & adhoc == FALSE) %>%
#   arrange(iso3, totdeaths)
# View(df_batch2)
# nrow(df_batch2) # Should be zero rows once all duplicates dealt with, unless otherwise noted in exclusions below.

# # Manual review of data points with same country, year, sex, and total deaths
# df_batch3 <- datWide %>%
#    filter(!(strata_id %in% v_exclude_strataid)) %>%
#     filter(!(article_id %in% v_exclude_article)) %>%
#     group_by(iso3, year_start, year_end, sex, totdeaths) %>%
#     mutate(N = n(),
#            va_alg_N = n_distinct(va_alg)) %>%
#     filter(N > 1) %>%
#     mutate(adhoc = grepl("adhoc",article_id,ignore.case = TRUE)) %>%
#     filter(N > 1 & adhoc == FALSE) %>%
#     arrange(iso3, totdeaths)
# View(df_batch3) # All of these duplicates should be multiple VA algorithms applied to same study data (va_alg_N should be 2)
# If not, drop one record. 
# Potential drop candidates include placebo/intervention strata, underlying/primary COD strata.

# Manual review of data points with same country, year, sex, age_lb_m, age_ub_m
# Do not have two different VA algorithms.
# df_batch4 <- datWide %>%
#   filter(!(strata_id %in% v_exclude_strataid)) %>%
#   filter(!(article_id %in% v_exclude_article)) %>%
#   group_by(iso3, year_start, year_end, sex, age_lb_m, age_ub_m) %>%
#   mutate(N = n(),
#          va_alg_N = n_distinct(va_alg)) %>%
#   mutate(adhoc = grepl("adhoc",article_id,ignore.case = TRUE)) %>%
#   filter(N > 1 & va_alg_N == 1 & adhoc == FALSE) %>%
#   arrange(iso3, totdeaths)
# View(df_batch4) # Scan strata_other1 to ensure these are for different locations
# If not, drop one record. 
# Potential drop candidates include placebo/intervention strata, underlying/primary COD strata.

# Manual review of data points with same country, year, age_lb_m, age_ub_m
# Do not have two different VA algorithms.
# df_batch5 <- datWide %>%
#   filter(!(strata_id %in% v_exclude_strataid)) %>%
#   filter(!(article_id %in% v_exclude_article)) %>%
#   group_by(iso3, year_start, year_end, age_lb_m, age_ub_m) %>%
#   mutate(N = n(),
#          va_alg_N = n_distinct(va_alg)) %>%
#   mutate(adhoc = grepl("adhoc",article_id,ignore.case = TRUE)) %>%
#   filter(N > 1 & va_alg_N == 1 & adhoc == FALSE) %>%
#   arrange(iso3, totdeaths)
# View(df_batch5)
# Identify if there are M, F, and T data points available and keep appropriate one depending on age/sex group.
# e.g., For 5-9y, if all were available, keep T. Note that if only M and F were available, these will be collapsed at a later stage.

# Study data points that are excluded from all age groups because they are LiST, GBD, CHAMPS
# !!!! Need to add ref_id for "10327" and "10661". These are ref_id. Find the article id in 5-19y data
v_exclude_article_listgbd <- c("R202210648", "R202210327", "R202210661", "R202223055",
                               "R2022312")

# Duplicate dropping
if(ageSexSuffix == "00to28d"){
  # strata_id
  # batch1
  v_exclude_strataid <- c("R20228-01") # R20228-01 is a duplicate of R202225680-01
  v_exclude_strataid <- c(v_exclude_strataid, "R202225431-01") #  R202225431-01 is a duplicate of R202211119-01
  v_exclude_strataid <- c(v_exclude_strataid, "R202225431-02") #  R202225431-02 is a duplicate of R202211119-02
  v_exclude_strataid <- c(v_exclude_strataid, "R202225180-02") #  R202225180-02 is primary cause, keeping underlying R202225180-05
  # batch4
  v_exclude_strataid <- c(v_exclude_strataid, "R202215420-01") #  R202215420-01 is intervention arm of a trial (R202215420-02 is control group)
  v_exclude_strataid <- c(v_exclude_strataid, "R202223172-01") #  R202223172-01 is intervention arm of a trial (R202223172-02 is control group)
  # Exclude entire article
  v_exclude_article <- c()
}
if(ageSexSuffix == "01to59m"){
  # strata_id
  v_exclude_strataid <- c("R2022346-01") # R2022346-01 is duplicate of R202210582-01
  v_exclude_strataid <- c(v_exclude_strataid, "R2022111-02") # R2022111-02 is duplicate of R202224967-02
  v_exclude_strataid <- c(v_exclude_strataid, "R2022111-01")  # R2022111-01 is duplicate of R202224967-01
  v_exclude_strataid <- c(v_exclude_strataid, "R202224909-01")  # R202224909-01 is a duplicate of R202210342-01, and has fewer causes reported
  v_exclude_strataid <- c(v_exclude_strataid, "R202224909-03") # R202224909-03 is a duplicate of R202210342-03
  v_exclude_strataid <- c(v_exclude_strataid, "R202210731-01") # R202210731-01 is a duplicate of R2022229-01
  v_exclude_strataid <- c(v_exclude_strataid, "R202225180-03") # R202225180-03 is primary cause, keeping underlying R202225180-06
  v_exclude_strataid <- c(v_exclude_strataid, "R202225180-01") # R202225180-01 is primary cause, keeping underlying R202225180-04
  # For looking at duplicates in batch 2
  # View(subset(df_batch2, strata_id %in% c("R202210731-01", "R2022229-01")))
  # batch3
  v_exclude_strataid <- c(v_exclude_strataid, "R202222094-02", "R202222094-04") # Intervention arm of a trial, with InterVA and SmartVA
  # For looking at duplicates in batch 3
  # View(subset(df_batch3, strata_id %in% c("R202210800-01")))
  # batch4
  v_exclude_strataid <- c(v_exclude_strataid, "R202210800-01") # Azithromycin intervention arm of a trial
  v_exclude_strataid <- c(v_exclude_strataid, "R202219200-01", "R202220750-01", "R202220750-02") # RotaSIIL intervention arm of a trial, and duplicated. Dropping duplicate control record as well
  v_exclude_strataid <- c(v_exclude_strataid, "R2022346-02") # Facility record strata
  # Exclude entire article
  v_exclude_article <- c("R202210827")
}
if(ageSexSuffix == "05to09y"){
  # strata_id
  # batch1
  v_exclude_strataid <- c("R202226081-04") # R202226081-04 is a duplicate of R202210243-04
  v_exclude_strataid <- c(v_exclude_strataid, "R202224967-03") # R202224967-03 is a duplicate of R2022111-03
  v_exclude_strataid <- c(v_exclude_strataid, "R2022346-03") # R2022346-03 is a duplicate of R202210582-02
  v_exclude_strataid <- c(v_exclude_strataid, "R202224909-04") # R202224909-04 is a duplicate of R202210342-04 and totdeaths is incorrect for one being excluded
  v_exclude_strataid <- c(v_exclude_strataid, "R202224909-02") # R202224909-02 is duplicate of R202210342-02 and totdeaths is incorrect for one being excluded
  v_exclude_strataid <- c(v_exclude_strataid, "R202210731-02") # R202210731-02 and R2022229-02 are from two different papers. They are the same 5-14y data point for PNG in 2018-2020 but with the CODs aggregated differently. keeping R2022229-02 because it has better cause aggregation for 5-9y. 
  # batch5
  v_exclude_strataid <- c(v_exclude_strataid, "R202225680-04", "R202225680-05") # Dropping sex-specific data points, using sex-combined "R20228-03"
  # Exclude entire article
  v_exclude_article <- c()
}
if(ageSexSuffix == "10to14y"){
  v_exclude_strataid <- c("R202224909-02", "R202224909-04") # R202224909-02 and R202224909-04 are duplicates of R202210342-02 and R202210342-04
  v_exclude_strataid <- c(v_exclude_strataid,"R202226081-04") # R202226081-04 is a duplicate of R202210243-04 # "KEN 2013
  v_exclude_strataid <- c(v_exclude_strataid, "R2022111-04") # R2022111-04 is a duplicate of R202224967-04 and reports malaria  # MDG 2014
  v_exclude_strataid <- c(v_exclude_strataid, "R2022132-01") # R2022132-01 is a duplicate of R202224933-01 # PNG 2011
  v_exclude_strataid <- c(v_exclude_strataid, "R202210582-02") # R202210582-02 is a duplicate of R2022346-03 # UGA 2016
  v_exclude_strataid <- c(v_exclude_strataid, "R202210731-02") # R202210731-02 and R2022229-02 are from two different papers. They are the same 5-14y data point for PNG in 2018-2020 but with the CODs aggregated differently. keeping R2022229-02 because it has better cause aggregation for 5-9y. 
  # batch2 has India adhoc data points that are for different regions
  # batch5
  v_exclude_strataid <- c(v_exclude_strataid, "R202225680-04", "R202225680-05") # Dropping sex-specific data points, using sex-combined "R20228-03"
  # Exclude entire article
  v_exclude_article <- c()
}
if(ageSexSuffix == "15to19yF"){
  # !!!! CHANGE TO STRATA ID
  v_exclude_strataid <- c("PNG-2011-132-R2022132-01-144-228-T") # Pure duplicate
  v_exclude_strataid <- c(v_exclude_strataid, "PNG-2019-10731-R202210731-03-180-288-T") # One has CODs aggregated in "Other" category. One has them disaggregated.
  # Exclude entire article
  v_exclude_article <- c()
}
if(ageSexSuffix == "15to19yM"){
  # !!!! CHANGE TO STRATA ID
  v_exclude_strataid <- c("PNG-2011-132-R2022132-01-144-228-T") # Pure duplicate
  v_exclude_strataid <- c(v_exclude_strataid, "PNG-2019-10731-R202210731-03-180-288-T") # One has CODs aggregated in "Other" category. One has them disaggregated.
  # Exclude entire article
  v_exclude_article <- c()
}

# Create empty data frame for exclusions
dat_exc <- datWide[0,]
if(length(v_exclude_strataid) > 0 |  length(v_exclude_article) > 0){
  dat_exc <- datWide %>%
    filter(strata_id %in% v_exclude_strataid | 
             article_id %in% v_exclude_article |
              article_id %in% v_exclude_article_listgbd) %>%
    mutate(exclude_reason = ifelse(article_id %in% v_exclude_article_listgbd, "GBD, LiST, or CHAMPS", "Duplicate"))
}

# Exclude duplicates
dat <- dat %>%
  filter(!(strata_id %in% v_exclude_strataid)) %>%
  filter(!(article_id %in% v_exclude_article)) %>%
  filter(!(article_id %in% v_exclude_article_listgbd))

# Tidy
v_other <- names(dat)[!(names(dat) %in% idVars)]
dat <- dat[,c(idVars, v_other)]
dat <- dat[order(dat$id),]

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/process-new-studies/temp/studies_dup-dropped_",ageSexSuffix,".csv",sep =""), row.names = FALSE)
write.csv(dat_exc, paste0("./gen/process-new-studies/audit/dat_exc0_",ageSexSuffix,".csv",sep =""), row.names = FALSE)
