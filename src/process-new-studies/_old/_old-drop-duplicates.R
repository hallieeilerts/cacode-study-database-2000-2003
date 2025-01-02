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

# Reshape cod_mapped and cod_n wide
# Create indicator for multiple VA algorithms for same study
v_id_col <- names(dat)[!(names(dat) %in% c("cod_mapped", "cod_n"))]
datWide <- dat %>%
  pivot_wider(
    id_cols = all_of(v_id_col),
    names_from = cod_mapped,
    values_from = cod_n,
    values_fn= function(x) sum(x, na.rm = TRUE)
  ) %>%
  arrange(id) %>%
  group_by(article_id, iso3, year_start, year_end, sex, totdeaths) %>%
  mutate(N = n()) %>% 
  mutate(va_mult_ind = ifelse(N > 1, 1, 0)) %>%
  mutate(va_mult_id = paste(article_id, iso3, year_start, year_end, substr(sex,1,1), totdeaths, sep = "-")) %>%
  mutate(va_mult_id = ifelse(va_mult_ind == 0, NA, va_mult_id))

# Manual review of data points with same country, year, sex, and total deaths
# df_batch1 <- datWide %>%
#   group_by(iso3, year_start, year_end, sex, totdeaths) %>%
#   mutate(N = n()) %>%
#   filter(N > 1 & va_mult_ind == 0)
# datWide %>%
#   filter(!(strata_id %in% v_exclude_strataid)) %>%
#   filter(!(article_id %in% v_exclude_article)) %>%
#   group_by(iso3, year_start, year_end, sex, totdeaths) %>%
#   mutate(N = n()) %>%
#   filter(N > 1 & va_mult_ind == 0) %>% View
#View(df_batch1)
# Manual review of data points with same country, year, and sex
# df_batch2 <- datWide %>%
#   filter(!(strata_id %in% v_exclude_strataid)) %>%
#   filter(!(article_id %in% v_exclude_article)) %>%
#   group_by(iso3, year_start, year_end, sex) %>%
#   mutate(N = n()) %>%
#   filter(N > 1 & va_mult_ind == 0)
#View(df_batch2)

# Ad-hoc duplicate dropping
if(ageSexSuffix == "00to28d"){
  # !!!! CHANGE TO STRATA ID
  v_exclude_strataid <- c("SLE-2019-8-R20228-01-0-1-T") # Duplicate of "SLE-2019-8-R20228-01-0-1-T
  v_exclude_strataid <- c(v_exclude_strataid, "ETH-2019-25431-R202225431-01-0-0-T") # Duplicate of ETH-2019-11119-R202211119-01-0-0-T
  v_exclude_strataid <- c(v_exclude_strataid, "ETH-2019-25431-R202225431-02-0-1-T") # Duplicate of ETH-2019-11119-R202211119-02-0-1-T
  v_exclude_strataid <- c(v_exclude_strataid, "BFA-2020-15420-R202215420-01-0-1-T") # Intervention arm of a trial
  v_exclude_article <- c()
}
if(ageSexSuffix == "01to59m"){
  # strata_id
  v_exclude_strataid <- c("R2022346-01") # R2022346-01 is duplicate of R202210582-01
  v_exclude_strataid <- c(v_exclude_strataid, "R2022111-02") # R2022111-02 is duplicate of R202224967-02
  v_exclude_strataid <- c(v_exclude_strataid, "R2022111-01")  # R2022111-01 is duplicate of R202224967-01
  v_exclude_strataid <- c(v_exclude_strataid, "R202224909-01")  # R202224909-01 is a duplicate of R202210342-01 
  v_exclude_strataid <- c(v_exclude_strataid, "R202224909-03") # R202224909-03 is a duplicate of R202210342-03
  v_exclude_strataid <- c(v_exclude_strataid, "R202210731-01") # R202210731-01 is a duplicate of R2022229-01
  # Sometimes easier to exclude entire article
  v_exclude_article <- c("R202210827")
}
if(ageSexSuffix == "05to09y"){
  # strata_id
  # batch1
  v_exclude_strataid <- c("R202226081-04") # "KEN-2013-26081-R202226081-04-60-168-T") # Pure duplicate
  v_exclude_strataid <- c(v_exclude_strataid, "R202224967-03") # "MDG-2014-24967-R202224967-03-60-108-T") # Pure duplicate
  v_exclude_strataid <- c(v_exclude_strataid, "R2022346-03") # "UGA-2016-346-R2022346-03-60-168-T") # Pure duplicate
  # batch2
  # View(subset(datWide, strata_id %in% c("R202224909-04","R202210342-04")))
  v_exclude_strataid <- c(v_exclude_strataid, "R202224909-04") # R202224909-04 is a duplicate of R202210342-04 (IND-2014-24909-89-60-168-F), and totdeaths is incorrect for one being excluded
  # View(subset(datWide, strata_id %in% c("R202224909-02","R202210342-02")))
  v_exclude_strataid <- c(v_exclude_strataid, "R202224909-02") # R202224909-02 is duplicate of R202210342-02 (IND-2014-10342-89-60-168-M), and totdeaths is incorrect for one being excluded
  v_exclude_strataid <- c(v_exclude_strataid, "R202225680-04", "R202225680-05") # Dropping sex-specific data points, using sex-combined "R20228-03"
  # Don't exclude PNG 2019 R202210731-02 and R2022229-02. These appear to be two different surveillance sites
  # Used to exclude those with different VA methods. Now keeping
  #v_exclude_strataid <- c(v_exclude_strataid, "BGD-2012-10645-R202210645-02-1-132-T") # Different methods for COD assignment. 
  #v_exclude_strataid <- c(v_exclude_strataid, "UGA-2016-346-R2022346-03-60-168-T") # Different methods for COD assignment.
  
  v_exclude_article <- c()
}
if(ageSexSuffix == "10to14y"){
  # !!!! CHANGE TO STRATA ID
  v_exclude_strataid <- c("IND-2014-24909-R202224909-04-60-168-F", "IND-2014-24909-R202224909-02-60-168-M")
  v_exclude_strataid <- c(v_exclude_strataid, "KEN-2013-26081-R202226081-04-60-168-T") # Pure duplicate
  v_exclude_strataid <- c(v_exclude_strataid, "MDG-2014-24967-R202224967-04-120-168-T") # Pure duplicate, keep the one that reports malaria
  v_exclude_strataid <- c(v_exclude_strataid, "SLE-2019-8-R20228-03-60-168-T") # Sex-combined data point, when we also have sex-specific data points
  v_exclude_strataid <- c(v_exclude_strataid, "PNG-2011-132-R2022132-01-144-228-T") # Pure duplicate
  v_exclude_strataid <- c(v_exclude_strataid, "PNG-2019-229-R2022229-02-60-168-T") # Appear to be duplicates but one has only "Other" (dropping) and one has diarrhea and "Other" category
  v_exclude_strataid <- c(v_exclude_strataid, "UGA-2016-346-R2022346-03-60-168-T") # Pure duplicate
  #v_exclude_strataid <- c(v_exclude_strataid, "BGD-2012-10645-R202210645-02-1-132-T") # Different methods for COD assignment. Only keep one.
  
  v_exclude_article <- c()
}
if(ageSexSuffix == "15to19yF"){
  # !!!! CHANGE TO STRATA ID
  v_exclude_strataid <- c("PNG-2011-132-R2022132-01-144-228-T") # Pure duplicate
  v_exclude_strataid <- c(v_exclude_strataid, "PNG-2019-10731-R202210731-03-180-288-T") # One has CODs aggregated in "Other" category. One has them disaggregated.
  
  v_exclude_article <- c()
}
if(ageSexSuffix == "15to19yM"){
  # !!!! CHANGE TO STRATA ID
  v_exclude_strataid <- c("PNG-2011-132-R2022132-01-144-228-T") # Pure duplicate
  v_exclude_strataid <- c(v_exclude_strataid, "PNG-2019-10731-R202210731-03-180-288-T") # One has CODs aggregated in "Other" category. One has them disaggregated.
  
  v_exclude_article <- c()
}

# Save information on multiple VA algorithms from same study to merge onto long format data
df_va_mult <- datWide %>%
  filter(va_mult_ind == 1) %>%
  ungroup() %>%
  select(va_mult_ind, va_mult_id) %>% 
  unique()

# Exclude duplicates
# Add multiple VA algorithms id
# Merge on information on multiple VA algorithms for same study
dat <- dat %>%
  filter(!(strata_id %in% v_exclude_strataid)) %>%
  filter(!(article_id %in% v_exclude_article)) %>%
  mutate(va_mult_id = paste(article_id, iso3, year_start, year_end, substr(sex,1,1), totdeaths, sep = "-")) %>%
  select(-c(va_mult_ind, va_mult_n)) %>%
  full_join(df_va_mult, join_by(va_mult_id)) %>%
  mutate(va_mult_id = ifelse(va_mult_ind == 0, NA, va_mult_id)) 

# Make a va_mult_id record number
df_va_mult_n <- dat %>% 
  filter(va_mult_ind == 1) %>%
  select(strata_id, va_mult_id, va_alg) %>%
  arrange(va_mult_id) %>%
  unique() %>%
  group_by(va_mult_id) %>%
  mutate(va_mult_n = 1:n())

dat <- dat %>%
  full_join(df_va_mult_n, join_by(strata_id, va_mult_id, va_alg))
  

# Tidy
v_other <- names(dat)[!(names(dat) %in% idVars)]
dat <- dat[,c(idVars, v_other)]
dat <- dat[order(dat$id),]

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/process-new-studies/temp/studies_dup-dropped_",ageSexSuffix,".csv",sep =""), row.names = FALSE)
