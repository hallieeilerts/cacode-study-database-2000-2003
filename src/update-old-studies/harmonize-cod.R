################################################################################
#' @description For neonates and postneonates, map and aggregate CODs, round to whole numbers
#' For 5-19, update names of old CODs
#' @return Study CODs aggregated by reclassified categories in long format 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
#' Inputs
source("./src/set-inputs.R")
## Old study databases with under-5 duplicates dropped
if(ageSexSuffix %in% c("00to28d","01to59m")){
  dat <- read.csv(paste0("./gen/update-old-studies/temp/studies_dup-dropped_", ageSexSuffix, ".csv", sep = ""))
}
## Old study databases with updated id variables to match new studies
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF", "15to19yM")){
  dat <- read.csv(paste0("./gen/update-old-studies/temp/studies_set-id_", ageSexSuffix, ".csv", sep = ""))
}
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
df_reclass <- df_reclass[,c("cod_mapped", "cod_reclass")]
v_cod_reclass <- unique(df_reclass$cod_reclass)
v_cod_mapped <- key_cod$cod_mapped

# Rename study COD names to match cod_mapped in COD reclassification key
# Reshape to COD long
# Merge on COD reclassification key
# Drop any COD not being reclassified
# Replaced mapped COD names with reclass COD names
# Reshape CODs back wide and aggregate reclassified COD
if(ageSexSuffix %in% c("00to28d")){
  
  datCOD <- dat %>% 
    rename_with(
      ~ case_when(
        . == "congenital" ~ "congen",
        . == "diarrhoea" ~ "dia",
        . == "pneumonia" ~ "lri",
        TRUE ~ .)
    ) %>%
    pivot_longer(cols = any_of(c(v_cod_mapped)),
                 names_to = c("cod_mapped"), 
                 values_to = "cod_n"
    ) %>% 
    left_join(df_reclass[,c("cod_mapped","cod_reclass")], by = join_by(cod_mapped)) %>% 
    filter(!is.na(cod_reclass)) %>% # If the cause is not getting reclassified, deaths should be dropped. 
    select(-c(cod_mapped)) %>% 
    mutate(cod_n = round(cod_n)) %>% # Round COD n to whole numbers. Actually already whole for neonates. Old needs to be done for 1-59m but including for consistency.
    group_by(strata_id) %>% 
    mutate(totdeaths_noOther = sum(cod_n, na.rm = TRUE)) %>% # Sum up deaths from all reported causes (old neonatal data doesn't include "Other")
    group_by(strata_id, cod_reclass) %>% 
    mutate(codReclassN = n()) %>% # If multiple mapped causes are going to be aggregated into a reclassified cause,
    mutate(cod_n = ifelse(is.na(cod_n) & codReclassN > 1, 0, cod_n)) %>% # Assign zero to any NA values for the reclassified cause. 
    # This will allow them to sum easily, while still retaining other NA values for causes not reported in the study at all.
    # Only seems necessary for 1-59m, but including here too just in case.
    pivot_wider(
      names_from = cod_reclass,
      values_from = cod_n
    ) %>% 
    mutate(Other = totdeaths - totdeaths_noOther) %>% # Recover Other from difference between totdeaths and sum of reported causes
    select(all_of(idVars), "totdeaths", any_of(v_cod_reclass)) # Using any_of because old neonate data doesn't have Other
  
}

if(ageSexSuffix %in% c("01to59m")){
  
  # There are a number of extra COD categories in the old 1-59m study data
  # If we want to extract malnutrition, need to figure out which "other" category to use.
  # To do so, investigate how causes added up to totdeaths.
  # dat %>%
  #   select(article_id, comment, iso3, malaria, other, other_orig, other_nomaln,
  #          injuries, measles, meningitis, neonatal, pneumonia, congenital, malnutrition, hiv,
  #          diarrhea, unknown, stillbirth, totdeaths_orig, totdeaths) %>%
  #   rowwise() %>%
  #   # if there is other_nomaln, calculate total with other_nomaln and other
  #   # if there is no other_nomaln, calculate total with other and no malnutrition
  #   mutate(test1 = ifelse(!is.na(other_nomaln),
  #                         sum(malaria, other_nomaln, injuries, meningitis, neonatal, pneumonia, congenital, diarrhea, malnutrition,  na.rm = T),
  #                         sum(malaria, other, injuries, meningitis, neonatal, pneumonia, congenital, diarrhea,  na.rm = T)
  #                         )) %>% 
  #   filter(abs(totdeaths-test1) > 1) %>% View
  #
  # After imposing the above rules, some study CODs still don't add up to totdeaths
  # For almost all of these, the "comment" says this is because "only some causes listed", "percentages dont add up to 100%"
  # In these cases, totdeaths is bigger than test1
  # In one case, there is no comment (article_id == B0101) and totdeaths is smaller than test1
  #
  # Plan:
  # 1. Apply the above rules to recalculate "other"
  #  a. If other_nomaln is reported, recode "other_nomaln" as "other"
  #  b. If other_nomaln is not reported, reduce "other" by the amount in malnutrition
  #  c. Make sure other does not become negative by b
  # 2. Calculate totdeaths sum
  #    malaria, injuries, meningitis, neonatal, pneumonia, congenital, diarrhea, malnutrition, and the new "other"
  # 3. If totdeaths is larger...
  #     Add residual deaths to other.
  #     (This happens when there is a "comment" that says "only some causes listed")
  # 4. If totdeaths is smaller...
  #     recode totdeaths as the sum of the above.
  # 5. Make sure other does 
  # Note: totdeaths currently does not include hiv, measles, unknown.
  #     It thus needs to be recalculated after cause mapping applied
  
  # # Testing rules
  # dat %>%
  #   select(article_id, comment, iso3, malaria, other, other_orig, other_nomaln,
  #          injuries, measles, meningitis, neonatal, pneumonia, congenital, malnutrition, hiv,
  #          diarrhea, unknown, stillbirth, totdeaths_orig, totdeaths) %>%
  #   rowwise() %>%
  #   mutate(other = ifelse(!is.na(other_nomaln), other_nomaln, other)) %>%
  #   mutate(other = ifelse(is.na(other_nomaln) & !is.na(malnutrition), other - malnutrition, other)) %>%
  #   mutate(test1 = sum(malaria, other, injuries, meningitis, neonatal, pneumonia, congenital, diarrhea, malnutrition,  na.rm = T)) %>%
  #   mutate(other = ifelse(totdeaths-test1 > 1 & !is.na(other), other + (totdeaths - test1), other)) %>%
  #   mutate(other = ifelse(totdeaths-test1 > 1 & is.na(other), (totdeaths - test1), other)) %>%
  #   mutate(test2 = sum(malaria, other, injuries, meningitis, neonatal, pneumonia, congenital, diarrhea, malnutrition,  na.rm = T)) %>%
  #   mutate(totdeaths = ifelse(totdeaths-test1 < -1, test2, totdeaths)) %>%
  #   filter(abs(totdeaths-test2) > 1 | other < 0) %>% View
  # # other has very small quantities negative in a few cases. Recode as 0.
  
  datRecalcOther <- dat %>%
    mutate(other = ifelse(!is.na(other_nomaln), other_nomaln, other)) %>% 
    mutate(other = ifelse(is.na(other_nomaln) & !is.na(malnutrition), other - malnutrition, other)) %>% 
    mutate(other = ifelse(other < 0, 0, other)) %>%
    rowwise() %>%
    mutate(test1 = sum(malaria, other, injuries, meningitis, neonatal, pneumonia, congenital, diarrhea, malnutrition,  na.rm = T)) %>%
    mutate(other = ifelse(totdeaths-test1 > 1 & !is.na(other), other + (totdeaths - test1), other)) %>%
    mutate(other = ifelse(totdeaths-test1 > 1 & is.na(other), (totdeaths - test1), other)) %>% 
    mutate(test2 = sum(malaria, other, injuries, meningitis, neonatal, pneumonia, congenital, diarrhea, malnutrition,  na.rm = T)) %>%
    mutate(totdeaths = ifelse(totdeaths-test2 < -1, test2, totdeaths)) %>%
    select(-c(totdeaths_orig, other_nomaln, maln_int, other_int, other_orig, test1, test2))
  
  # # Check all causes add up to total 
  # # (allow for some rounding errors by only checking differences bigger than 1. These small differences are corrected later)
  # datRecalcOther %>%
  #   mutate(checksum = sum(malaria, injuries, meningitis, neonatal, pneumonia, congenital, diarrhea, malnutrition, other,  na.rm = T)) %>%
  #   filter(abs(checksum - totdeaths) > 1) %>%
  #   select(article_id, comment, iso3, malaria, other,
  #          injuries, measles, meningitis, neonatal, pneumonia, congenital, malnutrition, hiv,
  #          diarrhea, unknown, stillbirth,  totdeaths, checksum) %>% nrow # 0
  # # Check no negative causes
  # datRecalcOther %>%
  #   mutate(checksum = sum(malaria, injuries, meningitis, neonatal, pneumonia, congenital, diarrhea, malnutrition, other,  na.rm = T)) %>%
  #   filter(if_any(all_of(c("malaria", "injuries", "meningitis", "neonatal", "pneumonia", "congenital", "diarrhea", "malnutrition", "other")), ~ . < 0)) %>%
  #   select(article_id, comment, iso3, malaria, other,
  #        injuries, measles, meningitis, neonatal, pneumonia, congenital, malnutrition, hiv,
  #        diarrhea, unknown, stillbirth,  totdeaths) %>% nrow # 0
  
  # Rename study COD names to match cod_mapped in COD reclassification key
  # Reshape to COD long
  # Merge on COD reclassification key, drop causes not getting reclassified
  # Drop any COD not being reclassified
  # Recalculate total deaths
  # Replaced mapped COD names with reclass COD names
  # Reshape CODs back wide and aggregate reclassified COD
  datCOD <- datRecalcOther %>% 
    rename_with(
      ~ case_when(
        . == "congenital" ~ "congen",
        . == "diarrhea" ~ "dia",
        . == "pneumonia" ~ "lri",
        . == "neonatal" ~ "neonatal_cond",
        . == "meningitis" ~ "mening",
        . == "measles" ~ "mea",
        . == "malaria" ~ "mal",
        . == "unknown" ~ "undt",
        TRUE ~ .)
    ) %>%
    pivot_longer(cols = any_of(c(v_cod_mapped)),
                 names_to = c("cod_mapped"), 
                 values_to = "cod_n"
    ) %>% 
    left_join(df_reclass[,c("cod_mapped","cod_reclass")], by = join_by(cod_mapped)) %>% 
    filter(!is.na(cod_reclass)) %>% # If the cause is not getting reclassified, deaths should be dropped. 
    select(-c(cod_mapped)) %>% 
    mutate(cod_n = round(cod_n)) %>% # Round COD n to whole numbers
    group_by(strata_id) %>% 
    mutate(totdeaths = sum(cod_n, na.rm = TRUE)) %>% # recalculate totdeaths
    group_by(strata_id, cod_reclass) %>% 
    mutate(codReclassN = n()) %>% # If multiple mapped causes are going to be aggregated into a reclassified cause,
    mutate(cod_n = ifelse(is.na(cod_n) & codReclassN > 1, 0, cod_n)) %>% # Assign any NA values for the reclassified cause as zero. 
    # This will allow them to sum easily while still keeping any other NA values for causes not reported in the study.
    pivot_wider(
      names_from = cod_reclass,
      values_from = cod_n,
      values_fn=sum
    ) %>% 
    select(-comment) %>%
    select(all_of(idVars), "totdeaths", any_of(v_cod_reclass)) # Using any_of because old neonate data doesn't have Other

}

# Update COD names to match this round
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF", "15to19yM")){
  
  datCOD <- dat %>% 
    rename_with(
      ~ case_when(
        . == "OtherCD" ~ "OtherCMPN",
        TRUE ~ .)
    ) %>%
    select(all_of(idVars), "totdeaths", all_of(v_cod_reclass))
  
}

# Check that all reclassified COD are reported
v_missing <- v_cod_reclass[!(v_cod_reclass %in% names(datCOD))]
if(length(v_missing) > 0){
  warning("Missing some expected CODs in old study database.")
  for(i in 1:length(v_missing)){
    datCOD$missingCOD <- NA
    names(datCOD)[which(names(datCOD) == "missingCOD")] <- v_missing[i]
  }
}

# Check that totdeaths is equal to sum of causes
totDif <- which(datCOD$totdeaths != apply(datCOD[, paste0(v_cod_reclass)], 1, sum, na.rm = T))
if(length(totDif)>0){
  warning("Sum of causes does not equal totdeaths.")
  #View(dat[totDif,])
}

# Check no negative CODs
codNeg <- datCOD %>%
  ungroup() %>%
  select(any_of(v_cod_reclass)) %>%
  filter(if_any(everything(), ~ . < 0))
if(nrow(codNeg)>0){
  warning("Negative value for COD.")
}

# Save output -------------------------------------------------------------

# Old model input that now has covariate names and scales as pred database
write.csv(datCOD , paste0("./gen/update-old-studies/temp/cod-wide_harmonized_", ageSexSuffix, ".csv"), row.names = FALSE)

