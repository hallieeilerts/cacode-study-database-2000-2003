################################################################################
#' @description Generate number of deaths from percentages or rates where necessary
#' @return Study data all reported in deaths (no proportions or mortality rates)
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Study CODs with cleaned columns
dat <- read.csv(paste0("./gen/combine-studies-adhoc/temp/studies_wide.csv", sep = ""))
################################################################################

# !!!!!
# Note 2024/12/19
# cod_den_n is very different from the sum of deaths for some studies.
# See neonate study with strata_id R2022312-01 as an example

# Removing some columns (same as Diana's code and renaming other columns)
dat <- dat %>%
  select(-c(cod_den_acmo, cod_den_acmo_unit, cod_denro_n, cod_denro_lb,
            cod_den_acme, cod_den_acme_unit)) %>%
  rename(den_type = cod_den,
         den_n = cod_den_n,
         den_p = cod_den_p,
         den_acmo = cod_den_acmo_comment,
         den_acmo_unit = cod_den_acmo_unit_comment,
         acmo_n = cod_denro_n_comment,
         lb = cod_denro_lb_comment,
         den_acme = cod_den_acme_comment,
         den_acme_unit = cod_den_acme_unit_comment) %>%
  mutate(totdeaths = ifelse(!is.na(den_n), den_n,
                            ifelse (!is.na(den_p), den_p, NA))) 
# Most of the den_p value is 100%. Can we use that value as totdeaths_orig

# Creating separate columns for cod number, proportion, mrcolumns, mortality ratio
# Loop over 1 to 55
n_cod_col <- max(as.numeric(gsub("\\D", "", names(dat)[grepl("cod", names(dat))])), na.rm = T)
for (j in 1:n_cod_col) {
  i <- sprintf("%02d", j)  # Format the loop counter as a two-digit number
  
  # Convert type`i'_comment to numeric
  dat <- dat %>%
    mutate(!!paste0("type", i, "_comment") := as.numeric(!!sym(paste0("type", i, "_comment")))) 
  
  # Generate new variables based on type
  dat <- dat %>%
    mutate(
      !!paste0("cod_p", j) := ifelse(!!sym(paste0("type", i)) == "cod_p", !!sym(paste0("type", i, "_comment")), NA_real_),
      !!paste0("cod_n", j) := ifelse(!!sym(paste0("type", i)) == "cod_n", !!sym(paste0("type", i, "_comment")), NA_real_),
      !!paste0("cod_mr", j) := ifelse(!!sym(paste0("type", i)) == "cod_mr", !!sym(paste0("type", i, "_comment")), NA_real_),
      !!paste0("cod_mro", j) := ifelse(!!sym(paste0("type", i)) == "cod_mro", !!sym(paste0("type", i, "_comment")), NA_real_)) 
  
  
  # Drop original variables
  dat <- dat %>%
    select(-!!sym(paste0("type", i)), -!!sym(paste0("type", i, "_comment")))
  
  # Rename variables
  dat <- dat %>%
    rename(!!paste0("cause_of_death", j) := !!sym(paste0("cod", i)))
}

# Reorder columns so that each cod_n, cod_p, cod_mr, and cod_mro follows its respective cause_of_death
column_order <- unlist(lapply(1:n_cod_col, function(j) {
  cause_col <- paste0("cause_of_death", j)
  if (cause_col %in% colnames(dat)) {
    c(cause_col, paste0("cod_n", j), paste0("cod_p", j), paste0("cod_mr", j), paste0("cod_mro", j))
  }
}))

# Ensure other columns are included and reorder dataframe
dat <- dat %>% 
  select(all_of(c(setdiff(colnames(dat), column_order), column_order)))

# Converting percentages, mortality rates and mortality ratios to numbers for CODs

# Loop through each possible cod_n column
dat <- dat %>%
  # Step 1: Calculate cod_n using cod_p (percentage) and totdeaths_orig
  mutate(across(starts_with("cod_n"), ~ if_else(
    # Condition: cod_n is NA, cod_p is not NA, and totdeaths_orig is not NA
    is.na(.x) & !is.na(as.numeric(get(paste0("cod_p", sub("cod_n", "", cur_column()))))) & !is.na(as.numeric(totdeaths)),
    # True: Calculate cod_n as (cod_p / 100) * totdeaths_orig (convert percentage to proportion)
    (as.numeric(get(paste0("cod_p", sub("cod_n", "", cur_column())))) / 100) * as.numeric(totdeaths),
    # False: Retain the original value of cod_n
    .x
  ))) %>%
  
  # Step 2: Calculate cod_n using cod_mr, totdeaths, and den_acme
  mutate(across(starts_with("cod_n"), ~ if_else(
    is.na(.x) & is.na(as.numeric(get(paste0("cod_p", sub("cod_n", "", cur_column()))))) & !is.na(as.numeric(totdeaths)) & !is.na(as.numeric(get(paste0("cod_mr", sub("cod_n", "", cur_column()))))) & !is.na(as.numeric(den_acme)),
    (as.numeric(get(paste0("cod_mr", sub("cod_n", "", cur_column())))) * as.numeric(totdeaths)) / as.numeric(den_acme),
    .x
  ))) %>%
  
  # Step 3: Calculate cod_n using cod_mro and lb
  mutate(across(starts_with("cod_n"), ~ if_else(
    is.na(.x) & is.na(as.numeric(get(paste0("cod_p", sub("cod_n", "", cur_column()))))) & !is.na(as.numeric(get(paste0("cod_mro", sub("cod_n", "", cur_column()))))) & !is.na(as.numeric(lb)),
    as.numeric(get(paste0("cod_mro", sub("cod_n", "", cur_column())))) * as.numeric(lb) / 1000,
    .x
  ))) 

# Delete unnecessary columns
dat <- dat %>%
  select(-c(den_type, den_n, den_p, den_acmo, den_acmo_unit, den_acme, den_acme_unit, diff_denom, 
            cod_mr_unit, cod_comments, acmo_n, lb))

# !!!!!
# Note 2024/12/19
# head(dat[grepl("R2022280",dat$strata_id, ignore.case= TRUE),])
# dat[grepl("R2022280",dat$strata_id, ignore.case= TRUE),"lb"]
# Has NA for deaths because lb is missing
# I drop data points like this in reshape-cod-long because not sure what else to do.

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/combine-studies-adhoc/temp/studies_convert-dths.csv",sep =""), row.names = FALSE)


