################################################################
# Cleaning CoD death reporting variables in the gen file
################################################################

#' Input: Causes of death data with required columns 
#' Output: spreadsheet with cleaned death reportin variables

# Initialize environment --------------------------------------------------

rm(list = ls())

# installing the packages

#install.packages(c("readr", "openxlsx", "dplyr", "readstata13", "gtsummary", "tidyr", "gt", "flextable", "stringr"))
install.packages("rlang")


#Loading the libraries

library(readr)
library(openxlsx)
library(dplyr)
library(readstata13)
library(gtsummary)
library(tidyr)
library(gt)
library(flextable)
library(stringr)
library(rlang)

# load the data

cod.deaths <- read.csv("./gen/process-distiller/output/cod clean age.csv")

# Removing some columns (same as Diana's code and renaming other columns)
cod.deaths <- cod.deaths %>%
  select(-c (cod_den_acmo, cod_den_acmo_unit, cod_denro_n, cod_denro_lb,
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
  mutate (totdeaths_orig = ifelse(!is.na(den_n), den_n,
                                   ifelse (!is.na(den_p), den_p, NA))) # Most of the den_p value is 100%. Can we use that value as totdeaths_orig


# Check various death denominators and any study IDs which need adjustment/discussion

## Tabulate Refid for "# of deaths"
tab_n <- cod.deaths %>% 
  filter(den_type == "# of deaths") %>% 
  count(Refid)

print(tab_n)

## List relevant variables for "# of deaths"
list_n <- cod.deaths %>% 
  filter(den_type == "# of deaths") %>%
  select(Refid, totdeaths_orig)

print(list_n)

# Tabulate Refid for "% of deaths"
tab_p <- cod.deaths %>% 
  filter(den_type == "% of deaths") %>% 
  count(Refid, type01)

print(tab_p)

 ## List relevant variables for "% of deaths"
list_p <- cod.deaths %>% 
  filter(den_type == "% of deaths") %>%
  select(Refid, totdeaths_orig)

print(list_p)

# Tabulate Refid for "Mortality Ratios"
tab_mro <- cod.deaths %>% 
  filter(den_type == "mortality ratio") %>% 
  count(Refid)

print(tab_mro)

   ## List relevant variables for "mortality ratio"
list_mro <- cod.deaths %>% 
  filter(den_type == "mortality ratio") %>%
  select(Refid, totdeaths_orig, den_acmo, den_acmo_unit, acmo_n, lb, cod_mr_unit)

print(list_mro)


# Tabulate Refid for "Mortality Rates"
tab_mr <- cod.deaths %>% 
  filter(den_type == "mortality rate") %>% 
  count(Refid)

print(tab_mr)

## List relevant variables for "mortality rate"
list_mr <- cod.deaths %>% 
  filter(den_type == "mortality rate") %>%
  select(Refid, totdeaths_orig, den_acme, den_acme_unit, cod_mr_unit)

print(list_mr)



# List study IDs which do not have any denominator 
no_den <- cod.deaths %>%
  filter (is.na(den_type) & is.na(totdeaths_orig) & is.na(den_n) & is.na(den_p) &
          is.na(den_acmo) & is.na(den_acmo_unit) & is.na(acmo_n) & is.na (lb) & is.na(den_acme) & is.na (den_acme_unit))
  

# Creating separate columns for cod number, proportion, mrcolumns, mortality ratio

# Loop over 1 to 53
for (j in 1:55) {
  i <- sprintf("%02d", j)  # Format the loop counter as a two-digit number
  
  # Convert type`i'_comment to numeric
  cod.deaths <- cod.deaths %>%
    mutate(!!paste0("type", i, "_comment") := as.numeric(!!sym(paste0("type", i, "_comment")))) 


  # Generate new variables based on type
  cod.deaths <- cod.deaths %>%
     mutate(
           !!paste0("cod_p", j) := ifelse(!!sym(paste0("type", i)) == "cod_p", !!sym(paste0("type", i, "_comment")), NA_real_),
           !!paste0("cod_n", j) := ifelse(!!sym(paste0("type", i)) == "cod_n", !!sym(paste0("type", i, "_comment")), NA_real_),
           !!paste0("cod_mr", j) := ifelse(!!sym(paste0("type", i)) == "cod_mr", !!sym(paste0("type", i, "_comment")), NA_real_),
           !!paste0("cod_mro", j) := ifelse(!!sym(paste0("type", i)) == "cod_mro", !!sym(paste0("type", i, "_comment")), NA_real_)) 
  

  # Drop original variables
  cod.deaths <- cod.deaths %>%
    select(-!!sym(paste0("type", i)), -!!sym(paste0("type", i, "_comment")))

  # Rename variables
  cod.deaths <- cod.deaths %>%
    rename(!!paste0("cause_of_death", j) := !!sym(paste0("cod", i)))
}
 

# Reorder columns so that each cod_n, cod_p, cod_mr, and cod_mro follows its respective cause_of_death
column_order <- unlist(lapply(1:55, function(j) {
  cause_col <- paste0("cause_of_death", j)
  if (cause_col %in% colnames(cod.deaths)) {
    c(cause_col, paste0("cod_n", j), paste0("cod_p", j), paste0("cod_mr", j), paste0("cod_mro", j))
  }
}))

# Ensure other columns are included and reorder dataframe
cod.deaths <- cod.deaths %>% 
  select(all_of(c(setdiff(colnames(cod.deaths), column_order), column_order)))


# Save output -------------------------------------------------------------

write.csv(cod.deaths, "./gen/process-distiller/output/cod clean den.csv", row.names = FALSE)


