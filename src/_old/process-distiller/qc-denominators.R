################################################################
# COD numbers quality check and data correction in the gen file
################################################################

#' Input: Causes of death data with required columns 
#' Output: Quality check ofnumbers for different causes and correcting data

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

cod.deaths <- read.csv("./gen/process-distiller/output/cod clean den.csv")

# Check whether reported #deaths by cause would add up to totdeaths_orig and correct any discrepancies

# Check discrepancies

# Discrepancies for number of death 
 # Create td_nckrt by summing across columns that start with 'cod_n'
 cod.qc.n <- cod.deaths %>%
   mutate(td_nckrt = ifelse(rowSums(!is.na(select(., starts_with("cod_n")))) > 0, 
                            rowSums(select(., starts_with("cod_n")), na.rm = TRUE), 
                            NA)) %>%
   # Convert totdeaths_orig and td_nckrt to numeric, forcing non-numeric values to NA
   mutate(totdeaths_orig = as.numeric(totdeaths_orig),
          td_nckrt = as.numeric(td_nckrt)) %>%
   # Filter out rows where totdeaths_orig or td_nckrt are NA after conversion
   filter(!is.na(totdeaths_orig) & !is.na(td_nckrt)) %>%
  # Create td_ndiff as the difference between totdeaths_orig and td_nckrt
    mutate(td_ndiff = totdeaths_orig - td_nckrt) %>%
    select (Refid, totdeaths_orig, td_nckrt, td_ndiff, starts_with("cod_n")) %>% #240 obs
   filter (td_ndiff != 0) # 98 obs



# Discrepancies for percentage of death 
 # Create td_pckrt by summing across columns that start with 'cod_p'
 cod.qc.p <- cod.deaths %>%
   mutate(td_pckrt = ifelse(rowSums(!is.na(select(., starts_with("cod_p")))) > 0, 
                            rowSums(select(., starts_with("cod_p")), na.rm = TRUE), 
                            NA)) %>%
   # Convert totdeaths_orig and td_pckrt to numeric, forcing non-numeric values to NA
   mutate(totdeaths_orig = as.numeric(totdeaths_orig),
          td_pckrt = as.numeric(td_pckrt)) %>%
   # Filter out rows where totdeaths_orig or td_nckrt are NA after conversion
   filter(!is.na(td_pckrt)) %>% #219 observations
   # Filter those IDs where total percentage is either low < 99 or more than 101%
   filter(td_pckrt < 99 | td_pckrt > 101) %>%
   select (Refid, totdeaths_orig, td_pckrt, starts_with("cod_p")) # 19 studies, 70 strata
  

 
 ## Check IDS where COD number is not directly reported in the article
# Total IDs where no COD number is reported 
 cod.nonum <- cod.deaths %>%
   filter(is.na(cod_n1) & cause_of_death1 != "") %>%
   count(Refid) # Total 48 studies, 251 strata
 
 # Study IDs where COD number is not reported but percentage is there cod_p1 is not missing
 cod.perc <- cod.deaths %>%
   filter(is.na(cod_n1) & cause_of_death1 != "") %>%
   filter(!is.na(cod_p1)) %>%
   count(Refid) # Total 43 studies, 219 strata

 # Study IDs where COD number is not reported but COD_mr1 (mortality rate) is not missing
 cod.mrate <- cod.deaths %>%
   filter(is.na(cod_n1) & cause_of_death1 != "") %>%
   filter(!is.na(cod_mr1)) %>%
   count(Refid) # 3 studies, 16 strata
 
 # Study IDs where COD number is not reported but COD_mro1 (mortality ratio) is not missing
 cod.mratio <- cod.deaths %>%
   filter(is.na(cod_n1) & cause_of_death1 != "") %>%
   filter(!is.na(cod_mro1)) %>%
   count(Refid) # 2 studies, 16 strata
 

 # Converting percentages, mortality rates and mortality ratios to numbers for CODs
 
 # Loop through each possible cod_n column
 cod.qc <- cod.deaths %>%
   # Step 1: Calculate cod_n using cod_p (percentage) and totdeaths_orig
   mutate(across(starts_with("cod_n"), ~ if_else(
     # Condition: cod_n is NA, cod_p is not NA, and totdeaths_orig is not NA
     is.na(.x) & !is.na(as.numeric(get(paste0("cod_p", sub("cod_n", "", cur_column()))))) & !is.na(as.numeric(totdeaths_orig)),
     # True: Calculate cod_n as (cod_p / 100) * totdeaths_orig (convert percentage to proportion)
     (as.numeric(get(paste0("cod_p", sub("cod_n", "", cur_column())))) / 100) * as.numeric(totdeaths_orig),
     # False: Retain the original value of cod_n
     .x
   ))) %>%
   
   # Step 2: Calculate cod_n using cod_mr, totdeaths_orig, and den_acme
   mutate(across(starts_with("cod_n"), ~ if_else(
     is.na(.x) & is.na(as.numeric(get(paste0("cod_p", sub("cod_n", "", cur_column()))))) & !is.na(as.numeric(totdeaths_orig)) & !is.na(as.numeric(get(paste0("cod_mr", sub("cod_n", "", cur_column()))))) & !is.na(as.numeric(den_acme)),
     (as.numeric(get(paste0("cod_mr", sub("cod_n", "", cur_column())))) * as.numeric(totdeaths_orig)) / as.numeric(den_acme),
     .x
   ))) %>%
   
   # Step 3: Calculate cod_n using cod_mro and lb
   mutate(across(starts_with("cod_n"), ~ if_else(
     is.na(.x) & is.na(as.numeric(get(paste0("cod_p", sub("cod_n", "", cur_column()))))) & !is.na(as.numeric(get(paste0("cod_mro", sub("cod_n", "", cur_column()))))) & !is.na(as.numeric(lb)),
     as.numeric(get(paste0("cod_mro", sub("cod_n", "", cur_column())))) * as.numeric(lb) / 1000,
     .x
   ))) 
 
 # Save output -------------------------------------------------------------
   
  write.csv(cod.qc, "./gen/process-distiller/output/cod qc.csv", row.names = FALSE)
   
 
