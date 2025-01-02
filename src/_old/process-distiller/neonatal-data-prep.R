################################################################
# Reshape and create the neonatal clean data
################################################################

#' Input: Harmonized causes and cod long file
#' Output: Neonatal clean data

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

# Import the mapped causes file
causes.map <- read.csv("./gen/process-distiller/audit/full_causemapping20240926.csv", na.strings = "", check.names = F)
# Import the cod long file
cod.long <- read.csv("./gen/process-distiller/audit/cod long_before exclude.csv", na.strings = "", check.names = F)

# Creating cod data only for neonates

## selecting only neonates data from causes mapped file and cod.long file
causes.map.nn <- causes.map %>%
  select(cause_of_death, Neonates)

cod.nn <- cod.long %>%
  filter(inc_neo==1 & age_ub_m <= 2)

## Merge causes mapped file to cod.nn file

cod.nn <- cod.nn %>%
  left_join(causes.map.nn, by = "cause_of_death")

#Remove the columns not reqd and make chnges in Neonates column
cod.nn <- cod.nn %>%
  select (-c(inc_neo, inc_1to59, inc_5to9y, inc_10to14y, inc_15to19y.m, inc_15to19y.f, cause_number, cause_of_death, cod_p, cod_mr, cod_mro))%>%
  mutate (Neonates = ifelse(Neonates == "NA" | Neonates == "typo?" | Neonates == "mal", "exclude", Neonates))%>%
  mutate(cod_n = as.numeric(cod_n))

## Turn the data into a wide format

cod.nn.wide <- cod.nn %>%
  pivot_wider(
    names_from = Neonates, 
    values_from = cod_n,
    values_fn = sum,
    names_sep = "_",  # Ensures that names are unique
    names_repair = "unique"  # Ensures unique column names
  )

# Preparing the cod.nn.wide data
## Adding a column for each cause to see if that cause has been reported or not

cod.nn.wide <- cod.nn.wide %>%
  mutate(sep = ifelse(is.na(sepsis_mening) | sepsis_mening==0, 0,1))%>%
  mutate(lr = ifelse(is.na(lri) | lri==0, 0,1))%>%
  mutate(hi = ifelse(is.na(hiv) | hiv==0, 0,1))%>%
  mutate(di = ifelse(is.na(dia) | dia==0, 0,1))%>%
  mutate(ot = ifelse(is.na(`other neonatal`) | `other neonatal`==0, 0,1))%>%
  mutate(pr = ifelse(is.na(preterm) | preterm==0, 0,1))%>%
  mutate(ip = ifelse(is.na(intrapartum) | intrapartum==0, 0,1))%>%
  mutate(co = ifelse(is.na(congen) | congen==0, 0,1))%>%
  mutate(inj = ifelse(is.na(injuries) | injuries==0, 0,1))%>%
  mutate(tet = ifelse(is.na(tetanus) | tetanus==0, 0,1))%>%
  mutate(sum_cau = rowSums(across(sep:tet), na.rm = TRUE))%>%
  mutate(remove = ifelse (sum_cau < 2, 1, remove))%>%
  filter(remove==0)



# Create a new column for sum of causes
nn.sum <- cod.nn.wide %>%
  mutate(sum_causes = rowSums(across(sepsis_mening:tetanus), na.rm = TRUE)) %>%
  mutate(totdeaths_orig = as.numeric(as.character(totdeaths_orig)))%>%
  mutate (exclude = as.numeric(as.character(exclude)))%>%
  mutate(exclude = as.numeric(ifelse(is.na(exclude), 0, exclude)))%>%
  mutate(totdeaths_orig = totdeaths_orig - exclude) %>%
  mutate(sum_causes = sum_causes - exclude)%>%
  select(-c (Facility, remove, exclude))
  

# Rescale the causes to tal deaths number if sum of causes is > total deaths
cause <- c("sepsis_mening", "lri", "hiv", "dia", "other neonatal", "preterm", "intrapartum", "congen", "injuries", "tetanus")
  
rescale <- nn.sum %>%
  rowwise() %>%
  mutate(across(all_of(cause), 
           ~ if(sum_causes > totdeaths_orig) round((.x / sum_causes) * totdeaths_orig,2) else round(.x,2)))%>%  
  ungroup()

# Remove columns not required

neo <- rescale %>%
  select(-c(sep:sum_causes))

#Save output -------------------------------------------------------------

write.csv(neo, "./gen/process-distiller/output/neonatal_upd.csv", na = "", row.names = FALSE)




 