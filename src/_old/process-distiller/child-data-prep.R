################################################################
# Reshape and create the 1-59mo clean data
################################################################

#' Input: Harmonized causes and cod long file
#' Output: 1-59mo clean data

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
causes.map.ch <- causes.map %>%
  select(cause_of_death, `1-59m`)

cod.ch <- cod.long %>%
  filter(inc_1to59==1)

## Merge causes mapped file to cod.ch file

cod.ch <- cod.ch %>%
  left_join(causes.map.ch, by = "cause_of_death")

#Remove the columns not reqd and make chnges in 1-59m column
cod.ch <- cod.ch %>%
  select (-c(inc_neo, inc_1to59, inc_5to9y, inc_10to14y, inc_15to19y.m, inc_15to19y.f, cause_number, cause_of_death, cod_p, cod_mr, cod_mro))%>%
  mutate (`1-59m` = ifelse(`1-59m` == "NA" | `1-59m` == "typo?", "exclude", `1-59m`))%>%
  mutate(cod_n = as.numeric(cod_n))

## Turn the data into a wide format

cod.ch.wide <- cod.ch %>%
  pivot_wider(
    names_from = "1-59m", 
    values_from = cod_n,
    values_fn = sum,
    names_sep = "_",  # Ensures that names are unique
    names_repair = "unique"  # Ensures unique column names
  )

# Preparing the cod.ch.wide data
## Adding a column for each cause to see if that cause has been reported or not

cod.ch.wide <- cod.ch.wide %>%
  mutate(ot = ifelse(is.na(other) | other==0, 0,1))%>%
  mutate(lr = ifelse(is.na(lri) | lri==0, 0,1))%>%
  mutate(hi = ifelse(is.na(hiv) | hiv==0, 0,1))%>%
  mutate(di = ifelse(is.na(dia) | dia==0, 0,1))%>%
  mutate(ma = ifelse(is.na(mal) | mal==0, 0,1))%>%
  mutate(me = ifelse(is.na(mea) | mea==0, 0,1))%>%
  mutate(men = ifelse(is.na(mening_enceph) | mening_enceph==0, 0,1))%>%
  mutate(co = ifelse(is.na(congen) | congen==0, 0,1))%>%
  mutate(neo = ifelse(is.na(neonatal) | neonatal==0, 0,1))%>%
  mutate(inj = ifelse(is.na(injuries) | injuries==0, 0,1))%>%
  mutate(sep = ifelse(is.na(sepsis) | sepsis==0, 0,1))%>%
  mutate(sum_cau = rowSums(across(ot:sep), na.rm = TRUE))%>%
  mutate(remove = ifelse (sum_cau < 2, 1, remove))%>%
  filter(remove==0)


# Create a new column for sum of causes
ch.sum <- cod.ch.wide %>%
  mutate(sum_causes = rowSums(across(other:sepsis), na.rm = TRUE)) %>%
  mutate(totdeaths_orig = as.numeric(as.character(totdeaths_orig)))%>%
  mutate (exclude = as.numeric(as.character(exclude)))%>%
  mutate(exclude = as.numeric(ifelse(is.na(exclude), 0, exclude)))%>%
  mutate(totdeaths_orig = totdeaths_orig - exclude) %>%
  mutate(sum_causes = sum_causes - exclude)%>%
  select(-c (Facility, remove, exclude))


# Rescale the causes to tal deaths number if sum of causes is > total deaths
cause_child <- c("other", "lri", "hiv", "dia", "mal", "mea", "mening_enceph", "congen", "neonatal", "injuries", "sepsis")

rescale.ch <- ch.sum %>%
  rowwise() %>%
  mutate(across(all_of(cause_child), 
                ~ if(sum_causes > totdeaths_orig) round((.x / sum_causes) * totdeaths_orig,2) else round(.x,2)))%>%  # Adjust causes if sum_causes > totdeaths
  ungroup()

# Remove columns not required

child <- rescale.ch %>%
  select(-c(ot:sum_causes))

#Save output -------------------------------------------------------------

write.csv(child, "./gen/process-distiller/output/1to59mo_data.csv", na = "", row.names = FALSE)
