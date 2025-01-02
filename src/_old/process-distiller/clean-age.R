################################################################
# Cleaning CoD age variables in the gen file
################################################################

#' Input: Causes of death data with required columns 
#' Output: spreadsheet with cleaned age variables in COD strata

# Initialize environment --------------------------------------------------

rm(list = ls())

# installing the packages

# install.packages(c("readr", "openxlsx", "dplyr", "readstata13", "gtsummary", "tidyr", "gt", "flextable", "stringr"))


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

# load the data

cod.age <- read.csv("./gen/process-distiller/output/study cod with fac.csv")

# Inspecting the distribution of ages in data
dist.lb <- cod.age %>%
  group_by(age_unit, age_lb)%>%
  tally

dist.ub <- cod.age %>%
  group_by(age_unit, age_ub)%>%
  tally


# Generating and Replacing Age Lower Bound in Months (age_lb_m)
cod.age <- cod.age %>%
  mutate(age_lb_m = case_when(
    age_unit == "months" ~ age_lb,           # Already in months
    age_unit == "days" ~ age_lb / 30,      # Converting days to months
    age_unit == "years" ~ age_lb * 12))       # Converting years to months
   
    

dist.lb.m <- cod.age %>%
  group_by(age_unit, age_lb, age_lb_m)%>%
  tally


# Generating and Replacing Age Upper Bound in Months (age_ub_m)
cod.age <- cod.age %>%
  mutate(age_ub_m = case_when(
    age_unit == "months" ~ age_ub,           # Already in months
    age_unit == "days" ~ age_ub / 30,      # Converting days to months
    age_unit == "years" ~ age_ub * 12))      # Converting years to months


dist.ub.m <- cod.age %>%
  group_by(age_unit, age_ub, age_ub_m)%>%
  tally

# Checking the distribution of lower and upper bound of ages
dist.age <- cod.age %>%
  group_by(age_lb_m, age_ub_m) %>%
  tally

# Save output -------------------------------------------------------------

write.csv(cod.age, "./gen/process-distiller/output/cod clean age.csv", row.names = FALSE)
