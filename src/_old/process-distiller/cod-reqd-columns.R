################################################################
# Cleaning CoD outcome variables in the raw download from distiller
################################################################

#' Input: Causes of death data downloaded from distiller
#' Output: spreadsheet with cleaned column names

# Initialize environment --------------------------------------------------

rm(list = ls())

# installing the packages

#install.packages(c("readr", "openxlsx", "dplyr", "readstata13", "gtsummary", "tidyr", "gt", "flextable", "stringr"))


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

cod <- read.xlsx("./data/distiller/CoD_ 120 studies_with facility.xlsx")


# Select columns required for database and rename as per Master study database_Jan 20 file and MR_7Oct2019 file last round

cod.prep <- cod %>%
  select (-c(User, Level, study_id, cod_loc_tab, cod_loc_pag)) 

# Save output -------------------------------------------------------------

write.csv(cod.prep, "./gen/process-distiller/output/study cod with fac.csv", row.names = FALSE)
