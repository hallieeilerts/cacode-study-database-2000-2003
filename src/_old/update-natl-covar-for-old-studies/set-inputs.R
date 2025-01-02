################################################################################
#' @description Manually sets age group and years being estimated.
#' @return Strings, booleans, integers, date defined below
################################################################################

#' Clear environment
rm(list = ls())

## Choose age/sex group ## FORMERLY LABEL ageGroup
## ageSexSuffix <- "00to28" # Not working yet
## ageSexSuffix <- "01to04" # Not working yet
# ageSexSuffix <- "05to09y"
# ageSexSuffix <- "10to14y"
ageSexSuffix <- "15to19yF"
# ageSexSuffix <- "15to19yM"

## Set years for update
Years <- 2000:2023

## Results date (for naming output files)
resDate <- format(Sys.Date(), format="%Y%m%d")
# resDate <- "20231002" # Set manually

## Sex labels
sexLabels <- c("Total", "Female", "Male") 

## ID vars
idVars <- c("iso3", "year", "sex")

## Path to Data Warehouse
pathDataWarehouse <- "C:/Users/HEilerts/Institute of International Programs Dropbox/Hallie Eilerts-Spinelli/CA-CODE/CA-CODE_DataWarehouse"
