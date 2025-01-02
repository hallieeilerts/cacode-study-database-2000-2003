################################################################################
#' @description Manually sets age group and years being estimated.
#' @return Strings, booleans, integers, date defined below
################################################################################

#' Clear environment
rm(list = ls())

## Choose age/sex group
#ageSexSuffix <- "00to28d"
#ageSexSuffix <- "01to59m"
ageSexSuffix <- "05to09y"
#ageSexSuffix <- "10to14y"
#ageSexSuffix <- "15to19yF"
#ageSexSuffix <- "15to19yM"

## Set path to Data Warehouse for pulling latest data inputs
pathDataWarehouse <- "C:/Users/HEilerts/Institute of International Programs Dropbox/Hallie Eilerts-Spinelli/CA-CODE/CA-CODE_DataWarehouse"

## Results date (for naming output files)
resDate <- format(Sys.Date(), format="%Y%m%d")

## Sex labels
sexLabels <- c("Total", "Female", "Male")

## ID vars
idVars <- c("recnr", "id", "ref_id", "article_id", 
            "citation", "author",
            "strata_id", "strata_other1",
            "location_long", "location_short", "location_fac",
            "nationalrep", "reterm", 
            "va_alg", "va_mult_ind", "va_mult_id", "va_mult_n",
            "iso3", "countryname", "sex", "age_lb_m", "age_ub_m",
            "year_start", "year_end", "year_mid")


## Minimum and maximum deaths for 5-19y
minDeaths <- 15
maxDeaths <- 5000

