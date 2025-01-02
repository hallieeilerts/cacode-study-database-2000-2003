
################################################################
# Cleaning study characteristics outcome variables in the raw download from distiller
################################################################

#' Input: study characteristics data downloaded from distiller
#' Output: spreadsheet with cleaned column names

# Initialize environment --------------------------------------------------

rm(list = ls())

# installing the packages

install.packages(c("readr", "openxlsx", "dplyr", "readstata13", "gtsummary", "tidyr", "gt", "flextable", "stringr"))


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

sc <- read.xlsx("./data/distiller/study characteristics_120 studies_with fac.xlsx")

# Insert full country names in country column, and creating a new column of iso3 country codes
 sc <- sc %>%
   mutate(country = recode(country,
                           "AFG" = "Afghanistan",
                           "BDI" = "Burundi",
                           "BFA" = "Burkina Faso",
                           "BGD" = "Bangladesh",
                           "CHN" = "China",
                           "CMR" = "Cameroon",
                           "ETH" = "Ethiopia",
                           "GHA" = "Ghana",
                           "GMB" = "Gambia",
                           "HND" = "Honduras",
                           "HTI" = "Haiti",
                           "IDN" = "Indonesia",
                           "IND" = "India",
                           "IRN" = "Iran",
                           "IRQ" = "Iraq",
                           "KEN" = "Kenya",
                           "LAO" = "Lao PDR",
                           "MDG" = "Madagascar",
                           "MOZ" = "Mozambique",
                           "MWI" = "Malawi",
                           "NAM" = "Namibia",
                           "NER" = "Niger",
                           "NGA" = "Nigeria",
                           "NPL" = "Nepal",
                           "PAK" = "Pakistan",
                           "PNG" = "Papua New Guinea",
                           "RWA" = "Rwanda",
                           "SIL" = "Sierra Leone",
                           "TAN" = "Tanzania",
                           "TMP" = "Timor Leste",
                           "UGA" = "Uganda",
                           "ZAF" = "South Africa",
                           "ZAM" = "Zambia")) %>%
   mutate(iso3 = recode(country,
                        "Afghanistan" = "AFG",
                        "Burundi" = "BDI",
                        "Burkina Faso" = "BFA",
                        "Bangladesh" = "BGD",
                        "China" = "CHN",
                        "Cameroon" = "CMR",
                        "Ethiopia" = "ETH",
                        "Ghana" = "GHA",
                        "Gambia" = "GMB",
                        "Honduras" = "HND",
                        "Haiti" = "HTI",
                        "Indonesia" = "IDN",
                        "India" = "IND",
                        "Iran" = "IRN",
                        "Iraq" = "IRQ",
                        "Kenya" = "KEN",
                        "Lao PDR" = "LAO",
                        "Madagascar" = "MDG",
                        "Mozambique" = "MOZ",
                        "Malawi" = "MWI",
                        "Namibia" = "NAM",
                        "Niger" = "NER",
                        "Nigeria" = "NGA",
                        "Nepal" = "NPL",
                        "Pakistan" = "PAK",
                        "Papua New Guinea" = "PNG",
                        "Rwanda" = "RWA",
                        "Sierra Leone" = "SLE",
                        "Tanzania" = "TZA",
                        "Timor Leste" = "TLS",
                        "Uganda" = "UGA",
                        "South Africa" = "ZAF",
                        "Zambia" = "ZMB"))

 
 # Select columns required for database and rename as per Master study database_Jan 20 file
 
 sc.prep <- sc %>%
   select (Refid, article_id, iso3, country, study_location, Verbal.Autopsy.Algorithm, Facility, surveillance_start, surveillance_end, `Unit:`,age_range_youngest_yr, age_range_oldest_yr )%>%
   rename(year_start = surveillance_start, year_end = surveillance_end, age_units = `Unit:`, age_lb = age_range_youngest_yr, age_ub = age_range_oldest_yr,
          va_alg = Verbal.Autopsy.Algorithm)


 
# Save output -------------------------------------------------------------

write.csv(sc.prep, "./gen/process-distiller/output/study charc with fac.csv", row.names = FALSE)
