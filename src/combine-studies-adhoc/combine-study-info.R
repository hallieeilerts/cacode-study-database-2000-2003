################################################################################
#' @description Combine study characteristics and COD extractions from distiller
#' Remove facility studies, missing denominators, non-HMM countries for 5-19,
#' ensure presence of all ID variables
#' @return Study COD data with all ID columns
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
require(countrycode)
#' Inputs
source("./src/set-inputs.R")
## Study CODs with cleaned columns
studychar <- read.csv(paste0("./gen/combine-studies-adhoc/temp/study-char_clean-col.csv", sep = ""))
studycod <- read.csv(paste0("./gen/combine-studies-adhoc/temp/study-cod_clean-col.csv", sep = ""))
studyloc <- read.csv(paste0("./gen/combine-studies-adhoc/temp/study-loc_clean-col.csv", sep = ""))
################################################################################

## Merge study characteristics and cod
dat1 <- merge(studychar, studycod, by = "ref_id")
nrow(dat1) == nrow(studycod) # Didn't lose any rows in merge
nrow(dat1) # 550

# If no country name is listed, but the information is in strata_other1, replace it.
dat1$tempid <- 1:nrow(dat1)
df_nocountry <- subset(dat1, is.na(countryname))
df_other <- subset(dat1, !(tempid %in% df_nocountry$tempid))
# Recode countryname when strata_other1 has prefix of "country-"
# Match iso3
df_replace1 <- df_nocountry[grepl("country", df_nocountry$strata_other1),]
df_replace1$countryname <- sub(".*\\-", "", df_replace1$strata_other1)
df_replace1$iso3 <- countrycode(df_replace1$countryname, origin = "country.name", destination = "iso3c")
df_nocountry <- subset(df_nocountry, !(tempid %in% df_replace1$tempid))
# Recode countryname when strata_other1 has a country listed (without prefix)
df_replace2 <- df_nocountry[grepl("benin|burkina|cameroon|ghana|india|kenya|malawi|mali|niger|nigeria|pakistan|senegal|tanzania|zambia", df_nocountry$strata_other1, ignore.case = TRUE),]
df_replace2$countryname <- sub("\\-.*", "", df_replace2$strata_other1)
df_replace2$iso3 <- countrycode(df_replace2$countryname, origin = "country.name", destination = "iso3c")
df_nocountry <- subset(df_nocountry, !(tempid %in% df_replace2$tempid))
# Recombine
dat1 <- rbind(df_other, df_replace1, df_replace2, df_nocountry)
nrow(dat1) # 550
nrow(subset(dat1, !is.na(iso3))) # 540 (10 missing iso3)
nrow(subset(dat1, !is.na(countryname))) # 540 (10 missing country names)

# If "VA1 and VA2" are listed in VA alg, but the information is in strata_other1, replace it.
dat1$tempid <- 1:nrow(dat1)
df_multva <- dat1[grepl("and", dat1$va_alg, ignore.case = TRUE),]
df_other <- subset(dat1, !(tempid %in% df_multva$tempid))
df_multva$va_alg[grepl("EAVA|Expert", df_multva$strata_other1)] <- "EAVA"
df_multva$va_alg[grepl("PCVA|Physician", df_multva$strata_other1)] <- "PCVA"
df_multva$va_alg[grepl("Tariff", df_multva$strata_other1, ignore.case = TRUE)] <- "Tariff"
df_multva$va_alg[grepl("Medical Assistants", df_multva$strata_other1, ignore.case = TRUE)] <- "PCVA"
df_multva$va_alg[grepl("SmartVA", df_multva$strata_other1, ignore.case = TRUE)] <- "SmartVA"
df_multva$va_alg[grepl("InterVA", df_multva$strata_other1, ignore.case = TRUE)] <- "InterVA"
df_multva$va_alg[df_multva$article_id == "R202210800"] <- "EAVA" 
# Ad-hoc corrections
df_other$va_alg[df_other$strata_id == "R202214959-01"] <- "EAVA"
df_other$va_alg[df_other$strata_id == "R202214959-02"] <- "PCVA"
# Recombine
dat1 <- rbind(df_other, df_multva)
dat1$tempid <- NULL
nrow(dat1) # 550

# Merge study locations
dat <- merge(dat1, studyloc, by = c("ref_id","strata_id"))
nrow(dat1) == nrow(dat)
nrow(dat) # 540
# Lost the 10 study data points corresponding to studies where CODs are aggregated across multiple countries.
# This is ok.

# Drop rows that are (1) missing cod_n due to missing denominators or (2) facility studies
dat <- dat %>%
  filter(remove==0) %>%
  select(-c(Facility, remove))
nrow(dat) # 305

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/combine-studies-adhoc/temp/studies_wide.csv",sep =""), row.names = FALSE)

