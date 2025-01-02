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
studychar <- read.csv(paste0("./gen/process-new-studies/temp/study-char_clean-col.csv", sep = ""))
studycod <- read.csv(paste0("./gen/process-new-studies/temp/study-cod_clean-col.csv", sep = ""))
studyloc <- read.csv(paste0("./gen/process-new-studies/temp/study-loc_clean-col.csv", sep = ""))
## Key with model classification for hmm/lmm countries
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("modelclass", dat_filename, ignore.case = TRUE)]
key <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
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

# Harmonize other algorithms
dat1$va_alg[grepl("Physician", dat1$va_alg, ignore.case = TRUE)] <- "PCVA"
dat1$va_alg[grepl("Other", dat1$va_alg, ignore.case = TRUE)] <- "Other"
dat1$va_alg[grepl("Method|Cannot", dat1$va_alg, ignore.case = TRUE)] <- "Not reported"

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

# !!! NOTE: CHECK IF DAVID AND JAMIE WANT THIS APPLIED TO NEONATES AND POSTNEONATES

if(ageSexSuffix %in% c("05to09y", "10to14y", "15to19yF", "15to19yM")){
  # Remove data points from COUNTRIES NOT VA
  v_keep <- subset(key, Group %in% c('HMM/LMM', 'HMM'))$ISO3
  dat <- subset(dat, iso3 %in% v_keep)
  nrow(dat) # 252
}

# Create reterm
dat$reterm <- paste(dat$iso3, dat$article_id, sep = "-")
dat$reterm[dat$nationalrep == 1] <- dat$iso3[dat$nationalrep == 1]

# Create location factor
dat$location_fac <- as.numeric(relevel(factor(dat$location_short), ref = sort(unique(dat$location_short))[1]))

# Create informative ID
dat$id <- paste(dat$iso3, dat$year_mid, dat$strata_id, dat$location_fac,
                round(dat$age_lb_m), round(dat$age_ub_m), substr(dat$sex,1,1), sep = '-')
dat <- dat[,c("id", names(dat)[!(names(dat) %in% "id")])]
length(unique(dat$id)) == nrow(dat)

# Ensure presence of all idvars
v_missing_id <- idVars[!(idVars %in% names(dat))]
# Should only contain "recnr" and indicators for study data points with multiple VA applied
if(length(v_missing_id) > 0){
  for(i in 1:length(v_missing_id)){
    dat$newcol <- NA
    names(dat)[which(names(dat) == "newcol")] <- v_missing_id[i]
  }
}

# Tidy
v_other_col <- names(dat)[!(names(dat) %in% idVars)]
dat <- dat[,c(idVars, v_other_col)]
dat <- dat[order(dat$id),]

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/process-new-studies/temp/studies_wide_",ageSexSuffix,".csv",sep =""), row.names = FALSE)

