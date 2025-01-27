################################################################################
#' @description Harmonize names for old study id columns with new study data
#' @return Old study COD data with all ID columns necessary for combining with new study data
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
library(stringr)
library(readstata13)
#' Inputs
source("./src/set-inputs.R")
## Old study databases
if(ageSexSuffix %in% c("00to28d")){
  dat_filename <- list.files("./data/study-data-old")
  dat_filename <- dat_filename[grepl("neonates", dat_filename)]
  dat  <- read.dta13(paste0("./data/study-data-old/", dat_filename, sep = ""), nonint.factors = T)
}
if(ageSexSuffix %in% c("01to59m")){
  dat_filename <- list.files("./data/study-data-old")
  dat_filename <- dat_filename[grepl("combined", dat_filename)]
  dat <- read.dta13(paste0("./data/study-data-old/", dat_filename, sep = ""), nonint.factors = T)
}
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF","15to19yM")){
  dat <- read.csv(paste0("./gen/recover-studies2019-5to19y/output/Studies2019_", ageSexSuffix, ".csv", sep = ""))
}
################################################################################

# Rename columns
# Remove unnecessary columns

if(ageSexSuffix %in% c("00to28d")){
  
  dat$nationalrep <- 0
  dat$nationalrep[dat$natrep == 1] <- 1
  
  dat <- dat %>%
    mutate(recnr = 1:n(),
           ref_id = studyid,
           strata_id = paste("R2019",studyid, sid, sep = "-"), # must be unique. studyid is not.
           location_short = sub('\\..*', '', lab),
           sex = sexLabels[1]) %>%
    rename(article_id = studyid,
           iso3 = isocode,
           location_long = lab,
           countryname = country,
           year_mid = year,
    ) %>%
    select(-c(sid, natrep, studyid_1, period, per_early, per_late, regSSA, regSA, premvslbw, first, last, N)) 
}


if(ageSexSuffix %in% c("01to59m")){
  
  dat$nationalrep[is.na(dat$nationalrep)] <- 0
  dat$reterm <- ifelse(dat$nationalrep == 1, dat$iso3, paste(dat$iso3, ".", dat$study_id))
  
  dat$location_char <- dat$study_location
  dat$location_short <- dat$location_char
  dat$location_char_n <- lengths(gregexpr("\\W+", dat$location_char))
  dat$location_char_capwords <- unlist(lapply(str_extract_all(dat$location_char, "\\b[A-Z]\\w+"), function(x) x[1]))
  dat$location_short[dat$location_char_n > 4 & !is.na(dat$location_char_capwords)] <- dat$location_char_capwords[dat$location_char_n > 4 & !is.na(dat$location_char_capwords)]
  #View(dat[,c("study_id","study_location","location_char_n","location_char_capwords","location_short")])

  dat$location_short[dat$study_id == "2803"] <- "Upper River Division, Gambia"
  dat$location_short[dat$study_id == "4001"] <- "Upper River Division, Gambia"
  dat$location_short[dat$study_id == "GM011_10"] <- "Farafenni HDSS"
  dat$location_short[dat$study_id == "GM011_2"] <- "Farafenni HDSS"
  dat$location_short[dat$study_id == "GM011_3"] <- "Farafenni HDSS"
  dat$location_short[dat$study_id == "GM011_4"] <- "Farafenni HDSS"
  dat$location_short[dat$study_id == "GM011_5"] <- "Farafenni HDSS"
  dat$location_short[dat$study_id == "GM011_6"] <- "Farafenni HDSS"
  dat$location_short[dat$study_id == "GM011_7"] <- "Farafenni HDSS"
  dat$location_short[dat$study_id == "GM011_8"] <- "Farafenni HDSS"
  dat$location_short[dat$study_id == "GM011_9"] <- "Farafenni HDSS"
  dat$location_short[dat$study_id == "R2013-1265501"] <- "Africa Centre DSS"
  dat$location_short[dat$study_id == "R2013-5376-01"] <- "North Bank Region, Gambia"
  dat$location_short[dat$study_id == "R2013-5376-01"] <- "North Bank Region, Gambia"
  dat$location_short[dat$study_id == "R20170341602"] <- "Western Cape, KwaZulu Natal"
  dat$location_short[dat$study_id == "R20170341604"] <- "Western Cape, KwaZulu Natal"
  dat$location_short[dat$study_id == "R20170653901"] <- "Kilite Awlaelo HDSS"
  dat$location_short[dat$study_id == "R20174751128"] <- "Bandafassi HDSS"
  dat$location_short[dat$study_id == "R20174751129"] <- "Bandafassi HDSS"
  dat$location_short[dat$study_id == "R20174751130"] <- "Bandafassi HDSS"
  dat$location_short[dat$study_id == "R20174751131"] <- "Bandafassi HDSS"
  dat$location_short[dat$study_id == "R20174751132"] <- "Bandafassi HDSS"
  dat$location_short[dat$study_id == "R20174751133"] <- "Bandafassi HDSS"
  dat$location_short[dat$study_id == "R20176000004"] <- "Indonesia SRS"
  dat$location_short[dat$study_id == "R20176000005"] <- "Indonesia SRS"
  dat$location_short[dat$study_id == "R20176001101"] <- "North Gondar"
  dat$location_short[dat$study_id == "R20176001202"] <- "Upper River Division, Gambia"
  dat$location_short[dat$study_id == "R20176001203"] <- "Upper River Division, Gambia"
  dat$location_short[dat$study_id == "R20176001204"] <- "Upper River Division, Gambia"
  dat$location_short[dat$study_id == "R20176001208"] <- "Upper River Division, Gambia"
  dat$location_short[dat$study_id == "R20176001256"] <- "Upper River Division, Gambia"
  
  # Manual correct of age_lb_m and age_ub_m (which were 2, 1)
  # doi: https://doi.org/10.1136/bmj.301.6743.103 
  # V Fauveau, B Wojtyniak, J Chakraborty, A M Sarder, A Briend
  # British Medical Journal 1990; 301 
  dat$age_lb_m[dat$study_id == "R20176002102"] <- 6
  dat$age_ub_m[dat$study_id == "R20176002102"] <- 35

  dat <- dat %>%
    mutate(recnr = 1:n(),
           ref_id = study_id,
           strata_id = study_id, # must be unique
           sex = sexLabels[1],
           year_mid = floor(year_start + (year_end - year_start)/2)) %>%
    rename(article_id = study_id,
           location_long = study_location,
           countryname = country,
           strata_other1 = other_strata_1
    )   %>%
    select(-c(whoregion, age_lb_spec, age_ub_spec,
              pdia, pinj, pmal, pneo, poth, ppne, pcon, pmen, ptot,
              new_R, comment, BN))
  
}



if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF", "15to19yM")){
  
  dat$nationalrep <- 0
  dat$nationalrep[dat$nation_rep == 1] <- 1
  
  # Create location_short
  dat$location_short <- dat$location_char
  dat$location_char_n <- lengths(gregexpr("\\W+", dat$location_char))
  dat$location_char_capwords <- unlist(lapply(str_extract_all(dat$location_char, "\\b[A-Z]\\w+"), function(x) x[1]))
  dat$location_short[dat$location_char_n > 4] <- dat$location_char_capwords[dat$location_char_n > 4]
  dat$location_short[dat$study_id == "R201708807801"] <- "Haryana"
  dat$location_short[dat$study_id == "R201702968401"] <- "Haryana"
  dat$location_short[dat$study_id == "R201703262201"] <- "Andhra Pradesh"
  dat$location_short[dat$study_id == "SN011_CODA_3"] <- "Bandafassi"
  dat$location_short[dat$study_id == "SN011_CODA_2"] <- "Bandafassi"

  dat <- dat %>%
    mutate(recnr = 1:n(),
           age_lb_m = age_lb*12,
           age_ub_m = age_ub*12,
           sex = ifelse(sex == "B", substr(sexLabels[1],1,1), sex),
           strata_id = paste(study_id, sid, sep = "-")) %>% # must be unique. study_id is not.
    rename(ref_id = Refid,
           article_id = study_id,
           location_long = location_char,
           countryname = whoname,
           year_mid = year,
    ) %>%
    select(-c(location_char_n, location_char_capwords, nation_rep, RecNr, strata_ur, comment,new_R,
              VR,  wbinc15, wbreg13, whocode, whoreg6, age_lb, age_ub, idAux)) 
}

## Ensure presence of all idvars

v_missing_id <- idVars[!(idVars %in% names(dat))]
if(length(v_missing_id) > 0){
  for(i in 1:length(v_missing_id)){
    dat$newcol <- NA
    names(dat)[which(names(dat) == "newcol")] <- v_missing_id[i]
  }
}

## Create location_fac for under-5

# location_fac was already created in recover-study-id for 5-19
if(ageSexSuffix %in% c("00to28d", "01to59m")){
  dat$location_fac <- as.numeric(relevel(factor(dat$location_short), ref = sort(unique(dat$location_short))[1]))
  
  # Create informative ID
  # Keep ID the same for old 5-19 studies so the data points match the old model input.
  dat$id <- paste(dat$iso3, dat$year_mid, dat$strata_id, dat$location_fac,
                  round(dat$age_lb_m), round(dat$age_ub_m), substr(dat$sex,1,1), sep = '-')
  dat$id <- gsub("NA-NA", ageSexSuffix, dat$id) # If no age variables in ID, replace with ageSexSuffix
  
}

# Check id is unique
if(!(nrow(dat) == length(unique(dat$id)))){
  warning("Informative ID is not unique")
}

# Check that whatever variable has been re-coded as strata_id is unique
if(!(nrow(dat) == length(unique(dat$strata_id)))){
  warning("Variable recoded as strata_id is not unique")
}

# Check that age_lb_m is always less than age_ub_m
if(nrow(subset(dat, age_lb_m > age_ub_m))>0){
  warning("age_lb_m is greater than age_ub_m")
}
# Check no negative ages
if(nrow(subset(dat, age_lb_m < 0))>0){
  warning("negative age_lb_m")
}
if(nrow(subset(dat, age_ub_m < 0))>0){
  warning("negative age_ub_m")
}

# Tidy
v_other_col <- names(dat)[!(names(dat) %in% idVars)]
dat <- dat[,c(idVars, v_other_col)]
dat <- dat[order(dat$id),]

# Save output -------------------------------------------------------------

# Old model input that now has covariate names and scales as pred database
write.csv(dat, paste0("./gen/update-old-studies/temp/studies_set-id_", ageSexSuffix, ".csv"), row.names = FALSE)

