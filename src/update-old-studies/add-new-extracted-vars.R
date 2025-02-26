################################################################################
#' @description Add new information that was extracted for old studies
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
library(stringr)
library(readxl)
library(stringi)
library(readstata13)
#' Inputs
source("./src/set-inputs.R")
if(ageSexSuffix %in% c("00to28d")){
  ## Old study database
  dat_filename <- list.files("./data/study-data-old")
  dat_filename <- dat_filename[grepl("neonates", dat_filename)]
  studies <- read.dta13(paste0("./data/study-data-old/", dat_filename, sep = ""), nonint.factors = T)
  ## Newly extracted variables for old studies
  dat_filename <- list.files("./data/study-data-old-augmented")
  dat_filename <- dat_filename[grepl("neonat", dat_filename)]
  extract <- read_excel(paste0("./data/study-data-old-augmented/", dat_filename, sep = ""))
}
if(ageSexSuffix %in% c("01to59m")){
  ## Old study database
  dat_filename <- list.files("./data/study-data-old")
  dat_filename <- dat_filename[grepl("combined", dat_filename)]
  studies <- read.dta13(paste0("./data/study-data-old/", dat_filename, sep = ""), nonint.factors = T)
  ## Newly extracted variables for old studies
  dat_filename <- list.files("./data/study-data-old-augmented")
  dat_filename <- dat_filename[grepl("1to59", dat_filename)]
  extract <- read_excel(paste0("./data/study-data-old-augmented/", dat_filename, sep = ""))
}
################################################################################

if(ageSexSuffix %in% c("00to28d")){
  
  df_extract <- extract %>%
    mutate(
      age_lb_m = ifelse(period %in% c("early", "overall"), 0, 7/ 30),
      age_ub_m = ifelse(period == "early", 7/30, 28/30),
      author = gsub("(.+?)(\\,.*)", "\\1", citation)
    ) %>%
    rename_with(
      ~ case_when(
        . == "VA.algorithm" ~ "va_alg",
        . == "va_alg_stat" ~ "va_alg_src",
        . == "sid" ~ "study_id",
        . == "study_loc" ~ "location_long",
        TRUE ~ .)) %>%
    select(c("study_id", "va_alg", "va_alg_src", "age_lb_m", "age_ub_m", "citation", "author", "location_long"))
  
  # remove special characters
  df_extract$author[grepl("http", df_extract$author, ignore.case = TRUE)] <- NA
  df_extract$author <- stri_trans_general(df_extract$author, "Latin-ASCII")
  df_extract$citation <- stri_trans_general(df_extract$citation, "Latin-ASCII")
  
  
  df_studies <- studies %>%
    mutate(country = stri_trans_general(country, "Latin-ASCII")) %>%
    rename_with(
      ~ case_when(
        . == "sid" ~ "study_id",
        TRUE ~ .)) %>%
    mutate(study_id = trimws(study_id))
  
}

if(ageSexSuffix %in% c("01to59m")){
  
  df_extract <- extract %>%
    rename(
      "va_alg" = "VA.algorithm",
      "va_alg_src" = "va_alg_stat"
    ) %>%
    select(c("study_id", "va_alg", "va_alg_src", "age_lb_m", "age_ub_m"))
  
  # remove accents/special characters before saving to csv
  df_studies <- studies %>%
    mutate(country = stri_trans_general(country, "Latin-ASCII"),
           author = stri_trans_general(author, "Latin-ASCII"),
           citation = stri_trans_general(citation, "Latin-ASCII")) %>%
    mutate(study_id = trimws(study_id))
  
}

dat <- merge(df_studies, df_extract, by = "study_id", all.x = TRUE)

# Check if newly extracted information is available for all data points
#View(dat[,c("study_id", "iso3","citation","VA.algorithm","va_alg_src","age_lb_m.x","age_ub_m.x","age_lb_m.y","age_ub_m.y")])
#View(subset(dat, is.na(age_ub_m.y))[,c("study_id", "iso3","citation","va_alg","va_alg_src","age_lb_m.x","age_ub_m.x","age_lb_m.y","age_ub_m.y")])
#View(subset(dat, is.na(va_alg))[,c("study_id", "iso3","citation","va_alg","va_alg_src","age_lb_m.x","age_ub_m.x","age_lb_m.y","age_ub_m.y")])
nrow(subset(dat, is.na(va_alg))) 
# 3 missing VA information in 1-59m age group
# Astha identified these as duplicates and therefore didn't bother extracting new information

# Use old information when missing
if(ageSexSuffix %in% c("01to59m")){
  # This variable was already in the old 1-59m studies but not neonate
  dat$age_lb_m <- dat$age_lb_m.y
  dat$age_lb_m[is.na(dat$age_lb_m)] <- dat$age_lb_m.x[is.na(dat$age_lb_m)] 
  dat$age_ub_m <- dat$age_ub_m.y
  dat$age_ub_m[is.na(dat$age_ub_m)] <- dat$age_ub_m.x[is.na(dat$age_ub_m)] 
}

nrow(subset(dat, is.na(va_alg_src))) # 0 in 1-59m age group
dat$va_alg_src[is.na(dat$va_alg)] <- "study"

nrow(subset(dat, is.na(va_alg))) # 3 in 1-59m age group
dat$va_alg[is.na(dat$va_alg)] <- "Not reported"

nrow(dat) == nrow(studies)

if(ageSexSuffix %in% c("00to28d")){
dat <- dat %>%
  rename_with(
    ~ case_when(
      . == "study_id" ~ "sid", # Convert back to sid for neonate studies
      TRUE ~ .)) 
}

# Remove extra age columns
dat <- dat %>%
  select(-any_of(c("age_lb_m.x", "age_lb_m.y", "age_ub_m.x", "age_ub_m.y")))

# Save output -------------------------------------------------------------

# Old model input that now has covariate names and scales as pred database
write.csv(dat, paste0("./gen/update-old-studies/temp/studies_add-extracted-vars_", ageSexSuffix, ".csv"), row.names = FALSE)
