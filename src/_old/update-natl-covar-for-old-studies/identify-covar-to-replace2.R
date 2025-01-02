################################################################################
#' @description Match study covariate names in 2000-2020 model objects to 2023 prediction database
#' @return Key with HMM-StudyDatabase study/covariate rows and whether replacement with prediction database called for
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readstata13)
library(tidyverse)
library(data.table)
#' Inputs
source("./src/update-natl-covar-for-old-studies/set-inputs.R")

# 2020 study database with new id variable
dat <- read.csv(paste0("./gen/update-natl-covar-for-old-studies/output/ModInput2020-UpdatedNamesScales_", ageSexSuffix, ".csv",sep=""))

# Data extraction from dhs
dhs <- read.csv(paste0("./data/dhs/StudyDataDHS-long_20241004.csv",sep=""))

# Key matching old study and new pred covariate names
key_covarnames <- read.csv("./gen/update-natl-covar-for-old-studies/temp/key_study-pred-covarnames.csv")

################################################################################

df_replace <- dat %>% 
    pivot_longer(cols = !all_of(c("id","sid","reterm","totdeaths")),
                 names_to = c("variable"),
                 values_to = "value"
    ) %>%
  mutate(iso3 = substr(id, 1, 3),
         year = substr(id, 5, 8))

# # Reshape source columns long and match on key for prediction database covariate names
# df_src <- dat %>% select("iso3","id", ends_with("_source")) %>%
#   rename_with(function(c) str_replace(c, "_source", "")) %>% 
#   pivot_longer(cols = !all_of(c("id","iso3")),
#                names_to = c("variable"), 
#                values_to = "source"
#   ) 

# Using DHS data extraction
# Identify covariates that came from DHS survey and are subnational
v_dhs_covar <- unique(dhs$variable)
# Manually add covariates that have different names in dhs extraction
# Need to harmonize these names. Make dhs extraction match prediction database. Prediction database should take precedent.
v_dhs_covar <- c(v_dhs_covar, "vac_pab", "stunting", "underweight")
# Manually add covariates that aren't in dhs extraction this time (because not included in any model),
# but likely were pulled from dhs subnational in the past
v_dhs_covar <- c(v_dhs_covar, "imr_mf")

# Identify source of data points: 
# Article, DHS, and MICS data points do not need to be updated
df_src <- df_replace %>%
  mutate(source_type = ifelse( variable %in% v_dhs_covar, "Survey", NA)) %>%
  mutate(source_type = ifelse( is.na(source_type) & variable %in% key_covarnames$pred, "Prediction Database", source_type )) %>%
  mutate(source_type = ifelse( iso3 %in% "IND" &  !(variable %in% key_covarnames$pred) & variable %in% key_covarnames$pred_ind,
                               "India Subnational Prediction Database", source_type )) %>%
  mutate(replace = ifelse(source_type %in% c("Prediction Database"), TRUE, FALSE))

# Remove sex suffix
df_src$indicator <- sub("_f$|_m$|_mf$", "", df_src$variable)

# If any variable in the sex suffix group is from the survey, they should all be from survey
df_src <- df_src %>% 
  mutate(anysurv = ifelse(source_type == "Survey", 1, 0)) %>%
  group_by(indicator) %>%
  mutate(anysurv = max(anysurv))
df_src$source_type[df_src$anysurv == 1] <- "Survey"
df_src$replace[df_src$anysurv == 1] <- FALSE
df_src$anysurv <- df_src$indicator <-NULL

# Check
#View(subset(df_src, id == "IND-1982-27360-1-5-14-M"))

nrow(dat) == length(unique(df_src$id))

# Save output -------------------------------------------------------------

write.csv(df_src, paste0("./gen/update-natl-covar-for-old-studies/temp/key_study-covar-replacement_",ageSexSuffix,".csv",sep =""), row.names = FALSE)
