
################################################################
# Match covariate names from 2020 study database to 2021 India prediction database
################################################################

#' Output: key with study db covariate name matched to subnational India pred db covariate name

# Initialize environment --------------------------------------------------

rm(list = ls())
source("./src/util.R")

# Load data ---------------------------------------------------------------

# 2020 study database
study <- readstata13::read.dta13("./data/previous-database/20200930-HMM-StudyDatabase.dta")

# 2021 prediction database
pred <- read.csv("./data/prediction-database/PredicationDatabaseIND_2022series_2023-12-12.csv")

# Match covariate names ---------------------------------------------------

# Study database: take covariate names from source column
scovar <- study %>% select(ends_with("_source")) %>% 
  rename_with(function(c) str_replace(c, "_source", "")) %>%
  names()

# Study covariates that aren't used in model
# These were not included in the 2021 prediction database because they were not used in the model
v_unused <- c("vehicles", 
              "cannabis_f","cannabis_m", "cannabis_mf",
              "condom_risk_f","condom_risk_m","condom_risk_mf",
              "contraception_now",
              "clean_fuel", 
              "exbf",
              "edu_expect_f","edu_expect_m","edu_expect_mf",
              "edu_index", 
              "hdi", "ors",
              "pop_over15_f","pop_over15_m","pop_over15_mf",
              "tobacco_f","tobacco_m","tobacco_mf",
              "vehicles")
scovar <- scovar[!(scovar %in% v_unused)]

# Prediction database: take covariate names from source column
pcovar <- pred %>% select(ends_with("_source")) %>% 
  rename_with(function(c) str_replace(c, "_source", "")) %>%
  names()

# Prediction covariates that aren't in study database/aren't used
v_unused <- c("exbf", "literacy_fem", "gdp", "ifd", "itn", "lb", "nnd", "pab2", "pnd", "pnmr", "u5d")
pcovar <- pcovar[!(pcovar %in% v_unused)]

# Merge study and prediction database covariate names
covar <- data.frame(study = scovar) %>%
  full_join(data.frame(pred = pcovar), by = join_by(study == pred), keep = TRUE) %>%
  arrange(study, pred)

# Match up study and predication database covariate names by hand
notmatched_pred <- subset(covar, is.na(study))$pred
covar$pred_key <- covar$pred
covar$pred_key[covar$study == "bcg_mf"] <- "bcg"
covar$pred_key[covar$study == "dtp3_mf"] <- "dpt3"
covar$pred_key[covar$study == "hib3_mf"] <- "hib3"
covar$pred_key[covar$study == "mcv_mf"] <- "mcv"
covar$pred_key[covar$study == "ors_mf"] <- "ors"
covar$pred_key[covar$study == "pcv3_mf"] <- "pcv"
covar$pred_key[covar$study == "rota_last_mf"] <- "rota_last"
covar$pred_key[covar$study == "stunt_mf"] <- "stunting"
covar$pred_key[covar$study == "underwt_mf"] <- "underweight"

# Only keep study/prediction covariates which match
covar <- covar %>% 
  filter(!is.na(study) & !is.na(pred_key)) %>%
  select(-c(pred)) %>%
  rename(pred = pred_key)

# Double check if all manually assigned prediction names are in prediction database
sum(!(covar$pred %in% pcovar))

# Save output -------------------------------------------------------------

write.csv(covar, "./gen/update-old-studydb-covar/output/key_study-pred-covarnames_india.csv", row.names = FALSE)
