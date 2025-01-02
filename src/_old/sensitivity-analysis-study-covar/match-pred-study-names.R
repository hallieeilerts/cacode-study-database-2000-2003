################################################################
# Match covariate names from 2020 study database to 2021 prediction database
################################################################

#' Output: key with study db covariate name matched to pred db covariate name

# Initialize environment --------------------------------------------------

rm(list = ls())
source("./src/util.R")

# Load data ---------------------------------------------------------------

# 2020 study database
study <- readstata13::read.dta13("./data/previous-database/20200930-HMM-StudyDatabase.dta")

# 2021 prediction database
pred <- read.csv("./data/prediction-database/_old/PredicationDatabase_2022series_2023-02-21.csv")

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

# Prediction covariates that aren't in study database
v_unused <- c("literacy_fem", "mean_edu")
# Prediction database
# literacy_fem: Literacy rate, adult female (% of females ages 15 and above)
# literacy_f: Literacy rate, youth female (% of females ages 15-24)
# Checking the studydatabase stata file, literacy_f: Youth literacy rate, population 15-24 years
# So drop literacy_fem.
# Prediction database
# edu_mean: Average Years of Total Schooling of adolescents (15-24 years)
# education: Mean years of schooling
# Checking the studydatabase stata file, edu_mean: average years of school, or average years of school, male 15-19
# A bit unclear, but drop mean_edu. Checking plots, edu_mean has good match.
pcovar <- pcovar[!(pcovar %in% v_unused)]

# Merge study and prediction database covariate names
covar <- data.frame(study = scovar) %>%
  full_join(data.frame(pred = pcovar), by = join_by(study == pred), keep = TRUE) %>%
  arrange(study, pred)

# Match up study and predication database covariate names by hand
notmatched_pred <- subset(covar, is.na(study))$pred
covar$pred_key <- covar$pred
covar$pred_key[covar$study == "bcg_mf"] <- "bcg"
covar$pred_key[covar$study == "contraception_unmet"] <- "contraception_met"
covar$pred_key[covar$study == "dtp3_mf"] <- "dtp3"
covar$pred_key[covar$study == "hib3_mf"] <- "hib3"
covar$pred_key[covar$study == "mcv_mf"] <- "mcv"
covar$pred_key[covar$study == "pcv3_mf"] <- "pcv3"
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

write.csv(covar, "./gen/study-covar-sensitivity-analysis/output/key_study-pred-covarnames.csv", row.names = FALSE)
