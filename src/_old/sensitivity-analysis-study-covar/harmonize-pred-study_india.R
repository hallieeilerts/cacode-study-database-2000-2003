################################################################
# Harmonizes covariate names and scales in India prediction database with study database names
################################################################

#' Output: new India subnational prediction database with same covariate names and scales as study database

# Initialize environment --------------------------------------------------

rm(list = ls())
source("./src/util.R")

# Load data ---------------------------------------------------------------

# 2021 prediction database
pred <- read.csv("./data/prediction-database/PredicationDatabaseIND_2022series_2023-12-12.csv")

# Key matching old study and new pred covariate names
key <- read.csv("./gen/update-old-studydb-covar/output/key_study-pred-covarnames_india.csv")

# Save prediction database with same names and scales as study db ---------

# ID columns in pred database
v_pred_id_col <- c("state", "year")

# Steps:
# Reshape to long
# Merge on study database variable names
# Adjust scales
# Reshape to wide

predmain <- pred %>% select(all_of(c(v_pred_id_col, key$pred))) %>%
  pivot_longer(cols = !all_of(v_pred_id_col),names_to = c("variable"), values_to = "value") %>% # Reshape to long
  mutate(pred = variable) %>% # make new column with name of variable in prediction database
  left_join(.,key, by = "pred") %>% # join with key that links to study database
  mutate(value = case_when( # adjust values that are in different scales
    !(value %in% c("sab")) ~ value,
    pred == "sab" ~ value/100,
  )
  ) %>%  
  select(all_of(c(v_pred_id_col, "study", "value"))) %>% 
  pivot_wider(names_from = study, values_from = value)

predraw <- pred %>% select(all_of(c(v_pred_id_col, paste(key$pred, "_raw", sep = ""))))  %>%
  pivot_longer(cols = !all_of(v_pred_id_col),names_to = c("variable"), values_to = "value") %>% # Reshape to long
  mutate(pred = str_replace(variable, "_raw", "")) %>% # make new column with name of variable in prediction database
  left_join(.,key, by = "pred") %>% # join with key that links to study database
  mutate(value = case_when( # adjust values that are in different scales
    !(value %in% c("sab")) ~ value,
    pred == "sab" ~ value/100,
  )
  ) %>%  
  mutate(study = paste(study, "_raw", sep = "")) %>% # Add suffix onto study database name
  select(all_of(c(v_pred_id_col, "study", "value"))) %>%
  pivot_wider(names_from = study, values_from = value)

predsm <- pred %>% select(any_of(c(v_pred_id_col, paste(key$pred, "_sm", sep = ""))))  %>%
  pivot_longer(cols = !all_of(v_pred_id_col),names_to = c("variable"), values_to = "value") %>% # Reshape to long
  mutate(pred = str_replace(variable, "_sm", "")) %>% # make new column with name of variable in prediction database
  left_join(., key, by = "pred") %>% # join with key that links to study database. left join because there is not a smoothed value for every covariate.
  mutate(value = case_when( # adjust values that are in different scales
    !(value %in% c("sab")) ~ value,
    pred == "sab" ~ value/100,
  )
  ) %>%  
  mutate(study = paste(study, "_sm", sep = "")) %>% # Add suffix onto study database name
  select(all_of(c(v_pred_id_col, "study", "value"))) %>%
  pivot_wider(names_from = study, values_from = value)

predsource <- pred %>% select(all_of(c(v_pred_id_col, paste(key$pred, "_source", sep = ""))))  %>%
  pivot_longer(cols = !all_of(v_pred_id_col),names_to = c("variable"), values_to = "value") %>% # Reshape to long
  mutate(pred = str_replace(variable, "_source", "")) %>% # make new column with name of variable in prediction database
  left_join(.,key, by = "pred") %>% # join with key that links to study database
  mutate(study = paste(study, "_source", sep = "")) %>% # Add suffix onto study database name
  select(all_of(c(v_pred_id_col, "study", "value"))) %>%
  pivot_wider(names_from = study, values_from = value)

pred_new <- predmain %>%
  full_join(., predraw, by = v_pred_id_col) %>%
  full_join(., predsm, by = v_pred_id_col) %>%
  full_join(., predsource, by = v_pred_id_col)

# Order columns
v_col_order <- sort(names(pred_new))
v_col_order <- v_col_order[!(v_col_order %in% v_pred_id_col)]
pred_new <- pred_new[,c(v_pred_id_col, v_col_order)]

# Save output -------------------------------------------------------------

# Prediction database that now has same covariate names and scales as study database
# Some covariates removed if they weren't in study database (literacy_fem, mean_edu)
write.csv(pred_new, "./gen/update-old-studydb-covar/output/pred-adjusted_india.csv", row.names = FALSE)
