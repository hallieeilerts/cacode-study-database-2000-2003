
################################################################
# Update values in study database
################################################################


# Initialize environment --------------------------------------------------

rm(list = ls())
source("./src/util.R")

# Load data ---------------------------------------------------------------

# 2020 study database
study <- readstata13::read.dta13("./data/previous-database/20200930-HMM-StudyDatabase.dta")

# Study covariate sources replacement key
key_replacement <- read.csv("./gen/update-old-studydb-covar/output/key_study-covar-replacement.csv")

# 2021 prediction database with covariate names and scales updated to match study database
pred <- read.csv("./gen/update-old-studydb-covar/output/pred-adjusted.csv")

# 2021 India prediction database with covariate names and scales updated to match study database
pred_ind <- read.csv("./gen/update-old-studydb-covar/output/pred-adjusted_india.csv")

# Key matching old study and new pred covariate names
key_covarnames <- read.csv("./gen/update-old-studydb-covar/output/key_study-pred-covarnames.csv")

# Key matching old study and new india subnational pred covariate names
key_covarnames_ind <- read.csv("./gen/update-old-studydb-covar/output/key_study-pred-covarnames_india.csv")

# Key matching old study locations and India pred state names
key_indstates <- read.csv("./gen/update-old-studydb-covar/output/key_study-pred-states_india.csv")

# Replace study values with main values from prediction database ----------

# Reshape main study values long for covariates which may be replaced (we have updated prediction data)
studylong <- study %>% select(all_of(c(v_study_id_col, key_covarnames$study))) %>%
  pivot_longer(cols = !all_of(v_study_id_col),names_to = c("variable"), values_to = "value") %>%
  mutate(tempID = 1:n()) %>%
  mutate(new_R = as.numeric(new_R))

# Merge on study sources
studylong <- left_join(studylong, key_replacement, by = c(v_study_id_col, "variable")) 

# ID columns in pred database
v_pred_id_col <- c("iso3", "name_short_en", "year")
# Reshape main pred values long
predmain <- pred %>% select(all_of(c(v_pred_id_col, key_covarnames$study))) %>%
  pivot_longer(cols = !all_of(v_pred_id_col),names_to = c("variable"), values_to = "value") %>%
  select(-c(name_short_en))

# ID columns in India pred database
v_pred_ind_id_col <- c("state", "year")
# Select main value columns that need to be replaced
predmainInd <- pred_ind %>% select(any_of(c(v_pred_ind_id_col, key_covarnames_ind$study))) %>%
  pivot_longer(cols = !all_of(v_pred_ind_id_col), names_to = c("variable"), values_to = "value")

# Merge pred values onto study data points that need updating
studylong_addnewvals <- studylong %>%
  filter(replace == TRUE) %>% # Study data points marked for replacement (see identify-study-covar-sources.R)
  left_join(., key_indstates, by = "study_location") %>% # Merge on India state key
  left_join(., predmainInd, by = c("state", "year", "variable"), suffix = c("", "_updatedInd"))  %>% # Merge on India prediction data
  left_join(., predmain, by = c("iso3", "year", "variable"), suffix = c("", "_updated")) # Merge on prediction database
# Are there any covariates which were marked for replacement, but there are no updated values available in prediction database?
unique(subset(studylong_addnewvals, is.na(value_updated))$variable)
unique(subset(studylong_addnewvals, is.na(value_updated))$iso3)
# Yes, men_epi for China and Turkey

# Replace study data points with prediction database
# If India subnational is available, use that before national level
studymain_update <- studylong_addnewvals %>%
  mutate(value = ifelse(!is.na(value_updated), value_updated, value)) %>%
  mutate(value = ifelse(!is.na(value_updatedInd), value_updatedInd, value)) %>%
  mutate(updated = ifelse(!is.na(value_updated) | !is.na(value_updatedInd), TRUE, FALSE ))  # Indicator for whether data was updated

# Subset all rows not in update dataframe 
v_other <- subset(studylong, !(tempID %in% studymain_update$tempID))$tempID

# Recombine main study datapoints that weren't updated with those that were
studymain_recombine <- studylong %>%
  filter(tempID %in% v_other) %>%
  bind_rows(., studymain_update) %>%
  arrange(study_id) %>%
  select(-c(tempID, source, source_type, value_updated, value_updatedInd, replace, state)) %>%
  arrange(study_id) 

# Reshape data points wide
studymain_datwide <- studymain_recombine %>%
  select(-updated) %>%
  pivot_wider(names_from = variable, values_from = value) 

# Reshape information on whether data point was updated wide
studymain_updatewide <- studymain_recombine %>%
  select(-value) %>%
  mutate(updated = ifelse(is.na(updated), FALSE, updated)) %>%
  pivot_wider(names_from = variable, values_from = updated, names_glue = "{variable}_updated") 

# Combine wide data frames
studymain_new <- studymain_datwide %>%
  full_join(., studymain_updatewide, by = v_study_id_col) %>%
  arrange(study_id)

# Order columns
v_col_order <- sort(names(studymain_new))
v_col_order <- v_col_order[!(v_col_order %in% c(v_study_id_col))]
studymain_new <- studymain_new[,c(v_study_id_col, v_col_order)]

# Save outputs ------------------------------------------------------------

write.csv(studymain_new, "./gen/update-old-studydb-covar/output/study-updated-main.csv", row.names = FALSE)


