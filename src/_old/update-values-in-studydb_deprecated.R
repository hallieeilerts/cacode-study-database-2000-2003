
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
key_replacement <- read.csv("./gen/update/output/key_study-covar-replacement.csv")

# 2021 prediction database with covariate names and scales updated to match study database
pred <- read.csv("./gen/update/output/pred-adjusted.csv")

# 2021 India prediction database with covariate names and scales updated to match study database
predInd <- read.csv("./gen/update/output/pred-adjusted_india.csv")

# Key matching old study and new pred covariate names
key_covarnames <- read.csv("./gen/update/output/key_study-pred-covarnames.csv")

# Key matching old study locations and India pred state names
key_indstates <- read.csv("./gen/update/output/key_study-pred-states_india.csv")

# Replace study values with main values from prediction database ----------

# Reshape main study values long
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
v_predInd_id_col <- c("state", "year")
# Select main value columns that need to be replaced
predmainInd <- predInd %>% select(any_of(c(v_predInd_id_col, key_covarnames$study))) %>%
  pivot_longer(cols = !all_of(v_predInd_id_col), names_to = c("variable"), values_to = "value")

# Merge pred values onto study data points that need updating
studymain_update <- studylong %>%
  filter(replace == TRUE) %>% # Study data points marked for replacement (see identify-study-covar-sources.R)
  left_join(., key_indstates, by = "study_location") %>% # Merge on India state key
  left_join(., predmainInd, by = c("state", "year", "variable"), suffix = c("", "_updatedInd"))  %>% # Merge on India prediction data
  left_join(., predmain, by = c("iso3", "year", "variable"), suffix = c("", "_updated")) # Merge on prediction database
# Are there any covariates for which there are no updated values in prediction database?
unique(subset(studymain_update, is.na(value_updated))$variable)
# Yes, men_epi

#-------- PATCH -------------#
# Don't replace values coming from DHS for India.
# For other countries, DHS covariates are clearly labeled with DHS, and there has been no extrapolation/interpolation.
# The same is not true for India. 
# Covariates from the DHS are sometimes labelled NFHS, DLHS, or weighted indian states. Sometimes they say linear interpolation or extrapolation.
# Need to do this more carefully.
# For now, identify covariates that have DHS source for India, and don't update.
# df_src <- study %>% select(all_of(v_study_id_col), ends_with("_source")) %>%
#   rename_with(function(c) str_replace(c, "_source", "")) %>% 
#   pivot_longer(cols = !all_of(v_study_id_col),
#                names_to = c("variable"), 
#                values_to = "source"
#   ) %>% 
#   filter(iso3 == "IND") %>%
#   mutate(dhslikesource = case_when(
#     grepl("dlhs", source, ignore.case = TRUE) ~ TRUE,
#     grepl("nfhs", source, ignore.case = TRUE) ~ TRUE,
#     #grepl("weighted northeastern", source, ignore.case = TRUE) ~ TRUE,
#     #grepl("weighted smaller", source, ignore.case = TRUE) ~ TRUE,
#     grepl("master state-level data for prediction_India", source, ignore.case = TRUE) ~ TRUE,
#   )) %>%
#   distinct(variable)
# Ok this is all covariates.
# Don't replace any subnational India covariates, except for those variables that were updated in India prediction database

# previously thought the problem was limited to birthrate
# Don't replace covariate values for birthrate for India
# Based on comparison plots, looks like we previously had subnational values for this
# However we didn't have birthrate in newest India prediction database
#View(subset(studymain_update, study_id == "MDS20102013Tamil Nadu 101" & variable == "birthrate"))

studymain_update <- studymain_update %>%
  mutate(value_updated = ifelse(iso3 == "IND" & study_location != "nationwide" & !(variable %in% predmainInd$variable), NA, value_updated))
# If they are an India study that is not nationwide, and the variable is not in the new ind pred database, don't update
# Can update the variables that are in the ind pred database
# Can't update the others as they had subnational values before which we don't have now. 
# There was a subnational india database that included DHS covariates (in addition to Usha's) and had smoothing.
# --------------------------#

# Replace study data points with prediction database
# If india subnational is available, use that before national level
studymain_update <- studymain_update %>%
  mutate(value = ifelse(!is.na(value_updated), value_updated, value)) %>%
  mutate(value = ifelse(!is.na(value_updatedInd), value_updatedInd, value)) %>%
  mutate(updated = ifelse(!is.na(value_updated) | !is.na(value_updatedInd), TRUE, FALSE )) %>%
  filter(updated == TRUE)
 
# Identify study-covariates that have been updated
v_updated <- studymain_update$tempID

# Recombine main study datapoints that weren't updated with those that were
studymain_recombine <- studylong %>%
  filter(!(tempID %in% v_updated)) %>%
  mutate(updated = FALSE) %>%
  bind_rows(., studymain_update) %>%
  select(-c(tempID, source, source_type, value_updated, value_updatedInd, replace, state)) %>%
  arrange(study_id) 

# Reshape data points wide
studymain_datwide <- studymain_recombine %>%
  select(-updated) %>%
  pivot_wider(names_from = variable, values_from = value) 

# Reshape information on whether data point was replaced wide
studymain_replacewide <- studymain_recombine %>%
  select(-value) %>%
  pivot_wider(names_from = variable, values_from = updated, names_glue = "{variable}_updated") 

# Combine wide data frames
studymain_new <- studymain_datwide %>%
  full_join(., studymain_replacewide, by = v_study_id_col) %>%
  arrange(study_id)

# Order columns
v_col_order <- sort(names(studymain_new))
v_col_order <- v_col_order[!(v_col_order %in% c(v_study_id_col))]
studymain_new <- studymain_new[,c(v_study_id_col, v_col_order)]

# Replace study values with smoothed values from prediction database ---------

# Reshape sm pred values long
predsm <- pred %>% select(any_of(c(v_pred_id_col, paste(key_covarnames$study, "_sm", sep = "")))) %>%
  pivot_longer(cols = !all_of(v_pred_id_col),names_to = c("variable"), values_to = "value") %>%
  mutate(variable = str_replace(variable, "_sm", "")) %>%
  select(-c(name_short_en))

# Select main value columns that need to be replaced
predsmInd <- predInd %>% select(any_of(c(v_predInd_id_col, paste(key_covarnames$study, "_sm", sep = "")))) %>%
  pivot_longer(cols = !all_of(v_predInd_id_col), names_to = c("variable"), values_to = "value") %>%
  mutate(variable = str_replace(variable, "_sm", "")) 

# Merge pred values onto study data points that need updating
studysm_update <- studylong %>%
  filter(replace == TRUE) %>% # Study data points marked for replacement based on source
  left_join(., key_indstates, by = "study_location") %>% # Merge on India state key
  left_join(., predsmInd, by = c("state", "year", "variable"), suffix = c("", "_updatedInd"))  %>% # Merge on India prediction data
  left_join(., predsm, by = c("iso3", "year", "variable"), suffix = c("", "_updated")) 
# Are there any covariates for which there are no updated values in prediction database?
unique(subset(studysm_update, is.na(value_updated))$variable)
nrow(subset(studysm_update, is.na(value_updated))) # 3146
nrow(subset(studysm_update, !is.na(value_updated))) # 77627
# Yes, many do not have smoothed values in prediction database
# !!!!!**** With more time, make sure to replace anything missing a smoothed series with the main series.

#-------- PATCH -------------#
# Don't replace covariate values for birthrate for India
# Based on comparison plots, looks like we previously had subnational values for this
# However we didn't have birthrate in newest India prediction database
#View(subset(studysm_update, study_id == "MDS20102013Tamil Nadu 101" & variable == "birthrate"))
# studysm_update <- studysm_update %>%
#   mutate(value_updated = ifelse(iso3 == "IND" & study_location != "nationwide" & variable == "birthrate", NA, value_updated))

studysm_update <- studysm_update %>%
  mutate(value_updated = ifelse(iso3 == "IND" & study_location != "nationwide" & !(variable %in% predmainInd$variable), NA, value_updated))
# --------------------------#


# Replace study data points with prediction database
# If india subnational is available, use that before national level
studysm_update <- studysm_update %>%
  mutate(value = ifelse(!is.na(value_updated), value_updated, value)) %>%
  mutate(value = ifelse(!is.na(value_updatedInd), value_updatedInd, value)) %>%
  mutate(updated = ifelse(!is.na(value_updated) | !is.na(value_updatedInd), TRUE, FALSE )) %>%
  filter(updated == TRUE)

# Identify study-covariates that have been updated
v_updated <- studymain_update$tempID

# Recombine main study datapoints that weren't updated with those that were
studysm_recombine  <- studylong %>%
  filter(!(tempID %in% v_updated)) %>%
  mutate(updated = FALSE) %>%
  bind_rows(., studymain_update) %>%
  select(-c(tempID, source, source_type, value_updated, value_updatedInd, replace, state)) %>%
  arrange(study_id) 

# Reshape data points wide
studysm_datwide <- studysm_recombine %>%
  select(-updated) %>%
  pivot_wider(names_from = variable, values_from = value) 

# Reshape information on whether data point was replaced wide
studysm_replacewide <- studysm_recombine %>%
  select(-value) %>%
  pivot_wider(names_from = variable, values_from = updated, names_glue = "{variable}_updated") 

# Combine wide data frames
studysm_new <- studysm_datwide %>%
  full_join(., studysm_replacewide, by = v_study_id_col) %>%
  arrange(study_id)

# Order columns
v_col_order <- sort(names(studysm_new))
v_col_order <- v_col_order[!(v_col_order %in% c(v_study_id_col))]
studysm_new <- studysm_new[,c(v_study_id_col, v_col_order)]


# Save outputs ------------------------------------------------------------

write.csv(studymain_new, "./gen/update/output/study-updated-main.csv", row.names = FALSE)
write.csv(studysm_new, "./gen/update/output/study-updated-sm.csv", row.names = FALSE)


# Old code from identify-study-covar-sources

# df_src1 <- df_src1 %>%
#   mutate(borrowed = case_when(
#     grepl("barro", value, ignore.case = TRUE) ~ TRUE,
#     grepl("dhs", value, ignore.case = TRUE) ~ TRUE,
#     grepl("extrapolat", value, ignore.case = TRUE) ~ TRUE,
#     grepl("imputed", value, ignore.case = TRUE) ~ TRUE,
#     grepl("interpolation", value, ignore.case = TRUE) ~ TRUE,
#     grepl("ilo", value, ignore.case = TRUE) ~ TRUE,
#     grepl("jmp", value, ignore.case = TRUE) ~ TRUE,
#     grepl("ncd", value, ignore.case = TRUE) ~ TRUE,
#     grepl("owid", value, ignore.case = TRUE) ~ TRUE,
#     grepl("set to", value, ignore.case = TRUE) ~ TRUE,
#     grepl("unesco", value, ignore.case = TRUE) ~ TRUE,
#     grepl("unicef", value, ignore.case = TRUE) ~ TRUE,
#     grepl("unpd", value, ignore.case = TRUE) ~ TRUE,
#     grepl("undp", value, ignore.case = TRUE) ~ TRUE,
#     grepl("wb", value, ignore.case = TRUE) ~ TRUE,
#     grepl("who", value, ignore.case = TRUE) ~ TRUE,
#     grepl("wpp", value, ignore.case = TRUE) ~ TRUE
#   ))
# 
# df_src1 %>% filter(is.na(borrowed)) %>% distinct(value) %>% pull

# MICS? 
# I suppose don't replace mics. Because that hasn't been updated

# Don't update India covariates yet.
# This is model averaged country
# and matching up for subnational studies is not super easy
# Does every study location have the state name? Do later if yes.

# Add value for when borrowed from India prediction database
# "weighted Northeastern cluster states" , "weighted Northeastern cluster states - mf" ,  
# "weighted Smaller states and UT" ,"weighted Smaller states and UT - mf"   
# other names
# NFHS
# DLHS
# RCH
# linear interpolation
# extrapolation
# usha data

#mutate(DHS = grepl("dhs", value, ignore.case = TRUE)) %>%
#filter(DHS == TRUE)

# Sex as separate column
# # Subset and reformat all source columns
# df_src <- dat %>% select(all_of(v_study_id_col), ends_with("_source")) %>%
#   select(-ors_source) %>% # removing, because already have values for ors_f, ors_m, and ors_mf
#   rename_with(function(c) str_replace(c, "_source", "")) %>% 
#   rename_with(function(c) str_replace(c, "_m$", "__sexM"), ends_with("_m")) %>%
#   rename_with(function(c) str_replace(c, "_f$", "__sexF"), ends_with("_f")) %>%
#   rename_with(function(c) str_replace(c, "_mf$", "__sexMF"), ends_with("_mf")) %>% 
#   rename_with(function(c) str_glue("{c}__sexMF"), !contains("__sex") & !all_of(v_study_id_col)) %>% 
#   pivot_longer(cols = !all_of(v_study_id_col),
#                names_to = c("variable", "sex"), 
#                names_pattern = "(.*)__sex(.*)", 
#                values_to = "value"
#   ) %>%
#   mutate(indicator = sub('\\_source.*', '', variable))
