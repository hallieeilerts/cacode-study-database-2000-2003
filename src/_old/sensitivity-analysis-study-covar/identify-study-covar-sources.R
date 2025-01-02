
################################################################
# Identify study-covariate data points that need to be replaced using "source" column
################################################################

#' Output: data frame with one row per study/covariate and a column indicating the source for that covariate, and whether it should be replaced with new prediction database data

# Initialize environment --------------------------------------------------

rm(list = ls())
source("./src/util.R")

# Load data ---------------------------------------------------------------

# Load in 2020 study database
dat <- readstata13::read.dta13("./data/previous-database/20200930-HMM-StudyDatabase.dta")

# 2021 prediction database
pred <- read.csv("./data/prediction-database/PredicationDatabaseIND_2022series_2023-12-12.csv")

# Key matching old study and new pred covariate names
key_covarnames <- read.csv("./gen/update-old-studydb-covar/output/key_study-pred-covarnames.csv")

# Key matching study covariate names to India subnational prediction database covariate names
key_covarnames_india <- read.csv("./gen/update-old-studydb-covar/output/key_study-pred-covarnames_india.csv")

# Identifying values that need to be replaced -----------------------------

# Reshape source columns long
df_src <- dat %>% select(all_of(v_study_id_col), ends_with("_source")) %>%
  select(-ors_source) %>% # removing, because already have values for ors_f, ors_m, and ors_mf
  rename_with(function(c) str_replace(c, "_source", "")) %>% 
  pivot_longer(cols = !all_of(v_study_id_col),
               names_to = c("variable"), 
               values_to = "source"
  )

# Identify covariates that came from DHS survey
df_dhs <- df_src %>%
  mutate(DHS = grepl("dhs", source, ignore.case = TRUE)) %>%
  filter(DHS == TRUE) %>%
  select(variable) %>%
  arrange(variable) %>%
  distinct()

# Identify source of data points: 
# Article, DHS, and MICS data points do not need to be updated
df_src <- df_src %>%
  mutate(source_type = ifelse( grepl("article", source, ignore.case = TRUE), "Article", "Prediction Database")) %>%
  mutate(source_type = ifelse( !(variable %in% key_covarnames$study), "Not included in new Prediction Database", source_type)) %>%
  mutate(source_type = ifelse( iso3 %in% "IND" &
                                 study_location != "nationwide" &
                                 variable %in% key_covarnames_india$study,
                                  "India Subnational Prediction Database", source_type )) %>%
  mutate(source_type = ifelse( iso3 %in% "IND" & 
                                study_location != "nationwide" &
                                !(variable %in% key_covarnames_india$study) &
                                variable %in% df_dhs$variable, 
                                  "Subnational India values that are not included in new India Subnational Prediction Database", source_type )) %>%
  mutate(source_type = ifelse(grepl("dhs", source, ignore.case = TRUE), "DHS", source_type)) %>%
  mutate(source_type = ifelse(grepl("mics", source, ignore.case = TRUE) & source != "AGEDD-Epidemics", "MICS", source_type)) %>%
  mutate(replace = ifelse(source_type %in% c("Prediction Database", "India Subnational Prediction Database"), TRUE, FALSE))

#' Note on replacing subnational India data:
#' The India subnational prediction database was put together using (i) state-level data provided by Usha, (ii) data extracted from NFHS and DLHS (DHS-like) surveys for India, and (iii) ad-hoc sources like MAP, maybe others. The source column in the study database not always helpful for identifying covariates borrowed from the subnational india prediction database. Values were interpolated and extrapolated in the India subnational database, so for example, the source column won't say NFHS 1, but "linear interpolation". 
#' 
#' In short, study covariates that were borrowed from India Subnational Prediction database cannot be reliably identified by the source column. They are instead identified by country and whether the variable is in key_covarnames_india. They are labelled as "India Subnational Prediction Database".
#' 
#' Some India studies in the study database appear to have subnational-level covariate information for covariates that were not included in the latest India Subnational Prediciton Database (e.g., birth_healthfacility3, birthrate, childbearing, etc.). These values have been interpolated and extrapolated, and must have been obtained from either the DHS or a previous India Subnational Prediction Database that included more variables. 
#' For now, they can't be updated from the new India Subnational Prediction Database (because they weren't included) and should not be updated with the National Prediction database values because we will lose the subnational information. 
#' I'm identifying these covariates as those that can be obtained from DHS (by identifying any covariate for which DHS was listed as a source in the data) but were not included in the new India Subnational Prediction Database. They are labelled as "Subnational India values that are not included in new India Subnational Prediction Database".
#' 
#' There is a third group of covariates for India studies that have no subnational information available (from new India subnational database or a previous one) and should be updated with new prediction database values (e.g., alcohol, depression). They are labelled as "Prediction Database"
#' 
#' There is a fourth group of covariates for India studies that have no subnational information available and also were not included in new prediction database (e.g., cannabis). These are not included in key_covarnames are will be labelled as "Not included in new Prediction Database".
#' 
#' !!!!! When putting together the new study database... for studies taking place in India, we should first add covariates provided by the study. Second, we can borrow subnational covariates that were provided by Usha. Third, we should get subnational values from the DHS/NFHS/DLHS surveys. In this way, we should follow the same rules as applied to other studies, and select the survey value occurring within 10 years of the study. We should not borrow interpolated/extrapolated values from the Subnational India Prediction Database for these survey data sources.

# Save output -------------------------------------------------------------

write.csv(df_src, "./gen/update-old-studydb-covar/output/key_study-covar-replacement.csv", row.names = FALSE)

