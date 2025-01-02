
################################################################
# Identify India states for studies located in India
################################################################

#' Output: key with study db covariate name matched to pred db covariate name

# Initialize environment --------------------------------------------------

rm(list = ls())
source("./src/util.R")

# Load data ---------------------------------------------------------------

# Load in 2020 study database
dat <- readstata13::read.dta13("./data/previous-database/20200930-HMM-StudyDatabase.dta")

# 2021 prediction database
pred <- read.csv("./data/prediction-database/PredicationDatabaseIND_2022series_2023-12-12.csv")


# Identifying source of subnational data points ---------------------------

# Reshape source columns long for India studies
df_src <- dat %>% select(all_of(v_study_id_col), ends_with("_source")) %>%
  select(-ors_source) %>% # removing, because already have values for ors_f, ors_m, and ors_mf
  rename_with(function(c) str_replace(c, "_source", "")) %>% 
  pivot_longer(cols = !all_of(v_study_id_col),
               names_to = c("variable"), 
               values_to = "source"
  ) %>%
  filter(iso3 == "IND")

# Look at study_location for all India datapoints
v_study_loc <- unique(df_src$study_location)

# India states in prediction database
v_states <- unique(pred$state)

# Merge study locations and india states
key <- data.frame(study_location = v_study_loc) %>%
  full_join(data.frame(state = v_states), by = join_by(study_location == state), keep = TRUE) %>%
  arrange(study_location, state)

# States that don't match with any study
notmatched_pred <- subset(key, is.na(study_location))$state
# Studies that dont match with any state
notmatched_study <- subset(key, is.na(state))$study_location

# Drop studies that are nationwide. These should be matched with national level prediction database.
key <- subset(key, !(study_location == "nationwide"))

# Match up study and predication database location names by hand
key$state_key <- key$state
key$state_key[key$study_location == "45 villages in East and West Godavari in Andhra Pradesh"] <- "Andhra Pradesh"
key$state_key[key$study_location == "50 colonies/ villages in East Delhi"] <- "Delhi" 
key$state_key[key$study_location == "Ballabgarh"] <-"Haryana"
key$state_key[key$study_location == "In 1992, a community-based surveillance system was set up in eight villages from Raipur Rani Block in Panchkula district of Haryana state in India, which had a population of 11 864. Surveillance villages are located at about 3-5 km from the Community Health Centre and about 40 km from the state capital."] <- "Haryana"
key$state_key[key$study_location == "Jammu Kashmir"] <- "Jammu & Kashmir"
key$state_key[key$study_location == "rural Villupuram district(formerly part of South Arcot district) in Tamil Nadu, South India"] <-  "Tamil Nadu" 

# Only keep study/prediction states which match
key <- key %>% 
  filter(!is.na(study_location) & !is.na(state_key)) %>%
  select(-c(state)) %>%
  rename(state = state_key)

# Double check if all manually assigned state names are in prediction database
sum(!(key$state %in% v_states))

# Save output -------------------------------------------------------------

write.csv(key, "./gen/update-old-studydb-covar/output/key_study-pred-states_india.csv", row.names = FALSE)

