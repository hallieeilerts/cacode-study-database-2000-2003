################################################################
# Match covariate scales from 2020 study database to 2021 prediction database
################################################################

#' Output: plots comparing study/pred covariates, to be reviewed and figure out whether pred db scales need to be adjusted

# Initialize environment --------------------------------------------------

rm(list = ls())
source("./src/util.R")

# Load data ---------------------------------------------------------------

# 2020 study database
study <- readstata13::read.dta13("./data/previous-database/20200930-HMM-StudyDatabase.dta")

# 2021 prediction database
pred <- read.csv("./data/prediction-database/PredicationDatabaseIND_2022series_2023-12-12.csv")

# Key matching old study and new pred covariate names
key <- read.csv("./gen/update-old-studydb-covar/output/key_study-pred-covarnames_india.csv")

# Reshape study db to long ------------------------------------------------

# Select main value columns that need to be replaced
studyval <- study %>% select(all_of(c(v_study_id_col, key$study))) %>%
  pivot_longer(cols = !all_of(v_study_id_col),
               names_to = c("indicator"), 
               values_to = "value"
  )

# Select source columns that go with main values
studysrc <- study %>% select(all_of(c(v_study_id_col)), ends_with("_source")) %>%
  select(-ors_source) %>% # removing, because already have values for ors_f, ors_m, and ors_mf
  rename_with(function(c) str_replace(c, "_source", "")) %>% 
  pivot_longer(cols = !all_of(v_study_id_col),
               names_to = c("indicator"), 
               values_to = "source"
  ) %>%
  filter(indicator %in% key$study)

# Merge study value and source columns
studylong <- full_join(studyval, studysrc, by = c(v_study_id_col, "indicator")) %>%
  filter(iso3 == "IND")

# Reshape prediction db to long ------------------------------------------------

# ID columns in pred database
v_pred_id_col <- c("state", "year")

# Select main value columns that need to be replaced
predval <- pred %>% select(all_of(c(v_pred_id_col, key$pred))) %>%
  pivot_longer(cols = !all_of(v_pred_id_col),
               names_to = c("indicator"), 
               values_to = "value"
  )

# Merge on name key
predlong <- merge(predval, key, by.x = "indicator", by.y = "pred")

# # Adjust covariate values after looking at plots below
predlong$value <- ifelse(predlong$indicator == "sab", (predlong$value)/100, predlong$value)

# Use study database indicator name
predlong$indicator <- predlong$study
predlong$study <- NULL

# Merge study and pred data together --------------------------------------

df_plot <- merge(studylong, predlong, by.x = c("study_location", "year", "indicator"), 
              by.y = c("state", "year", "indicator"), suffixes = c("","_pred") )

# Plot differences
v_ind <- unique(df_plot$indicator)

# reshape source information to long
df_plotlong <- df_plot %>%
  mutate(state = study_location) %>%
  select(indicator, 
         state, year, study_id,
         value, value_pred) %>%
  pivot_longer(c(value, value_pred)) %>%
  mutate(series = ifelse(endsWith(name, '_pred'), "Prediction", "Study"))

# Plot prediction and study series
for(i in 1:length(v_ind)){
  plot <- df_plotlong%>% filter(indicator == v_ind[i]) %>%
    ggplot() +
    geom_point(aes(x = year, y = value, col = series)) +
    facet_wrap(~state)
  ggtitle(str_glue("{v_ind[i]}"))
  ggsave(str_glue("./gen/update-old-studydb-covar/audit/check-scales/india/{v_ind[i]}.pdf"), plot, height = 10, width = 8, units = "in") 
}
# Check each plot

# Covariates that look like they are using different scales ---------------

# sab: divided by 100
# Make adjustment in harmonize-pred-study_india.R

