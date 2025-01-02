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
pred <- read.csv("./data/prediction-database/PredicationDatabase_2022series_2023-02-21.csv")

# Key matching old study and new pred covariate names
key <- read.csv("./gen/update-old-studydb-covar/output/key_study-pred-covarnames.csv")

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
studylong <- full_join(studyval, studysrc, by = c(v_study_id_col, "indicator"))

# Keep data points that are clearly from prediction database
studylong <- studylong %>%
  mutate(borrowed = case_when(
    grepl("barro", source, ignore.case = TRUE) ~ TRUE,
    grepl("dhs", source, ignore.case = TRUE) ~ TRUE,
    grepl("extrapolat", source, ignore.case = TRUE) ~ TRUE,
    grepl("imputed", source, ignore.case = TRUE) ~ TRUE,
    grepl("interpolation", source, ignore.case = TRUE) ~ TRUE,
    grepl("ilo", source, ignore.case = TRUE) ~ TRUE,
    grepl("igme", source, ignore.case = TRUE) ~ TRUE,
    grepl("jmp", source, ignore.case = TRUE) ~ TRUE,
    grepl("map", source, ignore.case = TRUE) ~ TRUE,
    grepl("ncd", source, ignore.case = TRUE) ~ TRUE,
    grepl("noaa", source, ignore.case = TRUE) ~ TRUE,
    grepl("owid", source, ignore.case = TRUE) ~ TRUE,
    grepl("set to", source, ignore.case = TRUE) ~ TRUE,
    grepl("unesco", source, ignore.case = TRUE) ~ TRUE,
    grepl("unicef", source, ignore.case = TRUE) ~ TRUE,
    grepl("unpd", source, ignore.case = TRUE) ~ TRUE,
    grepl("undp", source, ignore.case = TRUE) ~ TRUE,
    grepl("wb", source, ignore.case = TRUE) ~ TRUE,
    grepl("who", source, ignore.case = TRUE) ~ TRUE,
    grepl("wpp", source, ignore.case = TRUE) ~ TRUE,
    indicator %in% c("height_mf","sex_age_mf_15", "sex_age_mf_18", "sex_age_mf_20") &
      grepl("average", source, ignore.case = TRUE) ~ TRUE
  )) %>%
  filter(borrowed == TRUE) %>%
  select(-borrowed)

# Check if there is atleast one value for each covariate still
sum(!(key$study %in% studylong$indicator))

# Reshape prediction db to long ------------------------------------------------

# ID columns in pred database
v_pred_id_col <- c("iso3", "name_short_en", "year")

# Select main value columns that need to be replaced
predval <- pred %>% select(all_of(c(v_pred_id_col, key$pred))) %>%
  pivot_longer(cols = !all_of(v_pred_id_col),
               names_to = c("indicator"), 
               values_to = "value"
  ) %>%
  select(-c(name_short_en))

# Merge on name key
predlong <- merge(predval, key, by.x = "indicator", by.y = "pred")

# Adjust covariate values after looking at plots below
predlong$value <- ifelse(predlong$indicator == "contraception_met" &
                           predlong$study == "contraception_unmet", (1-predlong$value)*100, predlong$value)
predlong$value <- ifelse(predlong$indicator == "sab", (predlong$value)/100, predlong$value)

# Use study database indicator name
predlong$indicator <- predlong$study
predlong$study <- NULL

# Merge study and pred data together --------------------------------------

data <- merge(studylong, predlong, by = c("iso3", "year", "indicator"), suffixes = c("","_pred") )

# Calculate difference between values
data$dif <- abs(data$value - data$value_pred)

# Create page number for plots
df_iso3page <- data.frame(iso3 = unique(data$iso3),
                          n_iso3 = 1:length(unique(data$iso3)))
df_iso3page <- df_iso3page %>%
  mutate(page = ceiling(n_iso3/9)) %>%
  select(iso3, page)
df_plot <- full_join(data, df_iso3page, by = c("iso3"))

# Plot differences
v_ind <- unique(df_plot$indicator)

# reshape source information to long
df_plotlong <- df_plot %>%
  select(page, indicator, 
         iso3, year, study_id,
         value, value_pred) %>%
  pivot_longer(c(value, value_pred)) %>%
  mutate(series = ifelse(endsWith(name, '_pred'), "Prediction", "Study"))

# Plot prediction and study series
for(i in 1:length(v_ind)){
  plot <- df_plotlong%>% filter(indicator == v_ind[i]) %>%
            ggplot() +
            geom_point(aes(x = year, y = value, col = series)) +
            facet_wrap(~iso3)
            ggtitle(str_glue("{v_ind[i]}"))
  ggsave(str_glue("./gen/update-old-studydb-covar/audit/check-scales/{v_ind[i]}.pdf"), plot, height = 10, width = 8, units = "in") 
}
# Check each plot

# Covariates that look like they are using different scales ---------------

# sab: divided by 100
# contraception_unmet: multiplied by 100
# Make adjustments in harmonize-pred-study.R


