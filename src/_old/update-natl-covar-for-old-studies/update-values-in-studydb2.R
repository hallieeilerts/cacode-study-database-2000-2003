################################################################################
#' @description 
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readstata13)
library(tidyverse)
library(data.table)
#' Inputs

# Set age group
source("./src/update-natl-covar-for-old-studies/set-inputs.R")

# 2020 study database with new id variable
study <- read.csv(paste0("./gen/update-natl-covar-for-old-studies/output/ModInput2020-UpdatedNamesScales_", ageSexSuffix, ".csv",sep=""))

# 2000-2023 prediction database
pred <- read.csv("./data/prediction-database/CovariateDatabase2023-long_20241003.csv")

# Study covariate sources replacement key
key_replacement <- read.csv(paste0("./gen/update-natl-covar-for-old-studies/temp/key_study-covar-replacement_",ageSexSuffix,".csv",sep =""))

################################################################################

# Subset rows from key that require replacement with prediction database values
key <- key_replacement %>%
  filter(replace == TRUE) %>%
  select(id, variable)

# Reshape study data to long
# And add column for q5to19
v_id <- c("sid","id","reterm","totdeaths")
v_var <- names(study)[!(names(study) %in% c(v_id))]
studylong <- study %>% 
  select(all_of(c(v_id, v_var))) %>%
  pivot_longer(cols = !all_of(c(v_id)),
               names_to = c("variable"), 
               values_to = "value"
  ) %>%
  mutate(tempid = 1:n()) %>%
  mutate(iso3 = substr(id, 1, 3),
         year = substr(id, 5, 8))

# Merge with replacement key and separate variables that need to be replaced from those that dont
df_replace <- merge(studylong, key, by = c("id","variable"))
df_keep <- subset(studylong, !(tempid %in% df_replace$tempid))
nrow(df_replace)
nrow(df_keep)
nrow(studylong)

# # ADD EMPTY VALUE FOR Q5TO9 WHICH WAS NOT IN STUDY DATABASE BUT SHOULD HAVE BEEN
# # WAS ONLY ADDED LATER TO MODEL OBJECT
# q5to19 <- df_replace[,c("id","iso3","year")]
# q5to19 <- q5to19[!duplicated(q5to19),]
# q5to19$variable <- "mr05to19"
# q5to19$value <- NA
# q5to19$tempid <- NA
# df_replace <- rbind(df_replace, q5to19)

# Merge studylong with long pred data for replacements
df_replace <- merge(df_replace, pred[,c("iso3","year","variable","value_main")], by = c("iso3","year","variable"))
df_replace$value <- df_replace$value_main
df_replace$value_main <- NULL

# Recombine
studyupd <- rbind(df_keep, df_replace)
studyupd$tempid <- NULL

# Reshape data points wide
studyupdWide <- studyupd %>%
  pivot_wider(names_from = variable, values_from = value) 

# Check
nrow(studyupdWide) == nrow(study)
length(unique(studyupdWide$id))
length(unique(study$id))

# Tidy
studyupdWide <- studyupdWide[order(studyupdWide$id),]

# Save outputs ------------------------------------------------------------

write.csv(studyupdWide, paste0("./gen/update-natl-covar-for-old-studies/output/StudyData2020-UpdatedCovar_",ageSexSuffix,"_",resDate,".csv"), row.names = FALSE)
