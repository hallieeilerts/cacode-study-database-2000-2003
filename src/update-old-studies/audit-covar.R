################################################################################
#' @description Create a match key for covariate names and scales between master 2000-2020 study db, 2021 India pred db, and 2000-2023 pred db
#' @return Key with covariate names, scales in all databases
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(gridExtra)
#' Inputs
source("./src/set-inputs.R")
## Old age-specific study database
#study <- read.csv(paste0("./gen/recover-studies2019-5to19y/temp/studydb-modinput-match_", ageSexSuffix, ".csv", sep = ""))
study <- read.csv(paste0("./gen/recover-studies2019-5to19y/output/Studies2019_", ageSexSuffix, ".csv", sep = ""))
## Prediction database
dat_filename <- list.files("./data/prediction-database")
dat_filename <- dat_filename[grepl("long", dat_filename, ignore.case = TRUE)]
pred <- read.csv(paste0("./data/prediction-database/", dat_filename, sep = ""))
## Key with matched study and prediction covariate names and scales
dat_filename <- list.files("./gen/update-old-studies/output")
dat_filename <- dat_filename[grepl("covariatekey", dat_filename, ignore.case = TRUE)] 
dat_filename <- tail(sort(dat_filename), 1) # most recent
key <- read.csv(paste0("./gen/update-old-studies/output/", dat_filename, sep = ""))
################################################################################

# Apply adjustment to study data
v_adj <- unique(subset(key, study_adjustment ==  "divide by 100"))$study
studyadj <- study
studyadj[,names(studyadj) %in% v_adj] <- studyadj[,names(studyadj) %in% v_adj]/100
v_adj <- unique(subset(key, study_adjustment ==  "subtract from 100 and divide by 100"))$study
studyadj[,names(studyadj) %in% v_adj] <- (100-studyadj[,names(studyadj) %in% v_adj])/100

# Sample of countries to spot check covariates
v_sample <- c("AFG", "BFA", "GHA", "IND", "NGA", "PAK", "TZA")
v_id <- c("Refid","study_id","iso3","year")
v_src <- names(study)[grepl("source",names(study))]
v_covar <- sub("_source$", "", v_src)

# Reshape studies long
studylong <- studyadj %>% 
  select(all_of(c(v_id, v_covar))) %>%
  pivot_longer(cols = !all_of(c(v_id)),
               names_to = c("variable"), 
               values_to = "value"
  ) %>%
  filter(iso3 %in% v_sample) 

# Merge on covar match key
df_study <- merge(studylong, key, by.x = "variable", by.y = "study")

# Merge with prediction database
df_pred <- pred[,c("variable","iso3","year","value_main")]

df_plot <- merge(df_pred, df_study, by.x = c("variable","iso3","year"), by.y = c("pred","iso3","year"))
length(unique(df_study$pred)[!is.na(unique(df_study$pred))])
length(unique(df_plot$variable))

# reshape source information to long
df_plotlong <- df_plot %>%
  pivot_longer(c(value, value_main)) %>% 
  mutate(source = ifelse(name == "value_main", "Pred DB", "Study DB"))


# plots <- plyr::dlply(df_plotlong, ~variable,
#                      function(x)
#                        ggplot(x) +
#                        ylab("") +
#                        ggtitle(x$variable) +
#                        geom_point(aes(x = year, y = value, col = source, shape = source)) +
#                        scale_shape_manual(values = c(1, 2)) +
#                        facet_wrap(~iso3) 
# )
# mg <- marrangeGrob(grobs = plots, nrow=1, ncol=1, top = NULL)
# ggsave(str_glue("./gen/update-old-studies/audit/check-scales.pdf"), mg, height = 10, width = 8, units = "in") 


