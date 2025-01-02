################################################################################
#' @description Check scales
#' @return plots
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readstata13)
library(tidyverse)
library(data.table)
library(ggplot2)
library(gridExtra)
#' Inputs
source("./src/update-natl-covar-for-old-studies/set-inputs.R")

# 2020 study database with new id variable
study <- read.csv(paste0("./gen/update-natl-covar-for-old-studies/temp/studies-adjusted_", ageSexSuffix, ".csv",sep=""))

# 2000-2023 prediction database
pred <- read.csv("./data/prediction-database/CovariateDatabase2023-long_20241003.csv")
################################################################################

# Sample of countries to spot check covariates
unique(study$iso3)
v_sample <- c("AFG", "BFA", "GHA", "IND", "NGA", "PAK", "TZA")
v_id <- c("id","iso3","year")
v_src <- names(study)[grepl("source",names(study))]
v_var <- names(study)[!(names(study) %in% c(v_id, v_src))]

# Reshape studies long
studylong <- study %>% 
  select(all_of(c(v_id, v_var))) %>%
  pivot_longer(cols = !all_of(c(v_id)),
               names_to = c("variable"), 
               values_to = "value"
  ) 

# Subset
df_study <- subset(studylong, iso3 %in% v_sample)[,c("variable","iso3","year", "value")]

# Merge with prediction database
df_pred <- pred[,c("variable","iso3","year","value_main")]


df_plot <- merge(df_pred, df_study, by = c("variable","iso3","year"))

# reshape source information to long
df_plotlong <- df_plot %>%
  pivot_longer(c(value, value_main)) %>% 
  mutate(source = ifelse(name == "value_main", "Pred DB", "Study DB"))


plots <- plyr::dlply(df_plotlong, ~variable,
                     function(x)
                       ggplot(x) +
                       ylab("") +
                       ggtitle(x$variable) +
                       geom_point(aes(x = year, y = value, col = source, shape = source)) +
                       scale_shape_manual(values = c(1, 2)) +
                       facet_wrap(~iso3) 
)
mg <- marrangeGrob(grobs = plots, nrow=1, ncol=1, top = NULL)
ggsave(str_glue("./gen/update-natl-covar-for-old-studies/audit/check-scales.pdf"), mg, height = 10, width = 8, units = "in") 



