################################################################################
#' @description Match study covariate names in 2000-2020 model objects to 2023 prediction database
#' @return Key with study db covariate name matched to pred db covariate name
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readstata13)
library(tidyverse)
library(data.table)
library(gridExtra)
#' Inputs
source("./src/update-natl-covar-for-old-studies/set-inputs.R")

# 2020 study database with new id variable
study <- read.csv(paste0("./gen/update-natl-covar-for-old-studies/temp/studies-with-id_", ageSexSuffix, ".csv",sep=""))

# 2000-2023 prediction database
pred <- read.csv("./data/prediction-database/CovariateDatabase2023-wide_20241003.csv")
predlong <- read.csv("./data/prediction-database/CovariateDatabase2023-long_20241003.csv")

# Prediction database codebook
pred_cb <- read.csv("./data/prediction-database/CovariateDatabase2023_CodebookByVar_20241003.csv")

# 2000-2022 subnational India prediction database
pred_ind <- read.csv("./data/prediction-database/PredicationDatabaseIND_2022series_2023-12-12.csv")

################################################################################


# Study database: take covariate names from source column
scovar <- study %>% select(ends_with("_source")) %>% 
  rename_with(function(c) str_replace(c, "_source", "")) %>%
  names()
# Old study database had both ors and ors_mf
# Drop ors
scovar <- scovar[(!scovar %in% "ors")]

# Prediction database: take covariate names from source column
pcovar <- pred %>% select(ends_with("_source")) %>% 
  rename_with(function(c) str_replace(c, "_source", "")) %>%
  names()
# India prediction database: take covariate names from source column
pcovar_ind <- pred_ind %>% select(ends_with("_source")) %>% 
  rename_with(function(c) str_replace(c, "_source", "")) %>%
  names()

# Merge study and prediction database covariate names
covar <- data.frame(study = scovar) %>%
  full_join(data.frame(pred = pcovar), by = join_by(study == pred), keep = TRUE) %>% 
  full_join(data.frame(pred_ind = pcovar_ind), by = join_by(study == pred_ind), keep = TRUE) %>%
  arrange(study, pred)

# Create column to fill in manually
# Match study names to pred
covar$predFillIn <- covar$pred
subset(covar, is.na(pred))$study
subset(covar, is.na(study))$pred
covar$predFillIn[covar$study == "alcohol"] <- "alcohol_mf"
#covar$predFillIn[covar$study == "bcg_f"] <- 
#covar$predFillIn[covar$study == "bcg_m"] <- 
covar$predFillIn[covar$study == "bcg_mf"] <- "vac_bcg"
covar$predFillIn[covar$study == "birth_healthfacility_3"] <- "birth_healthfacility3"
covar$predFillIn[covar$study == "birth_healthfacility_5"] <- "birth_healthfacility5" 
covar$predFillIn[covar$study == "contraception_unmet"] <- "contraception_met"
covar$predFillIn[covar$study == "dtp3_mf"] <- "vac_dtp3" 
covar$predFillIn[covar$study == "hib3_mf"] <- "vac_hib3" 
covar$predFillIn[covar$study == "imr"] <- "imr_mf" 
covar$predFillIn[covar$study == "lowest_wealth"] <- "wealth_lowest" 
#covar$predFillIn[covar$study == "mcv_f"] <- 
#covar$predFillIn[covar$study == "mcv_m"] <- 
covar$predFillIn[covar$study == "mcv_mf"] <- "vac_mcv1" 
#covar$predFillIn[covar$study == "ors"] <- 
covar$predFillIn[covar$study == "pab"] <- "vac_pab" 
covar$predFillIn[covar$study == "pcv3_mf"] <- "vac_pcv3" 
#covar$predFillIn[covar$study == "pop_over15_f"] <- 
#covar$predFillIn[covar$study == "pop_over15_m"] <- 
#covar$predFillIn[covar$study == "pop_over15_mf"] <- 
covar$predFillIn[covar$study == "rota_last_mf" ] <- "vac_rota_last" 
covar$predFillIn[covar$study == "sanitation" ] <- "wash_sanitation_improved"
covar$predFillIn[covar$study == "sex_age_f_15"] <- "sex_age15_f"
covar$predFillIn[covar$study == "sex_age_f_18"] <- "sex_age18_f"
covar$predFillIn[covar$study == "sex_age_f_20"] <- "sex_age20_f"
covar$predFillIn[covar$study == "sex_age_m_15"] <- "sex_age15_m"
covar$predFillIn[covar$study == "sex_age_m_18"] <- "sex_age18_m"
covar$predFillIn[covar$study == "sex_age_m_20"] <- "sex_age20_m"                                                 
covar$predFillIn[covar$study == "sex_age_mf_15"] <- "sex_age15_mf"
covar$predFillIn[covar$study == "sex_age_mf_18"] <- "sex_age18_mf"
covar$predFillIn[covar$study == "sex_age_mf_20"] <- "sex_age20_mf"  
#covar$predFillIn[covar$study == "stunt_f"] <-  
#covar$predFillIn[covar$study == "stunt_m"] <- 
covar$predFillIn[covar$study == "stunt_mf"] <- "stunting"
covar$predFillIn[covar$study == "u5pop"] <- "u5pop_mf"
#covar$predFillIn[covar$study == "underwt_f"] <-   
#covar$predFillIn[covar$study == "underwt_m"] <-   
covar$predFillIn[covar$study == "underwt_mf"] <- "underweight"
covar$predFillIn[covar$study == "water"] <- "wash_water_improved"

# Check work: pred and predfillin should always match
subset(covar, !is.na(pred) & pred != predFillIn)
# Check NAs in predfillin and make sure there is in fact no corresponding covariate in the prediction database for that study covariate

# Count appearance of pred fill-in covariates
setDT(covar)[,n:=.N,by=predFillIn]
# Discard those that were missing by at now assigned to a study covar (and thus appear twice)
covar <- subset(covar, !(is.na(study) & n == 2))

# Replace pred column with predfillin
covar$n <- covar$pred <- NULL
names(covar)[which(names(covar) == "predFillIn")] <- "pred"

# Create column to fill in manually
# Match study names to pred_id
covar$predIndFillIn <- covar$pred_ind
subset(covar, is.na(pred_ind))$study
subset(covar, is.na(study))$pred_ind
covar$predIndFillIn[covar$study == "bcg_mf"] <- "bcg"
covar$predIndFillIn[covar$study == "dtp3_mf"] <- "dpt3" 
covar$predIndFillIn[covar$study == "hib3_mf"] <- "hib3"
covar$predIndFillIn[covar$study == "literacy_f"] <- "literacy_fem" 
covar$predIndFillIn[covar$study == "mcv_mf"] <- "mcv" 
covar$predIndFillIn[covar$study == "ors_mf"] <- "ors" 
covar$predIndFillIn[covar$study == "pcv3_mf"] <- "pcv" 
covar$predIndFillIn[covar$study == "rota_last_mf" ] <- "rota_last" 
covar$predIndFillIn[covar$study == "stunt_mf"] <- "stunting"
covar$predIndFillIn[covar$study == "underwt_mf"] <- "underweight"

# Check work: pred_ind and predIndfillin should always match
subset(covar, !is.na(pred_ind) & pred_ind != predIndFillIn)
# Check NAs in predfillin and make sure there is in fact no corresponding covariate in the prediction database for that study covariate

# Count appearance of pred fill-in covariates
setDT(covar)[,n:=.N,by=predIndFillIn]
# Discard those that were missing by at now assigned to a study covar (and thus appear twice)
covar <- subset(covar, !(is.na(study) & !is.na(predIndFillIn) & n == 2))

# Replace pred_ind column with predIndfillin
covar$n <- covar$pred_ind <- NULL
names(covar)[which(names(covar) == "predIndFillIn")] <- "pred_ind"

# Merge on predication variable scale
covar <- merge(covar, pred_cb[,c("variable","scale")], by.x = "pred", by.y = "variable", all = TRUE)
names(covar)[which(names(covar) == "scale")] <- "pred_scale"

# Create column which contains all covar
covar$all <- covar$pred
covar$all[is.na(covar$all)] <- covar$study[is.na(covar$all)]
covar$all[is.na(covar$all)& is.na(covar$study)] <- covar$pred_ind[is.na(covar$all) & is.na(covar$study)]

# Tidy
covar <- covar[,c("all","study","pred","pred_scale", "pred_ind")]
covar <- covar[order(covar$all),]


# Update key with scale correction ----------------------------------------

# Drop India stuff for now
key <- covar
key <- subset(key, !(is.na(study) & is.na(pred)))
key$pred_ind <- NULL

key$study_adjustment <- NA
key$study_adjustment[!is.na(key$study)] <- "none"
key$study_adjustment[!is.na(key$study) & key$pred_scale == "zero_one"] <- "divide by 100"
key$study_adjustment[key$all %in% c("pop_male_15_29", "sab")] <- "none"
key$study_adjustment[key$all %in% c("pop_male_15_29", "sab")] <- "none"
key$study_adjustment[key$all %in% c("contraception_met")] <- "subtract from 100 and divide by 100"

# check-scales ------------------------------------------------------------

# Apply adjustment to study data
v_adj <- unique(subset(key, study_adjustment ==  "divide by 100"))$study
studyadj <- study
studyadj[,names(studyadj) %in% v_adj] <- studyadj[,names(studyadj) %in% v_adj]/100
v_adj <- unique(subset(key, study_adjustment ==  "subtract from 100 and divide by 100"))$study
studyadj[,names(studyadj) %in% v_adj] <- (100-studyadj[,names(studyadj) %in% v_adj])/100

# Sample of countries to spot check covariates
v_sample <- c("AFG", "BFA", "GHA", "IND", "NGA", "PAK", "TZA")
v_id <- c("id","iso3","year")
v_src <- names(study)[grepl("source",names(study))]
v_covar <- sub("_source$", "", v_src)

# Reshape studies long
studylong <- studyadj %>% 
  select(all_of(c(v_id, v_covar))) %>%
  pivot_longer(cols = !all_of(c(v_id)),
               names_to = c("variable"), 
               values_to = "value"
  ) %>%
  filter(iso3 %in% v_sample) %>%
  select(-id)

# Merge on covar match key
df_study <- merge(studylong, covar, by.x = "variable", by.y = "study")

# Merge with prediction database
df_pred <- predlong[,c("variable","iso3","year","value_main")]

df_plot <- merge( df_pred, df_study, by.x = c("variable","iso3","year"), by.y = c("pred","iso3","year"))
length(unique(df_study$pred)[!is.na(unique(df_study$pred))])
length(unique(df_plot$variable))

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
ggsave(str_glue("./gen/update-natl-covar-for-old-studies/audit/check-scales2.pdf"), mg, height = 10, width = 8, units = "in") 


# LOOKS GOOD!


# Save output -------------------------------------------------------------

write.csv(key, "./gen/update-natl-covar-for-old-studies/output/CovariateKey_Study2019Pred2023_20231008.csv", row.names = FALSE)
