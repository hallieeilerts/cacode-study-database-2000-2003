################################################################################
#' @description Update COD names for deaths in old model inputs
#' @return Old model inputs for 5-19 with updated COD names
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
#' Inputs
source("./src/set-inputs.R")
## Old model inputs
if(ageSexSuffix == "05to09y"){load("./data/model-inputs-old/20201217-Data5to9-VAMCM009-Test3.RData")}
if(ageSexSuffix == "10to14y"){load("./data/model-inputs-old/20201222-Data10to14-VAMCM009-Test8j.RData")}
if(ageSexSuffix == "15to19yF"){load("./data/model-inputs-old/20210207-Data15to19Fem-VAMCM009-Test9.RData")}
if(ageSexSuffix == "15to19yM"){load("./data/model-inputs-old/20210212-Data15to19Men-VAMCM009-Test9e.RData")}
################################################################################

# Rename CODs in old model input
names(deaths)[names(deaths) == 'OtherCD'] <- 'OtherCMPN'
names(deaths)[names(deaths) == 'RTA'] <- 'RTI'
names(deaths)[names(deaths) == 'Other_inj'] <- 'OtherInj'
names(deaths)[names(deaths) == 'Self_harm'] <- 'SelfHarm'
names(deaths)[names(deaths) == "Interp_violence"] <- "InterpVio"
deaths$cause[deaths$cause == "OtherCD"] <- "OtherCMPN"
deaths$cause[deaths$cause == "RTA"] <- "RTI"
deaths$cause[deaths$cause == "Other_inj"] <- "OtherInj"
deaths$cause[deaths$cause == "Self_harm"] <- "SelfHarm"
deaths$cause[deaths$cause == "Interp_violence"] <- "InterpVio"

# If 15-19, create sex column
if(!("sex" %in% names(deaths))){
  deaths$sex <- substr(deaths$id, nchar(deaths$id), nchar(deaths$id))
}

# Recode B and T for sex
deaths$sexRecode <- as.character(deaths$sex)
deaths$sexRecode[deaths$sexRecode == "B"] <- "T"

# Replace old sex column
deaths$sex <- as.factor(deaths$sexRecode)
deaths$sexRecode <- NULL

# Tidy
deaths <- deaths %>% relocate(sex, .after = id)

# Save output(s) ----------------------------------------------------------

save(deaths, file = paste("./gen/update-old-studies/output/ModInput2019-Deaths_", ageSexSuffix,".RData", sep = ""))

