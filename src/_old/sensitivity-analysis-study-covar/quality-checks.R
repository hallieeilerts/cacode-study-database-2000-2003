
################################################################
# Quality checks
################################################################


# Initialize environment --------------------------------------------------

rm(list = ls())
source("./src/util.R")

# Load data ---------------------------------------------------------------

resDate <- "20240614"

# 2020 study database
old <- readstata13::read.dta13("./data/previous-database/20200930-HMM-StudyDatabase.dta")

# Study database updated with main values from 2021 prediction database
main <- read.csv(paste("./gen/update-old-studydb-covar/output/20200930-HMM-StudyDatabase_updatedPred2021Main_",resDate,".csv",sep=""))

# Study database updated with smooth values from 2021 prediction database
#sm <-  read.csv(paste("./gen/update/update-old-studydb-covar/20200930-HMM-StudyDatabase_updatedPred2021Sm_",resDate,".csv",sep=""))

# Study database updated with main values from prediction: add extra columns from old study database ----------

nrow(old)
nrow(main)
#nrow(sm)

ncol(old)
ncol(main)
#ncol(sm)

test <- old %>%  mutate(new_R = as.numeric(new_R))
sum(is.na(test))
sum(is.na(main))
#sum(is.na(sm))
