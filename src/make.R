
################################################
# Project name: Compiling Study Database 2000-2023
################################################

# Initialize environment --------------------------------------------------

rm(list = ls())
source("./src/set-inputs.R")
source("./src/pull-data.R", local = new.env())

# Combine new studies and ad-hoc data -------------------------------------

source("./src/combine-studies-adhoc/clean-distiller-studychar.R", local = new.env())
source("./src/combine-studies-adhoc/clean-distiller-studycod.R", local = new.env())
source("./src/combine-studies-adhoc/clean-study-locations.R", local = new.env())
source("./src/combine-studies-adhoc/combine-study-info.R", local = new.env())
source("./src/combine-studies-adhoc/convert-to-deaths.R", local = new.env())
source("./src/combine-studies-adhoc/add-adhoc.R", local = new.env())
source("./src/combine-studies-adhoc/set-idvars.R", local = new.env())

# Clean new data points ---------------------------------------------------

source("./src/process-new-studies/reshape-cod-long.R", local = new.env())
source("./src/process-new-studies/cod-mapping.R", local = new.env())  # !! non-HMM are dropped from 5-19y. do for under-5?
source("./src/process-new-studies/drop-duplicates.R", local = new.env())
source("./src/process-new-studies/add-multiple-va-id.R", local = new.env())
source("./src/process-new-studies/cod-aggregation.R", local = new.env())
if(ageSexSuffix %in% c("05to09y", "10to14y")){source("./src/process-new-studies/redistribute-tb.R", local = new.env())}
source("./src/process-new-studies/adjust-total.R", local = new.env())
source("./src/process-new-studies/manage-other.R", local = new.env())
source("./src/process-new-studies/exclude-by-size.R", local = new.env()) # 
source("./src/process-new-studies/reclassify-cod.R", local = new.env())
source("./src/process-new-studies/merge-subnat-covar.R", local=new.env())
source("./src/process-new-studies/merge-new-study-pfpr.R", local=new.env())
source("./src/process-new-studies/merge-nat-covar.R", local=new.env())
source("./src/process-new-studies/audit-studies.R", local = new.env()) 

# Recover 2000-2019 study databases for 5-19y age groups ------------------

# NOTE
# For the 2000-2019 5-19 study database, the study database DTA file was manipulated a lot before arriving at the data points used in each model in the 2000-2019 round.
# Over the course of this manipulation, the id variable changes. Thus the id variable in the old study database DTA file does not match the model input.
# The old model input has covariates, but no source columns, making it impossible to update old national-level covariates there.
# This code below re-runs Pancho's old cleaning code; generates the study data base for 5-9, 10-14, 15-19f, 15-19m (dropping and combining various data points), and keeps the original id variable.
# Once the original id and the generated id corresponding to the model inputs are recovered, I merge the study database and model inputs (thus only keeping age-specific study data points included in each model), and update old national-level covariates using the source columns.
# This is not done for 2000-2019 neonate and postneonate study databases.
# We don't have a record of cleaning by Diana/Shefali/Simon/David/Jamie.
# So I just do some basic duplicate checking below. (drop-duplicates-under5.R)

if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF","15to19yM")){ 
  source("./src/recover-studies2019-5to19y/recover-study-id.R", local = new.env())
  source("./src/recover-studies2019-5to19y/match-studydb-modinput-datapoints.R", local = new.env())
  source("./src/recover-studies2019-5to19y/add-lmm-datapoints.R", local=new.env())
}

# Update old studies (national covariate values, COD names) ---------------

source("./src/update-old-studies/create-covar-key.R", local=new.env())
#source("./src/update-old-studies/audit-covar.R", local=new.env())
if(ageSexSuffix %in% c("00to28d", "01to59m")){
  source("./src/update-old-studies/add-new-extracted-vars.R", local=new.env())
}
source("./src/update-old-studies/set-idvars.R", local=new.env())
if(ageSexSuffix %in% c("00to28d", "01to59m")){
  source("./src/update-old-studies/drop-duplicates-under5.R", local=new.env())
}
source("./src/update-old-studies/harmonize-cod.R", local=new.env())
source("./src/update-old-studies/update-covar-names-scales.R", local=new.env())
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF","15to19yM")){ 
  source("./src/update-old-studies/merge-old-study-pfpr.R", local=new.env())
}
source("./src/update-old-studies/create-covar-replacement-key.R", local=new.env())
source("./src/update-old-studies/update-covar.R", local=new.env())
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF","15to19yM")){ 
  source("./src/update-old-studies/update-modinput-deaths.R", local=new.env())
}

# ad-hoc file for requesting subnational pfpr for 2000-2019 5-19y studies from MAP
#source("./src/update-old-studies/agecombined-5to19-studydb.R", local=new.env())

# Create new study database -----------------------------------------------

# Combining the old and new studies
source("./src/create-studydb/combine-studydb.R")
source("./src/create-studydb/drop-duplicates-bw-sysrev.R")
if(ageSexSuffix %in% c("01to59m")){
  source("./src/create-studydb/drop-malnutrition.R", local=new.env())
}
source("./src/create-studydb/create-codebook.R")
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF","15to19yM")){ 
  source("./src/create-studydb/combine-modinput-deaths.R")
  source("./src/create-studydb/create-modinput-studies.R")
}

