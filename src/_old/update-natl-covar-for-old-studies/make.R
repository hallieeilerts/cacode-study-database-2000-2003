
# USED FOR BARCELONA MEETING BUT SINCE OUTDATED

# Update old studies with new covariates ----------------------------------

# For creating new study database in 2000-2023 round.
# The covariate names and scales of the new prediction database are used.

source("./src/update-natl-covar-for-old-studies/set-inputs.R")

# Run Pancho's master study db cleaning code to get the "id" variable which is in the model input
# Create a key for for this
#source(str_glue("./src/update-natl-covar-for-old-studies/recover-study-id.R"), local=new.env())

# Resave master study db with id variable (can this and above step be combined? is a key necessary?)
#source(str_glue("./src/update-natl-covar-for-old-studies/add-study-id.R"), local=new.env())

# # When cleaning up all code, this should replace create-covar-match-key and check-scales
# # Create a match key for covariate names between master 2000-2020 study db and 2000-2023 pred db and pred db covar scale
# source(str_glue("./src/update-natl-covar-for-old-studies/create-covar-match-key-with-scales.R"), local=new.env())

# # Only need to do once
# # Create a match key for covariate names between master 2000-2020 study db and 2000-2023 pred db and pred db covar scale
# source(str_glue("./src/update-natl-covar-for-old-studies/create-covar-match-key.R"), local=new.env())

# Replace covariate names in master 2000-2020 study db with those of pred database and adjust scales
#source(str_glue("./src/update-natl-covar-for-old-studies/harmonize-study-cols-scales.R"), local=new.env())

# Update names in model inputs as well
source(str_glue("./src/update-natl-covar-for-old-studies/harmonize-study-cols-scales-modinput.R"), local=new.env())

# Only need to do once
# # Audit to make sure scales match between old studies and pred
# # After this, scale adjustments are manually added to harmonize-study-cols-scales
# source(str_glue("./src/update-natl-covar-for-old-studies/check-scales.R"), local=new.env())

# Identify covariates to in master 2000-2020 study db through source columns
#source(str_glue("./src/update-natl-covar-for-old-studies/identify-covar-to-replace.R"), local=new.env())
source(str_glue("./src/update-natl-covar-for-old-studies/identify-covar-to-replace2.R"), local=new.env())

# Update national-level covariates with new values from prediction database
#source(str_glue("./src/update-natl-covar-for-old-studies/update-values-in-studydb.R"), local=new.env())
source(str_glue("./src/update-natl-covar-for-old-studies/update-values-in-studydb2.R"), local=new.env())

# Note: here we update variables in the study database, and then save a age-specific version essentially.
# But what if we want to use new covariates in the model?
# In the new prediction database, we have alcohol_f and alcohol_m instead of just alcohol_mf
# If we were to switch to using the sex-specific covariate in the models, would need to incorporate that here somehow.