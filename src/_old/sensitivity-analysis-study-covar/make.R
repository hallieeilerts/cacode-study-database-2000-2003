# Update old studies with new covariates (but keep old covar labels and scales) ----------------------------------

# For conducting sensitivity analysis.
# Updates values in old study database that were borrowed from prediction data
# Updates them with values from the 2000-2023 prediction database
# The covariate names and scales of the old study database are used.
# This was made and shared with Pancho on June 14, 2024 for him to re-estimate the model and see how the covariates changed.
# I don't think it was carried out

# Match study covariate names to prediction covariate names
source(str_glue("./src/sensitivity-analysis-study-covar/match-pred-study-names.R"), local=new.env())
source(str_glue("./src/sensitivity-analysis-study-covar/match-pred-study-names_india.R"), local=new.env())

# Compare study covariate scales to prediction covariate scales in plots
source(str_glue("./src/sensitivity-analysis-study-covar/compare-pred-study-scales.R"), local=new.env())
source(str_glue("./src/sensitivity-analysis-study-covar/compare-pred-study-scales_india.R"), local=new.env())

# Match India states to study_locations in India
source(str_glue("./src/sensitivity-analysis-study-covar/match-pred-study-locations_india.R"), local=new.env()) 

# Identify study covariate values that need updating based on source column
# (i.e., are the covariate values from the article, prediction database, india subnational prediction database, or dhs?)
# (No need to update article or DHS datapoints)
source(str_glue("./src/sensitivity-analysis-study-covar/identify-study-covar-sources.R"), local=new.env()) 

# Create new prediction database with same covariate names and scales as study database
source(str_glue("./src/sensitivity-analysis-study-covar/harmonize-pred-study.R"), local=new.env())
source(str_glue("./src/sensitivity-analysis-study-covar/harmonize-pred-study_india.R"), local=new.env())

source(str_glue("./src/sensitivity-analysis-study-covar/update-values-in-studydb.R"), local=new.env())

source(str_glue("./src/sensitivity-analysis-study-covar/format-new-studydb.R"), local=new.env())

source(str_glue("./src/sensitivity-analysis-study-covar/quality-checks.R"), local=new.env())

source(str_glue("./src/sensitivity-analysis-study-covar/generate-comparison-plots.R"), local=new.env())



