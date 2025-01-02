# Update old studies with new covariates ----------------------------------

source("./src/update-covar-for-old-studies/set-inputs.R")
source("./src/update-covar-for-old-studies/pull-data.R", local = new.env())
source("./src/update-covar-for-old-studies/recover-study-id.R", local = new.env())
source("./src/update-covar-for-old-studies/check-study-id-recovery.R", local = new.env())
source("./src/update-covar-for-old-studies/create-covar-key.R", local=new.env())
source("./src/update-covar-for-old-studies/audit-covar.R", local=new.env())
source("./src/update-covar-for-old-studies/harmonize-covar-names-scales.R", local=new.env()) # MODEL INPUT
source("./src/update-covar-for-old-studies/create-covar-replacement-key.R", local=new.env())
source("./src/update-covar-for-old-studies/update-covar.R", local=new.env())