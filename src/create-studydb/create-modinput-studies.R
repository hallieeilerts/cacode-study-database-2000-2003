################################################################################
#' @description Create new model input for all stuides
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/set-inputs.R")
source("./src/create-studydb/fn_setCov.R")
## Combined old and new studies
dat_filename <- list.files("./gen/create-studydb/output")
dat_filename <- dat_filename[grepl("StudyDatabase2023", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- dat_filename[!grepl("Codebook", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename), 1)
dat <- read.csv(paste0("./gen/create-studydb/output/", dat_filename, sep = ""))
################################################################################

# limited id variables for model input
v_idvars <- c("recnr","id", "reterm", "totdeaths")

# Load covariate names
vxf <- fn_setCov(ageSexSuffix)

studies <- dat[, c(v_idvars, vxf)]
rownames(studies) <- NULL

# Save output(s) ----------------------------------------------------------

save(studies, file = paste("./gen/create-studydb/output/ModInput2023-Studies_",ageSexSuffix,"_",resDate,".RData", sep = ""))
