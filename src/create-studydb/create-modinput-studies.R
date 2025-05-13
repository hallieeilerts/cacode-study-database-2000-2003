################################################################################
#' @description Create new model input for all stuides
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/set-inputs.R")
## Combined old and new studies
dat_filename <- list.files("./gen/create-studydb/output")
dat_filename <- dat_filename[grepl("StudyDatabase2023", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- dat_filename[!grepl("Codebook", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename), 1)
dat <- read.csv(paste0("./gen/create-studydb/output/", dat_filename, sep = ""))
## Model covariate list
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("CovariateDatabase2023_ModelCovariateList", dat_filename, ignore.case = TRUE)]
dat_filename <- tail(sort(dat_filename), 1)
df_covar <- readxl::read_excel(paste0("./data/classification-keys/",dat_filename), sheet = "model-covar-long")
################################################################################

# limited id variables for model input
v_idvars <- c("recnr","id", "reterm", "totdeaths")

# Load covariate names
vxf <- subset(df_covar, Model == "HMM" & AgeSexSuffix == ageSexSuffix)$Covariate

studies <- dat[, c(v_idvars, vxf)]
rownames(studies) <- NULL

# Save output(s) ----------------------------------------------------------

save(studies, file = paste("./gen/create-studydb/output/ModInput2023-HMM-Studies_",ageSexSuffix,"_",resDate,".RData", sep = ""))
