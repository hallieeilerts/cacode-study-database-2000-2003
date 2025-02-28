################################################################################
#' @description Exclude studies with >= 25% Undetermined, < 2 CODs, and too few/many total deaths
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Study data with adjusted totals
dat <- read.csv(paste0("./gen/process-new-studies/temp/studies_mng-other_", ageSexSuffix, ".csv", sep = ""))
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Reclassified CODs for this age group (includes Other and Undetermined)
v_cod_reclass <- unique(subset(key, !is.na(cod_reclass))$cod_reclass)
# Exclude "TB" which has been redistributed (only present in 5-9y and 10-14y reclass vector)
v_cod_reclass <- v_cod_reclass[!(v_cod_reclass %in% "TB")]
# Exclude "Undetermined" which will be used here to eliminate studies
v_cod_reclass <- v_cod_reclass[!(v_cod_reclass %in% "Undetermined")]

# Create empty data frame for exclusions
dat_exc <- dat[0,]

# Exclude data points with < 2 non-missing and non-zero causes
idExclude <- which(apply(dat[, paste(v_cod_reclass)], 1,
                   function(x) {
                     if ( sum(!is.na(x) & round(x) != 0) < 2) {
                       return(1)
                     } else return(0)
                   }) == 1)
if(length(idExclude) > 0){
  dat_less2 <- dat[idExclude, ]
  dat_less2$exclude_reason <- "Less than 2 CODs reported"
  dat_exc <- rbind(dat_exc, dat_less2)
  dat <- dat[-idExclude, ]
}

# Exclude data points in which UNDETERMINED >= 25%
idExclude <- which(dat$Undetermined / dat$totdeaths >= .25)
if(length(idExclude) > 0 ){
  dat_undt <- dat[idExclude, ]
  dat_undt$exclude_reason <- "Undetermined >= 25%"
  dat_exc <- rbind(dat_exc, dat_undt)
  dat <- dat[-idExclude, ]
}

# Exclude Undetermined from data points
dat <- dat[, !names(dat) %in% c('Undetermined')]

# Re-calculate total deaths
dat$totdeaths <- apply(dat[, paste0(v_cod_reclass)], 1, sum, na.rm = T)

# Exclude data points with less than min deaths
idExclude <- which(dat$totdeaths < minDeaths)
if(length(idExclude) > 0 ){
  dat_small <- dat[idExclude, ]
  dat_small$exclude_reason <- "totdeaths < minDeaths"
  dat_exc <- dplyr::bind_rows(dat_exc, dat_small)
  dat <- dat[-idExclude, ]
}
  
# Exclude data points with MORE than max deaths
idExclude <- which(dat$totdeaths > maxDeaths)
if (length(idExclude) > 0) {
  dat_big <- dat[idExclude, ]
  dat_big$exclude_reason <- "totdeaths > maxDeaths"
  dat_exc <- dplyr::bind_rows(dat_exc, dat_big)
  dat <- dat[-idExclude, ]
}

# Fill in recnr that up to now was NAs
# No more exclusions will take place (except between old and new studies)
dat <- dat[order(dat$id),]
dat$recnr <- 1:nrow(dat)

# Tidy
dat <- dat[, c(idVars, "totdeaths", v_cod_reclass)]
rownames(dat) <- NULL

# Save output -------------------------------------------------------------

write.csv(dat, paste("./gen/process-new-studies/temp/studies_exc_", ageSexSuffix,".csv", sep = ""), row.names = FALSE)
write.csv(dat_exc, paste0("./gen/process-new-studies/audit/dat_exc2_",ageSexSuffix,".csv",sep =""), row.names = FALSE)




