################################################################################
#' @description Exclude by size and % of COD undetermined
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(data.table)
library(stringr)
library(readstata13)
#' Inputs
source("./src/set-inputs.R")
## Old study databases with newly extracted VA algorithm information
if(ageSexSuffix %in% c("00to28d", "01to59m")){
  dat <- read.csv(paste0("./gen/update-old-studies/temp/studies_cod-agg_", ageSexSuffix, ".csv"))
}
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
# Exclude "Undetermined" for certain checks
v_cod_noundt <- v_cod_reclass[!(v_cod_reclass %in% "Undetermined")]

# Create empty data frame for exclusions
dat_exc <- dat[0,]

# Exclude data points in which UNDETERMINED >= 25%
idExclude <- which(dat$Undetermined / dat$totdeaths >= .25)
if(length(idExclude) > 0 ){
  dat_undt <- dat[idExclude, ]
  dat_undt$exclude_reason <- "Undetermined >= 25%"
  dat_exc <- rbind(dat_exc, dat_undt)
  dat <- dat[-idExclude, ]
}

# Exclude data points with < 2 non-missing and non-zero causes
idExclude <- which(apply(dat[, paste(v_cod_noundt)], 1,
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

# Tidy
dat <- dat[,c(idVars, "totdeaths", v_cod_reclass)]
rownames(dat) <- NULL

# Save output -------------------------------------------------------------

write.csv(dat, paste("./gen/update-old-studies/temp/studies_exc_", ageSexSuffix,".csv", sep = ""), row.names = FALSE)
if(nrow(dat_exc) > 0){
  write.csv(dat_exc, paste0("./gen/update-old-studies/audit/dat_exc_",ageSexSuffix,".csv",sep =""), row.names = FALSE)
}


