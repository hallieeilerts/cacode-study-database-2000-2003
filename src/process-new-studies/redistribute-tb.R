################################################################################
#' @description Redistribute tb 
#' @return Study deaths with redistributed tb
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Study data with aggregated cods
dat <- read.csv(paste0("./gen/process-new-studies/temp/studies_cod-agg_", ageSexSuffix, ".csv", sep = ""))
## Single cause database
## Key with cod mapping for studies
dat_filename <- list.files("./data/single-causes")
dat_filename <- dat_filename[grepl("wide", dat_filename)] 
singlecauses <- read.csv(paste0("./data/single-causes/", dat_filename, sep = ""))
################################################################################

tb <- as.data.frame(singlecauses)[,c("AgeSexSuffix","ISO3","Year","TB","TBre")]
# Select age group
tb <- subset(tb, AgeSexSuffix == ageSexSuffix)
# Calculate percentage non-resp
tb$NRfrac <- tb$TBre/(tb$TB + tb$TBre)
tb <- tb[,c("ISO3", "Year", "NRfrac")]

# Merge onto study data
dat_tb <- merge(dat, tb, by.x = c("iso3","year_mid"), by.y = c("ISO3", "Year"), all.x = TRUE)

# REDISTRIBUTE TB: VA DATA
id1 <- which(!is.na(dat_tb$TB) & !is.na(dat_tb$OtherCMPN))
dat_tb$OtherCMPN[id1] <- (dat_tb$OtherCMPN + dat_tb$TB * dat_tb$NRfrac)[id1]

id1 <- which(!is.na(dat_tb$TB) & is.na(dat_tb$OtherCMPN))
dat_tb$OtherCMPN[id1] <- (dat_tb$TB * dat_tb$NRfrac)[id1]

id1 <- which(!is.na(dat_tb$TB) & !is.na(dat_tb$LRI))
dat_tb$LRI[id1] <- (dat_tb$LRI + dat_tb$TB * (1 - dat_tb$NRfrac))[id1]

id1 <- which(!is.na(dat_tb$TB) & is.na(dat_tb$LRI))
dat_tb$LRI[id1] <- (dat_tb$TB * (1 - dat_tb$NRfrac))[id1]

dat_tb$TB <- NULL

dat <- dat_tb[, names(dat_tb) != c('NRfrac')]

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/process-new-studies/temp/studies_tb-redist_",ageSexSuffix,".csv",sep =""), row.names = FALSE)


