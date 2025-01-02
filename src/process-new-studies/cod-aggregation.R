################################################################################
#' @description Merge on cod aggregation key, aggregation causes, collapse data points for different sexes where appropriate
#' @return Study deaths with aggregated causes
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Study data with duplicates dropped
dat <- read.csv(paste0("./gen/process-new-studies/temp/studies_va-mult-id_", ageSexSuffix, ".csv", sep = ""))
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
#' Functions 
source("./src/process-new-studies/fn_aggCODbySex.R")
################################################################################

# Mapped causes (same in all age groups)
v_cod_mapped <- key$cod_mapped

# Reclassified CODs for this age group
# (includes Other and Undetermined)
v_cod_reclass <- unique(subset(key, !is.na(cod_reclass))$cod_reclass)

# Select all columns except for cod_mapped and cod_n
v_id_col <- names(dat)[!(names(dat) %in% c("cod_mapped", "cod_n"))]

# Merge on cod reclassification key
# Round deaths to whole numbers
datReclass <- dat %>%
  left_join(key, join_by(cod_mapped))
  
# See study causes mapped to NA
# These will soon be subtracted from total deaths and dropped
# (Confirm that this is correct decision for these causes)
# (e.g., colvio, natdis, stillbirth; some additional depending on age group)
unique(subset(datReclass, is.na(cod_reclass))$cod_mapped)

# See mapped causes not present in any study
# Some rare causes might not be reported in any studies (colvio, natdis)
# Check to see if anything abnormal. For instance, 15-19 should not include very many studies that report neonatal causes.
v_cod_mapped[!(v_cod_mapped %in% unique(datReclass$cod_mapped))]

# Reshape wide and aggregate deaths by cod reclass key
datWide <- datReclass %>%
  pivot_wider(
    id_cols = all_of(v_id_col),
    names_from = cod_reclass,
    values_from = cod_n,
    values_fn=sum
  ) %>%
  arrange(id)

# There will be an NA column of CODs were reported in the studies that we don't want to include
# These are in the mapping key but reclassified to NA
# Drop and subtract from total deaths
if("NA" %in% names(datWide)){
  datWide$totdeaths[!is.na(datWide$`NA`)] <- datWide$totdeaths[!is.na(datWide$`NA`)] - datWide$`NA`[!is.na(datWide$`NA`)]
  datWide$`NA` <- NULL
}

# If any reclassified COD columns are missing because not reported in any of the study data, add them
if(!all(v_cod_reclass %in% names(datWide))){
  v_missing_cod <- v_cod_reclass[!(v_cod_reclass %in% names(datWide))]
  for(i in 1:length(v_missing_cod)){
    datWide$emptycol <- NA
    names(datWide)[which(names(datWide) == "emptycol")] <- v_missing_cod[i]
  }
}

# Collapse sex-specific data points from the same study for ages without sex split
if(ageSexSuffix %in% c("00to28d","01to59m","05to09y", "10to14y")){
  datWide <- fn_aggCODbySex(datWide, key[,c("cod_reclass","cod_level2")])
}

# If the study is small, also collapse sex-specific data points for ages with sex split
if(ageSexSuffix %in% c("15to19yF", "15to19yM")){
  # NOTE: This was to happen after the adjust-tot.R script
  # I don't think it'll make much of a difference to put it here though.
  # And much cleaner.
  datWide$idtemp <- paste(datWide$iso3, datWide$year, datWide$ref_id, 
                          datWide$age_lb_m, datWide$age_ub_m, sep = '-')
  idLess <- unique(datWide$idtemp[which(datWide$totdeaths < minDeaths & datWide$sex != sexLabels[1])])
  dat_agg <- subset(datWide, idtemp %in% idLess)
  if(nrow(dat_agg) > 0){
    dat_agg <- fn_aggCODbySex(dat_agg, key[,c("cod_reclass","cod_level2")])
    datWide <- rbind(datWide, dat_agg)
  }
  datWide <- datWide[!(names(datWide) %in% "idtemp")]
}

# Save output -------------------------------------------------------------

write.csv(datWide, paste0("./gen/process-new-studies/temp/studies_cod-agg_",ageSexSuffix,".csv",sep =""), row.names = FALSE)

