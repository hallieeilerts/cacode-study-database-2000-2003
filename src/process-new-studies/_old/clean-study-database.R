################################################################################
#' @description This is a temporary solution to formatting the study database. Need to covert as much code into functions as possible.
#' @return Age/sex specific study database
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readxl)
require(tidyverse)
#' Inputs
source("./src/process-new-studies/set-inputs.R")

#' Functions 
source("./src/process-new-studies/fn_aggCODbySex.R")

# New studies from 2000-2023 systematic review in long format
studydb <- read.csv("./data/distiller/cod long_before exclude.csv")
# Classification keys
key_studycodmapping <- read_excel("./data/classification-keys/StudyCauseMapping_20240926.xlsx")
key_ctryclass_u20 <- read.csv("./data/classification-keys/20201001-CountryModelClass.csv")
key_cod <- read.csv(paste("./data/classification-keys/key_cod_", ageSexSuffix, ".csv", sep=""))
# Single cause data for tb resp split
singlecauses <- read.csv("./data/single-causes/SingleCauseDatabase2023-wide_20241007.csv")
################################################################################


# Prepare COD keys --------------------------------------------------------

# Study cod mapping key
if(ageSexSuffix == "05to09y"){key_studycodmapping <- key_studycodmapping[,c("cause_of_death","5-9y")]
                              refCat <- "Diarrhoeal"}
if(ageSexSuffix == "10to14y"){key_studycodmapping <- key_studycodmapping[,c("cause_of_death","10-14y")]
                              refCat <- "LRI"}
if(ageSexSuffix == "15to19yF"){key_studycodmapping <- key_studycodmapping[,c("cause_of_death","15-19 f")]
                              refCat <- "SelfHarm"}
if(ageSexSuffix == "15to19yM"){key_studycodmapping <- key_studycodmapping[,c("cause_of_death","15-19 m")]
                                refCat <- "RTI"}
names(key_studycodmapping) <- c("cod_study", "cod_mapped")

# COD categorization key
# mapped/study reclassification/modeling cods
# key_cod for causes studies were mapped too, the disaggregations needed for study db cleaning, 
# groups needed for adding causes to general other categories during cleaning (when collapsing studies and same cods not reported in each stuy),
# reclassifying and the disaggregations for modeling

# Mapped CODs that are used in study database cleaning (either redistributed among other causes or will be modeled later)
key_cod_studydb_reclass <- subset(key_cod, !is.na(cod_studydb_reclass))[,c("cod_mapped", "cod_studydb_reclass")]
v_cod_reclass <- unique(key_cod_studydb_reclass$cod_studydb_reclass)

# CODs used in study database cleaning that are modeled
# (drops tb because it has been split between lri/othercmpn, drops causes we don't model like undt and otehr)
key_cod_modeled <- subset(key_cod, !is.na(cod_studydb_reclass))[,c("cod_studydb_reclass", "cod_modeled")]
key_cod_modeled <- key_cod_modeled[!duplicated(key_cod_modeled),]
v_cod_modeled <- unique(key_cod_modeled$cod_modeled)
v_cod_modeled <- v_cod_modeled[!is.na(v_cod_modeled)]

# CODs used in study database cleaning that are reclassified to larger other categories
key_cod_studydb_level2 <- subset(key_cod, !is.na(cod_studydb_level2))[,c("cod_studydb_reclass", "cod_studydb_level2")]
key_cod_studydb_level2 <- key_cod_studydb_level2[!duplicated(key_cod_studydb_level2),]
v_cod_studydb_level2 <- unique(key_cod_studydb_level2$cod_studydb_level2)
v_cod_studydb_level2 <- v_cod_studydb_level2[!is.na(v_cod_studydb_level2)]

# Load study database -----------------------------------------------------

dat <- studydb

# Update variable names
names(dat)[names(dat) == "iso3"] <- "ISO3"
names(dat)[names(dat) == 'strata_gender'] <- 'Sex'
names(dat)[names(dat) == 'study_location'] <- 'location'
names(dat)[names(dat) == 'totdeaths_orig'] <- 'totdeaths'
rownames(dat) <- NULL

# Set minimum number of deaths per data point
minDeaths <- 15
# Maximum number of deaths per data point
maxDeaths <- 5000

# Keep age/sex group of interest
if(ageSexSuffix == "05to09y"){dat <- subset(dat, inc_5to9y == 1)}
if(ageSexSuffix == "10to14y"){dat <- subset(dat, inc_10to14y == 1)}
if(ageSexSuffix == "15to19yF"){dat <- subset(dat, inc_15to19y.f == 1)}
if(ageSexSuffix == "15to19yM"){dat <- subset(dat, inc_15to19y.m == 1)}

# REMOVE data points from COUNTRIES NOT VA
idExclude <- which(!dat$ISO3 %in% key_ctryclass_u20$ISO3[key_ctryclass_u20$Group %in% c('HMM/LMM', 'HMM')])
unique(dat$ISO3[idExclude])
if (length(idExclude) > 0) {
  dat <- dat[-idExclude, ]
  dat <- droplevels(dat)
  rownames(dat) <- NULL
}

# Re-label variables
dat$Sex[dat$Sex == 1] <- "Male"
dat$Sex[dat$Sex == 2] <- "Female"
dat$Sex[dat$Sex == 3] <- "Both"
dat$Sex[dat$Sex == "Male"] <- 3
dat$Sex[dat$Sex == "Female"] <- 2
dat$Sex[dat$Sex == "Both"] <- 1
dat$Sex <- as.factor(dat$Sex)
levels(dat$Sex)[levels(dat$Sex) == 1] <- 'Both'
levels(dat$Sex)[levels(dat$Sex) == 2] <- 'Female'
levels(dat$Sex)[levels(dat$Sex) == 3] <- 'Male'
dat$ISO3[dat$ISO3 == "Mutliple"] <- "Multiple"

# Create year variable is middle year of study
dat$Year <- floor(dat$year_start + (dat$year_end - dat$year_start)/2)
names(dat)[names(dat) == "year"] <- "Year"

# Recode value that is problematic in sex aggregation later
dat$strata_other1[dat$Refid == "10342"] <- NA

# Drop rows that are...
# 1. missing cod_n due to missing denominators
# 2. facility studies
dat <- subset(dat, !(remove == 1))
# Takes care of all but one study still missing cod_n
dat <- subset(dat, !is.na(cod_n))

# Remove unnecessary columns
dat <- dat[!(names(dat) %in% c("Facility", "remove", "country", 
                               "inc_neo", "inc_1to59","inc_5to9y", "inc_10to14y", "inc_15to19y.f", "inc_15to19y.m"))]

# Order
dat <- dat[order(dat$Refid, dat$strata_id, dat$strata_other1, dat$strata_other2),]

# Merge on cause mapping and aggregate ------------------------------------

# Merge on age/sex specific mapping. Not doing this carefully right now.
# Will need to check this carefully to see what is dropped later.
dat <- merge(dat, key_studycodmapping, by.x = "cause_of_death", by.y = "cod_study")

# Drop causes that were not mapped to anything
dat <- subset(dat, !is.na(cod_mapped))
dat <- subset(dat, !(cod_mapped == "NA"))
# Check which causes weren't mapped and are getting dropped, but have deaths
# View(subset(dat, cod_mapped == "NA" & cod_n > 0))

# Merge on cod categorization key
# This will be used to reshape the data to wide
dat <- merge(dat, key_cod_studydb_reclass, by = "cod_mapped")

# Set id columns
v_id_cols <- c("Refid", "strata_id","strata_other1","strata_other2",
               "ISO3","location",
               "year_start", "year_end","Year", "age_lb_m", "age_ub_m", "Sex","totdeaths")

# Reshape wide and aggregate deaths by cod_studydb_reclass
dat_wide <- dat %>%
  pivot_wider(
    id_cols = v_id_cols,
    names_from = cod_studydb_reclass,
    values_from = cod_n,
    values_fn=sum
  )
# strata_id is the unique id. Save as id
# remove special characters
dat_wide$id <- sapply(dat_wide$strata_id, function(x) gsub("[^[:alnum:]\\s]", "-", x))
nrow(dat_wide) == length(unique(dat_wide$id))
dat_wide <- dat_wide[,c("id", names(dat_wide)[!(names(dat_wide) %in% "id")])]
# Order
dat_wide <- dat_wide[order(dat_wide$id),]

# If any modeled COD columns are completely missing because not reported in the study data, add them
if(!all(v_cod_reclass %in% names(dat_wide))){
  
  v_missing_cod <- v_cod_reclass[!(v_cod_reclass %in% names(dat_wide))]
  for(i in 1:length(v_missing_cod)){
    dat_wide$emptycol <- NA
    names(dat_wide)[which(names(dat_wide) == "emptycol")] <- v_missing_cod[i]
  }
}

#-----------------------------------------#
# COLLAPSE DATA POINTS WITH DIFFERENT SEX #
#-----------------------------------------#

# Create temporary id for setting which data points will be collapsible
# Same REFID, COUNTRY, YEAR and AGE
dat_wide$idtemp <- paste(dat_wide$Refid, 
                         dat_wide$ISO3, 
                         dat_wide$Year, 
                         dat_wide$age_lb_m, dat_wide$age_ub_m, sep = '-')

# Collapse sex-specific data points when NO SEX SPLIT
if(ageSexSuffix %in% c("00to28d","01to59m","05to09y", "10to14y")){
  
  dat_wide <- fn_aggCODbySex(dat_wide, key_cod_studydb_level2)
  
}

#-----------------# 
# REDISTRIBUTE TB #
#-----------------#

if(ageSexSuffix %in% c("05to09y", "10to14y")){
  
  dat_tb <- dat_wide
  
  tb <- as.data.frame(singlecauses)[,c("AgeSexSuffix","ISO3","Year","TB","TBre")]
  # Select age group
  tb <- subset(tb, AgeSexSuffix == ageSexSuffix)
  # Calculate percentage non-resp
  tb$NRfrac <- tb$TBre/(tb$TB + tb$TBre)
  tb <- tb[,c("ISO3", "Year", "NRfrac")]
  
  # Merge onto study data
  dat_tb <- merge(dat_tb, tb, by = c("ISO3", "Year"), all.x = TRUE)
  
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
  
  dat_tb <- dat_tb[, names(dat_tb) != c('NRfrac')]
  
  dat_wide <- dat_tb
}


#----------------------------#
# RECLASSIFY CAUSES OF DEATH #
#----------------------------#

# TB: ONLY applies to 15-19
cod <- v_cod_reclass
cod[cod == "TB"] <- "OtherCMPN"
cod <- unique(cod)

# I dont think it accounts for the "other dumpster" step though
# Recode other categories as NA if they have zero deaths
# head(as.data.frame(dat_wide))
# Not sure why...
# Other dumpster
dat_wide$OtherCMPN[which(dat_wide$OtherCMPN == 0)] <- NA
dat_wide$OtherNCD[which(dat_wide$OtherNCD == 0)] <- NA
dat_wide$OtherInj[which(dat_wide$OtherInj == 0)] <- NA
dat_wide$Other[which(dat_wide$Other == 0)] <- NA

#--------------------#
# REFERENCE CATEGORY #
#--------------------#

# Makes "deaths", but this is coded over later

# Choose REFERENCE category
cod <- as.factor(c(refCat, paste(cod[cod != refCat])))
cod <- relevel(cod, ref = refCat)
cod
# NOTE: Probably can just continue to call this v_cod_reclass. Changing name for now
# TO match pancho's code

#---------------------#
# ADJUST TOTAL DEATHS when they are smaller than sum of COD #
#---------------------#

dat_adj <- dat_wide

# INCREASE TOTAL DEATHS when they are SMALLER than the sum of COD
which(dat_adj$totdeaths - apply(dat_adj[, paste0(cod )], 1, sum, na.rm = T) < -1)
totSmall <- which(dat_adj$totdeaths - apply(dat_adj[, paste0(cod)], 1, sum, na.rm = T) < -.01)
length(totSmall)
dat_adj$totdeaths[totSmall] <- apply(dat_adj[totSmall, paste0(cod)], 1, sum, na.rm = T)

#--------------#
# ADJUST OTHER when total deaths are bigger than sum of COD #
#--------------#

# Data points in which total is larger than sum of COD
totLarge <- which(dat_adj$totdeaths - apply(dat_adj[, paste0(cod)], 1, sum, na.rm = T) > .01)
dat_adj$totdeaths[totLarge] 
apply(dat_adj[totLarge, paste0(cod)], 1, sum, na.rm = T)

otherCat <- c('OtherCMPN', 'OtherNCD', 'OtherInj', 'Other')

# Update OTHER
if (length(totLarge) > 1) {
  
  for (i in totLarge) {
    # Difference in deaths
    diffDeath <- dat_adj$totdeaths[i] - sum(dat_adj[i, paste0(cod)], na.rm = T)
    # Proportion in 'other' categories
    propOther <- dat_adj[i, paste(otherCat)] / sum(dat_adj[i, paste(otherCat)], na.rm = T)
    # Redistribute excess of deaths among 'other' categories
    dat_adj[i, paste(otherCat)] <- dat_adj[i, paste(otherCat)] + diffDeath * propOther
  }
}

#-----------------------#
# EXCLUDE SINGLE CAUSES #
#-----------------------#

# Single causes
codExclude <- c('CollectVio', 'NatDis')

# Exclude SINGLE CAUSE ESTIMTES
dat_adj <- droplevels(dat_adj[, !names(dat_adj) %in% codExclude])

# Re-define COD vector
cod <- droplevels(cod[!cod %in% codExclude])

# Re-calculate TOTAL DEATHS
dat_adj$totdeaths <- apply(dat_adj[, paste0(cod)], 1, sum, na.rm = T)

#-------------------------------------------------------#
# EXCLUDE DATA POINTS WITH ONLY OTHER-DUMPSTER REPORTED #
#-------------------------------------------------------#

dat_exc <- dat_adj

nonOtherCat <- v_cod_modeled[!(v_cod_modeled %in% otherCat)]

# Identify cases where all non-other causes are NA or sum to zero
idExclude <- apply(dat_exc[, paste(cod)], 1,
                   function(x) {
                     if ( all(is.na(x[names(x) %in% nonOtherCat])) |
                          sum(x[names(x) %in% nonOtherCat], na.rm=T) == 0) {
                       return(1)
                     } else return(0)
                   })
table(idExclude)

# Save before exclusions
dat_aud1 <- dat_exc[idExclude,]
if(nrow(dat_aud1) > 0){
  dat_aud1$exclude <- 1
  dat_aud1$exclude_reason <- "Only other dumpster reported"
}

# Exclude data points
if (sum(idExclude) > 0) {
  dat_exc <- dat_exc[which(idExclude == 0), ]
  rownames(dat_exc) <- NULL
  dat_exc <- droplevels(dat_exc)
}


#------------------------------------------------#
# AVOID DOUBLE COUNTING: OVERLAPPING DATA POINTS #
#------------------------------------------------#

# Same COUNTRY/REGION, YEAR, REFID, STRATA, AGE and SEX
dat_exc$idtemp1 <- paste(dat_exc$ISO3, dat_exc$Year, dat_exc$Refid, 
                         dat_exc$strata_id,
                         dat_exc$age_lb_m, dat_exc$age_ub_m, dat_exc$Sex, sep = '-')
# If length() and nrow() are different, need to decide to exclude a data point
length(unique(dat_exc$idtemp1))
nrow(dat_exc)
dupid <- dat_exc$idtemp1[which(duplicated(dat_exc$idtemp1))]
#View(subset(dat_exc, id %in% dupid))

# Same COUNTRY, YEAR, REFID, AGE and SEX
dat_exc$idtemp2 <- paste(dat_exc$ISO3, dat_exc$Year, 
                         dat_exc$age_lb_m, dat_exc$age_ub_m, dat_exc$Sex, sep = '-')
# If length() and nrow() are different, need to decide to exclude a data point
length(unique(dat_exc$idtemp2))
nrow(dat_exc)
dupidtemp2 <- dat_exc$idtemp2[which(duplicated(dat_exc$idtemp2))]
# View(subset(dat_exc, idtemp2 %in% dupidtemp2))
if(ageSexSuffix %in% c("05to09y", "10to14y")){ # Same data points get excluded in these age groups
  dat_exc <- subset(dat_exc, !(Refid == "10645" & strata_other1 == "method-Tariff reallocated")) # excluding this one because has more undetermined
  dat_exc <- subset(dat_exc, !(Refid == "75")) # Could exclude either
  dat_exc <- subset(dat_exc, !(Refid == "10243"))  # Excluding this one because deaths for Diarrhoeal look less accurate
  dat_exc <- subset(dat_exc, !(Refid == "24967")) # Could exclude either
  dat_exc <- subset(dat_exc,!(Refid == "25680")) # Could exclude either
  dat_exc <- subset(dat_exc,!(Refid == "346")) # Could exclude either
}
if(ageSexSuffix %in% c("15to19yF", "15to19yM")){ # Same data points get excluded in these age groups
  dat_exc <- subset(dat_exc, !(Refid == "24933")) # Could exclude either
}

#-----------------------------------------------#
# AVOID DOUBLE COUNTING: OVERLAPPING SEX GROUPS #
#-----------------------------------------------#

# Same COUNTRY, YEAR, REFID, and SEX
dat_exc$idtemp3 <- paste(dat_exc$ISO3, dat_exc$Year,  dat_exc$Refid, dat_exc$Sex, sep = '-')
length(unique(dat_exc$idtemp3))
nrow(dat_exc)
dupidtemp3 <- dat_exc$idtemp3[which(duplicated(dat_exc$idtemp3))]
# View(subset(dat_exc, idtemp3 %in% dupidtemp3))

# Same COUNTRY, YEAR, REFID
dat_exc$idtemp4 <- paste(dat_exc$ISO3, dat_exc$Year, dat_exc$Refid, sep = '-')
length(unique(dat_exc$idtemp4))
nrow(dat_exc)
dupidtemp4 <- dat_exc$idtemp4[which(duplicated(dat_exc$idtemp4))]
# View(subset(dat_exc, idtemp4 %in% dupidtemp4))

dat_exc <- dat_exc[!(names(dat_exc) %in% c("idtemp1","idtemp2", "idtemp3", "idtemp4"))]


#----------------#
# RANDOM EFFECTS #
#----------------#

# !!!!!! LOCATION IS THE INDICATOR FOR WHETHER THE DATA IS NATIONALLY REPRESENTATIVE
# Astha plans to add

# # RANDOM EFFECTS term: COUNTR/REGION, REFID and LOCATION
dat_exc$reterm <- paste(dat_exc$ISO3, dat_exc$Refid, sep = '-')
# dat$reterm <- paste(dat$ISO3, dat$Refid, dat$location, sep = '-')
# 
# # NATIONALLY representative studies
# unique(dat$reterm[dat$location == 1])
# dat$reterm[dat$location == 1]
# dat$reterm[dat$location == 1] <- dat$ISO3[dat$location == 1]
# unique(dat$reterm[dat$location == 1])
# 
# # Tidy up
# dat <- droplevels(dat)
# rownames(dat) <- NULL
# dim(dat)

#---------------------------------------#
# AGGREGATE DATA POINTS WITH FEW DEATHS #
#---------------------------------------#

dat_agg <- dat_exc

# Number of studies with less than 25 deaths
length(which(dat_agg$totdeaths < minDeaths))
length(which(dat_agg$totdeaths < minDeaths)) / nrow(dat_agg)

# Identify data points with same: COUNTRY/REGION, YEAR, REFID, LOCATION, and AGE
dat_agg$idtemp <- paste(dat_agg$ISO3, dat_agg$Year, dat_agg$Refid, 
                        dat_agg$age_lb_m, dat_agg$age_ub_m, sep = '-')
length(unique(dat_agg$idtemp))
nrow(dat_agg)

# Identify candidates for merging
idLess <- unique(dat_agg$idtemp[which(dat_agg$totdeaths < minDeaths & dat_agg$Sex != sexLabels[1])])
length(idLess)
table(dat_agg$Refid[dat_agg$totdeaths < minDeaths])
table(dat_agg$ISO3[dat_agg$totdeaths < minDeaths])
table(dat_agg$ISO3)

# This has not been tested yet because idLess always equals 0 for 15-19f or 15-19m
# If idLess > 0, would need to look closely to make sure runs ok
if(length(idLess) > 0 & ageSexSuffix %in% c("15to19yF", "15to19yM")){
  
  dat_agg <- fn_aggCODbySex(dat_agg, key_cod_studydb_level2)
  
}

# Re-calculate TOTAL DEATHS
dat_agg$totdeaths <- apply(dat_agg[, paste0(cod)], 1, sum, na.rm = T)

#------------------#
# MANAGE OTHER COD #
#------------------#

dat_mgmt <- dat_agg

# ASSIGN NA to OTHER when OTHER = 0 and other-specific reported
idOther <- which(dat_mgmt$Other == 0 & !is.na(dat_mgmt$OtherCMPN) &
                   !is.na(dat_mgmt$OtherNCD) & !is.na(dat_mgmt$OtherInj))
length(idOther)
if (length(idOther) > 0) dat_mgmt[idOther, 'Other'] <- NA

# RE-DISTRIBUTE OTHER when other-specific reported AND > 0
idOther <- which(!is.na(dat_mgmt$Other) & dat_mgmt$OtherCMPN >= 0 &
                   dat_mgmt$OtherNCD >= 0 & dat_mgmt$OtherInj >= 0)
if (length(idOther) > 0) {
  otherCat <- c('OtherCMPN', 'OtherNCD', 'OtherInj')
  for (i in idOther) {
    # Proportion of deaths in each 'other-specific' category
    propOther <- prop.table(dat_mgmt[i, otherCat])
    # Re-distribute deaths in 'Other' proportionally
    dat_mgmt[i, otherCat] <- dat_mgmt[i, otherCat] + propOther * dat_mgmt[i, 'Other']
    # Assign NA to Other
    dat_mgmt[i, 'Other'] <- NA
  }
}

# If other and other-specific are missing, other should be assigned a 0
# This is for the reclassification matrix
idOther <- which(is.na(dat_mgmt$Other) &
                   (is.na(dat_mgmt$OtherCMPN) | is.na(dat_mgmt$OtherNCD) | is.na(dat_mgmt$OtherInj)))
if(length(idOther) > 0){dat_mgmt$Other[idOther] <- 0}

# Exclude Other when all NA
if (all(is.na(dat_mgmt$Other))) {
  dat_mgmt <- dat_mgmt[, !names(dat_mgmt) %in% 'Other']
  cod <- droplevels(cod[cod != 'Other'])
}

# ROUND TO INTEGERS and Re-calculate TOTAL DEATHS
dat_mgmt[, paste0(cod)] <- round(dat_mgmt[, paste0(cod)])
dat_mgmt$totdeaths <- apply(dat_mgmt[, paste0(cod)], 1, sum, na.rm = T)

# DOUBLE CHECK
idOther <- which((dat_mgmt$Other == 0 | !is.na(dat_mgmt$Other)) &
                   !is.na(dat_mgmt$OtherCMPN) & !is.na(dat_mgmt$OtherNCD) & !is.na(dat_mgmt$OtherInj))
nrow(dat_mgmt[idOther, ])
idOther <- which(is.na(dat_mgmt$Other) &
                   (is.na(dat_mgmt$OtherCMPN) | is.na(dat_mgmt$OtherNCD) | is.na(dat_mgmt$OtherInj)))
nrow(dat_mgmt[idOther, ])

# Check there are no negative values
which(dat_mgmt[, paste(cod)] < 0)


#---------------------#
# EXCLUDE EXTREMELY LARGE OR SMALL DATA POINTS #
#---------------------#

dat_exc <- dat_mgmt

# Re-calculate TOTAL DEATHS
dat_exc$totdeaths <- apply(dat_exc[, paste0(cod)], 1, sum, na.rm = T)

# Exclude data points in which UNDETERMINED >= 25%
idExclude <- which(dat_exc$Undetermined / dat_exc$totdeaths >= .25)

# Save before exclusions
# (already excluded overlapping datapoints)
dat_aud2 <- dat_exc[idExclude,]
if(nrow(dat_aud2) > 0){
  dat_aud2$exclude <- 1
  dat_aud2$exclude_reason <- "Undetermined >= 25%"
}

# Exclude data points in which UNDETERMINED >= 25%
if (length(idExclude) > 0) dat_exc <- dat_exc[-idExclude, ]

# Exclude UNDETERMINED from remaining data points
dat <- dat[, !names(dat) %in% c('Undetermined')]
cod <- droplevels(cod[cod != 'Undetermined'])

# Re-calculate TOTAL DEATHS
dat_exc$totdeaths <- apply(dat_exc[, paste0(cod)], 1, sum, na.rm = T)

# Number of data points with less than 15 deaths
length(which(dat_exc$totdeaths < minDeaths))
length(which(dat_exc$totdeaths < minDeaths)) / nrow(dat_exc)

# Exclude data points with LESS than 15 DEATHS
# Save before exclusions
dat_aud3 <- subset(dat_exc, dat_exc$totdeaths < minDeaths | dat_exc$totdeaths > maxDeaths)
if(nrow(dat_aud3) > 0){
  dat_aud3$exclude <- 1
  dat_aud3$exclude_reason <- ifelse(dat_aud3$totdeaths < minDeaths, "totdeaths < minDeaths", "totdeaths > maxDeaths")
  dat_aud3 <- dat_aud3[order(dat_aud3$totdeaths),]
}

# Exclude data points with LESS than 15 DEATHS
idExclude <- which(dat_exc$totdeaths < minDeaths)
if (length(idExclude) > 0) {
  dat_exc <- droplevels(dat_exc[-idExclude, ])
  rownames(dat_exc) <- NULL
}
range(dat_exc$totdeaths)
sort(dat_exc$totdeaths[dat_exc$totdeaths < 25], decreasing = F)
length(which(dat_exc$totdeaths < 25))

# Exclude data points with MORE than 5000 DEATHS
idExclude <- which(dat_exc$totdeaths > maxDeaths)
if (length(idExclude) > 0) {
  dat_exc <- droplevels(dat_exc[-idExclude, ])
  rownames(dat_exc) <- NULL
}
range(dat_exc$totdeaths)
sort(dat_exc$totdeaths[dat_exc$totdeaths < 25], decreasing = F)
length(which(dat_exc$totdeaths < 25))

# Save excluded and non-excluded together
dat_aud <- dplyr::bind_rows(dat_aud1, dat_aud2, dat_aud3, dat_exc)
dat_aud$exclude[is.na(dat_aud$exclude)] <- 0
dat_aud <- dat_aud[names(dat_aud) %in% c("id","ISO3", "Year", "year_start", "year_end", "age_lb_m", "age_ub_m", "Sex", "totdeaths", "exclude", "exclude_reason")]
dat_aud <- dat_aud[order(dat_aud$id),]
write.csv(dat_aud, paste0("./gen/process-new-studies/audit/NewStudyDataPoints_",ageSexSuffix,".csv"), row.names = FALSE)

############################################################
### DEATHS DATABASE                                      ###  
############################################################

dat_study <- dat_exc

# Add new study id
dat_study$sid <- 1:nrow(dat_study) #paste0("sysrev24_", 1:nrow(dat_study))

# Tidy
dat_study <- dat_study[, c("sid", 'id', "ISO3", "Year", 'Sex', 'reterm', 'totdeaths', paste(cod))]
rownames(dat_study) <- NULL
dat_study <- droplevels(dat_study)

# Save output(s) ----------------------------------------------------------

write.csv(dat_study, paste("./gen/process-new-studies/output/StudyData2023_", ageSexSuffix,".csv", sep = ""), row.names = FALSE)

