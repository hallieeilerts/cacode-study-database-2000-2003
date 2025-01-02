#############################################################################
#                                                                           #  
# ADOLESCENT CAUSES OF DEATH: DATA CLEANING                                 #
#                                                                           #
# Created by: Francisco Villavicencio - fvillav1@jhu.edu                    #
#                                                                           #
#                                                            September 2020 #
#############################################################################

# Set age group
source("./src/update-covar-for-old-studies/set-inputs.R")

if(ageSexSuffix == "05to09y"){ ageLow <- 5 }
if(ageSexSuffix == "10to14y"){ ageLow <- 10 }
if(ageSexSuffix == "15to19yF"){ ageLow <- 15 
                                refCat <- "RTI"}
if(ageSexSuffix == "15to19yM"){ ageLow <- 15
                                refCat <- "SelfHarm"}

############################################################
### WORKING DIRECTORY AND PACKAGES                       ###  
############################################################

# Clear working directory
#rm(list = ls())

# Avoid scientific notation
options(scipen=999)

# Packages
library(readstata13)
library(dplyr)
library(tidyr)

# # Set working directory
# if (.Platform$OS.type == "unix") {
#   setwd("~/Dropbox/Adolescent/")
# } else setwd("C:/Users/FVillavicencio/Dropbox/Adolescent/")

# Version
vers <- '009'


############################################################
### MODEL PARAMETERS                                     ###  
############################################################

# Model
model <- 'VAMCM'

# Years for prediction
years <- 2000:2019

# Select age group to produce database
#ageLow <- 5
ageUp <- ageLow + 4
ageGroup <- paste0(ageLow, 'to', ageUp)
qAgeGr <- paste0('q', ageGroup)

# Reference category
if (ageLow == 5) refCat <- 'Diarrhoeal'
if (ageLow == 10) refCat <- 'LRI' 
#if (ageLow == 15) refCat <- 'RTI' #'Self_harm' # 

# Split by sex?
sexSplit <- T
if (ageLow < 15) sexSplit <- F

# Minimum number of deaths per data point
minDeaths <- 15

# Maximum number of deaths per data point
maxDeaths <- 5000

# Include VR data?
VRdata <- F

# Include only data points of VA countries
onlyVA <- T

# Iclude year as covariate?
yearCov <- F

# Include an age-specific mortality covariate (F if not)
# ageMort <- 'u5mr' 
# ageMort <- qAgeGr
ageMort <- 'q5to19'
# ageMort <- F

# Transform age-specific death rate?
# rateTrans <- 'sqrt'
# rateTrans <- 'log'
# rateTrans <- 'log-log'
# rateTrans <- 'sqrt-log'
rateTrans <- F
if (ageMort == F) rateTrans <- F

# Model Malaria
Malaria <- F
if (ageLow < 15) Malaria <- T

# Model Maternal
Maternal <- F
if (ageLow == 15 & refCat == 'SelfHarm') Maternal <- T


############################################################
### DATA                                                 ###  
############################################################

# # WORKING DIRECTORY OF DATA
# if (.Platform$OS.type == "unix") {
#   folder <- '/home/pancho/Documents/' 
# } else folder <- 'C:/Users/FVillavicencio/Dropbox/AdolescentCOD/'


# COUNTRY CLASSIFICATION
# countryClass <- read.csv(paste0(folder, 'Methods/20201001-CountryModelClass.csv'))
countryClass <- read.csv('./data/classification-keys/CountryModelClass_20201001.csv')

#---------#
# VA DATA #
#---------#

# ESTIMATION DATABASE
# dat <- read.dta13(paste0(folder, 'Data/_combined master study database_30Sep2020.dta'),
#                   nonint.factors = T)
dat <- readstata13::read.dta13("./data/study-data-old/20200930-HMM-StudyDatabase.dta", nonint.factors = T)
dat <- dat[, -ncol(dat)]

# Causes of death
# !! CHECK IF DATABASE UPDATED!
colCOD <- c(12:20, 22:26, 28:32, 36, 37, 21, 27, 33:35)   
com <- c('dia', 'hiv', 'lri', 'mal', 'mening', 'tb', 'maternal', 'typhoid', 'mea')
ncd <- c('cardio', 'congen', 'digest', 'endo', 'neoplasm')
inj <- c('rta', 'drown', 'intvio', 'selfharm')

# REMOVE data points from COUNTRIES NOT VA
idExclude <- which(!dat$iso3 %in% countryClass$ISO3[countryClass$Group %in% c('HMM/LMM', 'HMM')])
unique(dat$iso3[idExclude])
if (length(idExclude) > 0 & onlyVA) {
  dat <- dat[-idExclude, ]
  dat <- droplevels(dat)
  rownames(dat) <- NULL
}

# Malaria
malaria <- dat$mal / dat$totdeaths_orig
dat[which(malaria > .3), c('iso3', 'year', 'age_lb', 'age_ub',
                           'mal', 'totdeaths_orig')]

# # MOZAMBIQUE 2007: Downweight ICAM
# MOZ2007 <- read.dta13(paste0(folder, 'Data/92602_MOZINCAM_downweighted.dta'),
#                       nonint.factors = T)
# MOZ2007 <- MOZ2007[, -c(ncol(MOZ2007)-0:1)]
# which(names(MOZ2007) != names(dat))
# dat[dat$Refid == 92602, ] <- MOZ2007
# rm(MOZ2007)

# Initial Number of data points
ndat <- nrow(dat)

# Update variable names
names(dat)[names(dat) == 'iso3'] <- 'ISO3'
names(dat)[names(dat) == 'strata_gender'] <- 'sex'
names(dat)[names(dat) == 'study_location'] <- 'location'
names(dat)[names(dat) == 'totdeaths_orig'] <- 'totdeaths'

# Nation wide studies
dat$location[which(dat$nation_rep == 1)] <- 'nationwide'

# Madagascar
dat$location[which(dat$Refid == 'MDG')] <- 'Antananarivo'

# Re-label SEX variable
levels(dat$sex)[levels(dat$sex) == 'female'] <- 'F'
levels(dat$sex)[levels(dat$sex) == 'male'] <- 'M'
levels(dat$sex)[levels(dat$sex) == 'both'] <- 'B'


#-----------------------------------------#
# VA: REMOVE/MERGE OVERLAPPING DATAPOINTS #
#-----------------------------------------#
# !! CHECK THIS SECTION CAREFULLY IF DATABASE UDPATED

# DATA POINTS TO REMOVE
datRemove <- data.frame(Refid = character(), study_id = character(), 
                        ISO3 = character(), year = numeric(),
                        age_lb = numeric(), age_ub = numeric(),
                        sex = character(),
                        nation_rep = character(),
                        totdeaths = numeric())

# ANGOLA 2011
dat[dat$Refid == 25121, names(datRemove)]
idExclude <- which(dat$study_id %in% c('R201702512101', 'R201702512102'))
if (length(idExclude) > 0) {
  datRemove <- rbind(datRemove, dat[idExclude, names(datRemove)])
  dat <- dat[-idExclude, ]
  rownames(dat) <- NULL
}

# BANGLADESH
dat[dat$ISO3 == 'BGD', names(datRemove)]
table(dat[dat$ISO3 == 'BGD', 'year'])

# BURKINA FASO
dat[dat$ISO3 == 'BFA' & dat$year == 2010,  names(datRemove)]
dat[dat$ISO3 == 'BFA' & dat$year == 2010,  c(1, colCOD)]
idExclude <- which(dat$ISO3 == 'BFA' & dat$Refid == 26894 & dat$year == 2010)
if (length(idExclude) > 0) {
  datRemove <- rbind(datRemove, dat[idExclude, names(datRemove)])
  dat <- dat[-idExclude, ]
  rownames(dat) <- NULL
}

# CHINA
dat[dat$Refid == 25632, names(datRemove)]
pp <- apply(dat[dat$Refid == 25632, ], 2, function(x) {length(unique(x) == 1)})
pp[which(pp != 1)]
# If 'pollution_air' not kept as covariate, we will have keep R201702563204 and 
#   remove the other 3.
# idExclude <- which(dat$study_id %in% c('R201702563201', 'R201702563202', 'R201702563203'))
idExclude <- which(dat$study_id %in% c('R201702563204'))
if (length(idExclude) > 0) {
  datRemove <- rbind(datRemove, dat[idExclude, names(datRemove)])
  dat <- dat[-idExclude, ]
  rownames(dat) <- NULL
}
dat[dat$Refid == 67144, names(datRemove)]
idExclude <- which(dat$study_id %in% c('R201706714401', 'R201706714402'))
if (length(idExclude) > 0) {
  datRemove <- rbind(datRemove, dat[idExclude, names(datRemove)])
  dat <- dat[-idExclude, ]
  rownames(dat) <- NULL
}

# COTE D'IVOIRE
dat[dat$ISO3 == 'CIV' & dat$year == 2010, names(datRemove)]
dat[dat$ISO3 == 'CIV' & dat$year == 2010, c(1, colCOD)]
idExclude <- which(dat$ISO3 == 'CIV' & dat$Refid != 'INDEPTH' & dat$year == 2010)
if (length(idExclude) > 0) {
  datRemove <- rbind(datRemove, dat[idExclude, names(datRemove)])
  dat <- dat[-idExclude, ]
  rownames(dat) <- NULL
}

# ETHIOPIA 2011
dat[dat$Refid == 25252, c(names(datRemove), 'year_start', 'year_end')]
idExclude <- which(dat$study_id == 'R201702525207')
if (length(idExclude) > 0) {
  datRemove <- rbind(datRemove, dat[idExclude, names(datRemove)])
  dat <- dat[-idExclude, ]
  rownames(dat) <- NULL
}

# INDIA 1985, 1991, 1995: > 10,000 DEATHS
# dat[dat$Refid == 27360, c(names(datRemove), 'year_start', 'year_end')]
# idExclude <- which(dat$Refid == 27360 & dat$totdeaths > 10000)
# if (length(idExclude) > 0) {
#   datRemove <- rbind(datRemove, dat[idExclude, names(datRemove)])
#   dat <- dat[-idExclude, ]
#   rownames(dat) <- NULL
# }

# INDONESIA 2001: Collapse age-specific datapoints on a single 5-19 one.
IDNid <- 24135
dat[dat$Refid == IDNid, names(datRemove)]
IDN2001 <- dat[dat$Refid == IDNid, ]
IDN2001[, colCOD]
which(apply(IDN2001, 2, function(x) {length(unique(x))}) != 1)
IDN2001 <- IDN2001[IDN2001$sex == 'B' & IDN2001$strata_other1 != 'weighted', ]
IDN2001[1, 'age_lb'] <- min(IDN2001$age_lb)
IDN2001[1, 'age_ub'] <- max(IDN2001$age_ub)
IDN2001[1, colCOD] <- apply(IDN2001[, colCOD], 2, 
                            function(x) {
                              if (all(is.na(x))) {
                                return(NA)
                              } else return(sum(x, na.rm = T))
                            })
IDN2001 <- IDN2001[1, ]
IDN2001[, colCOD]
# Update data base with collapsed data point
dat[dat$study_id == IDN2001$study_id, ] <- IDN2001
# Remove unnecessary data points
idExclude <- which(dat$Refid == IDNid & dat$study_id != IDN2001$study_id)
if (length(idExclude) > 0) {
  datRemove <- rbind(datRemove, dat[idExclude, names(datRemove)])
  dat <- dat[-idExclude, ]
  rownames(dat) <- NULL
}
rm(IDN2001, IDNid)
dim(dat[dat$ISO3 == 'IDN', ])

# PAKISTAN
dat[dat$Refid == 'PAKVASA', names(datRemove)]
idExclude <- which(dat$Refid == 'PAKVASA' & dat$year == 2018 &
                     !dat$study_id %in% c('PAKVASA_1', 'PAKVASA_10', 'PAKVASA_19', 'PAKVASA_26'))
if (length(idExclude) > 0) {
  datRemove <- rbind(datRemove, dat[idExclude, names(datRemove)])
  dat <- dat[-idExclude, ]
  rownames(dat) <- NULL
}

# PAPUA NEW GUINEA 1983: Collapse age-specific datapoints on a single 5-19 one.
PNGid <- 36645
dat[dat$Refid == PNGid, names(datRemove)]
PNG1983 <- dat[dat$Refid == PNGid, ]
PNG1983[, colCOD]
# Check covariates are the same
which(apply(PNG1983, 2, function(x) {length(unique(x))}) != 1)
# New age bounds
PNG1983[1, 'age_lb'] <- min(PNG1983$age_lb)
PNG1983[1, 'age_ub'] <- max(PNG1983$age_ub)
# Collapse data points
for (i in colCOD) {
  
  # If ALL DIFFERENT than NA
  if (all(!is.na(PNG1983[, i]))) PNG1983[1, i] <- sum(PNG1983[, i]) 
  
  # If some NA some not
  if (any(is.na(PNG1983[, i])) & any(!is.na(PNG1983[, i]))) {
    
    # Check whether it is a DEFINED CAUSE
    if (names(PNG1983)[i] %in% c(com, ncd, inj)) {
      # Communicable
      if (names(PNG1983)[i] %in% com) {
        PNG1983[1, 'othercd'] <- 
          sum(PNG1983[1, 'othercd'], PNG1983[, i], na.rm = T)
      }
      # Non-communicable
      if (names(PNG1983)[i] %in% ncd) {
        PNG1983[1, 'otherncd'] <- 
          sum(PNG1983[1, 'otherncd'], PNG1983[, i], na.rm = T)
      }
      # Injuries
      if (names(PNG1983)[i] %in% inj) {
        PNG1983[1, 'otherinj'] <- 
          sum(PNG1983[1, 'otherinj'], PNG1983[, i], na.rm = T)
      }
      # Assing NA to new record
      PNG1983[1, i] <- NA
    } else PNG1983[1, i] <- sum(PNG1983[, i], na.rm = T)
  }
}
PNG1983[, colCOD]
PNG1983 <- PNG1983[1, ]
# Update data base with collapsed data point
dat[dat$study_id == PNG1983$study_id, ] <- PNG1983
# Remove unnecessary data points
idExclude <- which(dat$Refid == PNGid & dat$study_id != PNG1983$study_id)
if (length(idExclude) > 0) {
  datRemove <- rbind(datRemove, dat[idExclude, names(datRemove)])
  dat <- dat[-idExclude, ]
  rownames(dat) <- NULL
}
rm(PNG1983, PNGid)
dat[dat$ISO3 == 'PNG', names(datRemove)]

# SOUTH AFRICA: 37423
idExclude <- which(dat$Refid == 37423 & dat$strata_other1 != 'collapsed')
if (length(idExclude) != 0) {
  datRemove <- rbind(datRemove, dat[idExclude, names(datRemove)])
  dat <- dat[-idExclude, ]
  dat$strata_other1[dat$Refid == 37423] <- ''
  rownames(dat) <- NULL
}
dat[dat$Refid == '37423', names(datRemove)]
idExclude <- which(dat$study_id %in% c('R201703742301', 'R201703742307', 'R201703742304', 'R2017037423010',
                                       'R201703742302', 'R201703742308', 'R2017037423011', 'R201703742305',
                                       'R2017037423015', 'R2017037423018'))
if (length(idExclude) > 0) {
  datRemove <- rbind(datRemove, dat[idExclude, names(datRemove)])
  dat <- dat[-idExclude, ]
  rownames(dat) <- NULL
}
# This data point overlaps with age-specific data points
dat[dat$ISO3 == 'ZAF' & dat$Refid == 'INDEPTH' & dat$year == 2002, names(datRemove)]
idExclude <- which(dat$study_id == 'ZA031_CODA_4')
if (length(idExclude) > 0) {
  datRemove <- rbind(datRemove, dat[idExclude, names(datRemove)])
  dat <- dat[-idExclude, ]
  rownames(dat) <- NULL
}

# SOUTH AFRICA 25087: ONLY OTHER-DUMPSTER REPORTED
# dat[dat$Refid == 25087, c(names(datRemove), names(dat)[colCOD[-1]])]
# idExclude <- which(dat$Refid == 25087)
# if (length(idExclude) != 0) {
#   datRemove <- rbind(datRemove, dat[idExclude, names(datRemove)])
#   dat <- dat[-idExclude, ]
#   rownames(dat) <- NULL
# }

# TANZANIA 2007 (INDETPH)
dat[dat$ISO3 == 'TZA', names(datRemove)]
dat[dat$ISO3 == 'TZA', colCOD]

# Data points removed
ndat - nrow(dat)

# # Save
# SAVE <- F
# if (SAVE) {
#   write.csv(datRemove, row.names = F,
#             paste0('Methods/', format(Sys.Date(), "%Y%m%d"),
#                    '_VAMCMinputDataRemove.csv'))
# }
# 

#----------------------#
# VA: AGE AND LOCATION #
#----------------------#

# Select AGE GROUP
dat <- dat[dat$age_lb <= ageUp & dat$age_ub >= ageLow,  ]
idExclude <- which((dat$age_ub == ageLow & dat$age_lb < ageLow - 1) |
                     (dat$age_lb == ageUp & dat$age_ub > ageUp + 1))
if (length(idExclude) != 0) dat <- dat[-idExclude, ]
rownames(dat) <- NULL
dat <- droplevels(dat)

# Countries available
sort(unique(dat$ISO3))
length(unique(dat$ISO3))

# List of study locations
length(unique(dat$location))
length(which(is.na(dat$location)))

# Assing numeric value to each location, for simplicity
dat$location <- as.numeric(relevel(factor(dat$location), 
                                   ref = 'nationwide'))

# Tidy up
dat <- droplevels(dat)
rownames(dat) <- NULL


#-----------------------------------------#
# COLLAPSE DATA POINTS WITH DIFFERENT SEX #
#-----------------------------------------#

# Collapse data points same sex when NO SEX SPLIT
if (!sexSplit) {
  
  # Identify datapoints from same COUNTRY, REFID, LOCATION, YEAR and AGE
  dat$id2 <- paste(dat$ISO3, dat$Refid, dat$location, dat$year, 
                   dat$age_lb, dat$age_ub, sep = '-')
  length(unique(dat$id2))
  nrow(dat)
  
  for (j in unique(dat$id2)) {
    
    # Records from the same country
    upd <- which(dat$id2 == j)
    if (length(upd) == 2 & all(c('F', 'M') %in% dat$sex[upd])) {
      
      # Collapse causes of death
      for (i in colCOD) {
        
        # If ALL DIFFERENT than NA
        if (all(!is.na(dat[upd, i]))) dat[upd[1], i] <- sum(dat[upd, i]) 
        
        # If some NA some not
        if (any(is.na(dat[upd, i])) & any(!is.na(dat[upd, i]))) {
          
          # Check whether it is a DEFINED CAUSE
          if (names(dat)[i] %in% c(com, ncd, inj)) {
            # Communicable
            if (names(dat)[i] %in% com) {
              dat[upd[1], 'othercd'] <- 
                sum(dat[upd[1], 'othercd'], dat[upd, i], na.rm = T)
            }
            # Non-communicable
            if (names(dat)[i] %in% ncd) {
              dat[upd[1], 'otherncd'] <- 
                sum(dat[upd[1], 'otherncd'], dat[upd, i], na.rm = T)
            }
            # Injuries
            if (names(dat)[i] %in% inj) {
              dat[upd[1], 'otherinj'] <- 
                sum(dat[upd[1], 'otherinj'], dat[upd, i], na.rm = T)
            }
            # Assing NA to new record
            dat[upd[1], i] <- NA
          } else dat[upd[1], i] <- sum(dat[upd, i], na.rm = T)
        }
      }
      
      # Update sex
      dat$sex[upd[1]] <- 'B'
      # Remove record
      dat <- dat[-upd[2], ]
    }
  }
}

# Checks
length(unique(dat$id2))
nrow(dat)


#---------#
# VR DATA #
#---------#

# Recover VR data if VRdata == T
if (VRdata) {
  
  # VR data input
  VRdata <- read.dta13(paste0(folder, 'Data/VR data/VR data extraction_30Sep2020.dta'),
                       nonint.factors = T)
  names(VRdata)[names(VRdata) == 'iso3'] <- 'ISO3'
  VRdata <- VRdata[, !names(VRdata) %in% c('country')]
  
  # Select age group
  VRdata <- VRdata[VRdata$age_lb == ageLow, ]
  
  # Country-years of interest
  VRcountries <- read.csv(paste0(folder, 'Methods/20201001-VRforVAMCM.csv'))
  VRdata <- VRdata[VRdata$ISO3 %in% VRcountries$ISO3, ]
  VRdata <- merge(VRdata, VRcountries[, c('ISO3', 'Year')], 
                  by.x = c('ISO3', 'year'), by.y = c('ISO3', 'Year'),
                  all.x = F, all.y = T)
  
  # Sex
  if (sexSplit) {
    
    VRdata$sex[VRdata$sex == 1] <- 'M'
    VRdata$sex[VRdata$sex == 2] <- 'F'
    
  } else {
    
    # If no sex split, collapse data points
    # !! ASSUME NO NA in VR data
    VRdata <- aggregate(VRdata[, -c(1:5)], 
                        by = list(VRdata$ISO3, VRdata$year, 
                                  VRdata$age_lb, VRdata$age_ub), sum)
    names(VRdata)[1:4] <- c('ISO3', 'year', 'age_lb', 'age_ub')
    VRdata$sex <- 'B'
    VRdata <- VRdata[, c(1, 2, ncol(VRdata), 3:(ncol(VRdata)-1))]
    
  }
  
  # Refid, Study location (all nationwide) and random term
  VRdata$Refid <- 'VR'
  VRdata$location <- 1
  VRdata$reterm <- VRdata$ISO3
  rownames(VRdata) <- NULL
  
  # Add 'typhoid', 'other' and 'undt' to match code for VA data
  if ('typhoid' %in% names(dat)) VRdata$typhoid <- 0
  if ('other' %in% names(dat)) VRdata$other <- 0
  if ('undt' %in% names(dat)) VRdata$undt <- 0
  
} else VRdata <- NULL


#---------------------#
# PREDICTION DATABASE #
#---------------------#

# PREDICTION DATABASE
data.predict <- read.dta13(paste0('./data/pancho/covariates_teen_30Jun2020_dy.dta'))
names(data.predict)[names(data.predict) == 'iso3'] <- 'ISO3'
data.predict$Year <- data.predict$year

# Select COVARIATES for ESTIMATION of VR data points
if (!is.null(VRdata)) {

  VRcov <- data.predict[data.predict$ISO3 %in% VRdata$ISO3, ]
  VRcov <- merge(VRcov, VRcountries[, c('ISO3', 'Year')],
                  by.x = c('ISO3', 'year'), by.y = c('ISO3', 'Year'),
                  all.x = F, all.y = T)

}

# Select COUNTRIES and YEARS for PREDICTION
# VAMCM <- countryClass[countryClass$Group2010 == 'VAMCM', ]
VAMCM <- countryClass[countryClass$Group %in% c('HMM', 'HMM/LMM'), ]
data.predict <- data.predict[data.predict$Year %in% years, ]
data.predict <- data.predict[data.predict$ISO3 %in% VAMCM$ISO3, ]
rownames(data.predict) <- NULL


############################################################
### CAUSES OF DEATH                                      ###  
############################################################

# Reclassification of COD
fileName <- paste0('./data/pancho/ReclassifiedCOD', ageGroup, '.csv') 
reclass <- read.csv(fileName)
head(reclass)
table(reclass$Reclass)

# Initial list of causes of death
cod <- names(dat)[colCOD[-1]]

# Checks
which(!cod %in% reclass$Original)
reclass$Original[which(!reclass$Original %in% cod)]


#-----------------# 
# REDISTRIBUTE TB #
#-----------------#

# if (ageLow < 15) {
# 
#   # Recover estimates from TB program
#   progTB <- read.csv(paste0(folder, 'Data/ProgramTB/mcee_20201023_TB.csv'))
#   # Updated estimates for GMB and MOZ
#   datAux <- read.csv(paste0(folder, 'Data/ProgramTB/db_mortality_disaggregated_MCEEages_allyear1c_patch_2020-12-14.csv'))
#   progTB <- progTB[!progTB$iso3 %in% datAux$iso3, ]
#   progTB <- rbind(progTB, datAux)
#   rm(datAux)
#   # Tidy up
#   progTB <- progTB[order(progTB$iso3, progTB$year, progTB$age_group, progTB$sex), ]
#   rownames(progTB) <- NULL
# 
#   # Select age group
#   progTB <- progTB[progTB$age_group == paste(ageLow, ageUp, sep = '_'),
#                    c('iso3', 'year', 'mort.nh.nr.num', 'mort.nh.re.num')]
#   # Merge sexes
#   progTB <- aggregate(progTB[, 3:4], by = list(progTB$iso3, progTB$year), sum)
#   progTB <- progTB[order(progTB$Group.1, progTB$Group.2), ]
#   rownames(progTB) <- NULL
# 
#   # Calculate percentages (Non-resp)
#   progTB[, 3] <- progTB[, 3] / (progTB[, 3] + progTB[, 4])
#   progTB <- progTB[, -4]
#   names(progTB) <- c('ISO3', 'year', 'NRfrac')
# 
#   # Fill missing values
#   for (i in unique(progTB$ISO3)) {
#     if (all(!is.na(progTB[progTB$ISO3 == i, 3]))) next()
#     if (all(is.na(progTB[progTB$ISO3 == i, 3]))) {
#       # If all NA, split equally
#       progTB[progTB$ISO3 == i, 3] <- .5
#     } else {
#       # STEP 1: Identify NA and give them a value from adjacent years
#       for (k in 1:2) {
#         if (k%%2 == 1) {
#           idNa <- which(is.na(progTB[progTB$ISO3 == i, 3]))
#         } else idNa <- rev(which(is.na(progTB[progTB$ISO3 == i, 3])))
#         # Fill gaps (twice: onwards and backwards)
#         for (j in idNa) {
#           if (j != nrow(progTB[progTB$ISO3 == i, ])) {
#             if (!is.na(progTB[progTB$ISO3 == i, 3][j+1])) {
#               progTB[progTB$ISO3 == i, 3][j] <- progTB[progTB$ISO3 == i, 3][j+1]
#             } else if (j != 1) {
#               if (!is.na(progTB[progTB$ISO3 == i, 3][j-1])) {
#                 progTB[progTB$ISO3 == i, 3][j] <- progTB[progTB$ISO3 == i, 3][j-1]
#               }
#             }
#           } else if (!is.na(progTB[progTB$ISO3 == i, 3][j-1])) {
#             progTB[progTB$ISO3 == i, 3][j] <- progTB[progTB$ISO3 == i, 3][j-1]
#           }
#         }
#       }
#     }
#   }
# 
#   # Add LRI fraction to data set
#   dat <- merge(dat, progTB, by= c('ISO3', 'year'), all.x = T)
#   # Imput fraction of 2000 to missing values
#   dat <- merge(dat, progTB[progTB$year == 2000, ], by = 'ISO3', all.x = T)
#   dat$NRfrac.x[which(is.na(dat$NRfrac.x))] <- dat$NRfrac.y[which(is.na(dat$NRfrac.x))]
#   dat <- dat[, !names(dat) %in% c('year.y', 'NRfrac.y')]
#   names(dat)[names(dat) == 'year.x'] <- 'year'
#   names(dat)[names(dat) == 'NRfrac.x'] <- 'NRfrac'
# 
#   # REDISTRIBUTE TB: VA DATA
#   id1 <- which(!is.na(dat$tb) & !is.na(dat$othercd))
#   dat$othercd[id1] <- (dat$othercd + dat$tb * dat$NRfrac)[id1]
#   id1 <- which(!is.na(dat$tb) & is.na(dat$othercd))
#   dat$othercd[id1] <- (dat$tb * dat$NRfrac)[id1]
#   id1 <- which(!is.na(dat$tb) & !is.na(dat$lri))
#   dat$lri[id1] <- (dat$lri + dat$tb * (1 - dat$NRfrac))[id1]
#   id1 <- which(!is.na(dat$tb) & is.na(dat$lri))
#   dat$lri[id1] <- (dat$tb * (1 - dat$NRfrac))[id1]
#   dat$tb <- 0
#   dat <- dat[, names(dat) != 'NRfrac']
# 
#   # REDISTRIBUTE TB: VR DATA
#   VRdata <- merge(VRdata, progTB, by = c('ISO3', 'year'), all.x = T)
#   id1 <- which(!is.na(VRdata$tb) & !is.na(VRdata$othercd))
#   VRdata$othercd[id1] <- (VRdata$othercd + VRdata$tb * VRdata$NRfrac)[id1]
#   id1 <- which(!is.na(VRdata$tb) & is.na(VRdata$othercd))
#   VRdata$othercd[id1] <- (VRdata$tb * VRdata$NRfrac)[id1]
#   id1 <- which(!is.na(VRdata$tb) & !is.na(VRdata$lri))
#   VRdata$lri[id1] <- (VRdata$lri + VRdata$tb * (1 - VRdata$NRfrac))[id1]
#   id1 <- which(!is.na(VRdata$tb) & is.na(VRdata$lri))
#   VRdata$lri[id1] <- (VRdata$tb * (1 - VRdata$NRfrac))[id1]
#   VRdata$tb <- 0
#   VRdata <- VRdata[, names(VRdata) != 'NRfrac']
# 
#   # Remove unnecessary data
#   rm(progTB)
# 
# }
if (ageLow < 15) {
  dat$TB <- NA
}

#----------------------------#
# RECLASSIFY CAUSES OF DEATH #
#----------------------------#

# Collapse CD in Other CD
if (Maternal) {
  reclass$Reclass[reclass$Reclass %in% c('HIV', 'Measles')] <- 'OtherCD'  
} else reclass$Reclass[reclass$Reclass %in% c('HIV', 'Maternal', 'Measles')] <- 'OtherCD'


# TB: ONLY applies to 15-19
reclass$Reclass[reclass$Reclass == 'TB'] <- 'OtherCD'

# Malaria
if (!Malaria) reclass$Reclass[reclass$Reclass == 'Malaria'] <- 'OtherCD'

# Vectors with causes of death
cod <- unique(reclass$Reclass)
com <- unique(reclass$Reclass[which(reclass$Original %in% com)])
com <- com[com != 'OtherCD']
ncd <- unique(reclass$Reclass[which(reclass$Original %in% ncd)])
ncd <- ncd[ncd != 'OtherNCD']
inj <- unique(reclass$Reclass[which(reclass$Original %in% inj)])
inj <- inj[inj != 'OtherInj']

# Re-classify causes of death
for (i in 1:length(cod)) {
  orig <- reclass$Original[reclass$Reclass == cod[i]]
  if (length(orig) > 1) {
    dat[, paste(cod[i])] <- apply(dat[, paste(orig)], 1, 
                                  function(x) {
                                    if (all(is.na(x))) {
                                      return(NA)
                                    } else return(sum(x, na.rm = T))
                                  })
    if (!is.null(VRdata)) {
      VRdata[, paste(cod[i])] <- apply(VRdata[, paste(orig)], 1, 
                                       function(x) {
                                         if (all(is.na(x))) {
                                           return(NA)
                                         } else return(sum(x, na.rm = T))
                                       })
    }
  } else {
    dat[, paste(cod[i])] <- dat[, paste(orig)]
    if (!is.null(VRdata)) {
      VRdata[, paste(cod[i])] <- VRdata[, paste(orig)]
    }
  }
}

# Other dumpster
dat$OtherCD[which(is.na(dat$othercd) & dat$OtherCD == 0)] <- NA
dat$OtherNCD[which(is.na(dat$otherncd) & dat$OtherNCD == 0)] <- NA
dat$OtherInj[which(is.na(dat$otherinj) & dat$OtherInj == 0)] <- NA
dat$Other[which(is.na(dat$other) & dat$Other == 0)] <- NA
if (!is.null(VRdata)) {
  VRdata$OtherCD[which(is.na(VRdata$othercd) & VRdata$OtherCD == 0)] <- NA
  VRdata$OtherNCD[which(is.na(VRdata$otherncd) & VRdata$OtherNCD == 0)] <- NA
  VRdata$OtherInj[which(is.na(VRdata$otherinj) & VRdata$OtherInj == 0)] <- NA
  VRdata$Other[which(is.na(VRdata$other) & VRdata$Other == 0)] <- NA
}

# Delete unncessary columns
idExclude <- which(!reclass$Original %in% reclass$Reclass)
if (length(idExclude) != 0) {
  dat <- dat[, !names(dat) %in% paste(reclass$Original[idExclude])]
  if (!is.null(VRdata)) {
    VRdata <- VRdata[, !names(VRdata) %in% paste(reclass$Original[idExclude])]
  }
}


#--------------------#
# REFERENCE CATEGORY #
#--------------------#

# Number of deaths by cause
deaths <- apply(dat[, paste(cod)], 2, sum, na.rm = T)
par(mar = c(8, 4, 3, 1)) 
barplot(deaths, las = 2, main = 'Number of deaths by cause')
round(100*sort(prop.table(deaths), decreasing = T), 2)

# Number of times COD with highest fraction
deaths <- apply(dat[, paste(cod)], 1,
                function(x) {cod[which(x == max(x, na.rm = T))]})
deaths <- unlist(deaths)
sort(table(deaths), decreasing = T)

# Number of times COD reported
deaths <- apply(dat[, paste(cod)], 2, 
                function(x) {length(which(!is.na(x) & x != 0))})
sort(deaths, decreasing = T)

# Choose REFERENCE category
refCat
sort(apply(dat[, paste(cod)], 2, sum, na.rm = T), decreasing = T)
cod <- as.factor(c(refCat, paste(cod[cod != refCat])))
cod <- relevel(cod, ref = refCat)
cod

# Number of deaths by cause
deaths <- apply(dat[, paste(cod)], 2, sum, na.rm = T)
deaths <- deaths/sum(deaths)
par(mar = c(8, 4, 3, 1)) 
barplot(deaths, las = 2, 
        main = 'Fraction of reported deaths by cause, 5-9 years', 
        ylim = c(0, 0.25), cex.main = 2)
axis(2, seq(0, 0.25, .01), labels = NA)
sort(deaths/sum(deaths))
round(100*sort(prop.table(deaths), decreasing = T), 2)


#---------------------#
# ADJUST TOTAL DEATHS #
#---------------------#

# UPDATE TOTAL DEATHS when SMALLER than sum of COD
which(dat$totdeaths - apply(dat[, paste0(cod)], 1, sum, na.rm = T) < -1)
totSmall <- which(dat$totdeaths - apply(dat[, paste0(cod)], 1, sum, na.rm = T) < -.01)
length(totSmall)
dat$totdeaths[totSmall] <- apply(dat[totSmall, paste0(cod)], 1, sum, na.rm = T)


#--------------#
# ADJUST OTHER #
#--------------#

# Data points in which total is larger than sum of COD
totLarge <- which(dat$totdeaths - apply(dat[, paste0(cod)], 1, sum, na.rm = T) > .01)
dat$totdeaths[totLarge] 
apply(dat[totLarge, paste0(cod)], 1, sum, na.rm = T)

# Update OTHER
if (length(totLarge) > 1) {
  otherCat <- c('OtherCD', 'OtherNCD', 'OtherInj', 'Other')
  for (i in totLarge) {
    # Difference in deaths
    diffDeath <- dat$totdeaths[i] - sum(dat[i, paste0(cod)], na.rm = T)
    # Proportion in 'other' categories
    propOther <- dat[i, paste(otherCat)] / sum(dat[i, paste(otherCat)], na.rm = T)
    # Redistribute excess of deaths among 'other' categories
    dat[i, paste(otherCat)] <- dat[i, paste(otherCat)] + diffDeath * propOther
  }
}


#-----------------------#
# EXCLUDE SINGLE CAUSES #
#-----------------------#

# Single causes
codExclude <- c('CollectVio', 'NatDis')

# Exclude SINGLE CAUSE ESTIMTES
dat <- droplevels(dat[, !names(dat) %in% codExclude])
if (!is.null(VRdata)) VRdata <- droplevels(VRdata[, !names(VRdata) %in% codExclude])

# Re-define COD vector
cod <- droplevels(cod[!cod %in% codExclude])
com <- com[!com %in% codExclude]
ncd <- ncd[!ncd %in% codExclude]
inj <- inj[!inj %in% codExclude]

# Re-calculate TOTAL DEATHS
dat$totdeaths <- apply(dat[, paste0(cod)], 1, sum, na.rm = T)
if (!is.null(VRdata)) VRdata$totdeaths <- apply(VRdata[, paste0(cod)], 1, sum, na.rm = T)


#-------------------------------------------------------#
# EXCLUDE DATA POINTS WITH ONLY OTHER-DUMPSTER REPORTED #
#-------------------------------------------------------#

# Only if the 5000 deaths cap is NOT used.
# if (!max10k) {
#   
#   # Identify cases
#   idExclude <- apply(dat[, paste(cod)], 1, 
#                      function(x) {
#                        if (all(is.na(x[names(x) %in% c(com, ncd, inj)]))) {
#                          return(1)
#                        } else return(0)
#                      })
#   table(idExclude)
#   dat[which(idExclude == 1), names(datRemove)]
#   
#   # Exclude data points
#   if (sum(idExclude) > 0) {
#     datRemove <- rbind(datRemove, dat[which(idExclude == 1), names(datRemove)])
#     dat <- dat[which(idExclude == 0), ]
#     rownames(dat) <- NULL
#     dat <- droplevels(dat)
#   }
#   
# }


############################################################
### MANAGE VA DATA                                       ###
############################################################

#--------------------------------#
# CREATE A NEW UNIQUE IDENTIFIER #
#--------------------------------#

# Create NEW ID: Same COUNTRY/REGION, YEAR, REFID, LOCATION and AGE and SEX
dat$id <- paste(dat$ISO3, dat$year, dat$Refid, dat$location,
                dat$age_lb, dat$age_ub, dat$sex, sep = '-')
length(unique(dat$id))
nrow(dat)


#------------------------------------------------#
# AVOID DOUBLE COUNTING: OVERLAPPING DATA POINTS #
#------------------------------------------------#

# Create id2 to avoid double counting: Same COUNTRY, YEAR, LOCATION, AGE and SEX
dat$id2 <- paste(dat$ISO3, dat$year, dat$location,
                 dat$age_lb, dat$age_ub, dat$sex, sep = '-')
length(unique(dat$id2))
nrow(dat)

# Identify duplicates
dupl <- dat$id2[duplicated(dat$id2)]
dat[dat$id2 %in% dupl, names(datRemove)]
idExclude <- which(duplicated(dat$id2))

# Identify data points with OVERLAPPING AGES
dat$id2 <- paste(dat$ISO3, dat$year, dat$location, dat$sex, sep = '-')
length(unique(dat$id2))
nrow(dat)

# Identify duplicates
dupl <- dat$id2[duplicated(dat$id2)]
dat[dat$id2 %in% dupl, names(datRemove)]
idExclude <- which(duplicated(dat$id2))
if (length(idExclude) > 0) {
  dat <- droplevels(dat[-idExclude, ])
  rownames(dat) <- NULL
}


#-----------------------------------------------#
# AVOID DOUBLE COUNTING: OVERLAPPING SEX GROUPS #
#-----------------------------------------------#

# Identify datapoints from same COUNTRY, YEAR, SEX and REFID
dat$id2 <- paste(dat$ISO3, dat$Refid, dat$sex, dat$year, sep = '-')
length(unique(dat$id2))
nrow(dat)
unique(table(dat$id2))
unique(table(dat$id2[dat$Refid != 'MDS']))
unique(table(dat$id2[dat$Refid == 'MDS']))
unique(table(dat$id2[dat$Refid == 'MDG']))

# Identify data points with same: COUNTRY, YEAR, and REFID
dat$id2 <- paste(dat$ISO3, dat$year, dat$Refid, sep = '-')
length(unique(dat$id2))
nrow(dat)

# Check whether there are data points with 3+ sexes reported (SHOULD BE ONLY 1 or 2)
unique(table(dat$id2[dat$Refid != 'MDS']))

# Indian (MDS) states are the exception, because all states same Refid
unique(table(dat$id2[dat$Refid == 'MDS']))
table(dat$location[dat$Refid == 'MDS'], dat$year[dat$Refid == 'MDS'])
colSums(table(dat$location[dat$Refid == 'MDS'], dat$year[dat$Refid == 'MDS']))
length(unique(dat$location[dat$Refid == 'MDS']))

# Madagascar (MDG)
unique(table(dat$id2[dat$Refid == 'MDG']))
table(dat$year[dat$Refid == 'MDG'])
unique(table(dat$year[dat$Refid == 'MDG']))

# Check MANUALLY remainig studies
dupl <- dat$id2[duplicated(dat$id2)]
datCheck <- dat[dat$id2 %in% dupl & !dat$Refid %in% c('MDG', 'MDS'), names(datRemove)]
datCheck[order(datCheck$ISO3, datCheck$Refid, datCheck$year, datCheck$sex), ]

# Identify data points with same: COUNTRY/REGION, YEAR, REFID, LOCATION and AGE
dat$id2 <- paste(dat$ISO3, dat$year, dat$Refid,
                 dat$location,
                 dat$age_lb, dat$age_ub, sep = '-')
length(unique(dat$id2))
nrow(dat)
 
# # Check data points with data on FEMALES, MALES and BOTH
# k1 <- c(); k2 <- c()
# for (i in unique(dat$id2)) {
#   datAux <- dat[dat$id2 == i, ]
#   # If 2 records with the same id2
#   if (nrow(datAux) == 2) {
#     if ('B' %in% datAux$sex) {
#       k1 <- c(k1, i)
#       # If data on BOTH, exclude the sex-specific data point
#       idExclude <- dat$id2[dat$id2 == i & dat$sex %in% c('F', 'M')]
#       if (length(idExclude) > 0) {
#         dat <- dat[!dat$id2 %in% idExclude, ]
#       } else warning(paste('Something is wrong with', i))
#     } else if (!sexSplit) {
#       if (all(c('F', 'M') %in% datAux$sex)) {
#         # Merge sexes
#         datAux[1, c('totdeaths', paste(cod))] <-
#           apply(datAux[, c('totdeaths', paste(cod))], 2,
#                 function(x) {
#                   if (all(is.na(x))) {
#                     return(NA)
#                   } else return(sum(x, na.rm = T))
#                 })
#         # Keep only 1st row
#         datAux <- datAux[1, ]
#         # Update sex and id
#         datAux$sex <- 'B'
#         datAux$id <- paste(datAux$ISO3, datAux$year, datAux$Refid,
#                               datAux$location, datAux$age_lb, datAux$age_ub,
#                               datAux$sex, sep = '-')
#         # Update original database
#         dat[dat$id2 == i, ][1, ] <- datAux
#         idExclude <- which(dat$id2 == i & dat$sex != 'B')
#         if (length(idExclude) > 0) {
#           dat <- dat[-idExclude, ]
#         } else warning(paste('Something is wrong with', i))
#       } else warning(paste('Something is wrong with', i))
#     }
#   }
#   # If 3 records with the same id2
#   if (nrow(datAux) == 3) {
#     k2 <- c(k2, i)
#     # If FEMALES, MALES and BOTH reported, EXCLUDE depending on sexSplit
#     if (all(c('F', 'M', 'B') %in% datAux$sex)) {
#       # Exclude depending on if we want sex split or not
#       if (sexSplit) {
#         idExclude <- dat$id[dat$id2 == i & dat$sex == 'B']
#       } else idExclude <- dat$id[dat$id2 == i & dat$sex %in% c('F', 'M')]
#       if (length(idExclude) > 0) {
#         dat <- dat[!dat$id %in% idExclude, ]
#       } else warning(paste('Something is wrong with', i))
#     } else warning(paste('Something is wrong with', i))
#   }
#   # If more than 3 records, send a warning
#   if (nrow(datAux) > 3) warning(paste('Something is wrong with', i))
#   rm(datAux)
# }
# k1; k2


#----------------#
# RANDOM EFFECTS #
#----------------#

# RANDOM EFFECTS term: COUNTR/REGION, REFID and LOCATION
dat$reterm <- paste(dat$ISO3, dat$Refid, dat$location, sep = '-')

# MADAGASCAR
unique(dat$reterm[dat$Refid == 'MDG'])
dat$reterm[dat$Refid == 'MDG'] <- 'MDG-A'

# INDIA
unique(dat$reterm[dat$Refid == 'MDS'])
dat$reterm[dat$Refid == 'MDS'] <- paste0(dat$ISO3[dat$Refid == 'MDS'],
                                         dat$location[dat$Refid == 'MDS'])

# NATIONALLY representative studies
unique(dat$reterm[dat$location == 1])
dat$reterm[dat$location == 1]
dat$reterm[dat$location == 1] <- dat$ISO3[dat$location == 1]
unique(dat$reterm[dat$location == 1])

# Tidy up
dat <- droplevels(dat)
rownames(dat) <- NULL
dim(dat)


#---------------------------------------#
# AGGREGATE DATA POINTS WITH FEW DEATHS #
#---------------------------------------#

# Number of studies with less than 25 deaths
length(which(dat$totdeaths < minDeaths))
length(which(dat$totdeaths < minDeaths)) / nrow(dat)

# Identify data points with same: COUNTRY/REGION, YEAR, REFID, LOCATION, and AGE
dat$id2 <- paste(dat$ISO3, dat$year, dat$Refid, dat$location,
                 dat$age_lb, dat$age_ub, sep = '-')
length(unique(dat$id2))
nrow(dat)

# Identify candidates for merging
idLess <- unique(dat$id2[which(dat$totdeaths < minDeaths & dat$sex != 'B')])
length(idLess)
dat[dat$id2 %in% idLess, names(datRemove)]
table(dat$Refid[dat$totdeaths < minDeaths])
table(dat$ISO3[dat$totdeaths < minDeaths])
table(dat$ISO3)
dim(dat[dat$totdeaths < minDeaths, names(datRemove)])

# MERGE SEXES when less than min deaths in one of them
if (sexSplit) {
  
  # Merge VA datapoints
  if (length(idLess) > 0) {
  
    for (i in idLess) {
      
      # Rows to be updated: Same ID2
      upd <- which(dat$id2 == i)
      
      # Two records and both sexes reported
      if (length(upd) == 2 & all(c('F', 'M') %in% dat[upd, 'sex'])) {
        
        # Collapse total deaths
        dat[upd[1], 'totdeaths'] <- sum(dat[upd, 'totdeaths'], na.rm = T)
        
        # Collapse COD
        for (j in cod) {
          
          # If ALL DIFFERENT than NA
          if (all(!is.na(dat[upd, j]))) dat[upd[1], j] <- sum(dat[upd, j])
          
          # If some NA but not others
          if (any(is.na(dat[upd, j])) & any(!is.na(dat[upd, j]))) {
            
            # Check whether it is a DEFINED CAUSE
            if (j %in% c(com, ncd, inj)) {
              # Communicable
              if (j %in% com) {
                dat[upd[1], 'OtherCD'] <- 
                  sum(dat[upd[1], 'OtherCD'], dat[upd, j], na.rm = T)
              }
              # Non-communicable
              if (j %in% ncd) {
                dat[upd[1], 'OtherNCD'] <- 
                  sum(dat[upd[1], 'OtherNCD'], dat[upd, j], na.rm = T)
              }
              # Injuries
              if (j %in% inj) {
                dat[upd[1], 'OtherInj'] <- 
                  sum(dat[upd[1], 'OtherInj'], dat[upd, j], na.rm = T)
              }
              # Assing NA to new record
              dat[upd[1], j] <- NA
            } else dat[upd[1], j] <- sum(dat[upd, j], na.rm = T)
          }
        }
        # Update sex and exclude second record
        dat[upd[1], 'sex'] <- 'B'
        dat <- dat[-upd[2], ]
      } else warning(paste('Something is wrong with', i))
      
    }
    
    # Tidy up
    dat <- droplevels(dat)
    rownames(dat) <- NULL
  
  }
  
  # Check VR data points with few deaths
  if (!is.null(VRdata)) {
    
    idLess <- unique(VRdata$ISO3[which(VRdata$totdeaths < minDeaths & VRdata$sex != 'B')])
    
    if (length(idLess) > 0) {
      
      for (i in idLess) {
        
        # Rows to be updated: Same ID2
        upd <- which(VRdata$ISO3 == i)
        
        # Two records and both sexes reported
        if (length(upd) == 2 & all(c('F', 'M') %in% VRdata[upd, 'sex'])) {
          
          # Collapse total pop and deaths
          VRdata[upd[1], 'pop'] <- sum(VRdata[upd, 'pop'], na.rm = T)
          VRdata[upd[1], 'totdeaths'] <- sum(VRdata[upd, 'totdeaths'], na.rm = T)
          
          # Collapse COD
          for (j in cod) {
            
            # If ALL DIFFERENT than NA
            if (all(!is.na(VRdata[upd, j]))) VRdata[upd[1], j] <- sum(VRdata[upd, j])
            
            # If some NA but not others
            if (any(is.na(VRdata[upd, j])) & any(!is.na(VRdata[upd, j]))) {
              
              # Check whether it is a DEFINED CAUSE
              if (j %in% c(com, ncd, inj)) {
                # Communicable
                if (j %in% com) {
                  VRdata[upd[1], 'OtherCD'] <- 
                    sum(VRdata[upd[1], 'OtherCD'], VRdata[upd, j], na.rm = T)
                }
                # Non-communicable
                if (j %in% ncd) {
                  VRdata[upd[1], 'OtherNCD'] <- 
                    sum(VRdata[upd[1], 'OtherNCD'], VRdata[upd, j], na.rm = T)
                }
                # Injuries
                if (j %in% inj) {
                  VRdata[upd[1], 'OtherInj'] <- 
                    sum(VRdata[upd[1], 'OtherInj'], VRdata[upd, j], na.rm = T)
                }
                # Assing NA to new record
                VRdata[upd[1], j] <- NA
              } else VRdata[upd[1], j] <- sum(VRdata[upd, j], na.rm = T)
            }
          }
          # Update sex and exclude second record
          VRdata[upd[1], 'sex'] <- 'B'
          VRdata <- VRdata[-upd[2], ]
        } else warning(paste('Something is wrong with', i))
        
      }
      
      # Tidy up
      VRdata <- droplevels(VRdata)
      rownames(VRdata) <- NULL
      
    }
  }
}
table(dat$ISO3)
dim(dat)
dim(VRdata)

# Re-calculate TOTAL DEATHS
dat$totdeaths <- apply(dat[, paste0(cod)], 1, sum, na.rm = T)

# UPDATE data point ID
dat$id <- paste(dat$ISO3, dat$year, dat$Refid, dat$location,
                   dat$age_lb, dat$age_ub, dat$sex, sep = '-')
length(unique(dat$id))
nrow(dat)
if (!is.null(VRdata)) VRdata$id <- paste(VRdata$ISO3, VRdata$year, VRdata$Refid, 
                                         VRdata$location, VRdata$age_lb, 
                                         VRdata$age_ub, VRdata$sex, sep = '-')

#------------------#
# MANAGE OTHER COD #
#------------------#

# Assing NA to OTHER when OTHER = 0 and other-specific reported
idOther <- which(dat$Other == 0 & !is.na(dat$OtherCD) &
                   !is.na(dat$OtherNCD) & !is.na(dat$OtherInj))
length(idOther)
if (length(idOther) > 0) dat[idOther, 'Other'] <- NA

# RE-DISTRIBUTE OTHER' when other-specific reported AND > 0
idOther <- which(!is.na(dat$Other) & dat$OtherCD >= 0 &
                   dat$OtherNCD >= 0 & dat$OtherInj >= 0)
dat[idOther, c(names(datRemove), paste(cod))]
if (length(idOther) > 0) {
  otherCat <- c('OtherCD', 'OtherNCD', 'OtherInj')
  for (i in idOther) {
    # Proportion of deaths in each 'other-specific' category
    propOther <- prop.table(dat[i, otherCat])
    # Re-distribute deaths in 'Other' proportionally
    dat[i, otherCat] <- dat[i, otherCat] + propOther * dat[i, 'Other']
    # Assign NA to Other
    dat[i, 'Other'] <- NA
  }
}

# If 'Other-specific' missing, Other != NA (Missclassificaiton matrix)
idOther <- which(is.na(dat$Other) & 
                   (is.na(dat$OtherCD) | is.na(dat$OtherNCD) | is.na(dat$OtherInj)))
dat[idOther, c(names(datRemove), paste(cod))]
if (length(idOther) > 0) dat$Other[idOther] <- 0

# Exclude Other when all NA
if (all(is.na(dat$Other))) {
  dat <- dat[, !names(dat) %in% 'Other']
  cod <- droplevels(cod[cod != 'Other'])
}

# ROUND TO INTEGERS and Re-calculate TOTAL DEATHS
dat[, paste0(cod)] <- round(dat[, paste0(cod)])
dat$totdeaths <- apply(dat[, paste0(cod)], 1, sum, na.rm = T)
if (!is.null(VRdata)) {
  VRdata[, paste0(cod)] <- round(VRdata[, paste0(cod)])
  VRdata$totdeaths <- apply(VRdata[, paste0(cod)], 1, sum, na.rm = T)
  VRdata$Other[which(VRdata$Other == 0)] <- NA
}

# Other reported and specific-other are 0
idOther <- which(!is.na(dat$Other) & dat$OtherCD == 0)
dat[idOther, c(names(datRemove), paste(cod))]
# dat[idOther, 'OtherCD'] <- NA
idOther <- which(!is.na(dat$Other) & dat$OtherNCD == 0)
dat[idOther, c(names(datRemove), paste(cod))]
# dat[idOther, 'OtherNCD'] <- NA
idOther <- which(!is.na(dat$Other) & dat$OtherInj == 0)
dat[idOther, c(names(datRemove), paste(cod))]
# dat[idOther, 'OtherInj'] <- NA

# DOUBLE CHECK
idOther <- which((dat$Other == 0 | !is.na(dat$Other)) &
                   !is.na(dat$OtherCD) & !is.na(dat$OtherNCD) & !is.na(dat$OtherInj))
nrow(dat[idOther, ])
idOther <- which(is.na(dat$Other) & 
                   (is.na(dat$OtherCD) | is.na(dat$OtherNCD) | is.na(dat$OtherInj)))
nrow(dat[idOther, ])

# Check changes in countries available
sort(unique(dat$ISO3))
length(unique(dat$ISO3))

# Check there are no negative values
which(dat[, paste(cod)] < 0)


#---------------------#
# EXCLUDE DATA POINTS #
#---------------------#

# Re-calculate TOTAL DEATHS
dat$totdeaths <- apply(dat[, paste0(cod)], 1, sum, na.rm = T)

# Exclude data points in which UNDETERMINED >= 25%
idExclude <- which(dat$Undetermined / dat$totdeaths >= .25)
dat[idExclude, c(names(datRemove), paste(cod))]
if (length(idExclude) > 0) dat <- dat[-idExclude, ]
dim(dat)

# Exclude UNDETERMINED from remaining data points
dat <- dat[, !names(dat) %in% c('id2', 'Undetermined')]
if (!is.null(VRdata)) VRdata <- VRdata[, names(VRdata) != 'Undetermined']
cod <- droplevels(cod[cod != 'Undetermined'])

# Re-calculate TOTAL DEATHS
dat$totdeaths <- apply(dat[, paste0(cod)], 1, sum, na.rm = T)

# Number of data points with less than 15 deaths
length(which(dat$totdeaths < minDeaths))
length(which(dat$totdeaths < minDeaths)) / nrow(dat)

# Exclude data points with LESS than 15 DEATHS
idExclude <- which(dat$totdeaths < minDeaths)
if (length(idExclude) > 0) {
  dat <- droplevels(dat[-idExclude, ])
  rownames(dat) <- NULL
}
range(dat$totdeaths)
sort(dat$totdeaths[dat$totdeaths < 25], decreasing = F)
length(which(dat$totdeaths < 25))

# Exclude data points with MORE than 5000 DEATHS
idExclude <- which(dat$totdeaths > maxDeaths)
if (length(idExclude) > 0) {
  dat <- droplevels(dat[-idExclude, ])
  rownames(dat) <- NULL
}
range(dat$totdeaths)
sort(dat$totdeaths[dat$totdeaths < 25], decreasing = F)
length(which(dat$totdeaths < 25))

# VR data
if (!is.null(VRdata)) {
  idExclude <- which(VRdata$totdeaths < minDeaths)
  if (length(idExclude) > 0) {
    VRdata <- droplevels(VRdata[-idExclude, ])
    rownames(VRdata) <- NULL
  }
  range(VRdata$totdeaths)
}


############################################################
### COVARIATES                                           ###
############################################################

# Covaraite selection
covSelect <- F
if (covSelect) {
  
  # All Covarites
  vxf <- names(data.predict)[c(3:295, 302:411)]
  # Remove source and other additional info
  idSource <- c()
  idRaw <- c()
  idSm <- c()
  for (i in 1:length(vxf)) {
    if (grepl('_source', vxf[i])) idSource <- c(idSource, i)
    if (grepl('_raw', vxf[i])) idRaw <- c(idRaw, i)
    if (grepl('_sm', vxf[i])) idSm <- c(idSm, i)
  }
  if (length(idSource) > 0 | length(idRaw) > 0 | length(idSm) > 0) {
    vxf <- vxf[-c(idSource, idRaw, idSm)]
  }
  # Remove NA
  idNA <- apply(data.predict[, vxf], 2, function(x) {length(which(is.na(x)))})/nrow(dat)
  length(which(idNA > 0))
  vxf[idNA != 0]
  vxf <- vxf[idNA == 0]
  
  # Remove not available covars in input
  vxf <- vxf[which(vxf %in% names(dat))]
  
  # Remove COVS that look funny (02.10.2020 DOUBLE CHECK IF DATABASE IMPROVED!)
  vxf <- vxf[which(!vxf %in% c("condom_risk_f", "condom_risk_m", "condom_risk_mf", 
                               "contraception_now", "gni"))]
  
  # Check values
  SAVE <- F
  if (SAVE) {
    write.csv(dat[, c('Refid', 'ISO3', 'age_lb', 'age_ub', paste(vxf))],
              row.names = F,
              file = paste0('Methods/', format(Sys.Date(), "%Y%m%d"),
                            '-StudyCovs', ageGroup, '-', model, vers, '.csv'))
    write.csv(data.predict[, c('ISO3', paste(vxf))],
              row.names = F,
              file = paste0('Methods/', format(Sys.Date(), "%Y%m%d"),
                            '-PredictionCovs', ageGroup, '-', model, vers, '.csv'))
  }

} else {
  
  # INITIAL LIST OF COVARIATES FOR ESTIMATION
  if (ageLow == 5) {
    
    # Covariates
    vxf <- c('year', 
             # 'anc1',
             'bcg_mf',
             # 'birth_healthfacility_3',
             'corruption',
             # 'depression_mf', 'depression_f', 'depression_m',
             'dtp3_mf',
             'edu_mean_mf', 'edu_mean_f', 'edu_mean_m',
             'gini',
             'height_mf', 'height_f', 'height_m',
             # 'hib3_mf',
             'lbw',
             'literacy_mf', 'literacy_f', 'literacy_m', 
             'lowest_wealth',
             'mcv_mf',
             'obese_mf','obese_f', 'obese_m', 
             'ors_mf', 'ors_f', 'ors_m',
             # 'pcv3_mf',
             'pfpr', 
             'pollution_air',
             # 'rota_last_mf',
             # 'sex_age_f_15', 'sex_age_m_15', 'sex_age_mf_15',
             'thinness_mf', 'thinness_f', 'thinness_m', 
             # 'tobacco_mf', 'tobacco_f', 'tobacco_m',
             'u5pop',
             'underwt_mf',
             'unemployment_neet_f', 'unemployment_neet_m', 'unemployment_neet_mf',
             'urban', 
             'u5mr')
    
  }
  
  if (ageLow == 10) {
    
    # Covariates
    vxf <- c('year',
             'alcohol',
             'bcg_mf',
             'corruption',
             'depression_mf', 'depression_f', 'depression_m',
             'dtp3_mf',
             'edu_mean_mf', 'edu_mean_f', 'edu_mean_m',
             'gini',
             'height_mf', 'height_f', 'height_m',
             'hib3_mf',
             'literacy_mf', 'literacy_f', 'literacy_m', 
             'lowest_wealth',
             # 'mcv_mf',
             'obese_mf','obese_f', 'obese_m', 
             'ors_mf', 'ors_f', 'ors_m',
             'pcv3_mf',
             'pfpr', 
             'pollution_air',
             'rota_last_mf',
             'sex_age_f_15', 'sex_age_m_15', 'sex_age_mf_15',
             'thinness_mf', 'thinness_f', 'thinness_m', 
             'tobacco_mf', 'tobacco_f', 'tobacco_m',
             'unemployment_neet_f', 'unemployment_neet_m', 'unemployment_neet_mf',
             'urban', 
             'u5mr')
    
  }

  if (ageLow == 15) {
    
    # Covariates
    vxf <- c('year', 
             'alcohol',
             'anc1',
             'bcg_mf',
             'birth_healthfacility_3',
             'birthrate',
             'cannabis_mf', 'cannabis_f', 'cannabis_m',
             'childbearing',
             'contraception_unmet',
             'corruption',
             'depression_mf', 'depression_f', 'depression_m',
             'edu_mean_mf', 'edu_mean_f', 'edu_mean_m',
             'gini',
             'hdi',
             'height_mf', 'height_f', 'height_m',
             'labor_participation_f', 'labor_participation_m', 'labor_participation_mf',
             'lbw',
             'literacy_mf', 'literacy_f', 'literacy_m', 
             'lowest_wealth',
             'marriage_f', 'marriage_m',
             'obese_mf', 'obese_f', 'obese_m', 
             'pfpr', 
             'pollution_air',
             'pop_male_15_29',
             'sex_age_f_15', 'sex_age_m_15', 'sex_age_mf_15',
             'thinness_mf', 'thinness_f', 'thinness_m', 
             'tobacco_mf', 'tobacco_f', 'tobacco_m',
             'unemployment_neet_f', 'unemployment_neet_m', 'unemployment_neet_mf',
             'urban', 
             'u5mr')
    
  }  
}

############################### HERE

# Check
vxf[which(!vxf %in% names(dat))]
vxf[which(!vxf %in% names(data.predict))]


#---------------------------#
# COVARIATES for ESTIMATION #
#---------------------------#

# Parameters to keep
pars <- c('id', 'ISO3', 'Year', 'sex', 'reterm', 'totdeaths')

# Covariate matrix
dat$Year <- dat$year


### FLAG
covMat <- dat
#covMat <- dat[, c(pars, vxf)]

# VR data
if (!is.null(VRdata)) {
  
  VRdata$Year <- VRdata$year
  VRcov <- VRcov[, c('ISO3', vxf)]
  VRcov <- merge(VRdata[, paste(pars)], VRcov, by = 'ISO3', all.x = T, all.y = F)
  VRcov <- VRcov[, names(covMat)]
  covMat <- rbind(covMat, VRcov)
  rm(VRcov)
  
}

# Check missing values
idNa <- apply(covMat[, vxf], 2, function(x) {length(which(is.na(x)))}) / nrow(covMat)
if (length(which(idNa > 0)) > 0) {
  vxf <- vxf[which(idNa == 0)]
  covMat <- covMat[, c(pars, vxf)]
}

# Tidy up
covMat <- covMat[order(covMat$ISO3, covMat$Year), ]
rownames(covMat) <- NULL


#---------------------------#
# COVARIATES for PREDICTION #
#---------------------------#

# SMOOTHED covariates for PREDICTION
k <- c()
for (i in vxf) {
  if (paste(i, 'sm', sep = '_') %in% names(data.predict)) {
    data.predict[paste(i)] <- data.predict[paste(i, 'sm', sep = '_')]
    k <- c(k, i)
  } 
}

# Check
vxf[which(!vxf %in% k)]

# UPDATE prediction database
data.predict <- data.predict[, c('ISO3', 'Year', paste(vxf))]
data.predict <- droplevels(data.predict)
data.predict <- data.predict[order(data.predict$ISO3, data.predict$Year), ]
rownames(data.predict) <- NULL


#--------------------------#
# AGE-SPECIFIC DEATH RATES #
#--------------------------#

if (FALSE) {
#if (ageMort != F) {
  
  # Add new covariate to STUDY and PREDICTION databases
  qx <- read.csv('C:/Users/FVillavicencio/Dropbox/AdolescentCOD/IGME-Envelopes/20200908_Qx5to19_1980-2019_CrisisFree.csv')
  covMat <- merge(covMat, qx[, c('ISO3', 'Year', qAgeGr, 'q5to19')],
                  by = c('ISO3', 'Year'), all.x = T)
  data.predict <- merge(data.predict, qx[, c('ISO3', 'Year', qAgeGr, 'q5to19')],
                        by = c('ISO3', 'Year'), all.x = T)
  
  # Check if age-specific rate was already availabe in STUDY database
  MR <- which(!is.na(dat[, paste0('MR', ageLow, '_', ageUp)]))
  covMat[match(dat$id[MR], covMat$id), qAgeGr]
  
  # Sex specific rates
  vxf <- c(vxf, qAgeGr, 'q5to19')
  # if (sexSplit) {
  #   
  #   # Study database
  #   covMat <- merge(covMat, qx[, c('ISO3', 'Year', paste0(qAgeGr, c('F', 'M')))],
  #                   by = c('ISO3', 'Year'), all.x = T)
  #   names(covMat)[names(covMat) == qAgeGr] <- paste0(qAgeGr, '_mf')
  #   names(covMat)[names(covMat) %in% paste0(qAgeGr, c('F', 'M'))] <- paste0(qAgeGr, c('_f', '_m'))
  #   # Missing values
  #   idNa <- which(is.na(covMat[, paste0(qAgeGr, '_f')]))
  #   covMat[idNa, paste0(qAgeGr, '_f')] <- covMat[idNa, paste0(qAgeGr, '_mf')]
  #   idNa <- which(is.na(covMat[, paste0(qAgeGr, '_m')]))
  #   covMat[idNa, paste0(qAgeGr, '_m')] <- covMat[idNa, paste0(qAgeGr, '_mf')]
  #   # # Data points with study level MR
  #   # covMat[match(dat$id[MR], covMat$id), paste0(qAgeGr, '_f')] <-
  #   #   covMat[match(dat$id[MR], covMat$id), paste0(qAgeGr, '_mf')]
  #   # covMat[match(dat$id[MR], covMat$id), paste0(qAgeGr, '_m')] <-
  #   #   covMat[match(dat$id[MR], covMat$id), paste0(qAgeGr, '_mf')]
  # 
  #   # Prediction database
  #   data.predict <- merge(data.predict, 
  #                         qx[, c('ISO3', 'Year', paste0(qAgeGr, c('F', 'M')))],
  #                         by = c('ISO3', 'Year'), all.x = T)
  #   names(data.predict)[names(data.predict) == qAgeGr] <- paste0(qAgeGr, '_mf')
  #   names(data.predict)[names(data.predict) %in% paste0(qAgeGr, c('F', 'M'))] <- paste0(qAgeGr, c('_f', '_m'))
  #   
  #   # Covariates list
  #   vxf <- c(vxf, paste(qAgeGr, c('mf', 'm', 'f'), sep = '_'), 'q5to19')
  # } else vxf <- c(vxf, qAgeGr, 'q5to19')
  
  # Remove unnecessary data
  rm(qx)  

}
covMat$q5to19 <- NA

# Checks
vxf[which(!vxf %in% names(data.predict))]
vxf[which(!vxf %in% names(covMat))]
length(which(is.na(covMat)))


#-------------------------#
# SEX-SPECIFIC COVARIATES #
#-------------------------#

# Manually re-label 'sex_age' covariates
for (i in c(15, 18, 20)) {
  for (j in c('m', 'f', 'mf')) {
    varName <- paste('sex_age', j, i, sep = '_')
    if (varName %in% vxf) {
      vxf[vxf == varName] <- paste('sex_age', i, j, sep = '_')
      names(covMat)[names(covMat) == varName] <- paste('sex_age', i, j, sep = '_')
      names(data.predict)[names(data.predict) == varName] <- paste('sex_age', i, j, sep = '_')
    }
  }
}

# Identify COVARIATES with SEX-SPECIFIC VALUES
sexSpCovs <- c()
for (i in vxf) {
  idGender <- substr(i, nchar(i)-2, nchar(i))
  if (idGender == '_mf') {
    varName <- substr(i, 1, nchar(i)-3)
    if (all(paste0(varName, c('_f', '_m')) %in% vxf)) {
      sexSpCovs <- c(sexSpCovs, varName)
    }
  }
}
sexSpCovs

# Update covariates when sex-specific values available
# FLAG
# COMMENTING THIS OUT
# if (sexSplit) {
#   
#   # Expand prediction database
#   data.predict <- data.predict[rep(row.names(data.predict), each = 2), ]
#   data.predict$sex <- rep(c('F', 'M'), nrow(data.predict) / 2)
#   
#   for (i in sexSpCovs) {
#     
#     # STUDY DATABASE
#     covMat[covMat$sex == 'F', paste0(i, '_mf')] <- covMat[covMat$sex == 'F', paste0(i, '_f')]
#     covMat[covMat$sex == 'M', paste0(i, '_mf')] <- covMat[covMat$sex == 'M', paste0(i, '_m')]
#     
#     # PREDICTION DATABASE
#     data.predict[data.predict$sex == 'F', paste0(i, '_mf')] <- 
#       data.predict[data.predict$sex == 'F', paste0(i, '_f')]
#     data.predict[data.predict$sex == 'M', paste0(i, '_mf')] <- 
#       data.predict[data.predict$sex == 'M', paste0(i, '_m')]
#     
#   }
#   
# }

# Update COVARIATE LIST
vxf[vxf %in% paste0(sexSpCovs, '_mf')] <- sexSpCovs
vxf <- vxf[!vxf %in% paste0(sexSpCovs, '_f')]
vxf <- vxf[!vxf %in% paste0(sexSpCovs, '_m')]

# # Tidy up STUDY database
# names(covMat)[names(covMat) %in% paste0(sexSpCovs, '_mf')] <- sexSpCovs
# covMat <- covMat[, !names(covMat) %in% paste0(sexSpCovs, '_f')]
# covMat <- covMat[, !names(covMat) %in% paste0(sexSpCovs, '_m')]
# covMat <- droplevels(covMat)
# covMat <- covMat[, c(pars, vxf)]
# rownames(covMat) <- NULL
# head(covMat)

# Tidy up PREDICTION database
names(data.predict)[names(data.predict) %in% paste0(sexSpCovs, '_mf')] <- sexSpCovs
if (sexSplit) {
  data.predict <- data.predict[, c('ISO3', 'Year', 'sex', vxf)]
} else data.predict <- data.predict[, c('ISO3', 'Year', paste(vxf))]
data.predict <- droplevels(data.predict)
rownames(data.predict) <- NULL
head(data.predict)

# CHECK: Plot covariates
# country <- 'IND'
# covs <- c('alcohol', 'literacy_f', 'thinness') #, qAgeGr)
# dat1 <- data.predict[data.predict$ISO3 == country, ]
# dat1 <- dat1[, paste(c('ISO3', 'Year', covs))]
# dat1 <- dat1[dat1$Year %in% years, ]
# rownames(dat1) <- NULL
# # Plot
# par(mfrow = c(2, 2), las = 1, mar = c(4.5, 4, 3, 1))
# for (i in covs) {
#   plot(dat1$Year, dat1[, paste(i)], xlab = 'Year', 
#        ylab = '', main = i, type = 'l')
# }


#---------------------#
# COVARIATE SELECTION #
#---------------------#

# Select age-specific mortality measure
vxf <- vxf[!vxf %in% c('u5mr', qAgeGr, 'q5to19')]
vxf <- c('year', sort(vxf[vxf != 'year']))
if (ageMort != F) vxf <- c(vxf, ageMort)

# Identify covariates with lots of 0's
# id0 <- apply(covMat[, paste(vxf)], 2, 
#              function(x) {length(which(x == 0))})
# sort(round(id0 / nrow(dat), 3), decreasing = T)

# TB NOT modelled
vxf <- vxf[!vxf %in% 'bcg_mf']

# Is MALARIA modelled
if (!Malaria) vxf <- vxf[!vxf %in% 'pfpr']

# Age group 5 to 9
if (ageLow == 5) {
  
  # V0
  # vxf <- vxf[!vxf %in% c('gini', 'height',
  #                        'lowest_wealth', 'pop_male_15_29')]
  
  # V1
  # vxf <- vxf[!vxf %in% c('childbearing', 'edu_completion',
  #                        'lbw', 'gni', 'sex_age_18', 'urban')]
  
}

# Age group 10 to 14
if (ageLow == 10) {
  
  # V0
  # vxf <- vxf[!vxf %in% c('gini', 'height',
  #                        'lowest_wealth', 'pop_male_15_29')]
  
  # V1
  # vxf <- vxf[!vxf %in% c('childbearing', 'edu_completion',
  #                        'lbw', 'gni', 'sex_age_18', 'urban')]
  
  # vxf <- vxf[!vxf %in% c('edu_mean', 'hib3_mf', 'pcv3_mf', 'rota_last_mf', 'tobacco')]
  
}

SAVE <- F
if (SAVE & (ageLow == 5 | ageLow == 10)) {
  write.csv(x = cor(covMat[, vxf]), 
            file = paste0('Methods/', model, '-cor', ageGroup, '-v', vers, '.csv'))
}

# Age group 15 to 19
if (ageLow == 15) {
  
  # # V0 FEMALES
  # vxf <- vxf[!vxf %in% c('hdi', 'labor_participation',
  #                        'pop_male_15_29', 'marriage_m')]
  # # V1 FEMALES
  # vxf <- vxf[!vxf %in% c('bcg_mf', 'cannabis', 'gini',
  #                        'thinness', 'tobacco', 'unemployment_neet')]
  # # V0 MALES
  # vxf <- vxf[!vxf %in% c('anc1', 'birth_healthfacility_3',
  #                        'childbearing', 'contraception_unmet')]
  # # V1 MALES
  # vxf <- vxf[!vxf %in% c('bcg_mf', 'cannabis', 'literacy', 'obese', 
  #                        'thinness', 'tobacco')]
  
  # Remove COVARIATES with HIGH CORRELATION or FUNNY TRENDS
  # vxf <- vxf[!vxf %in% c('anc1', 'cannabis', 'corruption', 'hdi', 
  #                        'pollution_air', 'thinness', 'tobacco')]
  # vxf <- vxf[!vxf %in% c('anc1', 'cannabis', 'hdi', 'tobacco')]
  
  # V2 FEMALES
  # vxf <- vxf[!vxf %in% c('labor_participation', 'marriage_m', 
  #                        'pop_male_15_29', 'unemployment_neet')]
  # V2 MALES
  # vxf <- vxf[!vxf %in% c('birth_healthfacility_3', 'childbearing', 
  #                        'contraception_unmet', 'obese')]
  
  SAVE <- F
  if (SAVE) {
    # Females
    write.csv(x = cor(covMat[covMat$sex %in% c('F', 'B'), vxf]), 
              file = paste0('Methods/', model, '-cor', ageGroup, 'F_v', vers, '.csv'))
    
    # Males
    write.csv(x = cor(covMat[covMat$sex %in% c('M', 'B'), vxf]), 
              file = paste0('Methods/', model, '-cor', ageGroup, 'M_v', vers, '.csv'))
    
  }
  
}


#------------------#
# STUDIES DATABASE #
#------------------#

# Add 'sid' (to match previous code neonates)
covMat <- covMat[order(covMat$ISO3, covMat$Year), ]
rownames(covMat) <- NULL
covMat <- data.frame(sid = 1:nrow(covMat), covMat)
head(covMat)
tail(covMat)

# Save
SAVE <- F
if (SAVE) {
  covMat2 <- covMat
  pars <- c('id', 'Refid', 'study_id', 'age_lb', 'age_ub') 
  covMat2 <- merge(covMat2, dat[, pars],
                   by = 'id', all.x = T, all.y = F)
  covMat2 <- covMat2[, c(pars, names(covMat2)[!names(covMat2) %in% pars])]
  covMat2 <- covMat2[, !names(covMat2) %in% c('sid', 'reterm', 'year')]
  covMat2 <- covMat2[, c('ISO3', names(covMat2)[names(covMat2) != 'ISO3'])]
  rownames(covMat2) <- NULL
  
  # Save file
  write.csv(x = covMat2, row.names = F,
            paste0('Methods/', format(Sys.Date(), "%Y%m%d"),
                   '-StudyCovs', ageGroup, '-', model, vers, '.csv'))
}

# Update list of covariates
if (sexSplit) vxf <- c('sex', vxf)

# STUDIES database
# studies <- covMat[, names(covMat)[names(covMat) %in% c("sid", "id", "reterm", "totdeaths", vxf)]]
# studies <- studies[, c("sid", "id", "reterm", "totdeaths", vxf)]
# rm(covMat)
studies <- covMat
studies <- studies[,c("sid", "id", "reterm", "totdeaths", names(studies)[!(names(studies) %in% c("sid", "id", "reterm", "totdeaths"))])]

# Check all data points different covariates
dim(distinct(studies[, paste(vxf)]))
dim(studies)
#duppl <- which(duplicated(studies[, paste(vxf)]))
#duppl <- sort(c(duppl, duppl - 1), decreasing = F)
#studies[duppl, ][!duplicated(studies[duppl, ]), ]


#----------------------------#
# UPDATE PREDICTION DATABASE #
#----------------------------#

# Only selected covariates
#data.predict <- data.predict[, c('ISO3', 'Year', vxf)]
data.predict <- droplevels(data.predict)
rownames(data.predict) <- NULL

# SAVE
SAVE <- F
if (SAVE) {
  
  write.csv(x = data.predict, row.names = F,
            paste0('Methods/', format(Sys.Date(), "%Y%m%d"),
                   '-PredictCovs', ageGroup, '-', model, vers, '.csv'))
  
}

###################################### RESTART HERE

############################################################
### DEATHS DATABASE                                      ###  
############################################################

# Deaths from dat
deaths <- dat[, c('id', 'sex', 'totdeaths', paste(cod))]
rm(dat)
idVR <- NULL
if (!is.null(VRdata)) {
  
  VRdata <- VRdata[, c('id', 'sex', 'totdeaths', paste(cod))]
  VRdata <- VRdata[, names(deaths)]
  deaths <- rbind(deaths, VRdata)
  idVR <- VRdata$id
  rm(VRdata)
    
}
rownames(deaths) <- NULL
deaths <- droplevels(deaths)


#------------------------#
# DESCRIPTIVE STATISTICS #
#------------------------#

# Number of deaths by cause: MALES
deaths2 <- apply(deaths[deaths$sex %in% c('M', 'B'), paste(cod)], 2, sum, na.rm = T)
par(mar = c(8, 4, 3, 1)) 
barplot(deaths2, las = 2, main = paste0('Input data: Deaths by cause, Males, ',
                                        length(deaths$sex[deaths$sex %in% c('M', 'B')]),
                                        ' data points'))
round(100*sort(prop.table(deaths2), decreasing = T), 2)
# Distribution of deaths across datapoints
boxplot(deaths[deaths$sex %in% c('M', 'B'), paste(cod)], xaxt = 'n', freq = F,
        main = 'Input data: Distribution of COD accross datapoints, Males')
axis(1, at = 1:length(cod), labels = F)
text(1:length(cod), par("usr")[3] - 250, cod, srt = 45, xpd = T)
# Distribution of deaths across datapoints (> 500)
boxplot(deaths[deaths$sex %in% c('M', 'B'), paste(cod)], xaxt = 'n', ylim = c(0, 150),
        main = 'Input data: Distribution of COD accross datapoints, Males (<150 Interp_vio deaths)')
axis(1, at = 1:length(cod), labels = F)
text(1:length(cod), par("usr")[3] - 20, cod, srt = 45, xpd = T)


#------------------------------------------#
# REMOVE DATAPOINTS WITH EXTREME FRACTIONS #
#------------------------------------------#

deaths[deaths$totdeaths > 10000, ]
deaths[deaths$totdeaths > 5000 & deaths$totdeaths < 10000, ]

# MALES
# Calculate proportions
deaths2 <- deaths[deaths$sex %in% c('B', 'M'), ]
deaths2[, paste(cod)] <- deaths2[, paste(cod)] / deaths2$totdeaths
table(rowSums(deaths2[, paste(cod)], na.rm = T))

# Identify quantiles
top95 <- apply(deaths2[, c('totdeaths', paste(cod))], 2, quantile, .95, na.rm = T)
idExclude <- c()
# for (i in 'Interp_violence') {
for (i in cod[!cod %in% c('OtherCD', 'OtherNCD', 'OtherInj', 'Other')]) {
  idExclude <- c(idExclude,
                 which(deaths2[, paste(i)] > .5 & 
                         deaths2[, paste(i)] > top95[paste(i)] &
                         deaths2$totdeaths > top95['totdeaths']))
}
length(idExclude)
deaths[deaths$sex %in% c('B', 'M'), ][idExclude, ]
deaths2[idExclude, ]
# if (length(idExclude) > 0) {
#   idExclude <- deaths[deaths$sex %in% c('B', 'M'), ][idExclude, 'id']
#   deaths <- deaths[which(!deaths$id %in% idExclude), ]
#   rownames(deaths) <- NULL
#   # Update STUDIES database
#   studies <- studies[which(studies$id %in% deaths$id), ]
#   studies$sid <- 1:nrow(studies)
#   rownames(studies) <- NULL
# }

# Check
deaths[which(deaths$Interp_violence > 500), ]

# Number of deaths by cause: FEMALES
# deaths2 <- apply(deaths[deaths$sex %in% c('F', 'B'), paste(cod)], 2, sum, na.rm = T)
# barplot(deaths2, las = 2, paste0('Input data: Deaths by cause, Females, ',
#                                  length(deaths$sex[deaths$sex %in% c('F', 'B')]),
#                                  ' data points'))
# round(100*sort(prop.table(deaths2), decreasing = T), 2)
# 
# # Number of times COD REPORTED
# deaths2 <- apply(deaths[deaths$sex %in% c('M', 'B'), paste(cod)], 2,
#                  function(x) {length(which(!is.na(x)))})
# sort(deaths2, decreasing = T)
# 
# # Number of times COD with highest fraction
# deaths2 <- apply(deaths[deaths$sex %in% c('F', 'B'), paste(cod)], 1,
#                  function(x) {cod[which(x == max(x, na.rm = T))]})
# deaths2 <- unlist(deaths2)
# sort(table(deaths2), decreasing = T)
# 
# # Datapoints with more deaths
# deaths[order(deaths$totdeaths, decreasing = T), ][1:10, ]
# IND <- c('IND-1995-27360-1-15-24-B', 'IND-1991-27360-1-15-24-B', 'IND-1985-27360-1-15-24-B')
# sum(deaths$Cardiovascular[deaths$id %in% IND], na.rm = T) / 
#   sum(deaths$Cardiovascular, na.rm = T)
# sum(deaths$totdeaths[deaths$id %in% IND], na.rm = T) / 
#   sum(deaths$totdeaths, na.rm = T)


#--------------------------#
# MISCLASSIFICAIOTN MATRIX #
#--------------------------#

# Recover sid from STUDIES database
deaths <- merge(deaths, studies[, c('sid', 'id')], by = 'id')
deaths <- deaths[, c('sid', names(deaths)[names(deaths) != 'sid'])]

# Turn columns into rows
deaths <- gather(deaths, 'cause', 'n', paste(cod))
deaths <- deaths[, names(deaths) %in% c('sid', 'id', 'sex', 'totdeaths', 'cause', 'n')]

# Delete NA
deaths <- deaths[!is.na(deaths$n), ]
deaths <- deaths[order(deaths$sid), ]
rownames(deaths) <- NULL

# Count COD with 0 deaths
unique(deaths$n)
sort(table(deaths$n), decreasing = T)
round(100*length(which(deaths$n == 0)) /nrow(deaths), 2)

# MISCLASSIFICATION MATRIX
deaths <- cbind(deaths, 
                matrix(0, nrow = nrow(deaths), ncol = length(cod),
                       dimnames = list(NULL, cod)))

# STEP 1: Reported COD
for (i in cod) {
  deaths[, paste(i)][deaths$cause == i] <- 1
}

# STEP 2: Missing COD
missingCOD <- apply(deaths[, paste(cod)], 2, sum) / length(unique(studies$id))
missCOD <- c()
if (any(missingCOD != 1)) {
  
  warning('There are some missing COD. Check VAMCM data cleaning.')

  misclass <- aggregate(reclass[, -1], by = list(reclass$Reclass), 
                        unique)[, -1]
  for (i in unique(deaths$id)) {
    datAux <- deaths[deaths$id == i, ]
    # Check if COD available in data point (excluding 'Other')
    for (j in cod[cod != 'Other']) {
      # If COD not available in current data point
      if (!j %in% datAux$cause) {
        # Recover corresponding Level 2 COD (OtherCD, OtherNCD, OtherInj)
        codLev2 <- misclass$Level2[misclass$Reclass == paste(j)]
        # If level 2 COD available in current data point
        if (codLev2 %in% datAux$cause) {
          # Fill corresponding cell with 1 (misclassified COD)
          deaths[deaths$id == i & deaths$cause == codLev2, paste(j)] <- 1
          rm(codLev2)
        } else {
          # Recover corresponding Level 3 COD (Other)
          codLev3 <- misclass$Level3[misclass$Reclass == paste(j)]
          # If level 3 COD available in current data point
          if (codLev3 %in% datAux$cause) {
            # Fill corresponding cell with 1 (misclassified COD)
            deaths[deaths$id == i & deaths$cause == codLev3, paste(j)] <- 1
            rm(codLev3)
          } else missCOD <- c(missCOD, i)
        }
      }
    }
    rm(datAux)
  }
}

# Check if any records with issues compare selection
nrow(studies[which(studies$id %in% missCOD), ])
nrow(deaths[deaths$id %in% missCOD, ])

# Exclude Other from the deaths database
deaths <- deaths[, !names(deaths) %in% c('Other','Undetermined')]
cod <- droplevels(cod[!cod %in% c('Other','Undetermined')])
cod

# EXCLUDE TB (Not modelled!): Assing it to LRI and Other CD
# if ('TB' %in% cod) {
#   deaths[deaths$cause == 'TB', 'LRI'] <- 1
#   deaths[deaths$cause == 'TB', 'OtherCD'] <- 1
#   deaths <- deaths[, names(deaths) != 'TB']
#   cod <- droplevels(cod[cod != 'TB'])
# }

# Check studies with issues
idOther <- which(apply(deaths[, paste(cod)], 1, sum) == 0)
nrow(deaths[idOther, ])

# Check MISCLASSIFICATION MATRIX is complete
M <- matrix(NA, nrow = length(unique(deaths$sid)), ncol = length(cod))
for (i in 1:length(unique(deaths$id))) {
  id <- unique(deaths$id)[i]
  M[i, ] <- colSums(deaths[deaths$id == id, paste(cod)])  
}
# All values MUST be 1!!
table(M)

# Reported COD (MAY include Other, that is not modelled)
unique(deaths$cause)
unique(deaths$cause)[which(!unique(deaths$cause) %in% cod)]


############################################################
### TRANSFORM COVARIATES                                 ###  
############################################################

# TRANSFORM RATE
if (rateTrans != F) {
  
  # Log transformation
  if (rateTrans %in% c('log', 'log-log', 'sqrt-log')) {
    
    k <- ifelse(rateTrans == 'log-log', 2, 1)
    
    for (i in 1:k) {
      
      if ('u5mr' %in% names(studies)) {
        studies$u5mr <- log(studies$u5mr)
        data.predict$u5mr <- log(data.predict$u5mr)
      }
      if (ageMort %in% names(studies)) {
        studies[, paste(ageMort)] <- log(studies[, paste(ageMort)])
        data.predict[, paste(ageMort)] <- log(data.predict[, paste(ageMort)])
      }
      
    }
    
  }
  
  # Square root transformation
  if (rateTrans %in% c('sqrt', 'sqrt-log')) {
    
    if ('u5mr' %in% names(studies)) {
      studies$u5mr <- sqrt(studies$u5mr)
      data.predict$u5mr <- sqrt(data.predict$u5mr)
    }
    if (ageMort %in% names(studies)) {
      studies[, paste(ageMort)] <- sqrt(studies[, paste(ageMort)])
      data.predict[, paste(ageMort)] <- sqrt(data.predict[, paste(ageMort)])
    }
    
  }
  
}

# YEAR as COVARIATE?
if (!yearCov) {
  studies <- studies[, names(studies) != 'year']
  data.predict <- data.predict[, names(data.predict) != 'year']
  vxf <- vxf[vxf != 'year']
}


############################################################
### OUTPUT                                               ###  
############################################################

# Random effects ('grouping' category)
vxr <- 'reterm'

# Causes of death
vdt <- cod

# Test id
test <- 'Test8jNASA'

# Save output
SAVE <- F
if (SAVE) {

  if (sexSplit) {
    
    # Auxiliary objects
    studies2 <- studies
    deaths2 <- deaths
    data.predict2 <- data.predict
    noCov <- c('sex', 'anc1', 'cannabis', 'hdi', # 'height', 'literacy', 
               'lbw', 'tobacco')
    vxf2 <- vxf[!vxf %in% noCov]
    
    # FEMALES
    sex <- 'Fem'
    vxf <- vxf2[!vxf2 %in% c('corruption', 'height', 'labor_participation', 
                             'marriage_m', 'pop_male_15_29')]
    vxf <- vxf[!vxf %in% c('corruption', 'edu_mean', 'obese', 'marriage_f',
                           'pollution_air', 'unemployment_neet')]
    # vxf <- vxf[!vxf %in% c('contraception_unmet', 'depression', 'literacy', 'thinness')]
    vxf <- vxf[!vxf %in% c('birthrate')]
    # vxf <- vxf[!vxf %in% c('alcohol')]
    # vxf <- vxf[!vxf %in% c('corruption')]
    # vxf <- vxf[!vxf %in% c('thinness')]
    # vxf <- vxf[!vxf %in% c('obese')]
    # vxf <- vxf[!vxf %in% c('sex_age_15')]
    
    SAVE <- F
    # if (SAVE) {
    #   write.csv(x = cor(studies[studies$sex %in% c('F', 'B'), vxf]), 
    #             file = paste0('Methods/', model, '-cor', ageGroup, 'F_v', vers, '.csv'))  
    # }
    
    # Studies
    studies <- studies2[studies2$sex %in% c('F', 'B'), ]
    #studies <- studies[, !names(studies) %in% c(noCov, vxf2[!vxf2 %in% vxf])]
    studies$sid <- 1:nrow(studies)
    rownames(studies) <- NULL
    # Deaths
    deaths <- deaths2[deaths2$sex %in% c('F', 'B'), ]
    deaths <- merge(deaths[, !names(deaths) %in% 'sid'],
                    studies[, c('id', 'sid')],
                    by = 'id', all.x = T)
    deaths <- deaths[, c('sid', 
                         names(deaths)[!names(deaths) %in% c('sid', 'sex')])]
    # Prediction
    data.predict <- data.predict2[data.predict2$sex == 'F', ]
    data.predict <- 
      data.predict[, !names(data.predict) %in% c(noCov, vxf2[!vxf2 %in% vxf])]
    rownames(data.predict) <- NULL
    fileName <- paste0('Data/', ageGroup, '/', format(Sys.Date(), "%Y%m%d"),
                       '-dataPredictVAMCM', ageLow, '-', ageUp, 'F.csv')
    # write.csv(data.predict, file = fileName, row.names = F)
    
    # Check all data points different covariates
    dim(distinct(studies[, paste(vxf)]))
    dim(studies)
    
    # Save output
    save(deaths, studies, data.predict, vxf, vxr, vdt, model,
         ageLow, vers, test, refCat, sexSplit, sex, 
         rateTrans, yearCov, ageMort,
         file = paste0('Data/', ageGroup, '/', format(Sys.Date(), "%Y%m%d"),
                       '-Data', ageLow, 'to', ageUp, sex, '-', model,
                       vers, '-', test, '.RData'))
    
    # MALES
    sex <- 'Men'
    vxf <- vxf2[!vxf2 %in% c('birth_healthfacility_3', 'birthrate', 'childbearing',
                             'contraception_unmet', 'marriage_f')]
    vxf <- vxf[!vxf %in% c('obese', 'thinness', 'edu_mean')] # 'edu_mean',
    vxf <- vxf[!vxf %in% c('height', 'sex_age_15')]
    # vxf <- vxf[!vxf %in% c('unemployment_neet')]
    # vxf <- vxf[!vxf %in% c('height', 'literacy', 'obese')]
    vxf <- vxf[!vxf %in% c('pop_male_15_29')]
    # vxf <- vxf[!vxf %in% c('urban')]
    vxf <- vxf[!vxf %in% c('pollution_air')]
    # vxf <- vxf[!vxf %in% c('lowest_wealth')]
    # vxf <- vxf[!vxf %in% c('thinness')]
    # vxf <- vxf[!vxf %in% c('corruption')]
    
    # Studies
    studies <- studies2[studies2$sex %in% c('M', 'B'), ]
    studies <- studies[, !names(studies) %in% c(noCov, vxf2[!vxf2 %in% vxf])]
    studies$sid <- 1:nrow(studies)
    rownames(studies) <- NULL
    # Deaths
    deaths <- deaths2[deaths2$sex %in% c('M', 'B'), ]
    deaths <- merge(deaths[, !names(deaths) %in% 'sid'],
                    studies[, c('id', 'sid')],
                    by = 'id', all.x = T)
    deaths <- deaths[, c('sid', 
                         names(deaths)[!names(deaths) %in% c('sid', 'sex')])]
    # Prediction
    data.predict <- data.predict2[data.predict2$sex == 'M', ]
    data.predict <- 
      data.predict[, !names(data.predict) %in% c(noCov, vxf2[!vxf2 %in% vxf])]
    rownames(data.predict) <- NULL
    fileName <- paste0('Data/', ageGroup, '/', format(Sys.Date(), "%Y%m%d"),
                       '-dataPredictVAMCM', ageLow, '-', ageUp, 'M.csv')
    # write.csv(data.predict, file = fileName, row.names = F)
    
    # Check all data points different covariates
    dim(distinct(studies[, paste(vxf)]))
    dim(studies)
    
    # Save output
    save(deaths, studies, data.predict, vxf, vxr, vdt, model,
         ageLow, vers, test, refCat, sexSplit, sex,
         rateTrans, yearCov, ageMort,
         file = paste0('Data/', ageGroup, '/', format(Sys.Date(), "%Y%m%d"),
                       '-Data', ageLow, 'to', ageUp, sex, '-', model,
                       vers, '-', test, '.RData'))
    
  } else {
    
    # Exclude covariates
    if (ageLow == 5) {
      vxf <- vxf[!vxf %in% c('mcv_mf', 'obese', 'u5pop', 'underwt_mf')]
      vxf <- vxf[!vxf %in% c('literacy', 'ors')]
      # vxf <- vxf[!vxf %in% c('ors')]  
    }
    if (ageLow == 10) {
      vxf <- vxf[!vxf %in% c('hib3_mf', 'literacy', 'pcv3_mf', 
                             'rota_last_mf', 'unemployment_neet', 'tobacco')]
      # vxf <- vxf[!vxf %in% c('alcohol', 'depression', 'thinness')]
      vxf <- vxf[!vxf %in% c('edu_mean')]
      vxf <- vxf[!vxf %in% c('height')]
      vxf <- vxf[!vxf %in% c('obese')]
      # vxf <- vxf[!vxf %in% c('thinness')]
      # vxf <- vxf[!vxf %in% c('gini')]
      # vxf <- vxf[!vxf %in% c('depression')]
      # vxf <- vxf[!vxf %in% c('height', 'literacy', 'thinness')]
      # vxf <- vxf[!vxf %in% c('depression', 'obese')]
      # vxf <- vxf[!vxf %in% c('edu_mean', 'gini', 'hib3_mf', 'pcv3_mf', 'rota_last_mf', 'tobacco')]
      # vxf <- vxf[!vxf %in% c('height', 'literacy', 'obese', 'thinness')]
    }
    # vxf <- vxf[!vxf %in% c('pollution_air')]
    
    # Studies
    #studies <- studies[, names(studies) %in% c("sid", "id", "reterm", "totdeaths", vxf)]
    # Prediction
    data.predict <- data.predict[, names(data.predict) %in% c('ISO3', 'Year', vxf)]
    fileName <- paste0('Data/', ageGroup, '/', format(Sys.Date(), "%Y%m%d"),
                       '-dataPredictVAMCM', ageLow, '-', ageUp, '.csv')
    # write.csv(data.predict, file = fileName, row.names = F)
      
    # Check all data points different covariates
    #dim(distinct(studies[, paste(vxf)]))
    #dim(studies)
    
    # No sex split
    sex <- NULL
    # save(deaths, studies, data.predict, vxf, vxr, vdt, model,
    #      ageLow, vers, test, refCat, sexSplit, sex,
    #      rateTrans, yearCov, ageMort,
    #      file = paste0('Data/', ageGroup, '/', format(Sys.Date(), "%Y%m%d"),
    #                    '-Data', ageLow, 'to', ageUp, sex, '-', model,
    #                    vers, '-', test, '.RData'))  
  }
}

# SAVE KEY ----------------------------------------------------------------

write.csv(studies, paste0("./gen/update-covar-for-old-studies/temp/studydb-old-allcovar_", ageSexSuffix,".csv", sep =""))

