################################################################################
#' @description The studies model input doesnt have an id that matches source columns in HMM database. Need to retrieve it.
#' @return Age-specific id keys for studies
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readstata13)
library(tidyverse)
library(data.table)
#' Inputs

# Set age group
source("./src/update-covar-for-old-studies/set-inputs.R")

# 2020 master study database (pre-cleaning)
dat <- readstata13::read.dta13("./data/study-data-old/20200930-HMM-StudyDatabase.dta")

# COUNTRY CLASSIFICATION
countryClass <- read.csv('./data/classification-keys/CountryModelClass_20201001.csv')
################################################################################

# Create id variable in for HMM-StudyDatabase that shows up in model inputs
# See Pancho's code in VAMCM-DataCleaning009
# Run code up to 1223 for creation of id variable

if(ageSexSuffix == "05to09y") {
  ageLow <- 5
  ageUp <- 9
}
if(ageSexSuffix == "10to14y") {
  ageLow <- 10
  ageUp <- 14
}
if(ageSexSuffix %in% c("15to19yF","15to19yM")) {
  ageLow <- 15
  ageUp <- 19
}

# Model
model <- 'VAMCM'

# Years for prediction
years <- 2000:2019

# Select age group to produce database
ageGroup <- paste0(ageLow, 'to', ageUp)
qAgeGr <- paste0('q', ageGroup)

# Reference category
if (ageLow == 5) refCat <- 'Diarrhoeal'
if (ageLow == 10) refCat <- 'LRI' 
if (ageLow == 15) refCat <- 'RTA' #'Self_harm' # 

# Split by sex?
sexSplit <- T
if (ageLow < 15) sexSplit <- F

# Minimum number of deaths per data point
minDeaths <- 15

# Maximum number of deaths per data point
maxDeaths <- 5000

# Include VR data?
VRdata <- T

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


# ESTIMATION DATABASE
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
if (length(idExclude) > 0) {
  dat <- dat[-idExclude, ]
  dat <- droplevels(dat)
  rownames(dat) <- NULL
}

# # Malaria
# malaria <- dat$mal / dat$totdeaths_orig
# dat[which(malaria > .3), c('iso3', 'year', 'age_lb', 'age_ub',
#                            'mal', 'totdeaths_orig')]
# 
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
#names(dat)[names(dat) == 'study_location'] <- 'location'
dat$location <- dat$study_location
names(dat)[names(dat) == 'totdeaths_orig'] <- 'totdeaths'

# Nation wide studies
dat$location[which(dat$nation_rep == 1)] <- 'nationwide'

# Madagascar
dat$location[which(dat$Refid == 'MDG')] <- 'Antananarivo'

# Re-label SEX variable
dat$sex[dat$sex == 3] <- "male"
dat$sex[dat$sex == 2] <- "female"
dat$sex[dat$sex == 1] <- "both"
dat$sex <- as.factor(dat$sex)
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
IDN2001[1, 'age_lb'] <- min(as.numeric(as.character(IDN2001$age_lb)))
IDN2001[1, 'age_ub'] <- max(as.numeric(as.character(IDN2001$age_ub)))
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

#head(matchmult)
#subset(dat, ISO3 == "IND" & year == 2005 & age_lb == 5 & age_ub == 9)[,c("location","location_fac")]


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

############################################################
### CAUSES OF DEATH                                      ###  
############################################################

# Initial list of causes of death
cod <- names(dat)[colCOD[-1]]

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
  
}
table(dat$ISO3)
dim(dat)

# Re-calculate TOTAL DEATHS
dat$totdeaths <- apply(dat[, paste0(cod)], 1, sum, na.rm = T)

# UPDATE data point ID
dat$id <- paste(dat$ISO3, dat$year, dat$Refid, dat$location,
                dat$age_lb, dat$age_ub, dat$sex, sep = '-')
length(unique(dat$id))
nrow(dat)


key <- dat[,c("id","Refid","study_id","study_location","ISO3","year","year_start","year_end","age_lb","age_ub","strata_ur","sex","strata_other1")]
# Convert id var back to original form
names(key)[names(key) == 'ISO3'] <- 'iso3'
names(key)[names(key) == 'sex'] <- 'strata_gender'
key$strata_gender <- as.character(key$strata_gender)
key$strata_gender[key$strata_gender == "M"] <- 3
key$strata_gender[key$strata_gender == "F"] <- 2
key$strata_gender[key$strata_gender == "B"] <- 1

# Save output -------------------------------------------------------------

write.csv(key, paste0("./gen/update-covar-for-old-studies/temp/key_studyid_",ageSexSuffix,".csv",sep =""), row.names = FALSE)
