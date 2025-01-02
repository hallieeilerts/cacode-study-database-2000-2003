################################################################################
#' @description Apply reclassification matrices to deal with CODs that were not reported by study
#' @return deaths in the model input for new studies
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
#' Inputs
source("./src/set-inputs.R")
## Study data with too small and too large data points excluded
dat <- read.csv(paste0("./gen/process-new-studies/temp/studies_exc-size_", ageSexSuffix, ".csv", sep = ""))
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- head(sort(dat_filename),1) # Most recent
key <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

#--------------------------#
# MISCLASSIFICATION MATRIX #
#--------------------------#

# Reclassified CODs for this age group
# (includes Other and Undetermined)
v_cod_reclass <- unique(subset(key, !is.na(cod_reclass))$cod_reclass)
# Exclude "TB" which has been redistributed (only present in 5-9y and 10-14y reclass vector)
# Exclude "Undetermined" because any deaths attributed to Undetermined have been excluded in exclude-by-size
v_cod_reclass <- v_cod_reclass[!(v_cod_reclass %in% c("TB", "Undetermined"))]
v_cod_reclass <- v_cod_reclass[!is.na(v_cod_reclass)]
# Exclude "Other" from vector further down in this script

deaths <- dat
par <- c("recnr", "id", "sex", "totdeaths", "cause", "n")

# Turn columns into rows
deaths <- gather(deaths, 'cause', 'n', paste(v_cod_reclass))
deaths <- deaths[, names(deaths) %in% par]

# Delete NA
deaths <- deaths[!is.na(deaths$n), ]
deaths <- deaths[order(deaths$recnr), ]
rownames(deaths) <- NULL

# Count COD with 0 deaths
unique(deaths$n)
sort(table(deaths$n), decreasing = T)
round(100*length(which(deaths$n == 0)) /nrow(deaths), 2)

# MISCLASSIFICATION MATRIX
deaths <- cbind(deaths, 
                matrix(0, nrow = nrow(deaths), ncol = length(v_cod_reclass),
                       dimnames = list(NULL, v_cod_reclass)))


# STEP 1: Fill in 1 if COD is reported in the study
for (i in v_cod_reclass) {
  deaths[, paste(i)][deaths$cause == i] <- 1
}

reclass <-  key[,c("cod_reclass", "cod_level2", "cod_level3", "cod_level4")]
names(reclass) <- c("Reclass", "Level2", "Level3", "Level4")
misclass <- aggregate(reclass, by = list(reclass$Reclass), unique)[,-1]

# STEP 2: Missing COD
# Adds a 1 redistributing Other categories to any unreported COD for the study

# Codes a fraction for each cause that shows the number of studies reporting it
missingCOD <- apply(deaths[, paste(v_cod_reclass)], 2, sum) / length(unique(deaths$id))
missCOD <- c()
if (any(missingCOD != 1)) {
  
  warning('There are some missing COD. Check VAMCM data cleaning.')
  
  for (i in unique(deaths$id)) {
    datAux <- deaths[deaths$id == i, ]
    # Check if COD available in data point (excluding 'Other')
    for (j in v_cod_reclass[v_cod_reclass != 'Other']) {
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

# For 5-19, exclude "Other" from columns and reclassification vector
# Other is redistributed to other-specific and any other modeled COD that was not reported.
# For under-5, the general "Other" category is modeled (i think?)
if(ageSexSuffix %in% c("05to09y", "10to14y","15to19yF","15to19yM")){
  deaths <- deaths[, !names(deaths) %in% c('Other')]
  v_cod_reclass <- v_cod_reclass[!v_cod_reclass %in% c('Other')]
}

# Check studies with issues
idOther <- which(apply(deaths[, paste(v_cod_reclass)], 1, sum) == 0)
nrow(deaths[idOther, ])

# Check MISCLASSIFICATION MATRIX is complete
M <- matrix(NA, nrow = length(unique(deaths$recnr)), ncol = length(v_cod_reclass))
for (i in 1:length(unique(deaths$id))) {
  id <- unique(deaths$id)[i]
  M[i, ] <- colSums(deaths[deaths$id == id, paste(v_cod_reclass)])  
}
# All values MUST be 1!!
table(M)
# !!!!!! Hallie note (Nov 27, 2024)
# I previously had an issue with some values not being 1.
# This was because certain CODs were missing in various studies and there was no "Other" in the cause column to map them to.
# I fixed this by adding back Pancho's code in "manage-other" that I had commented out. 

# Reported COD
# May include Other, that is not modeled for 5-19. It is modeled for under-5. (I think)
unique(deaths$cause)
unique(deaths$cause)[which(!unique(deaths$cause) %in% v_cod_reclass)]
# line 2106 in Pancho's code in VAMCM-DataCleaning009

# !!!!! NOTE
# Other and Undetermined are no longer columns
# Other is a value in the cause column. Undt should not be in the cause column.
# Neither should be any causes that were mapped to NA in cod-aggregation (e.g., natdis or colvio)

# Recode sex as single letter
deaths$sexRecode <- as.character(deaths$sex)
deaths$sexRecode[deaths$sexRecode == "Total"] <- "T"
deaths$sexRecode[deaths$sexRecode == "Female"] <- "F"
deaths$sexRecode[deaths$sexRecode == "Male"] <- "M"

# Replace old sex column
deaths$sex <- as.factor(deaths$sexRecode)
deaths$sexRecode <- NULL

# Save output(s) ----------------------------------------------------------

save(deaths, file = paste("./gen/process-new-studies/output/ModInput2023-NewDeaths_", ageSexSuffix,".RData", sep = ""))

