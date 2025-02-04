################################################################################
#' @description Extract necessary columns from study characteristics
#' @return Study info columns for countryname, iso3, location, VA algorithm
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(readxl)
require(tidyverse)
require(countrycode)
#' Inputs
source("./src/set-inputs.R")
## Study and ad-hoc data points
dat <- read.csv(paste0("./gen/combine-studies-adhoc/temp/studies_add-ad-hoc.csv", sep = ""))
################################################################################

# Generate age variables in months
dat <- dat %>%
  mutate(age_lb = as.numeric(age_lb),
         age_ub = as.numeric(age_ub)) %>%
  mutate(age_lb_m = case_when(
            age_lb_unit == "months" ~ age_lb,      # Already in months
            age_lb_unit == "days" ~ age_lb / 30,   # Converting days to months
            age_lb_unit == "years" ~ age_lb * 12), # Converting years to months
        age_ub_m = case_when(
            age_ub_unit == "months" ~ age_ub,       # Already in months
            age_ub_unit == "days" ~ age_ub / 30,    # Converting days to months
            age_ub_unit == "years" ~ age_ub * 12)   # Converting years to months
  ) %>%
  select(-c(age_lb_unit, age_ub_unit, age_lb, age_ub))

# Check that age_lb_m is always less than age_ub_m
if(nrow(subset(dat, age_lb_m > age_ub_m))>0){
  warning("age_lb_m is greater than age_ub_m")
}
# Check no negative ages
if(nrow(subset(dat, age_lb_m < 0))>0){
  warning("negative age_lb_m")
}
if(nrow(subset(dat, age_ub_m < 0))>0){
  warning("negative age_ub_m")
}

# Re-label sexes
dat$sex[dat$sex == 1] <- sexLabels[3]
dat$sex[dat$sex == 2] <- sexLabels[2]
dat$sex[dat$sex == 3] <- sexLabels[1]
dat$sex[dat$sex == "total"] <- sexLabels[1]
dat$sex[dat$sex == "female"] <- sexLabels[2]
dat$sex[dat$sex == "male"] <- sexLabels[3]

# National representative
dat$nationalrep[dat$nationalrep == "Regional"] <- 0
dat$nationalrep[dat$nationalrep == "no"] <- 0
dat$nationalrep[dat$nationalrep == "National"] <- 1
dat$nationalrep[dat$nationalrep == "yes"] <- 1
dat$nationalrep <- as.numeric(dat$nationalrep)

# Harmonize VA algorithms
dat$va_alg[grepl("Physician", dat$va_alg, ignore.case = TRUE)] <- "PCVA"
dat$va_alg[grepl("Other", dat$va_alg, ignore.case = TRUE)] <- "Other"
dat$va_alg[grepl("inter", dat$va_alg, ignore.case = TRUE)] <- "InterVA"
dat$va_alg[grepl("insilico", dat$va_alg, ignore.case = TRUE)] <- "InSilico"
dat$va_alg[grepl("method not reported|cannot", dat$va_alg, ignore.case = TRUE)] <- "Not reported"
dat$va_alg[grepl("va not done|medical records|desath certificates", dat$va_alg, ignore.case = TRUE)] <- "Death certificates or medical records"
dat$va_alg[is.na(dat$va_alg)] <- "Not reported"

# Source of VA algorithm information
dat$va_alg_src <- "study"

# Create reterm
dat$reterm <- paste(dat$iso3, dat$article_id, sep = "-")
dat$reterm[dat$nationalrep == 1] <- dat$iso3[dat$nationalrep == 1]

# Create location factor
dat$location_fac <- as.numeric(relevel(factor(dat$location_short), ref = sort(unique(dat$location_short))[1]))

# Create informative ID
dat$id <- paste(dat$iso3, dat$year_mid, dat$strata_id, dat$location_fac,
                round(dat$age_lb_m), round(dat$age_ub_m), substr(dat$sex,1,1), sep = '-')
dat <- dat[,c("id", names(dat)[!(names(dat) %in% "id")])]
length(unique(dat$id)) == nrow(dat)

# Ensure presence of all idvars
v_missing_id <- idVars[!(idVars %in% names(dat))]
# Should only contain "recnr" and indicators for study data points with multiple VA applied
if(length(v_missing_id) > 0){
  for(i in 1:length(v_missing_id)){
    dat$newcol <- NA
    names(dat)[which(names(dat) == "newcol")] <- v_missing_id[i]
  }
}

# Tidy
v_other_col <- names(dat)[!(names(dat) %in% idVars)]
dat <- dat[,c(idVars, v_other_col)]
dat <- dat[order(dat$id),]

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/combine-studies-adhoc/output/StudiesAdHoc2023.csv", sep =""), row.names = FALSE)


