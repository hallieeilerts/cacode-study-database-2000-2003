################################################################################
#' @description Identify studies that distinguish between preterm and lbw for neonate age group
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Study CODs in long format
if(ageSexSuffix == "00to28d"){
  dat <- read.csv(paste0("./gen/process-new-studies/temp/studies_long_", ageSexSuffix, ".csv", sep = ""))
}
################################################################################

dat$preterm <- grepl("preterm|prematurity|short gestation", dat$cause_of_death, ignore.case = TRUE)
dat$lbw <- grepl("lbw|low birth weight|low birthweight", dat$cause_of_death, ignore.case = TRUE)

dat <- dat %>%
  group_by(strata_id) %>%
  mutate(has_preterm = sum(preterm),
         has_lbw = sum(lbw)) %>%
  mutate(premvslbw = ifelse(has_preterm > 0 & has_lbw > 0 & preterm != lbw, 1, 0 ))

#table(dat$premvslbw)
#v_strata <- unique(subset(dat, premvslbw == 1)$strata_id)
#dat <- dat[order(dat$strata_id),]
#View(subset(dat, strata_id %in% v_strata)[,c("strata_id", "cause_of_death", "preterm", "lbw", "has_preterm", "has_lbw", "premvslbw")])

dat_premvslbw <- dat %>%
  group_by(strata_id) %>%
  summarise(premvslbw = max(premvslbw)) %>%
  select(strata_id, premvslbw)

# Save output -------------------------------------------------------------

write.csv(dat_premvslbw, paste0("./gen/process-new-studies/temp/dat_premvslbw_",ageSexSuffix,".csv",sep =""), row.names = FALSE)

