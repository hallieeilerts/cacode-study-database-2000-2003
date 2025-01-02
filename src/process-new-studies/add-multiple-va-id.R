################################################################################
#' @description Detect duplicates and drop, add indicator for multiple VA algorithms for same study
#' @return No duplicates in study data
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Study data with cleaned columns
dat <- read.csv(paste0("./gen/process-new-studies/temp/studies_dup-dropped_", ageSexSuffix, ".csv", sep = ""))
################################################################################

# Reshape cod_mapped and cod_n wide
v_id_col <- names(dat)[!(names(dat) %in% c("cod_mapped", "cod_n"))]
datWide <- dat %>%
  pivot_wider(
    id_cols = all_of(v_id_col),
    names_from = cod_mapped,
    values_from = cod_n,
    values_fn= function(x) sum(x, na.rm = TRUE)
  )

# Create variables for having multiple VA algorithms from same study
df_va_mult <- datWide %>%
  mutate(va_mult_id = paste(article_id, iso3, year_start, year_end, substr(sex,1,1), totdeaths, sep = "-")) %>%
  group_by(va_mult_id) %>%
  mutate(N = n(),
         va_alg_N = n_distinct(va_alg)) %>%
  mutate(va_mult_ind = ifelse(N > 1 & va_alg_N > 1, 1, 0)) %>%
  filter(va_mult_ind == 1) %>%
  mutate(va_mult_id = ifelse(va_mult_ind == 0, NA, va_mult_id)) %>%
  select(strata_id, va_mult_ind, va_mult_id, va_alg) %>%
  mutate(va_mult_n = 1:n())

# Merge on information on multiple VA algorithms for same study
dat <- dat %>%
  select(-c(va_mult_ind, va_mult_id, va_mult_n)) %>%
  full_join(df_va_mult, join_by(strata_id, va_alg))

# Tidy
v_other <- names(dat)[!(names(dat) %in% idVars)]
dat <- dat[,c(idVars, v_other)]
dat <- dat[order(dat$id),]

# Save output -------------------------------------------------------------

write.csv(dat, paste0("./gen/process-new-studies/temp/studies_va-mult-id_",ageSexSuffix,".csv",sep =""), row.names = FALSE)
