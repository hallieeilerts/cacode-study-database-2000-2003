################################################################################
#' @description Reshape COD values long
#' @return 
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
require(tidyverse)
#' Inputs
source("./src/set-inputs.R")
## Study CODs with wide columns for cod number, proportion, rate
#dat <- read.csv(paste0("./gen/process-new-studies/temp/studies_convert-dths.csv", sep = ""))
dat <- read.csv(paste0("./gen/process-new-studies/temp/studies_convert-dths_", ageSexSuffix, ".csv", sep = ""))
################################################################################

# Column names
cause_cols <- paste0("cause_of_death", 1:55)
cod_n_cols <- paste0("cod_n", 1:55)
cod_p_cols <- paste0("cod_p", 1:55)
cod_mr_cols <- paste0("cod_mr", 1:55)
cod_mro_cols <- paste0("cod_mro", 1:55)
# Combine all sets of cause and associated columns into one list
all_cols <- c(cause_cols, cod_n_cols, cod_p_cols, cod_mr_cols, cod_mro_cols)

# Reshape to long format
datLong <- dat %>%
  pivot_longer(cols = starts_with("cause_of_death")|starts_with("cod_n")
               |starts_with("cod_p") |starts_with("cod_mr") | starts_with("cod_mro"),
               names_to = c(".value", "cause_number"), 
               names_pattern = "(.*?)(\\d+)") %>%
  filter(!is.na(cause_of_death))  # Remove rows where cause of death is missing


# Changing the cause of death column entries to lower case and arranging them alphabetically
datLong <- datLong %>%
  mutate(cause_of_death = tolower(cause_of_death)) %>%
  arrange(cause_of_death)


# Remove data points that don't have deaths
datLong <- subset(datLong, !is.na(cod_n))

# Save output -------------------------------------------------------------

write.csv(datLong, paste0("./gen/process-new-studies/temp/studies_long_",ageSexSuffix,".csv",sep =""), row.names = FALSE)
