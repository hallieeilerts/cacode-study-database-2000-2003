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
dat <- read.csv(paste0("./gen/combine-studies-adhoc/output/StudiesAdHoc2023.csv", sep = ""))
## Key with model classification for hmm/lmm countries
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("modelclass", dat_filename, ignore.case = TRUE)]
key <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

n_cod_col <- max(as.numeric(gsub("\\D", "", names(dat)[grepl("cod", names(dat))])), na.rm = T)

# Column names
cause_cols <- paste0("cause_of_death", 1:n_cod_col)
cod_n_cols <- paste0("cod_n", 1:n_cod_col)
cod_p_cols <- paste0("cod_p", 1:n_cod_col)
cod_mr_cols <- paste0("cod_mr", 1:n_cod_col)
cod_mro_cols <- paste0("cod_mro", 1:n_cod_col)
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

# Delete unnecessary columns
datLong <- datLong %>%
  select(-c(cause_number, cod_p, cod_mr, cod_mro))%>%
  mutate(cod_n = as.numeric(cod_n))

# Save output -------------------------------------------------------------

write.csv(datLong, paste0("./gen/process-new-studies/temp/studies_long_",ageSexSuffix,".csv",sep =""), row.names = FALSE)
