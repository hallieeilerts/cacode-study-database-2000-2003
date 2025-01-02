
################################################################
# Format new study database to exactly match old
################################################################


# Initialize environment --------------------------------------------------

rm(list = ls())
source("./src/util.R")

# Load data ---------------------------------------------------------------

# 2020 study database
old <- readstata13::read.dta13("./data/previous-database/20200930-HMM-StudyDatabase.dta")

# Study database updated with main values from 2021 prediction database
main <- read.csv("./gen/update-old-studydb-covar/output/study-updated-main.csv")

# Study database updated with main values from prediction: add extra columns from old study database ----------

# Identify columns from original study database that need to be added
# These will be CODs, source columns, and covariates that weren't updated.
v_other_col <- names(old)[!(names(old) %in% names(main))]
# First 33 columns are CODs or mortality rates. Put these in their own vector
v_cod_col <- v_other_col[1:33]
v_other_col <- v_other_col[!(v_other_col %in% v_cod_col)]
# Only keep id columns and those that need to be added
old_extracol <- old[,c(v_study_id_col, v_cod_col, v_other_col)]
# Convert new_R column to integer for merge
old_extracol$new_R <- as.numeric(old_extracol$new_R)

# Merge so new study database has all the columns as old, plus extra columns which indicate whether covariate was updated
main_format <- main %>%
  full_join(., old_extracol, by = v_study_id_col)

# Put id and cod columns first (updated columns still included in case this version of object is wanted later)
v_col_order <- sort(names(main_format))
v_col_order <- v_col_order[!(v_col_order %in% c(v_study_id_col, v_cod_col))]
main_format <- main_format[,c(v_study_id_col, v_cod_col, v_col_order)]

# Create codebook
v_col_selection <- main_format %>% select(ends_with("_source"), ends_with("_updated")) %>% names()
v_source <- v_col_selection [grepl("source", v_col_selection, ignore.case = TRUE)]
v_source_var <- str_replace(v_source, "_source", "")
v_updated <- v_col_selection [grepl("updated", v_col_selection, ignore.case = TRUE)]
v_updated_var <- str_replace(v_updated, "_updated", "")
codebook <- data.frame(variable = v_source_var) %>%
  full_join(., data.frame(variable = v_updated_var), by = "variable", keep = TRUE, suffix = c("","_updated")) %>%
  rename(updated = variable_updated) %>%
  mutate(updated = ifelse(!is.na(updated), TRUE, FALSE))

# Remove "updated" columns and put columns in same order as old study database
main_format1 <- main_format[names(old)]

# Put rows in same order as old study database
df_roworder <- old %>%
               mutate(tempId = paste(iso3, Refid, study_id, year, sep = "-"),
                      rank = 1:n()) %>%
              select(tempId, rank)  
main_format2 <- main_format1 %>%
                mutate(tempId = paste(iso3, Refid, study_id, year, sep = "-")) %>%
                left_join(., df_roworder, by = "tempId") %>%
                arrange(rank) %>%
                select(-c(tempId, rank))

# Save outputs ------------------------------------------------------------

write.csv(main_format2, paste("./gen/update-old-studydb-covar/output/20200930-HMM-StudyDatabase_updatedPred2021Main_",format(Sys.Date(), format="%Y%m%d"),".csv",sep = ""), row.names = FALSE)
write.csv(codebook, paste("./gen/update-old-studydb-covar/output/codebook_",format(Sys.Date(), format="%Y%m%d"),".csv",sep = ""), row.names = FALSE)
