library(stringr)

# Deletes and recreates the folder
fn_initEnvironmentData <- function(subfolder_name) {
  # ⚠ - Delete the entire data sub-directory to make sure we don't have old data
  if (unlink(str_glue("./data/{subfolder_name}"), recursive = TRUE) == 1) {
    stop(str_glue("Failed to delete data. Make sure you don't have any files open from that directory."))
  }
  
  # Create data fodler
  dir.create(str_glue("./data/{subfolder_name}"))
}
