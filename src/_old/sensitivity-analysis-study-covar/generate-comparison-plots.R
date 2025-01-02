
################################################################
# Compare old study database to new database with udpated values
################################################################


# Initialize environment --------------------------------------------------

rm(list = ls())
source("./src/util.R")

# Load data ---------------------------------------------------------------

resDate <- "20240614"

# 2020 study database
old <- readstata13::read.dta13("./data/previous-database/20200930-HMM-StudyDatabase.dta")

# Study database updated with main values from 2021 prediction database
new <- read.csv(paste("./gen/update-old-studydb-covar/output/20200930-HMM-StudyDatabase_updatedPred2021Main_",resDate,".csv",sep=""))

# Reshape to long ---------------------------------------------------------

# Vector of main covariate values
v_maincovar <- old %>% select(ends_with("_source")) %>% 
  rename_with(function(c) str_replace(c, "_source", "")) %>%
  names()

# Reshape old study db long
oldlong <- old %>% select(all_of(c(v_study_id_col, v_maincovar))) %>%
  pivot_longer(cols = !all_of(v_study_id_col),names_to = c("variable"), values_to = "value") %>%
  mutate(series = "old")

# Reshape new study db long
newlong <- new %>% select(all_of(c(v_study_id_col, v_maincovar))) %>%
  pivot_longer(cols = !all_of(v_study_id_col), names_to = c("variable"), values_to = "value") %>%
  mutate(series = "new")

# Combine
data <- rbind(oldlong, newlong)

# Plot --------------------------------------------------------------------

plot <- data %>%
  mutate(tempId = paste(iso3, Refid, study_id, year, sep = "-")) %>%
  arrange(tempId)

# Create page number for plots
df_tempIdpage <- data.frame(tempId = unique(plot$tempId),
                            n_tempId = 1:length(unique(plot$tempId)))
df_tempIdpage <- df_tempIdpage %>%
  mutate(page = ceiling(n_tempId/110)) %>%
  select(tempId, page)
df_plot <- full_join(plot, df_tempIdpage, by = c("tempId"))

v_var <- unique(df_plot$variable)

for(i in 1:length(v_var)){
  plots <- plyr::dlply(df_plot %>% filter(variable == v_var[i]), ~page,
                       function(x)
                         ggplot(x) +
                         ylab("") + xlab("temp study id") +
                         ggtitle(str_glue("{v_var[i]}")) +
                         geom_point(aes(x = tempId, y = value, col = series, shape = series)) +
                         scale_shape_manual(values = c(1, 2)) +
                         coord_flip() +
                         theme(axis.text.y = element_text(size = 6))
  )
  mg <- marrangeGrob(grobs = plots, nrow=1, ncol=1, top = NULL)
  ggsave(str_glue("./gen/update-old-studydb-covar/audit/compare-studydb/{v_var[i]}.pdf"), mg, height = 10, width = 8, units = "in") 
}


