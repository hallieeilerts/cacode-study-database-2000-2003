################################################################################
#' @description Visualize COD distribution
#' @return
################################################################################
#' Clear environment
rm(list = ls())
#' Libraries
library(tidyverse)
library(ggplot2)
#' Inputs
source("./src/set-inputs.R")
## Study db
dat_filename <- list.files("./gen/create-studydb/output")
dat_filename <- dat_filename[grepl("studydatabase2023", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[!grepl("codebook", dat_filename, ignore.case = TRUE)] 
dat_filename <- dat_filename[!(grepl("noMal", dat_filename, ignore.case = TRUE))] 
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
dat <- read.csv(paste0("./gen/create-studydb/output/", dat_filename, sep = ""))
## Key with cod reclassification
dat_filename <- list.files("./data/classification-keys")
dat_filename <- dat_filename[grepl("codreclassification", dat_filename, ignore.case = TRUE)]
dat_filename <- dat_filename[grepl(ageSexSuffix, dat_filename)] 
dat_filename <- tail(sort(dat_filename),1) # Most recent
key_cod <- read.csv(paste0("./data/classification-keys/", dat_filename, sep = ""))
################################################################################

# Reclassified CODs for this age group (includes Other and Undetermined)
v_cod_reclass <- unique(subset(key_cod, !is.na(cod_reclass))$cod_reclass)
# Exclude "TB" which has been redistributed (only present in 5-9y and 10-14y reclass vector)
v_cod_reclass <- v_cod_reclass[!(v_cod_reclass %in% "TB")]
# Exclude "Undetermined" for plots
v_cod_noundt <- v_cod_reclass[!(v_cod_reclass %in% "Undetermined")]

# plot distribution of causes of death
if(ageSexSuffix %in% c("00to28d","01to59m")){
  p1 <- dat %>%
    select(all_of(c("round", "strata_id", v_cod_reclass))) %>%
    pivot_longer(
      cols = v_cod_reclass,
      names_to = "cod"
    ) %>%
    group_by(round, strata_id) %>%
    mutate(total = sum(value, na.rm = T)) %>%
    mutate(per = value/total) %>%
    mutate(codabbrev = ifelse(cod == "Malnutrition", substr(cod, 1, 4), substr(cod, 1, 3))) %>%
    filter(!is.na(per)) %>%
    ggplot(aes(x= strata_id, y = per, fill = cod)) +
    geom_bar(colour = "black", stat = "identity") +
    geom_text(aes(label = codabbrev),size = 2, position = position_stack(vjust = 0.5)) +
    coord_flip()
  ggsave(paste0("./gen/create-studydb/audit/cod-dist_",ageSexSuffix, "_",format(Sys.Date(), format="%Y%m%d"),".png"), p1, 
         height = 100, limitsize = F)
}else{
  p1 <- dat %>%
    select(all_of(c("round", "strata_id", v_cod_reclass))) %>%
    pivot_longer(
      cols = v_cod_reclass,
      names_to = "cod"
    ) %>%
    group_by(round, strata_id) %>%
    mutate(total = sum(value, na.rm = T)) %>%
    mutate(per = value/total) %>%
    mutate(codabbrev = ifelse(cod == "Malnutrition", substr(cod, 1, 4), substr(cod, 1, 3))) %>%
    filter(!is.na(per)) %>%
    ggplot(aes(x= strata_id, y = per, fill = cod)) +
    geom_bar(colour = "black", stat = "identity") +
    geom_text(aes(label = codabbrev),size = 2, position = position_stack(vjust = 0.5)) +
    coord_flip()
  ggsave(paste0("./gen/create-studydb/audit/cod-dist_",ageSexSuffix, "_",format(Sys.Date(), format="%Y%m%d"),".png"), p1, 
         height = 50, width = 7, limitsize = F)
}


# condensed plot
p2 <- dat %>%
  select(all_of(c("round", "strata_id", v_cod_reclass))) %>%
  pivot_longer(
    cols = v_cod_reclass,
    names_to = "cod"
  ) %>%
  group_by(round, strata_id) %>%
  mutate(total = sum(value, na.rm = T)) %>%
  mutate(per = value/total) %>%
  filter(!is.na(per)) %>%
  ggplot(aes(x= strata_id, y = per, fill = cod)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())
ggsave(paste0("./gen/create-studydb/audit/cod-dist-small_",ageSexSuffix, "_",format(Sys.Date(), format="%Y%m%d"),".png"), p2, height = 8)

