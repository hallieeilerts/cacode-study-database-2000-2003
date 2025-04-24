#################################################################################
# Investigating data points with multiple VA algoritms

# add-multiple-va-id.R
# get list of studies with multiple VA ids
unique(subset(dat, va_mult_ind == 1)$strata_id)

# this indicator is created using totdeaths
# all have the same total deaths in the beginning
dat %>%
  filter(strata_id %in% v_multva) %>%
  arrange(strata_id) %>%
  View()
# differences in total deaths only arise after processing
# starts in cod-aggregation

# 0-1m
v_multva <- c("R202210645-03","R202210645-04","R202222679-01","R202222679-02","R202214959-01","R202214959-02","2022COMSAMOZ01",
              "2022COMSAMOZ02", "2022COMSAMOZ03", "R202222679-05","R202222679-06","R202222679-09","R202222679-10","R202222679-13", 
              "R202222679-14","R2022397-01","R2022397-02",  "2022VASAPAK01","2022VASAPAK02","2022VASAPAK03","2022VASAPAK04")

# 1-59m
v_multva <- c("R202222679-03", "R202222679-04", "2022COMSAMOZ04", "2022COMSAMOZ05", "2022COMSAMOZ06", "R202222679-07", "R202222679-08", 
              "R202222094-01", "R202222094-03", "R202222679-11", "R202222679-12", "R202222679-15", "R202222679-16", "R2022397-03",
              "R2022397-04", "2022VASAPAK05", "2022VASAPAK06", "2022VASAPAK07", "2022VASAPAK08")

# 1. what happens to "2022VASAPAK03" in 0-1m?
# excluded in exclusion-criteria for having more than 25% undetermined

# 2. why do these data points often have different totdeaths?

# - one of the algorithms assigned a cause that we subtract from total deaths and drop (stillbirth)
# - the algorithms assigned different numbers of deaths to "undetermined", which isn't included in total deaths
# - the study only reported a subset of top causes for each algorithm
# - the study reported % of deaths for CODs, and rounding differences make each have a different total


# summing up deaths for mult va points
foo <- subset(datWide, strata_id %in% v_multva)
foo <- foo[order(foo$strata_id),]
apply(subset(foo, strata_id %in% v_multva)[, paste0(v_cod_reclass)], 1, sum, na.rm = T)


# reshape-cod-long.R
# Ran this code at the end to see
# points where the deaths for va_mult don't add up to the total

datLong %>%
  arrange(strata_id) %>%
  group_by(strata_id) %>%
  mutate(check = sum(cod_n)) %>% 
  filter(strata_id %in% v_multva) %>%
  View()
datLong %>%
  filter(strata_id == "R2022397-02") %>%
  group_by(strata_id) %>%
  mutate(check = sum(cod_n)) %>% 
  View

# clean-study-citations.R
# look up article when deaths don't add up to total
subset(dat, ref_id == "22679")$citation

# convert-to-deaths.R
# I would open the article and compare reporting of percentages with what's found in convert-to-deaths.R
# ran this code at the end
# added up the percentages by hand from the data we extracted

dat %>%
  filter(strata_id == "R202222679-03") %>% 
  View()
4.1+3.3+2.7+0.3+16.5+4.3+ 21.9+1.3+20.8+19.1+1.1+4.7
# R202222679-15
2.8+0.6+0.6+2+5.7+3.7+22.3+0.6+16.4+36.3+3+5.9

### 0-1m
## R202214959-01 adds up to less. ref_id 14959
## R202214959-02 adds up to less. ref_id 14959
# I looked at the article and for both of these,
# ercentages of CODs were only provided for a subset of top causes
# For R202214959-01 it added up to 90%
# For R202214959-02 it added up to 97.5%
## R2022397-02 adds up to more. ref_id 397
# I looked at the article and
# the percentages in the article add up to 100.4

### 1-59m
## R202222679-03 adds up to more. ref_id 22679
## R202222679-15 adds up to less. ref_id 22679
# I looked at supplementary material from article (additional file 1b)
# and compared to the numbers we have in convert-to-deaths
# They look close, but consistently different. I wonder if they were extracted from somewhere else.
# In any case, don't want to check the extraction at this point.
# For R202222679-03 the percentages in convert-to-deaths add up to 100.1%
# For R202222679-15 the percentages in convert-to-deaths add up to 99.9%


# exclusion-criteria.R
# ran this code to check when data points got excluded
dat %>%
  filter(strata_id %in% v_multva) %>%
  arrange(strata_id) %>%
  View()
nrow(subset(dat, strata_id %in% v_multva)) # 21 in 0-1m to start
dat %>%
  filter(strata_id %in% "2022VASAPAK03") %>%
  arrange(strata_id) %>%
  View()
