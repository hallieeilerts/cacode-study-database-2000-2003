################################################################
# Harmonize and group COD 
################################################################

#' Input: Causes of death data with different types of causes
#' Output: Group causes according to ICD-10

# Initialize environment --------------------------------------------------

rm(list = ls())

# installing the packages

#install.packages(c("readr", "openxlsx", "dplyr", "readstata13", "gtsummary", "tidyr", "gt", "flextable", "stringr"))
install.packages("rlang")


#Loading the libraries

library(readr)
library(openxlsx)
library(dplyr)
library(readstata13)
library(gtsummary)
library(tidyr)
library(gt)
library(flextable)
library(stringr)
library(rlang)

# load the data

causes <- read.csv("./gen/process-distiller/output/cod qc.csv")

# Reordering the columns
causes <- causes %>%
  rename(strata_id = deaths_by_cause_k)%>%
  select(-c (age_unit, age_lb, age_ub, strata_ur, den_type, den_n, den_p, den_acmo, den_acmo_unit, den_acme, den_acme_unit, diff_denom, cod_mr_unit,
             cod_comments, acmo_n, lb))

# Excluding the facility studies and studies without denominators
#causes.exc <- causes %>%
 # filter (remove==0)%>%
  #select (-c (Facility, remove))

# Reshape the cod.deaths dataset into a long format
cause_cols <- paste0("cause_of_death", 1:55)
cod_n_cols <- paste0("cod_n", 1:55)
cod_p_cols <- paste0("cod_p", 1:55)
cod_mr_cols <- paste0("cod_mr", 1:55)
cod_mro_cols <- paste0("cod_mro", 1:55)

# Combine all sets of cause and associated columns into one list
all_cols <- c(cause_cols, cod_n_cols, cod_p_cols, cod_mr_cols, cod_mro_cols)


# Reshape to long format
cod.long <- causes %>%
  pivot_longer(cols = starts_with("cause_of_death")|starts_with("cod_n")
                      |starts_with("cod_p") |starts_with("cod_mr") | starts_with("cod_mro"),
              names_to = c(".value", "cause_number"), 
              names_pattern = "(.*?)(\\d+)") %>%
  filter(!is.na(cause_of_death))  # Remove rows where cause of death is missing



# Changing the cause of death column entries to lower case and arranging them alphabetically
cod.long <- cod.long %>%
  mutate(cause_of_death = tolower(cause_of_death)) %>%
  arrange(cause_of_death)

# Store the long data format to audit folder for quality check purposes
write.csv(cod.long, "./gen/process-distiller/audit/cod long_before exclude.csv", row.names = FALSE)



--------------------------------------------
  
  
# Import the mapped causes file
causes.map <- read.csv("./gen/process-distiller/audit/full_causemapping20240926.csv", na.strings = "", check.names = F)

# Creating cod data only for neonates

## selecting only neonates data from causes mapped file and cod.long file
causes.map.nn <- causes.map %>%
  select(cause_of_death, Neonates)
  

cod.nn <- cod.long %>%
  filter(inc_neo==1)

## Merge causes mapped file to cod.nn file

cod.nn <- cod.nn %>%
  left_join(causes.map.nn, by = "cause_of_death")

#Remove the columns not reqd.
cod.nn <- cod.nn %>%
  select (-c(inc_neo, inc_1to59, inc_5to9y, inc_10to14y, inc_15to19y.m, inc_15to19y.f, cause_number, cause_of_death, cod_p, cod_mr, cod_mro))

## Turn the data into a wide format

cod.nn.wide <- cod.nn %>%
  pivot_wider(
    names_from = Neonates, 
    values_from = cod_n,
    values_fn = sum,
    names_sep = "_",  # Ensures that names are unique
    names_repair = "unique"  # Ensures unique column names
  )


# Preparing the cod.nn.wide data
## Adding a column for each cause to see if that cause has been reported or not

cod.nn.wide <- cod.nn.wide %>%
  mutate(sep = ifelse(is.na(sepsis_mening) | sepsis_mening==0, 0,1))%>%
  mutate(lr = ifelse(is.na(lri) | lri==0, 0,1))%>%
  mutate(hi = ifelse(is.na(hiv) | hiv==0, 0,1))%>%
  mutate(di = ifelse(is.na(dia) | dia==0, 0,1))%>%
  mutate(ma = ifelse(is.na(mal) | mal==0, 0,1))%>%
  mutate(ot = ifelse(is.na(`other neonatal`) | `other neonatal`==0, 0,1))%>%
  mutate(pr = ifelse(is.na(preterm) | preterm==0, 0,1))%>%
  mutate(ip = ifelse(is.na(intrapartum) | intrapartum==0, 0,1))%>%
  mutate(co = ifelse(is.na(congen) | congen==0, 0,1))%>%
  mutate(inj = ifelse(is.na(injuries) | injuries==0, 0,1))%>%
  mutate(tet = ifelse(is.na(tetanus) | tetanus==0, 0,1))%>%
  mutate(sum_cau = rowSums(across(sep:tet), na.rm = TRUE))%>%
  mutate(remove = ifelse (sum_cau < 2, 1, remove))%>%
  filter(remove==0)

# Now excluding the studies 

cod.nn.wide



# Download the neonatal wide data to qc the number for sum of causes
write.csv(cod.nn.wide, "./gen/process-distiller/audit/nn.wide.qc.csv", na = "", row.names = FALSE)

# Creating cod data only for 1-59mo

## selecting only 1-59m data from causes mapped file and cod.long file
causes.map.ch <- causes.map %>%
  select(cause_of_death, "1-59m")


cod.ch <- cod.long %>%
  filter(inc_1to59==1)

## Merge causes mapped file to cod.nn file

cod.ch <- cod.ch %>%
  left_join(causes.map.ch, by = "cause_of_death")

#Remove the columns not reqd.
cod.ch <- cod.ch %>%
  select (-c(inc_neo, inc_1to59, inc_5to9y, inc_10to14y, inc_15to19y.m, inc_15to19y.f, cause_number, cause_of_death, cod_p, cod_mr, cod_mro))

## Turn the data into a wide format

cod.ch.wide <- cod.ch %>%
  pivot_wider(
    names_from = "1-59m", 
    values_from = cod_n,
    values_fn = sum,
    names_sep = "_",  # Ensures that names are unique
    names_repair = "unique"  # Ensures unique column names
  )


# Download the 1-59m wide data to qc the number for sum of causes
write.csv(cod.ch.wide, "./gen/process-distiller/audit/ch.wide.qc.csv", na = "", row.names = FALSE)


# Creating cod data only for 5-9y

## selecting only 5-9y data from causes mapped file and cod.long file
causes.map.5to9 <- causes.map %>%
  select(cause_of_death, "5-9y")


cod.5to9 <- cod.long %>%
  filter(inc_5to9y==1)

## Merge causes mapped file to cod.nn file
cod.5to9 <- cod.5to9 %>%
  left_join(causes.map.5to9, by = "cause_of_death")

#Remove the columns not reqd.
cod.5to9 <- cod.5to9 %>%
  select (-c(inc_neo, inc_1to59, inc_5to9y, inc_10to14y, inc_15to19y, cause_number, cause_of_death, cod_p, cod_mr, cod_mro))

## Turn the data into a wide format

cod.5to9.wide <- cod.5to9 %>%
  pivot_wider(
    names_from = "5-9y", 
    values_from = cod_n,
    values_fn = sum,
    names_sep = "_",  # Ensures that names are unique
    names_repair = "unique"  # Ensures unique column names
  )


# Creating cod data only for 10-14y

## selecting only 10-14y data from causes mapped file and cod.long file
causes.map.10to14 <- causes.map %>%
  select(cause_of_death, "10-14y")


cod.10to14 <- cod.long %>%
  filter(inc_10to14y==1)

## Merge causes mapped file to cod.nn file
cod.10to14 <- cod.10to14 %>%
  left_join(causes.map.10to14, by = "cause_of_death")

#Remove the columns not reqd.
cod.10to14 <- cod.10to14 %>%
  select (-c(inc_neo, inc_1to59, inc_5to9y, inc_10to14y, inc_15to19y, cause_number, cause_of_death, cod_p, cod_mr, cod_mro))

## Turn the data into a wide format

cod.10to14.wide <- cod.10to14 %>%
  pivot_wider(
    names_from = "10-14y", 
    values_from = cod_n,
    values_fn = sum,
    names_sep = "_",  # Ensures that names are unique
    names_repair = "unique"  # Ensures unique column names
  )



# Creating cod data only for 15-19m

## selecting only 15-19m data from causes mapped file and cod.long file
causes.map.15to19m <- causes.map %>%
  select(cause_of_death, "15-19.m")


cod.15to19m <- cod.long %>%
  filter(inc_15to19y==1 & strata_gender != 2)

## Merge causes mapped file to cod.nn file
cod.15to19m <- cod.15to19m %>%
  left_join(causes.map.15to19m, by = "cause_of_death")

#Remove the columns not reqd.
cod.15to19m <- cod.15to19m %>%
  select (-c(inc_neo, inc_1to59, inc_5to9y, inc_10to14y, inc_15to19y, cause_number, cause_of_death, cod_p, cod_mr, cod_mro))

## Turn the data into a wide format

cod.15to19m.wide <- cod.15to19m %>%
  pivot_wider(
    names_from = "15-19.m", 
    values_from = cod_n,
    values_fn = sum,
    names_sep = "_",  # Ensures that names are unique
    names_repair = "unique"  # Ensures unique column names
  )


# Creating cod data only for 15-19f

## selecting only 15-19f data from causes mapped file and cod.long file
causes.map.15to19f <- causes.map %>%
  select(cause_of_death, "15-19.f")


cod.15to19f <- cod.long %>%
  filter(inc_15to19y==1 & strata_gender != 1)

## Merge causes mapped file to cod.nn file
cod.15to19f <- cod.15to19f %>%
  left_join(causes.map.15to19f, by = "cause_of_death")

#Remove the columns not reqd.
cod.15to19f <- cod.15to19f %>%
  select (-c(inc_neo, inc_1to59, inc_5to9y, inc_10to14y, inc_15to19y, cause_number, cause_of_death, cod_p, cod_mr, cod_mro))

## Turn the data into a wide format

cod.15to19f.wide <- cod.15to19f %>%
  pivot_wider(
    names_from = "15-19.f", 
    values_from = cod_n,
    values_fn = sum,
    names_sep = "_",  # Ensures that names are unique
    names_repair = "unique"  # Ensures unique column names
  )














# Creating a column of cod from cause of death column

cod.long <- cod.long %>%
  mutate(cod = cause_of_death)  # Set cod as the cause_of_death column initially

# Categorize diarrhea-related causes and leave others unchanged: category 1
cod.long <- cod.long %>%
  mutate(cod = case_when(
    cause_of_death %in% c("diarrhea", "diarrhoea", "gastroenteritis", 
                          "diarrheal diseases", "diarrhoeal diseases", 
                          "intestinal infection disease", "cholera", 
                          "complex diarrhoea", "complex diarrhea", 
                          "complex dysentery", "diarrhoea and gastroenteritis of presumed infectious origin (A09)", 
                          "persistent diarrhea", "diarrhea (nn)", "diarrhea (pn)", 
                          "01.04 diarrhoeal diseases", "acute diarrhea",
                          "acute diarrheal disease", "acute non-watery diarrhoea", 
                          "acute watery diarrhoea", "chronic diarrhea", 
                          "diarrheal and other intestinal diseases", "dysentery", 
                          "pigbel/dysentery", "diarrhoea and gastroenteritis of presumed infectious origin (a09)", 
                          "persistent diarrhoea with severe malnutrition", 
                          "acute abdominal conditions: gastroenteritis, dysentery, pigbel",
                          "diarrhea with blood", "diarrhea/dysentery", "diarrheal disease", 
                          "diarrhoea/gastro-enteritis", "diarrhoeal disease", "gl infection and diarrhea",
                          "intestinal infections diseases", "intestinal infectious diseases (incl. diarrhoeal diseases)") ~ "1",
    TRUE ~ cod # Keep existing values for other causes
  ))


# Categorizing for Injuries V01-Y89: catg 2
cod.long <- cod.long %>%
  mutate(cod = case_when(
    cause_of_death %in% c("injury", "unintentional accidents", "injuries", 
                          "injuries external", "drowning", "domestic or traffic accident",
                          "homicide", "rta", "suicide", "traffic accidents", "fall", 
                          "traffic accident", "accident", "assault", "exposure to fire", 
                          "other injuries", "transport accidents", "other natural", 
                          "accidental drowning and submersion", "accidents", 
                          "intentional self-harm", "external", 
                          "accidental poisoning and noxious substances", 
                          "contact with venomous plants/animals", 
                          "other and unspecified external cod", 
                          "other and unspecified external cause of death", 
                          "road traffic accident", "accidental burns and smoke inhalation", 
                          "accidental fall", "accidental poisoning and noxious substances", 
                          "contact with venomous plants/animals", "contact with venomous animals and plants",
                          "contact with venomous of poison animal and plants", "poisoning", 
                          "other acident", "accident/injury", "asphyxia", "injury/fall (pn)", 
                          "12.01 toad traffic accident", "12.03 accid fall", 
                          "12.04 accid drowning and submersion", 
                          "12.06 contact with venomous plant/animal", 
                          "12.07 accid poisoning & noxious subs", 
                          "12.99 other and unspecified external CoD", 
                          "accid drowning and submersion", "accident/trauma", 
                          "extended burns", "external causes", "other accidents", 
                          "snake bite", "road traffic accidents", "trauma", 
                          "murder", "child abuse and neglect", "accidental exposure to smoke, fire and",
                          "accidental poisoning and exposure to no", 
                          "contact with venomous animals and plant", "other and unspecified external cause", "12.01 road traffic accident",
                          "12.02 other transport accident", "12.04 acid drowning and submersion",
                          "12.05 accid expos to smoke fire & flame", "12.05 accid expos. to smoke fire & flame", "12.05 acid expos to smoke fire & flame", "12.06 contact with venomous animal", 
                          "12.07 accid poisoning & noxious subs.", "12.08 intentional self-harm", "12.09 assault",
                          "accident or injuries", "accident-domestic",
                          "accident-rta", "accident-work-related", "accidental asphyxia", "accidental drowning", "accidental drowning & submersion",
                          "accidental exposure to fire, smoke", "accidental exposure to smoke fire and flame", "accidental exposure to smoke fire and flames", "accidental exposure to smoke, fire, and flames",
                          "accidental exposure to smoke ï¬re and ï¬‚ame", "accidental falls", "accidental injury", "accidental poisoning", "accidental poisoning & exposure to noxious substance",
                          "accidental poisoning and exposure to noxious substance","accidental trauma", "accidents and injuries", "acds",
                          "acid drowning and submersion", "acid expos to smoke fire & flame", 
                          "falling down", "falls", "fire disaster", "fire, heat, and hot substances",
                          "firearm", "fires","gunshot", "hanging", "homecide-communal clash",
                          "homecide-cult/gan g killing", "homecide-domestic", "homecide-fight", "homecide-robbery",
                          "injuries or accidents", "injury, poisoning and certain other consequences of external causes",
                          "intentional injury", "intentional self harm", "interpersonal violence","natural accident", "poisonings", "road injuries", "road traffic",
                          "road traffic injury", "self-harm", "transport", "transport accident", "transport injuries",
                          "transport related accidents", "trauma (injury, poisoning, burning, violence, etch)", "traï¬ƒc accident","unintentional injuries", "work accident",
                          "electrical injury", "external cause of death (injuries and accidents)",
                          "injury, poisoning, and certain other consequences of external") ~ "2",  # Set cod = "2" for injury-related causes
    cod == "1" ~ "1",  # Keep previously assigned cod = "1" for diarrhea-related causes (if already categorized)
    TRUE ~ cod  # Keep the original cod value for other causes
  ))

# Categorizing Malaria - catg 3
cod.long <- cod.long %>%
  mutate(cod = case_when(
    cause_of_death %in% c("malaria", "01.05 malaria", "definite malaria", "probable malaria",
                          "severe malaria", "cerebral malaria" ) ~ "3",  # Set cod = "3" for malaria-related causes
    cod == "1" ~ "1",  # Keep previously assigned cod = "1" for diarrhea-related causes
    cod == "2" ~ "2",  # Keep previously assigned cod = "2" for injury-related causes
    TRUE ~ cod  # Keep the original cod value for other causes
  ))


# Categorizing Measles - catg 4
cod.long <- cod.long %>%
  mutate(cod = case_when(
    cause_of_death %in% c("measles","01.06 measles", "childhood cluster diseases (measles)â‰¤5")~"4",
    cod == "1" ~ "1",
    cod == "2" ~ "2",
    cod == "3" ~ "3",
    TRUE ~ cod))

# Categorizing Meningitis - catg 5 
cod.long <- cod.long %>%
  mutate(cod = case_when(
    cause_of_death %in% c("meningitis", "meningitis and encephalitis", 
                          "meningitis (G03)", "meningitis/encephalitis", 
                          "01.07 meningitis and encephalitis","meningitis (g03)",
                          "meningitis or encephalitis", "meningitis, unspecified", "meningitis/encephalitis",
                          "meningitis/sepsis", "sepsis/meningitis", "community-acquired meningitis",
                          "encephalitis", "encephalopathy") ~ "5",  # Set cod = "5" for meningitis-related causes
    cod == "1" ~ "1",  
    cod == "2" ~ "2",  
    cod == "3" ~ "3",
    cod == "4" ~ "4",
    TRUE ~ cod  # Keep the original cod value for other causes
  ))

# Neonatal causes - catg 6  
cod.long <- cod.long %>%
  mutate(cod = case_when(
    cause_of_death %in% c("birth asphyxia", "10.02 birth asphyxia", "birth asphyxia and birth trauma", "birth asphyxia and trauma",
                          "birth injury and or asphyxia", "birth trauma", "birth injuries", "birth injury",
                          "birth asphyxia and perinatal respiratory infections", "complications of intrapartum events (birth asphyxia)",
                          "complications of intrapartum events", "conditions in the perinatal period", "conditions in the perinatal period (p00â€“p96)",
                          "conditions of the perinatal period", "conditions originating in the perinatal period", "conditions in the perinatal period(p00â€“p96)",
                          "ipre (mainly birth asphyxia)", "neonatal Ã¢â‚¬â€œ asphyxia", 
                          "prematurity", "birth asyphyxia", "immaturity-related conditions", "immaturity",
                          "10.01 prematurity",
                          "complications of prematurity", "extreme prematurity", "low birth weight and prematurity",
                          "premature", "preterm", "preterm/low birth weight", "low birth weight/prematurity complications",
                          "preterm birth and low birth weight", "prematurity/lbw", "n9: low birthweight and prematurity",
                          "neonatal causes (birth asphyxia, prematurity, and neonatal sepsis", "neonatal Ã¢â‚¬â€œ prematurity",
                          "premature", "prematurity and low birth weight", "prematurity and or low birth weight", 
                          "prematurity low birth weight", "prematurity/lbw", "prematurity/low birth weight", 
                          "preterm birth/lbw", "preterm birth/low birth weight", 
                          "low birth weight and premature", "necrotising enterocolitis", 
                          "pad, premature associated disease including encephalopathy of prematurity, retinopathy of prematurity, necrotizing enterocolitis and bronchopulmonary dysplasia",
                          "birth complications", "other immaturity-related conditions", 
                          "fetus and newborn affected by maternal factors and by complications of pregnancy, labour and delivery (P01, P02, P03)", 
                          "intrauterine hypoxia and birth asphyxia (P20, P21)", 
                          "respiratory distress of newborn (P22)", 
                          "birth asphyxia and birth trauma", "low birth weight/pre-term", 
                          "birth Asphyxia (nn)", "lbw", "lbw & prematurity (nn)", 
                          "lbw/Prematurity", "low birth weight", "lbw/prematurity", 
                          "fetus and newborn affected by maternal factors and by complications of pregnancy, labour and delivery (p01, p02, p03)", 
                          "chilbirth", "intrauterine hypoxia and birth asphyxia (p20, p21)", 
                          "fetal suffering", "respiratory distress of newborn (p22)", 
                          "certain conditions originating in the perinatal period", 
                          "perinatal diseases", "perinatal conditions", "perinatal", "all other unidentified perinatal causes",
                          "intracranial hemorrhage of the newborn", "neonatal pneumonia", "10.03 neonatal pneumonia",
                          "lower respiratory tract infections < 1 month", "neonatal", 
                          "other neonatal", "other neonatal diseases", "neonatal tetanus", 
                          "haemorrhagic and haematological disorders of fetus and newborn/haemorrhagic disorder, kern icterus, jaundice", 
                          "severe infection (nn)", "bacterial sepsis of newborn", 
                          "anemia of pregnancy", "obstetric hemorrhage", "09.04 obstetric haemorrhage",
                          "obstructed labor", 
                          "other and unspecified maternal cause of death", "09.99 unspecified maternal cod",
                          "09.07 anaemia of pregnancy",
                          "other and unspecified neonatal cause of death", 
                          "pregnancy-induced hypertension", "09.03 pregnancy-induced hypertension",
                          "pregnancy-related sepsis", "09.06 pregnancy-related sepsis", "09.07 anaemia of pregnancy",
                          "other perinatal conditions", "neonatal infection", "deaths from neonatal diseases",
                          "early neonatal infection", "early neonatal infections", "hemolytic disorders of the newborn",
                          "neonatal sepsis", "10.04 neonatal sepsis", "severe infection (nn)", "others (nn)", 
                          "other and unspecified neonatal cod", "10.99 other and unspecified neonatal death", "10.99 unspecified neonatal cod",
                          "haemorrhagic and haematological disorders of fetus and newborn/haemorrhagic disorder, kern icterus, jaundice", 
                          "childbirth", "certain perinatal causes", 
                          "neonatal/congenital causes", "intrapartum", "intrapartum complications",
                          "necrotizing enterocolitis") ~ "6",  # Set cod = "6" for neonatal-related causes
    cod == "1" ~ "1",  # Keep previously assigned cod = "1" for diarrhea-related causes
    cod == "2" ~ "2",  # Keep previously assigned cod = "2" for injury-related causes
    cod == "3" ~ "3",  # Keep previously assigned cod = "3" for malaria-related causes
    cod == "4" ~ "4", # measles
    cod == "5" ~ "5",  # Keep previously assigned cod = "5" for meningitis-related causes
    TRUE ~ cod  # Keep the original cod value for other causes
  ))


# Category 7 - other 
cod.long <- cod.long %>%
  mutate(cod = case_when(
    cause_of_death %in% c("tetanus", "childhood cluster diseases (tetanus)Ã¢â€°Â¤5", "other tetanus",
                          "sepsis or tetanus", "04.01 acute cardiac disease", "04.99 other and unspecified cardiac dis",
                          "04.99 unspecified cardiac dis", "acute cardiac disease", "cardiac arrest", "cardiac disease",
                          "cardio-circulatory diseases", "cardio-respiratory", "cardiovascular", "childhood cardiovascular diseases",
                          "deaths from cardiovascular diseases", "myocarditis", "n7: respiratory and cardiovascular disorders", 
                          "other and unspecified cardiac dis", "other and unspecified cardiac disease", "other cardiac disease",
                          "other cardiovascular diseases", "respiratory and cardiovascular disorders", "respiratory and cardiovascular disorders (respiratory failure)",
                          "circulatory system", "other (circulatory system)",
                          "cardiovascular diseases", "diseases of the circulatory system", 
                          "diseases of the digestive system", "digestive system", "gastrointestinal diseases", 
                          "all other gastrointestinal disorders", "gastro-intestinal diseases", "gastrointestinal anomalies",
                          "gastrointestinal disorder", "gastrointestinal disorders", "other gastrointestinal disease",
                          "digestive system", "diseases of the digestive system", "other (digestive system)", "other digestive diseases",
                          "nervous system", "nervous system (g00Ã¢â‚¬â€œg99)", "other disorders of the nervous system",
                          "diseases of the nervous system", "infectious diseases", "infectious", 
                          "01.99 other and unspecified infect dis", "01.99 other and unspecified infect. dis.",
                          "acute bacterial sepsis and severe infections", "all other specified infectious and parasitic diseases",
                          "deaths from infectious and parasitic diseases", "endemic infections", "endemic infections",
                          "epidemic infections", "febrile infections", "hypoxia + infection", "infection",
                          "infection and parasitic diseases", "infections", "infectious & parasitic diseases", "infectious and parasitic diseases",
                          "other (infectious and parasitic diseases )", "other and unspecified infect dis",
                          "other infection", "other infections", "other infectious and parasitic causes", "other infectious cause",
                          "other infectious diseases", "other infectious diseases, perinatal and nutritional conditions",
                          "presumed/suspected infection", "serious infections", "severe infections", "suspected infection",
                          "unspecified infectios diseases", "unspecified infectious diseases", "unspecified infectious disease", "urinary infection",
                          "all other specified infectious and parasitic diseases", "all other unspecified infectious and parasitic diseases",
                          "intracranial abscess", "haemorrhage", "haemorrhagic diseases", "hypoxic ischaemic encephalopathy", 
                          "ischaemic heart disease", "hematopoietic system diseases", "hemorrhage", "hemorrhagic fever",
                          "intracranial hemorrhage", "septicaemia", "blood diseases", "blood/blood forming organs",
                          "blood/blood forming organs (d50Ã¢â‚¬â€œd89)", "diseases of the blood & blood-forming organs & certain disorders involving the immune system",
                          "cancers", "other cancers",
                          "central nervous system diseases", "mental and behavioral disorders", 
                          "neoplasms", "02.01 oral neoplasms","02.03 respiratory neoplasms",
                          "02.02 digestive neoplasms", "02.04 breast neoplasms", "02.05 & 02.06 reproductive neoplasms",
                          "02.99 other and unspecified neoplasms", "all other malignant neoplasm",
                          "digestive neoplasms", "digestive organs neoplasm", "female genital organs neoplasm",
                          "ill-defined and unspecific sites neoplasm","lip, oral cavity and oropharynx neoplasm",
                          "male genital organs neoplasm","reproductive neoplasms","respiratory and intrathoracic organs neoplasm",
                          "respiratory neoplasms","other respiratory system illness (excluding ari, tuberculosis, and neoplasms)",
                          "malignant neoplasm", "neoplasm", "neoplasms",
                          "brain and nervous system cancer", "interpersonal brain and nervous system cancer",
                          "neoplasm", "lung cancer", "deaths from neoplasms", 
                          "other disease of nervous system", 
                          "other infectious and parasistic disease", "other natural", 
                          "urinary tract diseases", "fevers and convulsions", "convulsions", "convulsions and disorders of cerebral status",
                          "intracranial hemorrhage", 
                          "miscellaneous", "n10: miscellaneous",
                          "other", "12.99 other and unspecified", "98 other and unspecified ncd", "all other causes (with 1 each)",
                          "all other causes of death", "all other specified causes", "all others (with only 1 case each)",
                          "deaths from Ã¢â‚¬Å“otherÃ¢â‚¬Â diseases", "other (circulatory system)",
                          "other (digestive system)", "other (endocrine, nutritional and metabolic diseases )",
                          "other (external causes of mortality )", "other (infectious and parasitic diseases )",
                          "other (not elsewhere classifiable )", "other (respiratory system )", "other and unspecified",
                          "other and unspecified cardiac dis", "other and unspecified cardiac disease", "other and unspecified infect dis",
                          "other and unspecified neoplasms", "other and unspecified perinatal cause of death",
                          "other cancers", "other cardiac disease", "other cardiovascular diseases","other causes",
                          "other causes of death", "other cd", "other child causes of death", "other cods",
                          "other communicable diseases", "other digestive diseases", "other diseases",
                          "other disorders of fluid, electrolyte, and acid-base balance", "other disorders of the nervous system",
                          "other endocrine, metabolic, blood, and immune disorders", "other gastrointestinal disease", "other group",
                          "other group i", "other group ii", "other group l", "other group ll", "other immunodeficiencies",
                          "other infection", "other infections", "other infectious cause","other infectious causes",
                          "other infectious diseases","other infectious diseases, perinatal and nutritional conditions",
                          "other ncd", "other nco", "other neoplasm", "other neoplasms", "other neurological disorders",
                          "other non-communicable causes", "other non-communicable diseases", "other noncommunicable causes",
                          "other respiratory disease", "other respiratory system illness (excluding ari, tuberculosis, and neoplasms)",
                          "other site injury", "other skin and subcutaneous diseases", "other tetanus", "other viral disease",
                          "other(circulatory system )", "other(external causes of mortality)", "others",
                          "others external causes", "sepsis and other infections", "septicaemia", "septicemia",
                          "sepsis", "certain infectious and parasitic diseases", 
                          "endocrine, nutritional and metabolic diseases", "nutritional endocrinology and metabolic disease", 
                          "nervous system and sense organs", "other infection", 
                          "sudden infant death syndrome", "all other causes", "breast neoplasms", 
                          "chronic obstructive pulmonary disease", "copd/asthma", "cvd",
                          "diabetes mellitus", "digestive neoplasms", 
                          "epilepsy", "non-communicable", "oral neoplasms", "other and unspecified cardiac disease", 
                          "other and unspecified infectious disease", "other and unspecified neoplasms", 
                          "other and unspecified noncommunicable diseases", "renal failure", 
                          "reproductive neoplasms", "reproductive neoplasms male/female", "respiratory neoplasms", 
                          "sepsis (non-obstetric)", "severe anaemia", "severe anemia", "stroke", 
                          "other and unspecified NCD", "sickle cell with crisis", "cancer", "chickenpox", 
                          "diseases of the digestive system (k46, k56, k57, k63, k74, k75, k83, k92)", 
                          "epilepsy/ mrigi", "gall blooder deases", "heart Diseases", "heart disease", 
                          "hoping caugh", "kidney problem", "leprosy", "leukaemia (c91 – c95)", "mental disease", 
                          "other fever", "other diseases", "Other fevers", "others diseases", 
                          "titanus", "tumor", "others", "nutritional diseases", 
                          "other infectious and parasitic diseases", "other noncommunicable diseases", 
                          "vaccine Preventable Diseases", "anemia", "fever, unspecified", "jaundice", 
                          "miscellaneous: convulsion, epilepsy, hepatic failure, cancer, intestinal obstruction, CNS disorder, umbilical hemmorhage, stroke, abscess, thalassemia, other disease of intestine", 
                          "other infections: septicemia, CNS tuberculosis, typhoid, dengue, chicken pox, rabies, viral infection", 
                          "others", "septicemia & fever", "sudden death", "cardiac arrest", "others (pn)", 
                          "acute abdomen", "acute cardiac disease", "asthma", "liver cirrhosis", 
                          "acute bacterial sepsis & severe infection", "fever of unknown origin", 
                          "01.99 other and unspecified infect dis", "03.01 severe anaemia", 
                          "04.03 sickle cell with crisis", "05.02 asthma", "06.01 acute abdomen", 
                          "06.02 liver cirrhosis", "08.01 epilepsy", "miscellaneous: convulsion, epilepsy, hepatic failure, cancer, intestinal obstruction, cns disorder, umbilical hemmorhage, stroke, abscess, thalassemia, other disease of intestine", 
                          "all other diseases", "anaemia", "chronic abdominal conditions", 
                          "chronic obstructive lung disease", "coma/fever", "diphteria", "diphtheria", 
                          "disease of g.i.", "dropsy", "e.n.t. disease", "ent disease", "fever", 
                          "gi haemorrhage", "heart disease/failure", "heart diseases", "hydrocephalus", 
                          "leukemia", "liver disease", "malignant neoplasms", "other and unspecified cardiac dis", 
                          "other and unspecified infect dis", "other and unspecified ncd", "other causes", 
                          "other conditions", "other fevers", "other infections", "other infectious diseases", 
                          "other neurological diseases", "other tumors", "other, rest of 000-136", "peritonitis", 
                          "pertussis", "rheumatism", "pyrexia of unknown origin", "skin and subcutaneous infection", 
                          "skin diseas", "skin disease", "small pox", "vaccine preventable diseases", 
                          "veneral disease", "whooping cough", "fevers: malaria, meningitis, encephalitis, typhoid", 
                          "intestinal obstruction and hernia 550-553 560", 
                          "other infections: septicemia, CNS tuberculosis, typhoid, dengue, chicken pox, rabies, viral infection", 
                          "septicemia", "unexplained fever", "febrile illness (pyrexia of unknown origin)", 
                          "other and unspecified infectious diseas", "other and unspecified non-communicable", 
                          "tuberculosis", "01.09 pulmonary tuberculosis", "pulmonary tuberculosis", 
                          "tb", "tuberculosis (a16)", "typhoid fever", "typhoid", "typhoid/paratyphoid",
                          "04.99 other and unspecified cardiac dis.", "05.01 chronic obstructive pulmonary dis",
                          "05.01 chronic obstructive pulmonary dis.", "07.01 renal failure",
                          "12.99 unspecified external cod", "acute abdominal conditions (incl. intestinal obstruction)",
                          "anaemias", "anemias","asthematic attack","blood/blood forming organs (d50â€“d89)",
                          "brain diseases", "brain injury", "brain tumor", "c22.0 liver cell carcinoma","cerebrovascular disease",
                          "cerebral infarction", "childhood cluster diseases (tetanus)â‰¤5",
                          "chronic obstructive pulmonary dis", "deaths from genitourinary diseases",
                          "deaths from neurological diseases", "deaths from â€œotherâ€ diseases",
                          "dengue hemorrhagic fever", "diabetes", "dm", "diaphragmatic hernia", "diseases of liver",
                          "diseases of the genitourinary system", "diseases of the musculoskeletal system",
                          "diseases of the musculoskeletal system & connective tissue",
                          "diseases of the musculoskeletal system (m00â€“m99)","endocrine, nutritional & metabolic diseases",
                          "epilepticus", "genito-urinary diseases","genitourinary system",
                          "genitourinary system (n00â€“n99)", "heart failure", "intestinal infections diseases",
                          "intestinal obstruction", "ischemic heart disease", "jaundice syndrome",
                          "kidney", "kidney disease", "kidney diseases", "kidney failure", "liver cell carcinoma",
                          "liver diseases", "liver failure", "malignancies", "mental and behavioral disorder",
                          "mental and behavioral disorder (f00â€“f99)", "mental and behavioural disorder",
                          "mental and behavioural disorder (f00â€“f99)", "mental illness", "metabolic and anzymatic disorder",
                          "metabolic and nutritional diseases", "metabolic conditions", "metabolic disorder",
                          "nephritis and nephrosis", "nervous system (g00â€“g99)",
                          "neurologic diseases", "neurological", "non communicable",
                          "non-communicable diseases", "nutritional deficiencies", "other (endocrine, nutritional and metabolic diseases)",
                          "other (external causes of mortality)", "other (infectious and parasitic diseases)",
                          "other (not elsewhere classifiable)", "renal disorders", "thalassemia", "thalessemia", "01.10 pertussis", "sids, sudden infant death syndrome") ~ "7",  # Set cod = "7" for other causes
    cod == "1" ~ "1",  
    cod == "2" ~ "2",  
    cod == "3" ~ "3", 
    cod == "4" ~ "4",
    cod == "5" ~ "5",  
    cod == "6" ~ "6",  
    TRUE ~ cod  # Keep the original cod value for other causes
  ))

# Category 8 - "pneumonia" H65-66, J00-22, J85, P23
cod.long <- cod.long %>%
  mutate(cod = case_when(
    cause_of_death %in% c("pneumonia", "ari", "lower rti", "lower respiratory tract infections", "lower respiratory tract infection",
                          "respiratory infection", "acute respiratory infection including", "severe pneumonia",
                          "acute respiratory infection, including pneumonia", "acute resp infect pneumonia",
                          "acute resp infection including pneumonia", "acute respiratory infection (including pneumonia)",
                          "acute respiratory infections", "acute respiratory infections (incl. pneumonia)",
                          "acute lower respiratory tract infections", "influenza", "lrti", 
                          "pneumonia (j18)", "respiratory infections", "lower respiratory infections",
                          "acute respiratory infection", "penumonia", "pneumonia (pn)", 
                          "01.02 acute resp infect incl pneumonia", "01.02 acute resp infect incl. pneumonia",
                          "acute lower respiratory infection", "acute resp infect incl pneumonia", 
                          "alri", "influenza and pneumonia", "post measles pneumonia", 
                          "acute lower respiratory infection with severe malnutrition", 
                          "acute respiratory infection including pneumonia", "respiratory diseases", 
                          "diseases of the respiratory system", "respiratory illness", 
                          "respiratory system", "other respiratory system", "other (respiratory system)",
                          "respiratory", "non-tb respiratory infections",
                          "respiratory disease", "respiratory problem", "community-acquired pneumonia",
                          "other respiratory disease", "respiratory failure", "upper respiratory tract infections") ~ "8",  # Set cod = "8" for pneumonia-related causes
    cod == "1" ~ "1",  
    cod == "2" ~ "2",  
    cod == "3" ~ "3",
    cod == "4" ~ "4",
    cod == "5" ~ "5",  
    cod == "6" ~ "6",  
    cod == "7" ~ "7",  
    TRUE ~ cod  
  ))

