#
# This script
# generates lifetime diagnoses for NESDA participants by updating baseline up to Wave 6
# for depression, anxiety and bipolar disorder
# Input: diagnosis files from all waves
# Output: NESDA_lifetime_diagnoses.csv (all controls) and NESDA_lifetime_depression_cleaned.csv (controls cleaned from anxiety/bipolar)
# Status: Sure hope it runs.

# Set up
packages = c("dplyr", 
             "foreign",
             "here", 
             "stringr", 
             "tidyr")

## Now load or install&load all 
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)


#### Read in diagnosis files of all previous waves ####
# DEP
depression_1 <- read.spss(here("Data", "NESDA_DAP2406", "N1_A257D.sav"), to.data.frame = TRUE,use.value.labels = FALSE) # acidep11 is 'lifetime depression diagnoses present'
depression_2 <- read.spss(here("Data", "NESDA_DAP2406", "N1_C257D.sav"), to.data.frame = TRUE,use.value.labels = FALSE)
depression_3 <- read.spss(here("Data", "NESDA_DAP2406", "N1_D257D.sav"), to.data.frame = TRUE,use.value.labels = FALSE)
depression_4 <- read.spss(here("Data", "NESDA_DAP2406", "N1_E257D.sav"), to.data.frame = TRUE,use.value.labels = FALSE)
depression_5 <- read.spss(here("Data", "NESDA_DAP2406", "N1_F257D.sav"), to.data.frame = TRUE,use.value.labels = FALSE)
# ANX
anxiety_1 <- read.spss(here("Data", "NESDA_DAP2406", "N1_A259D.sav"), to.data.frame = TRUE,use.value.labels = FALSE) # aanxy22 is 'lifetime anxiety diagnoses present'
anxiety_2 <- read.spss(here("Data", "NESDA_DAP2406", "N1_C259D.sav"), to.data.frame = TRUE,use.value.labels = FALSE)
anxiety_3 <- read.spss(here("Data", "NESDA_DAP2406", "N1_D259D.sav"), to.data.frame = TRUE,use.value.labels = FALSE)
anxiety_4 <- read.spss(here("Data", "NESDA_DAP2406", "N1_E259D.sav"), to.data.frame = TRUE,use.value.labels = FALSE)
anxiety_5 <- read.spss(here("Data", "NESDA_DAP2406", "N1_F259D.sav"), to.data.frame = TRUE,use.value.labels = FALSE)
# BIP
bipolar_2 <- read.spss(here("Data", "NESDA_DAP2406", "N1_C262D.sav"), to.data.frame = TRUE,use.value.labels = FALSE) # Cbip10 is lifetime bipolar diagnoses present
bipolar_3 <- read.spss(here("Data", "NESDA_DAP2406", "N1_D262D.sav"), to.data.frame = TRUE,use.value.labels = FALSE)
bipolar_4 <- read.spss(here("Data", "NESDA_DAP2406", "N1_E262D.sav"), to.data.frame = TRUE,use.value.labels = FALSE)


#### Update lifetime variable ####
# build lifetime depressive disorder variable
lifetime_depression <- depression_1[, c("pident", "acidep11")] %>% 
  full_join(depression_2[, c("pident", "ccidep11")], by = "pident") %>% 
  full_join(depression_3[, c("pident", "dcidep11")], by = "pident") %>% 
  full_join(depression_4[, c("pident", "ecidep11")], by = "pident") %>% 
  full_join(depression_5[, c("pident", "fcidep11")], by = "pident")

lifetime_depression$sum_dep <- rowSums(lifetime_depression[,2:6], na.rm = TRUE) # sum of all waves (if never diagnosed this will be zero, if ever diagnosed this will be > zero)
lifetime_depression$ever_dep <- apply(lifetime_depression, 1, function(x) if (x["sum_dep"] == 0) {0} else {1}) # make new never vs ever variable

# repeat for lifetime MDD only, no dysthymia
lifetime_MDD <- depression_1[, c("pident", "acidep09")] %>% 
  full_join(depression_2[, c("pident", "ccidep09")], by = "pident") %>% 
  full_join(depression_3[, c("pident", "dcidep09")], by = "pident") %>% 
  full_join(depression_4[, c("pident", "ecidep09")], by = "pident") %>% 
  full_join(depression_5[, c("pident", "fcidep09")], by = "pident")

lifetime_MDD$sum_MDD <- rowSums(lifetime_MDD[,2:6], na.rm = TRUE) # sum of all waves (if never diagnosed this will be zero, if ever diagnosed this will be > zero)
lifetime_MDD$ever_MDD <- apply(lifetime_MDD, 1, function(x) if (x["sum_MDD"] == 0) {0} else {1}) # make new never vs ever variable

table(lifetime_MDD$ever_MDD)

# update anxiety diagnosis variable
colnames(anxiety_2)[1] <- "pident" # correct inconsistent colnames
colnames(anxiety_3)[1] <- "pident"

lifetime_anxiety <- anxiety_1[, c("pident", "aanxy22")] %>% 
  full_join(anxiety_2[, c("pident", "canxy22")], by = "pident") %>% 
  full_join(anxiety_3[, c("pident", "danxy22")], by = "pident") %>% 
  full_join(anxiety_4[, c("pident", "eanxy22")], by = "pident") %>% 
  full_join(anxiety_5[, c("pident", "fanxy22")], by = "pident")

lifetime_anxiety$sum_anx <- rowSums(lifetime_anxiety[,2:6], na.rm = TRUE) # sum of all waves (if never diagnosed this will be zero, if ever diagnosed this will be > zero)
lifetime_anxiety$ever_anx <- apply(lifetime_anxiety, 1, function(x) if (x["sum_anx"] == 0) {0} else {1}) # make new never vs ever variable

table(lifetime_anxiety$ever_anx)

# update bipolar diagnosis variable
colnames(bipolar_2)[1] <- "pident"
colnames(bipolar_3)[1] <- "pident"

lifetime_bipolar <- bipolar_2[, c("pident", "Cbip10")] %>% 
  full_join(bipolar_3[, c("pident", "dbip10")], by = "pident") %>% 
  full_join(bipolar_4[, c("pident", "ebip10")], by = "pident")

lifetime_bipolar$sum_bip <- rowSums(lifetime_bipolar[,2:4], na.rm = TRUE) # sum of all waves (if never diagnosed this will be zero, if ever diagnosed this will be > zero)
lifetime_bipolar$ever_bip <- apply(lifetime_bipolar, 1, function(x) if (x["sum_bip"] == 0) {0} else {1}) # make new never vs ever variable

table(lifetime_bipolar$ever_bip)


#### Unite in one dataframe ####
# Merge
lifetime_diagnoses <- lifetime_depression[, c("pident", "ever_dep")] %>% 
  full_join(lifetime_MDD[, c("pident", "ever_MDD")], by = "pident") %>% 
  full_join(lifetime_anxiety[, c("pident", "ever_anx")], by = "pident") %>% 
  full_join(lifetime_bipolar[, c("pident", "ever_bip")], by = "pident")

# when there is no info on bipolar (NA) we assume that there is no bipolar
lifetime_diagnoses$ever_bip <- replace_na(lifetime_diagnoses$ever_bip, 0) # so replace NAs with 0

# Check comorbidities
table(lifetime_diagnoses$ever_dep, lifetime_diagnoses$ever_anx) # there are 518 clear controls and 290 to be removed. seems like a lot of people (1710) have both - true? true!
table(lifetime_diagnoses$ever_dep, lifetime_diagnoses$ever_bip) # 806 clear controls and 2 with no depression but bipolar. 170 people with both depression and bipolar
table(lifetime_diagnoses$ever_MDD)

write.csv(lifetime_diagnoses, file = here("Data", "NESDA_lifetime_diagnoses.csv"))

# Some code for removing controls with anxiety or bipolar from the depression controls
# remove controls with anxiety and/or bipolar

lifetime_diagnoses <- read.csv(here("Data", "NESDA_lifetime_diagnoses.csv"))
head(lifetime_diagnoses)


lifetime_diagnoses$caseDEP <- as.factor(apply(lifetime_diagnoses, 1, function(x) if (x["ever_dep"] == 0 && (x["ever_anx"] == 1 || x["ever_bip"] == 1)) {NA} else {x["ever_dep"]}))
# and again for MDD only (sets NA if no MDD but anxiety/bipolar/dysthymia)
lifetime_diagnoses$caseMDD <- as.factor(apply(lifetime_diagnoses, 1, function(x) if (x["ever_MDD"] == 0 && (x["ever_anx"] == 1 || x["ever_bip"] == 1 || x["ever_dep"] == 1)) {NA} else {x["ever_MDD"]}))
 
lifetime_depression_cleaned <- lifetime_diagnoses
table(lifetime_depression_cleaned$caseMDD) # 517 controls and 2173 cases

write.csv(lifetime_depression_cleaned, file = here("Data", "NESDA_lifetime_depression_cleaned.csv"))
