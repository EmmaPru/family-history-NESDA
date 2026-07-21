
# What does this script do?
# This script preprocesses the diagnoses of depression and anxiety in the FTI dataset.
# It creates a dataset with the diagnoses of depression and anxiety for each family member.
# It also creates a dataset with the prevalence of depression and anxiety diagnoses in the FTI dataset.

# What are the inputs?
# The input is the dataset with the diagnoses of depression and anxiety in the FTI dataset.

# What are the outputs?
# The outputs are two datasets:
# 1. A dataset with the diagnoses of depression and anxiety for each family member.
# 2. A dataset with the prevalence of depression and anxiety diagnoses in the FTI dataset.

# This script
# 1) assesses and processes FTI diagnoses for depression and anxiety
# 2) creates diagnoses based on simple and elaborate criteria, with and without imputation
# 3) compares prevalences and cross-tables of different assessment types
# Input: age.RData (from NESDA_FTI_preprocessing_general.R)
# Output: FTI_diagnoses.csv (with diagnoses for depression and anxiety for each family member)
# Status: A delicate construction.

#  Load packages and functions
source(file = "Scripts/setup.R")

theme_set(theme_minimal()) # set default theme for ggplot

# Load data
load(file = here("Data", "age.RData"))

# Note that the interview has two possible breaks.  
#     1) If g05 is 'no' or 'don't know', the disorder questions are not asked. 
#     2) If there is no 'yes' in the first two core questions, the follow-up questions are not asked.

#### Depression ####
# Investigate depression questions
dep <- age |> 
  select(pident, fammember, g05, d01, d02, d05, d05a, d05b, d06, d07a, d07b)

dep_long <- dep |> 
  pivot_longer(
    cols = starts_with("d"),
    names_to = "question",
    values_to = "answer"
  ) |> 
  group_by(question) 
dep_long |> head()

# plot that shows frequency of answers 1/2/3 for each questionnaire item
p1 <- dep_long %>%
  mutate(text = fct_reorder(question, answer)) %>%
  ggplot( aes(x=answer, color=text, fill=text)) +
  geom_histogram(alpha=0.8, binwidth = 1) +
  scale_fill_viridis(discrete=TRUE, end = 0.8, option = 'inferno') +
  scale_color_viridis(discrete=TRUE, end = 0.8, option = 'inferno') +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Count") +
  facet_wrap(~text)
p1
ggsave(here("Results", "Dep_items_frequencies.png"), p1)



d01 <- dep_long |> filter(question == "d01") |> 
  mutate(fammember = substr(fammember, start = 1, stop = 2))

head(d01)

table(d01$answer, d01$fammember, useNA = 'always')

p2 <- d01 %>%
  mutate(text = fct_reorder(fammember, answer)) %>%
  ggplot( aes(x=answer, color=text, fill=text)) +
  geom_histogram(alpha=0.8, binwidth = 1) +
  scale_fill_viridis(discrete=TRUE, end = 0.8, option = 'inferno') +
  scale_color_viridis(discrete=TRUE, end = 0.8, option = 'inferno') +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Count") +
  facet_wrap(~text)
p2
ggsave(here("Results", "d01_different_fammembers.png"), p2)

rm(d01)

# **     - Ever had a depressive episode (ffti..._d01)  
# **     - Ever had a period of loss of interest (anhedonia) (ffti..._d02)
# 
# **     - Ever treated for depressive complaints by a general practitioner, RIAGG, psychologist, or psychiatrist (ffti..._d05)  
# **     - Ever used medication for depressive complaints (ffti..._d06),  
# **     - Ever admitted (hospital/psych ward) for depressive complaints (ffti..._d07a)  
# **     - Ever received ECT (ffti..._d07b) 


## First two dep questions only

# Diagnose based only on core items
dep <- dep |> 
  mutate( 
    dep_core = case_when(
      d01 == 1 | d02 == 1 ~ 1,
      is.na(d01) & is.na(d02) ~ NA_real_,
      TRUE ~ 0
    ))

table(dep$d01, dep$d02, useNA = 'always') # 2 is horizontal, 1 is vertical
table(dep$dep_core, useNA = 'always')

# Considering only the two core anxiety items there are mostly NAs. There are mostly NAs, way fewer controls than cases.


## Follow-up questions only

# Diagnose based only on fu items
dep <- dep |> 
  mutate( 
    dep_fu = case_when(
      d05 == 1 | d06 == 1 | d07a == 1 | d07b == 1 ~ 1,
      is.na(d05) & is.na(d06) & is.na(d07a) & is.na(d07b) ~ NA_real_,
      TRUE ~ 0
    ))

table(dep$dep_fu, useNA = 'always')

# For the follow up, there are more NAs, fewer diagnoses.

# Diagnose based on all items
## Simple vs. elaborate way of diagnosing

# Post hoc: We could 1) impute NAs with 0 or 2) impute with prevalence 3) delete.
# Eleonore did this implicitly - 'hidden zero imputation', which explains greater number of NAs.

### No imputation
dep <- dep |>
  mutate( 
    depdiag_simple = case_when(
      d01 == 1 ~ 1, # if d01 yes (implies g05 yes) then case
      g05 %in% c(2) | d01 == 2 ~ 0, # if g05 no or d01 no then control
      TRUE ~ NA_real_, # if g05 in (yes, dk, NA) and d01 in (dk, NA) then NA
    )) |> mutate( 
      depdiag_elaborate = case_when(
        (d01 == 1 | d02 == 1) & (d05 == 1 | d06 == 1 | d07a == 1 | d07b == 1) ~ 1, # case if there is one yes in core and one yes in follow-ups
        
        g05 %in% c(2) | # control if g05 no
          (d01 == 2 & d02 != 1) & (d01 != 1 & d02 == 2) ~ 0, # control if one of core is no and the other is not yes
        
        TRUE ~ NA_real_, # else NA, specifically if yes in core items is not confirmed by follow-up items
      ))

head(dep)

### Impute with zero
dep <- dep |>
  mutate( 
    depdiag_simple_0imputed = case_when(
      d01 == 1 ~ 1, # if d01 yes (implies g05 yes) then case
      #g05 %in% c(2) | d01 == 2 ~ 0, # if g05 no or d01 no then control
      TRUE ~ 0, # if g05 in (yes, dk, NA) and d01 in (dk, NA) then NA
    )) |> mutate( # from the previous paper
      depdiag_elaborate_0imputed = case_when(
        (d01 == 1 | d02 == 1) & (d05 == 1 | d06 == 1 | d07a == 1 | d07b == 1) ~ 1, # case if there is one 1 in core and one 1 in follow-ups
        
        TRUE ~ 0 # control for all other combinations of stuff
      ))

head(dep)

### Previous paper
# Reproduce here exactly the partial imputation of Eleonore's paper.
# 
# Info on a relative was only thought to be truly missing if g05 (any psychological problems) was NA.
# Any other combinations of item responses, including any number of missing answers, lead to classification of the relative as control. See also  
# FLS_DataCleaning_FLS_FH_otherpsych: "Give the rest (i.e., those that did not endorse the items with a yes and that were non-missing) a '0' for no or don't know." 
dep <- dep |> 
  mutate( # from the previous paper
    depdiag_2020paper = case_when(
      (d01 == 1 | d02 == 1) & (d05 == 1 | d06 == 1 | d07a == 1 | d07b == 1) ~ 1,
      is.na(g05) ~ NA_real_,
      TRUE ~ 0
    )
  )

head(dep)


# Get and compare prevalences
K_DEP <- list()
table(dep$depdiag_simple, useNA = 'always')
K_DEP[["simple"]] <- sum(dep$depdiag_simple, na.rm = TRUE) / sum(!is.na(dep$depdiag_simple))

table(dep$depdiag_elaborate, useNA = 'always')
K_DEP[["elaborate"]] <- sum(dep$depdiag_elaborate, na.rm = TRUE) / sum(!is.na(dep$depdiag_elaborate))

table(dep$depdiag_simple_0imputed, useNA = 'always')
K_DEP[["simple_0imputed"]] <- sum(dep$depdiag_simple_0imputed, na.rm = TRUE) / sum(!is.na(dep$depdiag_simple_0imputed)) # should be same as non-imputed

table(dep$depdiag_elaborate_0imputed, useNA = 'always')
K_DEP[["elaborate_0imputed"]] <- sum(dep$depdiag_elaborate_0imputed, na.rm = TRUE) / sum(!is.na(dep$depdiag_elaborate_0imputed))

table(dep$depdiag_2020paper, useNA = 'always')
K_DEP[["simple_partial"]] <- NA
K_DEP[["elaborate_partial"]] <- sum(dep$depdiag_2020paper, na.rm = TRUE) / sum(!is.na(dep$depdiag_2020paper)) # These 24 NAs are all due to g05 missing.
# Does this partial impute prevalence correspond to what is in the 2020 paper (if it's there)?

K_DEP


### Compare prevalences
prevalences_dep <- rbind(c(K_DEP[["simple_0imputed"]], K_DEP[["elaborate_0imputed"]]),
                     c(K_DEP[["simple"]], K_DEP[["elaborate"]]),
                     c(K_DEP[["simple_partial"]], K_DEP[["elaborate_partial"]]))
colnames(prevalences_dep) <- c("Simple", "Elaborate")
rownames(prevalences_dep) <- c("Zero imputation", "No imputation", "Partial imputation")
prevalences_dep
save(prevalences_dep, file = here("Results", "prevalences_dep.RData"))


### Compare simple and elaborate (cross-tables)
cross_noimpute_dep <- table(dep$depdiag_simple, dep$depdiag_elaborate, useNA = 'always')
rownames(cross_noimpute_dep) <- c("Simple Control", "Simple Case", "Simple NA")
colnames(cross_noimpute_dep) <- c("Elaborate Control", "Elaborate Case", "Elaborate NA")
cross_noimpute_dep
save(cross_noimpute_dep, file = here("Results", "crosstable_noimputation_dep.RData"))

cross_impute0_dep <- table(dep$depdiag_simple_0imputed, dep$depdiag_elaborate_0imputed, useNA = 'always')
rownames(cross_impute0_dep) <- c("Simple Control", "Simple Case", "Simple NA")
colnames(cross_impute0_dep) <- c("Elaborate Control", "Elaborate Case", "Elaborate NA")
cross_impute0_dep
save(cross_impute0_dep, file = here("Results", "crosstable_impute0_dep.RData"))

# In non-imputed:
# Simple Control/Elaborate Case probably implies that it became an elaborate case due to d02.
# Simple NA/Elaborate Case should mean that d01 was missing but d02 was yes.
# There are no Simple Cases/Elaborate Control by definition - if there d01 is yes, elaborate questioning either confirms them as case or classifies as NA if the info is confusing.


#### Anxiety ####
# Investigate anxiety questions
anx <- age |> 
  select(pident, fammember, g05, a01, a02, a05, a05a, a05b, a06, a07)

anx_long <- anx |> 
  pivot_longer(
    cols = starts_with("a"),
    names_to = "question",
    values_to = "answer"
  ) |> 
  group_by(question) 
anx_long |> head()

# plot that shows frequency of answers 1/2/3 for each questionnaire item
p1 <- anx_long %>%
  mutate(text = fct_reorder(question, answer)) %>%
  ggplot( aes(x=answer, color=text, fill=text)) +
  geom_histogram(alpha=0.8, binwidth = 1) +
  scale_fill_viridis(discrete=TRUE, end = 0.8, option = 'inferno') +
  scale_color_viridis(discrete=TRUE, end = 0.8, option = 'inferno') +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Count") +
  facet_wrap(~text)
p1
ggsave(here("Results", "Anxiety items - Answer frequencies.png"), p1)

# Plot a01 in types of family members
a01 <- anx_long |> filter(question == "a01") |> 
  mutate(fammember = substr(fammember, start = 1, stop = 2))
head(a01)
table(a01$answer, a01$fammember, useNA = 'always')

p2 <- a01 %>%
  mutate(text = fct_reorder(fammember, answer)) %>%
  ggplot( aes(x=answer, color=text, fill=text)) +
  geom_histogram(alpha=0.8, binwidth = 1) +
  scale_fill_viridis(discrete=TRUE, end = 0.8, option = 'inferno') +
  scale_color_viridis(discrete=TRUE, end = 0.8, option = 'inferno') +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Count") +
  facet_wrap(~text)
p2
ggsave(here("Results", "a01_different_fammembers.png"), p2)
rm(a01)


## First two anx questions only

# Diagnose based only on core items
anx <- anx |> 
  mutate( 
    anx_core = case_when(
      a01 == 1 | a02 == 1 ~ 1,
      is.na(a01) & is.na(a02) ~ NA_real_,
      TRUE ~ 0
    ))

table(anx$a01, anx$a02, useNA = 'always') # 2 is horizontal, 1 is vertical
table(anx$anx_core, useNA = 'always')

# Considering only the two core anxression items there are mostly NAs. There are mostly NAs, way fewer controls than cases.


## Follow-up questions only

# Diagnose based only on fu items
anx <- anx |> 
  mutate( 
    anx_fu = case_when(
      a05 == 1 | a06 == 1 | a07 == 1 ~ 1,
      is.na(a05) & is.na(a06) & is.na(a07) ~ NA_real_,
      TRUE ~ 0
    ))

table(anx$anx_fu, useNA = 'always')

# For the follow up, there are more NAs, fewer diagnoses.

# Diagnose based on all items
## Simple vs. elaborate way of diagnosing

# Post hoc: We could 1) impute NAs with 0 or 2) impute with prevalence 3) delete.
# Eleonore did this implicitly - 'hidden zero imputation', which explains greater number of NAs.

### No imputation
anx <- anx |>
  mutate( 
    anxdiag_simple = case_when(
      a01 == 1 ~ 1, # if a01 yes (implies g05 yes) then case
      g05 %in% c(2) | a01 == 2 ~ 0, # if g05 no or a01 no then control
      TRUE ~ NA_real_, # if g05 in (yes, dk, NA) and a01 in (dk, NA) then NA
    )) |> mutate( 
      anxdiag_elaborate = case_when(
        (a01 == 1 | a02 == 1) & (a05 == 1 | a06 == 1 | a07 == 1) ~ 1, # case if there is one yes in core and one yes in follow-ups
        
        g05 %in% c(2) | # control if g05 no
          (a01 == 2 & a02 != 1) & (a01 != 1 & a02 == 2) ~ 0, # control if one of core is no and the other is not yes
        
        TRUE ~ NA_real_, # else NA, specifically if yes in core items is not confirmed by follow-up items
      ))

head(anx)

### Impute with zero
anx <- anx |>
  mutate( 
    anxdiag_simple_0imputed = case_when(
      a01 == 1 ~ 1, # if a01 yes (implies g05 yes) then case
      #g05 %in% c(2) | a01 == 2 ~ 0, # if g05 no or a01 no then control
      TRUE ~ 0, # if g05 in (yes, dk, NA) and a01 in (dk, NA) then NA
    )) |> mutate( # from the previous paper
      anxdiag_elaborate_0imputed = case_when(
        (a01 == 1 | a02 == 1) & (a05 == 1 | a06 == 1 | a07 == 1) ~ 1, # case if there is one 1 in core and one 1 in follow-ups
        
        TRUE ~ 0 # control for all other combinations of stuff
      ))

head(anx)

### Previous paper
# Reproduce here exactly the partial imputation of Eleonore's paper.
# 
# Info on a relative was only thought to be truly missing if g05 (any psychological problems) was NA.
# Any other combinations of item responses, including any number of missing answers, lead to classification of the relative as control. See also  
# FLS_DataCleaning_FLS_FH_otherpsych: "Give the rest (i.e., those that did not endorse the items with a yes and that were non-missing) a '0' for no or don't know." 
anx <- anx |> 
  mutate( # from the previous paper
    anxdiag_2020paper = case_when(
      (a01 == 1 | a02 == 1) & (a05 == 1 | a06 == 1 | a07 == 1) ~ 1,
      is.na(g05) ~ NA_real_,
      TRUE ~ 0
    )
  )

head(anx)


# Get and compare prevalences
K_anx <- list()
table(anx$anxdiag_simple, useNA = 'always')
K_anx[["simple"]] <- sum(anx$anxdiag_simple, na.rm = TRUE) / sum(!is.na(anx$anxdiag_simple))

table(anx$anxdiag_elaborate, useNA = 'always')
K_anx[["elaborate"]] <- sum(anx$anxdiag_elaborate, na.rm = TRUE) / sum(!is.na(anx$anxdiag_elaborate))

table(anx$anxdiag_simple_0imputed, useNA = 'always')
K_anx[["simple_0imputed"]] <- sum(anx$anxdiag_simple_0imputed, na.rm = TRUE) / sum(!is.na(anx$anxdiag_simple_0imputed)) # should be same as non-imputed

table(anx$anxdiag_elaborate_0imputed, useNA = 'always')
K_anx[["elaborate_0imputed"]] <- sum(anx$anxdiag_elaborate_0imputed, na.rm = TRUE) / sum(!is.na(anx$anxdiag_elaborate_0imputed))

table(anx$anxdiag_2020paper, useNA = 'always')
K_anx[["simple_partial"]] <- NA
K_anx[["elaborate_partial"]] <- sum(anx$anxdiag_2020paper, na.rm = TRUE) / sum(!is.na(anx$anxdiag_2020paper)) # These 24 NAs are all due to g05 missing.
# Does this partial impute prevalence correspond to what is in the 2020 paper (if it's there)?

K_anx


### Compare prevalences
prevalences_anx <- rbind(c(K_anx[["simple_0imputed"]], K_anx[["elaborate_0imputed"]]),
                         c(K_anx[["simple"]], K_anx[["elaborate"]]),
                         c(K_anx[["simple_partial"]], K_anx[["elaborate_partial"]]))
colnames(prevalences_anx) <- c("Simple", "Elaborate")
rownames(prevalences_anx) <- c("Zero imputation", "No imputation", "Partial imputation")
prevalences_anx
save(prevalences_anx, file = here("Results", "prevalences_anx.RData"))


### Compare simple and elaborate (cross-tables)
cross_noimpute_anx <- table(anx$anxdiag_simple, anx$anxdiag_elaborate, useNA = 'always')
rownames(cross_noimpute_anx) <- c("Simple Control", "Simple Case", "Simple NA")
colnames(cross_noimpute_anx) <- c("Elaborate Control", "Elaborate Case", "Elaborate NA")
cross_noimpute_anx
save(cross_noimpute_anx, file = here("Results", "crosstable_noimputation_anx.RData"))

cross_impute0_anx <- table(anx$anxdiag_simple_0imputed, anx$anxdiag_elaborate_0imputed, useNA = 'always')
rownames(cross_impute0_anx) <- c("Simple Control", "Simple Case", "Simple NA")
colnames(cross_impute0_anx) <- c("Elaborate Control", "Elaborate Case", "Elaborate NA")
cross_impute0_anx
save(cross_impute0_anx, file = here("Results", "crosstable_impute0_anx.RData"))


# Save for future use
diagnoses <- full_join(dep, anx, by = c("pident", "fammember", "g05"))
write.csv(diagnoses, file = here("Data", "FTI_diagnoses.csv"))