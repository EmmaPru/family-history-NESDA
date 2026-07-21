# This script:
# 1) Reads in NESDA FTI data
# 2) Cleans and reshapes the data into a tidy format
# 3) Extracts family size information
# 4) Calculates ages of family members with specific conditions
# Input: NESDA FTI raw data files, demographics, and interview info
# Output: family size df, age df (with all FTI questions for further use)
# Status: Messy but don't want to mess with it

# Load required packages and set up environment
source(file = "Scripts/setup.R")
theme_set(theme_minimal()) # set default theme for ggplot

# Read in NESDA data
# Family tree file
fti_raw <- read_sav(here("Data", "NESDA_DAP2406", "N1_F121R.sav"))
# Demographics for age
ppt_age <- read_sav(here("Data", "NESDA_DAP2406", "N1_F100D.sav"))
interview_info <- read_sav(here("Data", "NESDA_DAP2406", "N1_F055R.sav"))


# What is in the data?
glimpse(fti_raw)
fti_raw$fftimo_g05

#### Data cleaning and reshaping ####
#
# pivot_longer
# arguments: cols specifies the columns that need to be pivoted
# names_to: the variable stored in the column names
# values_to: the variable stored in the cell values
fti <- fti_raw |>
  mutate(
    across(everything(), as.character)
  ) |>
  pivot_longer(
    cols = starts_with("ffti"),
    names_to = "question",
    values_to = "answer"
  ) |>
  mutate(
    question = str_remove(question, pattern = "ffti")
  )

glimpse(fti)

# Remove questions with answers that are character strings
#
# some questions'answers are compulsory strings:
# *_naam - sibs name (20 cols)
# *_g03 - where does sib live now (20 cols)
# *_o02 - description of other psychological problems
# fortunately they are not important for me right now.
fti <- fti |>
  filter(
    !(str_detect(question, "naam") | str_detect(question, "g03") | str_detect(question, "o02"))
  ) |>
  mutate(
    answer = as.numeric(answer)
  )
# removing in theory 20 + 20 + 22 = 62 observations per person. mother and father don't have name and place.


#### Check completeness of questionnaire ####
# How many ppt did not complete the questions? i.e. which participants have less than 3 non-NAs?
fti |>
  group_by(pident) |>
  filter(sum(!is.na(answer)) == 2) |>
  summarise()
# 298 participants did not complete the questionnaire (less than 3 non-NAs). 297 of them have only NAs, one participant has 2 non-NAs.
# 297 participants contain only NAs. Eleonore determined completeness only based on item g05 and therefore also eliminated one additional participant who only has 2 non-NAs - on other questions.
# We can exclude this participant too by allowing min 2 non-NAs rather than min 0 NAs.


# new df containing only those participants who officially completed the questionnaire
fti_complete <- fti |>
  group_by(pident) |>
  filter(sum(!is.na(answer)) > 2)


#### Family size ####
# Extract recorded sib numbers
famsize <- fti_complete |>
  filter(question %in% c("bro", "bro_nr", "sis", "sis_nr")) |>
  pivot_wider(
    id_cols = pident,
    names_from = question,
    values_from = answer
  ) |>
  mutate(
    sib_nr = sum(bro_nr, sis_nr, na.rm = TRUE)
  )

# bro and sis: 1 = yes, 2 = no
# bro_nr and sis_nr: answer is number of sibs
famsize
table(famsize$sib_nr, useNA = "always")

# checked also against Eleonore's documentation of which sib numbers went wrong due to NAs - all fine.

# these shouldn't exist (i.e. yes to having siblings but number of siblings is 0):
famsize |> filter(bro == 1 & bro_nr == 0)
famsize |> filter(sis == 1 & sis_nr == 0)
# and luckily they don't

# remove from fti_complete
fti_complete <- fti_complete |>
  filter(!question %in% c("bro", "bro_nr", "sis", "sis_nr"))

# Family size histogram
pdf("famsize_histogram.pdf", width = 7, height = 5)
ggplot(data = famsize, aes(x = sib_nr)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(breaks = c(0:16))
dev.off()
# Recorded sib/sibnr combinations are plausible.


# Let's remove the non-existent siblings

# separate the question column
# check if there are completely empty rows
fti_complete |>
  separate(
    question, c("fammember", "question")
  ) |>
  group_by(
    pident, fammember
  ) |>
  filter(
    sum(!is.na(answer)) == 0 # there are no non-NAs
  )
# It turns out more family members have only NA than have any answers. Not surprising given the huge number of possible siblings (20).
# We remove all those non-existent family members.
# remove any completely empty rows
fti_existing <- fti_complete |>
  separate(
    question, c("fammember", "question")
  ) |>
  group_by(
    pident, fammember
  ) |>
  filter(
    sum(!is.na(answer)) > 0 # there should be some non-NAs
  ) |>
  ungroup()
# in this operation we would loose bro and sis which indicate whether there are any brothers or sisters (because of the way the column question is split)
# fortunately we removed them earlier
glimpse(fti_existing)
rm(fti_complete)

#### Age ####
# Calculate age of family members

# Conditions:
# - If person has already died, age is replaced by age at death
# - Year of birth mother cannot be before 1896
# - Year of birth father cannot be before 1885
# - Becoming a parent at 10 is the youngest possible age (there are no too young mothers and fathers)
# - Siblings can be no more than 40 years apart in age (no brothers that are too young/old)

# Let's look at the birth years
g01 <- fti_existing |>
  filter(question == "g01") |>
  mutate(fammember = substr(fammember, start = 1, stop = 2))

# Histogram of each type of family member
ggplot(data = filter(g01, fammember == "mo"), mapping = aes(x = answer)) +
  geom_histogram()

ggplot(data = filter(g01, fammember == "fa"), mapping = aes(x = answer)) +
  geom_histogram()

ggplot(data = filter(g01, fammember == "br"), mapping = aes(x = answer)) +
  geom_histogram()

ggplot(data = filter(g01, fammember == "si"), mapping = aes(x = answer)) +
  geom_histogram()

# Put them all together in a funny but pretty plot - weird things happen with density
ggplot(data = g01, mapping = aes(x = answer, color = fammember)) +
  geom_density()

# For each parent, the plots show a few implausible birth years.
# remove unrealistic years of birth for mother and father, then pivot to wide
age <- fti_existing |>
  mutate(
    answer = case_when(
      fammember == "mo" & question == "g01" & answer <= 1895 ~ NA_real_,
      fammember == "fa" & question == "g01" & answer <= 1884 ~ NA_real_,
      TRUE ~ answer
    )
  ) |>
  pivot_wider(
    id_cols = c(pident, fammember),
    names_from = question,
    values_from = answer
  )
head(age)

# combine the two age-related dataframes
age_assist <- interview_info |>
  mutate(yoi = format(as.Date(interview_info$fintdat, format = "%d-%m-%Y"), "%Y")) |> # turn date of interview into year of interview
  full_join(ppt_age, by = "pident") |> # join two dfs
  select(pident, yoi, fage) |>
  mutate(pident = as.character(pident), yoi = as.numeric(yoi))
head(age_assist)

# combine with full df
age <- age |>
  left_join(age_assist, by = "pident") |>
  mutate(
    age = case_when(
      g02 == 2 & !is.na(g04) ~ g04 - g01, # if dead and year of death known
      g02 == 2 & is.na(g04) ~ NA_real_, # if dead and year of death not known
      TRUE ~ yoi - g01 # otherwise year of interview minus year of birth
    )
  ) |>
  relocate(c(fage, yoi, age), .after = pident)


age |> filter(g02 == 2 & is.na(g04))
# There are 36 relatives who have died but no year of death is given.

# Comparison with age of participant is not necessary, as it follows form Eleonores notes that none of the relatives were deemed too young.


# Histogram of age per type of family member
# Make data frame that contains only fammember mo/fa/br/si and their age
age_by_type_of_fammember <- age |>
  select(fammember, age) |>
  mutate(fammember = substr(fammember, start = 1, stop = 2))

ggplot(data = filter(age_by_type_of_fammember, fammember == "mo"), mapping = aes(x = age)) +
  geom_histogram() +
  labs(
    title = "Age of Mother"
  )

ggplot(data = filter(age_by_type_of_fammember, fammember == "fa"), mapping = aes(x = age)) +
  geom_histogram() +
  labs(
    title = "Age of Father"
  )

ggplot(data = filter(age_by_type_of_fammember, fammember == "br"), mapping = aes(x = age)) +
  geom_histogram() +
  labs(
    title = "Age of Brother"
  )

ggplot(data = filter(age_by_type_of_fammember, fammember == "si"), mapping = aes(x = age)) +
  geom_histogram() +
  labs(
    title = "Age of Sister"
  )

# Put them all together in a funny but pretty plot - slightly weird things happen with density
ggplot(data = age_by_type_of_fammember, mapping = aes(x = age, color = fammember)) +
  geom_density()


#### Any psychological problems ####
# How frequently were any psychological problems reported?
g05 <- fti_existing |>
  filter(question == "g05") |>
  mutate(fammember = substr(fammember, start = 1, stop = 2)) # this line removes the sib numbering

table(g05$answer, useNA = "always")

# Overall count
ggplot(data = g05, mapping = aes(x = answer)) +
  geom_histogram(alpha = 0.7, binwidth = 1)

p <- g05 %>%
  mutate(text = fct_reorder(fammember, answer)) %>%
  ggplot(aes(x = answer, color = text, fill = text)) +
  geom_histogram(alpha = 0.8, binwidth = 1) +
  scale_fill_viridis(discrete = TRUE, end = 0.8, option = "inferno") +
  scale_color_viridis(discrete = TRUE, end = 0.8, option = "inferno") +
  theme(
    legend.position = "none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)
  ) +
  xlab("") +
  ylab("Assigned Probability (%)") +
  facet_wrap(~text)
p
# No problems is more common than any problems. Except for mothers, where problems are reported more often than not.


#### Other ####
# Is 'don't know' equivalent to NA?
head(age)
g05is3 <- filter(age, g05 == 3)
table(g05is3$d01, useNA = "always") # each column has only NAs. because these questions were skipped.
# g05:
# for the zero imputation there should be no difference. NAs become controls, 3s become controls.
# for no imputation there should be no difference. 'else', including 3 becomes NA
# d - items. 3 should be treated as NA
# makes no difference for zero imputation, no imputation simple and partial imputation
# can make a difference for no imputation elaborate control condition. with the previous way of coding any 3s involved would lead to NA, now 3s in d01 and d02 lead to control (same as NA)


#### Save ####
write.csv(famsize, file = here("Data", "FTI_famsize.csv"))
write.csv(age, file = here("Data", "FTI_age.csv")) # contains all questions
