# Revisions after PsychMed reviewer comments
## 21 May 2026 ##

# 1) Add new PGS based on clumping and thresholding (C+T) approach, and compare its performance to the PRScs PGS
# 2) Add covariates (age, sex, SES/EA, PCs) to the models and see how this affects performance of the different predictors
# 3) Direct comparison uni/multivariate models with PRScs vs. PRScs and PAFGRS together

# Clear workspace
rm(list = ls())
source(file = "Scripts/setup.R")


# Load old, scaled data
indicators_scaled <- fread(file = here("Int_results", "indicators_scaled.csv"))
head(indicators_scaled)
# indicators_scaled: use for model comparison

# Load new clumping and thresholding PGS
new_pgs <- fread(file = here("Data", "prs_ct_pident.csv"))
head(new_pgs)
new_pgs <- new_pgs |>
  select(pident, SCORE1_AVG_0.2_Z) |> # Note that this is already a z-score
  rename(pgs_ct = SCORE1_AVG_0.2_Z) |>
  mutate()

# Merge with existing dataframe (making sure to not reintroduce excluded relatives -> left_join)
indicators_scaled_wide <- indicators_scaled |>
  pivot_wider(names_from = "indicator", values_from = "value")
complete_df <- indicators_scaled_wide |>
  left_join(new_pgs, by = "pident")
cor(complete_df$pgs_ct, complete_df$PRS) # correlation between PGSs is 0.88 - not too bad...
head(complete_df)
complete_df <- complete_df |>
  pivot_longer(cols = -c(pident, coding, DEP_index), names_to = "indicator", values_to = "value")
head(complete_df, 10)

# Run logistic regression for prediction of depression by new PGS + get AUC
# Assess performance for different predictors
results_scaled <- list(
  assess_performance_flexibly(complete_df, coding_type = "DEP_simple", outcome = "DEP_index", predictors = c("plusminus")),
  assess_performance_flexibly(complete_df, coding_type = "DEP_simple", outcome = "DEP_index", predictors = c("proportion")),
  assess_performance_flexibly(complete_df, coding_type = "DEP_simple", outcome = "DEP_index", predictors = c("FLS")),
  assess_performance_flexibly(complete_df, coding_type = "DEP_simple", outcome = "DEP_index", predictors = c("PAFGRS")),
  assess_performance_flexibly(complete_df, coding_type = "DEP_simple", outcome = "DEP_index", predictors = c("PAFGRSplus")),
  assess_performance_flexibly(complete_df, coding_type = "DEP_simple_with_ANX_simple", outcome = "DEP_index", predictors = c("PAFGRSplus")),
  assess_performance_flexibly(complete_df, coding_type = "DEP_simple_with_ANX_simple", outcome = "DEP_index", predictors = c("PRS")),
  assess_performance_flexibly(complete_df, coding_type = "DEP_simple_with_ANX_simple", outcome = "DEP_index", predictors = c("pgs_ct")),
  assess_performance_flexibly(complete_df, coding_type = "DEP_simple_with_ANX_simple", outcome = "DEP_index", predictors = c("PAFGRSplus", "PRS")),
  assess_performance_flexibly(complete_df, coding_type = "DEP_simple_with_ANX_simple", outcome = "DEP_index", predictors = c("PAFGRSplus", "pgs_ct"))
)
# Convert results list to dataframe
results_list_scaled <- map(results_scaled, "results")
results_list_scaled
results_df_scaled <- do.call(rbind, lapply(results_list_scaled, as.data.frame))
rownames(results_df_scaled) <- NULL
results_df_scaled

# Add column with ORs per SD increase
results_df_scaled <- results_df_scaled |>
  mutate(
    OR = exp(coeff),
    ci_lower = exp(coeff - 1.96 * coeff.SE), # Lower bound of CI
    ci_upper = exp(coeff + 1.96 * coeff.SE) # Upper bound of CI
  )
results_df_scaled

results_df_scaled_for_table <- results_df_scaled |>
  mutate(
    AUC = round(AUC, 3), `SE (AUC)` = round(SE, 3), Coefficient = round(coeff, 3), `SE (Coefficient)` = round(coeff.SE, 3),
    OR = round(OR, 2), `95% CI Lower` = round(ci_lower, 2), `95% CI Upper` = round(ci_upper, 2), `P-value` = pvalue, .keep = "unused"
  )
write.csv(results_df_scaled_for_table, file = here("Int_results", "results_scaled_with_new_pgs.csv"), row.names = FALSE)

# Display item 4
predictors_4a <- c("FH+/-", "Proportion", "FLS", "PAFGRS", "PAFGRS+Sex/Age", "PAFGRS+Sex/Age/ANX", "PGS (PRScs)", "PGS (C+T)", "PAFGRS+Sex/Age/ANX\n& PGS (PRScs)", "PAFGRS+Sex/Age/ANX\n& PGS (C+T)")
predictors_4a <- factor(predictors_4a, levels = predictors_4a)
predictors_4a

data <- results_df_scaled |>
  select(AUC, SE) |>
  filter(!duplicated(AUC)) # Exclude duplicate AUC values in row 10 and 12
data
display_4 <- ggplot(data = data) + # Excluding duplicate AUC values in row 10 and 12
  aes(x = predictors_4a, y = AUC) +
  geom_errorbar(aes(x = predictors_4a, ymin = AUC - SE, ymax = AUC + SE), colour = "#35A0ABFF", width = .2, position = position_nudge(0), size = 1) +
  geom_point(colour = "#35A0ABFF", size = 9 / .pt, shape = 18, position = position_nudge(0)) +
  ylim(0.50, 0.75) +
  ylab("AUC") +
  xlab("") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, size = 9, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 9)
  )
display_4
ggsave(here("Results", "AUC_figure_both_PGS.pdf"), display_4, width = 21, height = 12.7, units = "cm")

# Display item 4b
predictors_4b <- c(
  "FH+/-", "Proportion", "FLS", "PAFGRS", "PAFGRS+Sex/Age", "PAFGRS+Sex/Age/ANX", "PGS (PRScs)", "PGS (C+T)",
  "PAFGRS+Sex/Age/ANX\nwith PGS (PRScs)", "PGS (PRScs) \n with PAFGRS+Sex/Age/ANX", "PAFGRS+Sex/Age/ANX\nwith PGS (C+T)", "PGS (C+T) \nwith PAFGRS+Sex/Age/ANX"
)
predictors_4b <- factor(predictors_4b, levels = predictors_4b)
predictors_4b

head(results_df_scaled)

# position of p-values should be just above top error

display_4b <- ggplot(data = results_df_scaled) +
  aes(x = predictors_4b, y = OR) +
  geom_errorbar(aes(x = predictors_4b, ymin = ci_lower, ymax = ci_upper), colour = "#35A0ABFF", width = .2, position = position_nudge(0), size = 1) +
  geom_point(colour = "#35A0ABFF", size = 9 / .pt, shape = 18, position = position_nudge(0)) +
  # add p-value at 45 degree angle above error bars. round prs to 2 decimals and present in scientific notation, so e.g. 1.23e-12
  geom_text(aes(y = ci_upper + 0.05, label = ifelse(!is.na(pvalue), paste("p =", signif(pvalue, 2)), "")),
    angle = 45, vjust = 0, hjust = 0, size = 3
  ) +
  ylim(1, 3) +
  ylab("Odds Ratio (per SD increase)") +
  xlab("") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, size = 9, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 9)
  )
display_4b
ggsave(here("Results", "OR_figure_both_PGS.pdf"), display_4b, width = 21, height = 12.7, units = "cm")

# Figure 4c
head(complete_df, 10)
df_4c <- complete_df |>
  filter(coding == "DEP_simple_with_ANX_simple") |>
  pivot_wider(names_from = indicator, values_from = value) |>
  dplyr::select(DEP_index, PAFGRSplus, pgs_ct) |>
  filter(!is.na(pgs_ct))

head(df_4c)
head(indicators_scaled)

PGS_PAFGRS_df <- df_4c
variable <- "pgs_ct" # Change to "PAFGRS" for PAFGRS analysis
num_quantiles <- 5 # Number of quantiles to create

head(PGS_PAFGRS_df)
## Plot: Display item 4b
pgs_results_quantiles <- logistic_regression_quantiles(PGS_PAFGRS_df, "pgs_ct", num_quantiles = 5) # You can change the num_quantiles to your desired value
pafgrs_results_quantiles <- logistic_regression_quantiles(PGS_PAFGRS_df, "PAFGRSplus", num_quantiles = 5)
# Table for supplement
head(pgs_results_quantiles)
quantile_details <- cbind(
  pgs_results_quantiles$quantile, pafgrs_results_quantiles$se, pafgrs_results_quantiles$case_nr, pafgrs_results_quantiles$control_nr,
  pgs_results_quantiles$se, pgs_results_quantiles$case_nr, pgs_results_quantiles$control_nr
)

# Combine the results into one data frame
combined_results_quantiles <- bind_rows(pgs_results_quantiles, pafgrs_results_quantiles)
combined_results_quantiles

display_4c <- ggplot(combined_results_quantiles) +
  aes(x = quantile, y = odds_ratio, color = variable) +
  geom_point(size = 9 / .pt, shape = 18, position = position_dodge(0.22)) + # Dodge points horizontally
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, position = position_dodge(0.22), size = 1) + # Thicker error bars
  labs(
    x = "Quantile", y = "Odds Ratio", color = "Genetic Indicator"
  ) +
  scale_color_manual(
    values = c("pgs_ct" = "#3A2C59FF", "PAFGRSplus" = "#5ACCADFF"),
    labels = c("pgs_ct" = "PGS (C+T)", "PAFGRSplus" = "PAFGRS+Sex/Age/ANX")
  ) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 9),
    axis.text = element_text(size = 9),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 9)
  )
display_4c
ggsave(here("Results", "OR_quantiles_both_PGS.pdf"), display_4c, width = 21, height = 12.7, units = "cm")

#### Covariates ####
covariates <- read_sav(file = here("Data", "NESDA_DAP2406", "N1_A100R.sav")) |>
  select(pident, Sexe, Age, aedu)
PCs <- read_sav(file = here("Data", "DAP2406_PRS_pident.sav")) |>
  select(pident, starts_with("PC"))

# Basic checks
attributes(covariates$aedu)
attributes(covariates$Sexe) # do I need to recode to 0 vs 1?
attributes(covariates$Age)
sum(is.na(covariates$Sexe)) # No missing
sum(is.na(covariates$Age)) # No missing
sum(is.na(covariates$aedu)) # 0 missing

# Merge with existing dataframe (making sure to not reintroduce excluded relatives)
covariates <- left_join(covariates, PCs, by = "pident")

df_with_covariates <- complete_df |>
  pivot_wider(names_from = "indicator", values_from = "value") |>
  left_join(covariates, by = "pident") |>
  pivot_longer(cols = -c(pident, coding, DEP_index), names_to = "indicator", values_to = "value")
head(df_with_covariates, 15)

# Assess performance for different predictors
df <- df_with_covariates
results_scaled <- list(
  assess_performance_flexibly_covariates(df, coding_type = "DEP_simple", outcome = "DEP_index", predictors = c("plusminus")),
  assess_performance_flexibly_covariates(df, coding_type = "DEP_simple", outcome = "DEP_index", predictors = c("proportion")),
  assess_performance_flexibly_covariates(df, coding_type = "DEP_simple", outcome = "DEP_index", predictors = c("FLS")),
  assess_performance_flexibly_covariates(df, coding_type = "DEP_simple", outcome = "DEP_index", predictors = c("PAFGRS")),
  assess_performance_flexibly_covariates(df, coding_type = "DEP_simple", outcome = "DEP_index", predictors = c("PAFGRSplus")),
  assess_performance_flexibly_covariates(df, coding_type = "DEP_simple_with_ANX_simple", outcome = "DEP_index", predictors = c("PAFGRSplus")),
  assess_performance_flexibly_covariates(df, coding_type = "DEP_simple_with_ANX_simple", outcome = "DEP_index", predictors = c("PRS")),
  assess_performance_flexibly_covariates(df, coding_type = "DEP_simple_with_ANX_simple", outcome = "DEP_index", predictors = c("PAFGRSplus", "PRS"))
)

# Convert results list to dataframe
results_list_scaled_cov <- map(results_scaled, "results")
results_df_scaled_cov <- do.call(rbind, lapply(results_list_scaled_cov, as.data.frame))
rownames(results_df_scaled_cov) <- NULL
results_df_scaled_cov

map(results_scaled, "summary") # Should I print them for the supplement?

# Add column with ORs per SD increase
results_df_scaled_cov <- results_df_scaled_cov |>
  mutate(
    OR = exp(coeff),
    ci_lower = exp(coeff - 1.96 * coeff.SE), # Lower bound of CI
    ci_upper = exp(coeff + 1.96 * coeff.SE) # Upper bound of CI
  )
results_df_scaled_cov

results_df_scaled_for_table_cov <- results_df_scaled_cov |>
  mutate(
    AUC = round(AUC, 3), `SE (AUC)` = round(SE, 3), Coefficient = round(coeff, 3), `SE (Coefficient)` = round(coeff.SE, 3),
    OR = round(OR, 2), `95% CI Lower` = round(ci_lower, 2), `95% CI Upper` = round(ci_upper, 2), `P-value` = pvalue, .keep = "unused"
  )

write.csv(results_df_scaled_for_table_cov, file = here("Int_results", "results_scaled_with_cov.csv"), row.names = FALSE)

# Display item 4
predictors_4a <- c("FH+/-", "Proportion", "FLS", "PAFGRS", "PAFGRS+Sex/Age", "PAFGRS+Sex/Age/ANX", "PGS (PRScs)", "PAFGRS+Sex/Age/ANX\n& PGS (PRScs)")
predictors_4a <- factor(predictors_4a, levels = predictors_4a)
predictors_4a

data <- results_df_scaled_cov |>
  select(AUC, SE) |>
  filter(!duplicated(AUC)) # Exclude duplicate AUC values in row 10 and 12
data
display_4 <- ggplot(data = data) + # Excluding duplicate AUC values in row 10 and 12
  aes(x = predictors_4a, y = AUC) +
  geom_errorbar(aes(x = predictors_4a, ymin = AUC - SE, ymax = AUC + SE), colour = "#35A0ABFF", width = .2, position = position_nudge(0), size = 1) +
  geom_point(colour = "#35A0ABFF", size = 9 / .pt, shape = 18, position = position_nudge(0)) +
  ylim(0.50, 0.85) +
  ylab("AUC") +
  xlab("") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, size = 9, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 9)
  )
display_4
ggsave(here("Results", "AUC_figure_covariates.pdf"), display_4, width = 21, height = 12.7, units = "cm")

# Display item 4b
predictors_4b <- c(
  "FH+/-", "Proportion", "FLS", "PAFGRS", "PAFGRS+Sex/Age", "PAFGRS+Sex/Age/ANX", "PGS (PRScs)",
  "PAFGRS+Sex/Age/ANX\nwith PGS (PRScs)", "PGS (PRScs) \n with PAFGRS+Sex/Age/ANX"
)
predictors_4b <- factor(predictors_4b, levels = predictors_4b)
predictors_4b


display_4b <- ggplot(data = results_df_scaled_cov) +
  aes(x = predictors_4b, y = OR) +
  geom_errorbar(aes(x = predictors_4b, ymin = ci_lower, ymax = ci_upper), colour = "#35A0ABFF", width = .2, position = position_nudge(0), size = 1) +
  geom_point(colour = "#35A0ABFF", size = 9 / .pt, shape = 18, position = position_nudge(0)) +
  ylim(1, 3) +
  ylab("Odds Ratio (per SD increase)") +
  xlab("") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, size = 9, vjust = 1, hjust = 1),
    axis.text.y = element_text(size = 9)
  )
display_4b
ggsave(here("Results", "OR_figure_covariates.pdf"), display_4b, width = 21, height = 12.7, units = "cm")


unadjusted <- fread(file = here("Int_results", "results_scaled_table.csv"))
# Make table AUC (SE), OR (CI-CI), P-value
unadjusted_table <- unadjusted |>
  mutate(
    AUC_SE = paste0(round(AUC, 3), " (", round(`SE (AUC)`, 3), ")"),
    OR_CI = paste0(round(OR, 2), " (", round(`95% CI Lower`, 2), "-", round(`95% CI Upper`, 2), ")"),
    P_value = `P-value`
  ) |>
  select(AUC_SE, OR_CI, P_value)
unadjusted_table

adjusted <- fread(file = here("Int_results", "results_scaled_with_cov.csv"))
# Make table AUC (SE), OR (CI-CI), P-value
adjusted_table <- adjusted |>
  mutate(
    AUC_SE = paste0(round(AUC, 3), " (", round(`SE (AUC)`, 3), ")"),
    OR_CI = paste0(round(OR, 2), " (", round(`95% CI Lower`, 2), "-", round(`95% CI Upper`, 2), ")"),
    P_value = `P-value`
  ) |>
  select(AUC_SE, OR_CI, P_value)
adjusted_table
combined_table <- cbind(unadjusted_table, adjusted_table)
combined_table
write.csv(combined_table, file = here("Results", "table_comparison_unadjusted_adjusted.csv"), row.names = FALSE)

## Direct comparison uni/multivariate models with PRScs vs. PRScs and PAFGRS together
coding_type <- "DEP_simple_with_ANX_simple"
predictors <- c("PAFGRSplus", "PRS")
outcome <- "DEP_index"

df <- indicators_scaled |>
  filter(coding == coding_type) |>
  pivot_wider(names_from = indicator, values_from = value)

PGS_model <- glm(as.formula(paste(outcome, "~", "PRS")), data = df, family = "binomial")
PAFGRS_model <- glm(as.formula(paste(outcome, "~", "PAFGRSplus")), data = df, family = "binomial")
combined_model <- glm(as.formula(paste(outcome, "~", paste(predictors, collapse = " + "))), data = df, family = "binomial")

summary(PGS_model)
summary(PAFGRS_model)
summary(combined_model)

lmtest::lrtest(PGS_model, combined_model)
lmtest::lrtest(PAFGRS_model, combined_model)
