# Setup for LT-FH project
# devtools::install_github("BioPsyk/PAFGRS")

# Special installs for snellius (R version 4.3.2):
# -
# Install remotes if you don't have it
# install.packages("remotes")
# Install older versions of Matrix and MASS
# remotes::install_version("Matrix", version = "1.6-0", repos = "http://cran.us.r-project.org")
# remotes::install_version("MASS", version = "7.3-60", repos = "http://cran.us.r-project.org")
# -

#### Install and load packages ####
## First specify the packages of interest
packages <- c(
  "data.table",
  "devtools"
  # , "flextable" # installation error
  , "haven",
  "here", "locfit" ## for smoothing with locfit)
  , "Matrix", "MASS" # older versions for snellius
  , "mvnfast",
  "PAFGRS", "patchwork",
  "pROC", "psych",
  "purrr", "reshape2", "stringr", "tidyverse",
  "viridis"
)
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


#### Fixed parameters ####
# Population prevalence
Ks <- c(DEP = 0.19, ANX = 0.2, SCZ = 0.005, BIP = 0.013) # prevalence in Netherlands from NEMESIS-2

Ks_female <- c(DEP = 0.243, ANX = 0.234)
Ks_male <- c(DEP = 0.131, ANX = 0.159)


# Prevalence by sex and age from mcgrath et al. 2023
age_points <- c(0, 15, 30, 45, 60, 75, 140) # Note: age > 75 -> K(75). 
# From inspecting visually, the curve displays reasonable estimates up to age 110.
# The last value in prevalence vectors below, corresponding to age 140, is a reasonable value selected for modelling purposes (not contained in the literature).
K_DEP_female <- c(0, 0.034, 0.168, 0.256, 0.309, 0.34, 0.35) # cumulative incidence of MDD in females.
K_DEP_male <- c(0, 0.02, 0.096, 0.148, 0.184, 0.201, 0.211) # cumulative incidence of MDD in males.
K_ANX_female <- c(0, 0.130, 0.205, 0.257, 0.293, 0.31, 0.32) # cumulative incidence of ANX in females.
K_ANX_male <- c(0, 0.076, 0.12, 0.15, 0.17, 0.183, 0.193) # cumulative incidence of ANX in males.

# Scale to NL lifetime prevalence
# Doubt: population prevalence is not the same as lifetime prevalence at oldest age.
DEP_scaler_male <- 0.131 / 0.184
DEP_scaler_female <- 0.243 / 0.309
ANX_scaler_male <- 0.159 / 0.17
ANX_scaler_female <- 0.234 / 0.293

# K info in one df
K_info <- as.data.frame(cbind(age_points, K_DEP_female * DEP_scaler_female, K_DEP_male * DEP_scaler_male, K_ANX_female * ANX_scaler_female, K_ANX_male * ANX_scaler_male))
colnames(K_info) <- c("ages", "DEP_female", "DEP_male", "ANX_female", "ANX_male")

# # Pre-calculate thresholds for full integer ages between 1 and 140
#disorder_thresholds <- list()
#disorder_thresholds[["DEP"]][["F"]] <- get_thresholds(ppt_age = c(1:140, NA), ppt_sex = rep("F", 141), K_info = K_info, Ks_male = Ks_male, Ks_female = Ks_female, disorder = "DEP")
#disorder_thresholds[["DEP"]][["M"]] <- get_thresholds(ppt_age = c(1:140, NA), ppt_sex = rep("M", 141), K_info = K_info, Ks_male = Ks_male, Ks_female = Ks_female, disorder = "DEP")
#disorder_thresholds[["ANX"]][["F"]] <- get_thresholds(ppt_age = c(1:140, NA), ppt_sex = rep("F", 141), K_info = K_info, Ks_male = Ks_male, Ks_female = Ks_female, disorder = "ANX")
#disorder_thresholds[["ANX"]][["M"]] <- get_thresholds(ppt_age = c(1:140, NA), ppt_sex = rep("M", 141), K_info = K_info, Ks_male = Ks_male, Ks_female = Ks_female, disorder = "ANX")
#save(disorder_thresholds, file = here("Int_results", "disorder_thresholds.RData"))
load(here("Int_results", "disorder_thresholds.RData"))


# Heritabilities
h2ls <- c(DEP = 0.4, ANX = 0.4, SCZ = 0.77, BIP = 0.67) # twin heritability estimates from Polderman et al.

# Genetic correlations
rg_SCZ.BIP.DEP.ANX <- matrix(c( # ldsc regression genetic correlation estimates from Grotzinger et al.
  1, 0.682, 0.357, 0.367,
  0.682, 1, 0.343, 0.297,
  0.357, 0.343, 1, 0.867,
  0.367, 0.357, 0.867, 1
), 4, 4)
rownames(rg_SCZ.BIP.DEP.ANX) <- colnames(rg_SCZ.BIP.DEP.ANX) <- c("SCZ", "BIP", "DEP", "ANX")
rg_DEP.ANX <- rg_SCZ.BIP.DEP.ANX[c("DEP", "ANX"), c("DEP", "ANX")]
rg_SCZ.BIP <- rg_SCZ.BIP.DEP.ANX[c("SCZ", "BIP"), c("SCZ", "BIP")]
rg_DEP.ANX.BIP <- rg_SCZ.BIP.DEP.ANX[c("DEP", "ANX", "BIP"), c("DEP", "ANX", "BIP")]

# FLS parameters - values from Eleonore's script
# K_population <- c(DEP = 0.187) # population prevalence of depression
K_relative <- c(DEP = 0.4, ANX = 0.36, DEP.ANX = 0.5) # relative risk of depression in first degree relatives (parameter a)
K_sporadic <- c(DEP = 0.0935, ANX = 0.074, DEP.ANX = 0.134) # sporadic prevalence of depression (parameter b)
AO_lower <- c(DEP = 10, ANX = 5, DEP.ANX = 5) # lower limit of onset range (parameter c)
AO_upper <- c(DEP = 65, ANX = 50, DEP.ANX = 65) # upper limit of onset range (parameter d)


# Types of diagnostic data
types <- c("DEP_elaborate", "DEP_simple", "DEP_elaborate_0imputed", "DEP_simple_0imputed", "DEP_2020paper")
types_ANX <- c("DEP_simple_with_ANX_simple", "DEP_elaborate_with_ANX_elaborate")
all_types <- c(types, types_ANX)

predictors <- c("plusminus", "proportion", "FLS", "PAFGRS", "PAFGRSplus", "PRS")

#### Additional functions ####

# Function to build the original covariance matrix, with cov of g, e, l for all fam_members and the means
# # Example
# fam_members <- c("index", "sib1", "sib2", "f", "m")
# h2ls <- c(DEP = 0.35, ANX = 0.4) # named!!!!
# rg <- 1
# means <- build_initial(fam_members = fam_members, h2ls = h2l, rg = rg)$means
# sigma <- build_initial(fam_members = fam_members, h2ls = h2l, rg = rg)$sigma
# # #
build_initial <- function(fam_members, h2ls, rg) {
  # fam_members is a vector with names of family members, in the format of c("sib1", "sib2", "sibn"). for now only supports first degree relatives.
  # h2ls is a named vector of heritabilities in the format of c(disorderA = x, disorderB = y)
  # rg is the square matrix of genetic correlations between the disorders. in the case of one disorder only, rg = 1
  # Output: list with sigma and means
  # Format of sigma is
  fam_size <- length(fam_members)
  disorders <- names(h2ls)
  disorder_num <- length(disorders)
  # Disorder characteristics
  re <- rg # for now try the code with environmental correlation same as genetic correlation

  # Name objects
  names <- paste(rep(fam_members, each = length(disorders)), disorders, sep = "_") # fam_member name combined with disorder name, sorted by fam_member rather than disorder
  names <- paste(rep(c("g", "e", "l"), each = fam_size * disorder_num), names, sep = "_") # add g, e, l

  # Means: zero
  means <- rep(0, 3 * fam_size * disorder_num)
  names(means) <- names

  # Covariance matrix components
  # relatedness coefficients: .5 for sib pairs, 1 for self
  relatedness <- matrix(.5, fam_size, fam_size)
  diag(relatedness) <- 1
  shared_e <- diag(fam_size) # shared environment, assume 0 between sibs

  cov_g_oneperson <- diag(sqrt(h2ls), nrow = disorder_num) %*% rg %*% diag(sqrt(h2ls), nrow = disorder_num) # genetic covariance across disorders but within person. specify nrow for the case that x is a scalar
  cov_g <- relatedness %x% cov_g_oneperson # genetic covariance across disorders and for the whole family

  cov_e_oneperson <- diag(sqrt(1 - h2ls), nrow = disorder_num) %*% re %*% diag(sqrt(1 - h2ls), nrow = disorder_num)
  cov_e <- shared_e %x% cov_e_oneperson # environmental covariance matrix

  cov_ge <- matrix(0, dim(cov_g)[1], dim(cov_g)[1]) # rGE: zero in the population

  cov_l <- cov_g + cov_e # cov of full liabilities. is this right? yes when cov_ge = 0?
  cov_gl <- cov_g
  cov_el <- cov_e

  # Full sigma
  sigma <- cbind(rbind(cov_g, cov_ge, cov_gl), rbind(cov_ge, cov_e, cov_el), rbind(cov_gl, cov_el, cov_l))
  rownames(sigma) <- colnames(sigma) <- names

  return(list(sigma = sigma, means = means))
}


# To use in simulation
build_thresholds <- function(Ks, fam_members) {
  # Ks is a named vector of population prevalences in the format of c(disorderA = x, disorderB = y)
  # fam_members is a vector with names of family members, in the format of c("sib1", "sib2", "sibn"). for now only supports first degree relatives.
  # later on fam_members can become age vector with family_members as names
  disorders <- names(Ks)
  fam_dis <- paste(rep(fam_members, each = length(Ks)), disorders, sep = "_")
  thresholds <- -qnorm(Ks, 0, 1)
  threshold_vector <- rep(thresholds, length(fam_members))
  names(threshold_vector) <- fam_dis
  return(threshold_vector)
}

# To use for making thresholds from real data that has at least info on sex
get_thresholds <- function(ppt_age, ppt_sex, K_info, Ks_male, Ks_female, disorder) {
  # Function that turns a combination of age and disorder prevalence by age into the correct threshold
  # Input:
  # ppt_age is vector of ages.
  # ppt_sex is vector of sexes.
  # K_info is df of age points with their prevalence.
  # Ks_male is named vector of disorder prevalence in males only
  # Ks_female is named vector of disorder prevalence in females only
  # disorder is name of disorder
  # Output:  ppt_thresholds: vector of thresholds

  # define functions to draw Ks from
  female_function <- splinefun(K_info[, "ages"], K_info[, paste0(disorder, "_female")], method = "natural")
  male_function <- splinefun(K_info[, "ages"], K_info[, paste0(disorder, "_male")], method = "natural")

  # conditions
  # we don't allow for sex = NA, this will not exist in NESDA family data
  # if age is na add average K for this sex
  ppt_thresholds <- c()
  for (i in 1:length(ppt_age)) {
    age <- ppt_age[i]
    sex <- ppt_sex[i]

    if (is.na(age) & sex == "F") {
      ppt_thresholds[i] <- -qnorm(Ks_female[disorder])
    } else if (sex == "F") {
      ppt_thresholds[i] <- -qnorm(female_function(age))
    } else if (is.na(age) & sex == "M") {
      ppt_thresholds[i] <- -qnorm(Ks_male[disorder])
    } else if (sex == "M") {
      ppt_thresholds[i] <- -qnorm(male_function(age))
    }
  }
  return(ppt_thresholds)
}



simulate_FH <- function(index_disorder, Ks, h2ls, rg, sim_reps, sim_size, fam_members) {
  # requires build_thresholds(), build_initial(), assess_performance()
  # requires all indicator functions
  # index_disorder is a character string, the name of the disorder for which prediction is to be assessed
  # Ks is a named vector of population prevalences in the format of c(disorderA = x, disorderB = y)
  # h2ls is a named vector of heritabilities in the format of c(disorderA = x, disorderB = y)
  # rg is the square matrix of genetic correlations between the disorders. in the case of one disorder only, rg = 1
  # sim_reps is the number of times the simulation is repeated in a loop
  # sim_size is the number of individuals that are simulated as supplied to rmvn()
  # fam_members is a vector with names of family members, in the format of c("sib1", "sib2", "sibn"). for now only supports first degree relatives.

  thresholds <- build_thresholds(Ks, fam_members)
  fam_dis <- names(thresholds)

  g_names <- paste(c("g"), rep(fam_dis), sep = "_")
  e_names <- paste(c("e"), rep(fam_dis), sep = "_")

  # Build sigma and means for simulation
  initial <- build_initial(fam_members, h2ls, rg)
  means_sim <- initial$means[c(g_names, e_names)]
  sigma_sim <- initial$sigma[c(g_names, e_names), c(g_names, e_names)]

  # Simulate in a loop
  output <- data.frame()
  temp_list <- list()
  for (i_run in 1:sim_reps) {
    samples <- as.data.frame(rmvn(n = sim_size, mu = means_sim, sigma = sigma_sim))
    colnames(samples) <- c(names(means_sim))

    # create liability and disease status columns
    for (i in fam_dis) {
      liability <- paste("l", i, sep = "_")
      samples[liability] <- samples[paste("g", i, sep = "_")] + samples[paste("e", i, sep = "_")] # build liability

      Disease <- i
      samples[Disease] <- rep(0, sim_size) # create Disease column
      samples[Disease][samples[liability] >= thresholds[i]] <- 1 # change disease status to 1 if liability is above threshold
    }

    # calculate indicators
    Disease_cols <- fam_dis[-c(1:length(Ks))]
    plusminus_indicator <- as.data.frame(cbind(FH_plusminus(FH = samples[, Disease_cols, drop = FALSE])))
    proportion_indicator <- as.data.frame(cbind(FH_proportion(FH = samples[, Disease_cols, drop = FALSE])))
    dense_prop_indicator <- as.data.frame(cbind(FH_proportion_dense(FH = samples[, Disease_cols, drop = FALSE])))
    pa_fgrs_indicator <- as.data.frame(t(apply_pafgrs(FH = samples[, Disease_cols, drop = FALSE], Ks = Ks, h2ls = h2ls)))

    # assess performance
    index_labels <- paste("index", index_disorder, sep = "_")
    K <- Ks[index_disorder]

    plusminus.performance <- unlist(lapply(index_labels, function(x) assess_performance_dichot(true_status = samples[[x]], predictor = plusminus_indicator$V1, K)))
    names(plusminus.performance) <- paste("FH", index_disorder, "lm.status.plusminus", names(plusminus.performance), sep = "_")

    proportion.performance <- unlist(lapply(index_labels, function(x) assess_performance_dichot(true_status = samples[[x]], predictor = proportion_indicator$V1, K)))
    names(proportion.performance) <- paste("FH", index_disorder, "lm.status.proportion", names(proportion.performance), sep = "_")

    denseprop.performance <- unlist(lapply(index_labels, function(x) assess_performance_dichot(true_status = samples[[x]], predictor = dense_prop_indicator$V1, K)))
    names(denseprop.performance) <- paste("FH", index_disorder, "lm.status.denseprop", names(denseprop.performance), sep = "_")

    pafgrs.performance <- unlist(lapply(index_labels, function(x) assess_performance_dichot(true_status = samples[[x]], predictor = pa_fgrs_indicator$postM, K)))
    names(pafgrs.performance) <- paste("PAFGRS", index_disorder, "lm.status.muliab", names(pafgrs.performance), sep = "_")
    #
    # prepare output
    temp_list[[length(temp_list) + 1]] <- cbind(
      rbind(liab.performance), rbind(gliab.performance), rbind(prob.performance), rbind(plusminus.performance),
      rbind(proportion.performance), rbind(denseprop.performance), rbind(pafgrs.performance)
    )
  }

  output <- as.data.frame(do.call("rbind", temp_list))
  return(output)
}



# Run all
full_sim <- function(sim.input.list = sim.input.list) {
  # requires sumulate_FH and all its dependencies
  # sim.input.list is a list, each item contains all the arguments needed for a simulation, format is list(name="DEP, 1 fam member", index_disorder = "DEP", Ks = c(DEP = 0.19), h2ls = c(DEP = 0.4), rg = 1, sim_reps = sim_reps, sim_size = sim_size, fam_members = c("index", "sib1"))
  outcome_list <- list()

  for (i_list in 1:length(sim.input.list)) { # runs once for each list element

    list2env(sim.input.list[[i_list]], env = environment())

    ## run simulation
    ## outcome: all results from 100 runs dim= c(100,~20)
    outcome <- simulate_FH(index_disorder = index_disorder, Ks = Ks, h2ls = h2ls, rg = rg, sim_reps = sim_reps, sim_size = sim_size, fam_members = fam_members)

    ## summarise simulation
    ## outcome_meanse: mean over 100 runs, dim= c(2,~20)
    means <- colMeans(outcome)
    ses <- sapply(colnames(outcome), function(x) sd(outcome[, x]) / sqrt(nrow(outcome)))
    outcome_meanse <- rbind(means, ses)
    rownames(outcome_meanse) <- c("mean", "se")

    outcome_list[[length(outcome_list) + 1]] <- list(outcome = outcome, outcome_meanse = outcome_meanse, input = unlist(sim.input.list[[i_list]]))
  }

  return(outcome_list = outcome_list)
}



# data = FH; methods = c("plusminus", "proportion", "FLS", "PAFGRS"); disorders = c("DEP"); Ks = c(DEP = 0.2); h2l = c(DEP = 0.4); rg = 1; a = a; b = b; c = c; d = d
# score_FH(data = ped_data, methods = c("plusminus", "proportion", "proportion_dense", "FLS", "PAFGRS"), disorders = c("cancer"), Ks = c(cancer = 0.2), h2l = c(cancer = 0.4),
#        rg = 1, a = a, b = b, c = c, d = d, K_info = K_info, Ks_male = Ks_male, Ks_female = Ks_female)


#### Assess performance ####

# Wouter's function to calculate ICI
f_ICI <- function(Y, P) {
  # Y is the observed values (vector), P is the predicted values (vector)
  na.index <- is.na(Y) | is.na(P)
  Y <- Y[na.index == FALSE]
  P <- P[na.index == FALSE]
  loess.calibrate <- locfit(Y ~ P) ## Austin PC. The Integrated Calibration Index (ICI) and related metrics for quantifying the calibration of logistic regression models. Statistics in Medicine 2019.pdf
  P.calibrate <- predict(loess.calibrate, newdata = P)
  ICI <- mean(abs(P.calibrate - P), na.rm = TRUE) ## very rarely NA, when e.g. prob_scz=prob_bip=0
  return(ICI)
}


# Convert r-squared from observed to liability scale
# From Wouter
r2o_to_r2l <- function(K, P, r2o) {
  # K is the prevalence of the disorder
  # P is the proportion of cases in the case-control sample
  # r2o is R2 on the observed scale
  ## Lee et al. 2012 Genet Epidemiology
  t <- -qnorm(K, mean = 0, sd = 1) # disease threshold
  z <- dnorm(t) # height of the normal distribution at T
  i1 <- z / K # mean liability of A1 (eg Falconer and Mackay)
  k1 <- i1 * (i1 - t) # reduction in variance in A1
  i0 <- -z / (1 - K) # mean liability of A0
  k0 <- i0 * (i0 - t) # reduction in variance in A0

  theta <- i1 * (P - K) / (1 - K) * (i1 * (P - K) / (1 - K) - t) # theta in equation (15) Lee et al. 2012 Genet Epidemiology
  cv <- K * (1 - K) / z^2 * K * (1 - K) / (P * (1 - P)) # C in equation (15)
  R2 <- r2o * cv / (1 + r2o * theta * cv)

  return(R2)
}


assess_performance_cont <- function(true_status, predictor) {
  # for continuous outcomes
  # true_status and predictor are both vectors, both may contain continuous values. e.g. true_status may be the true liability and predictor the mean liability calculated from LT_FH
  intercept <- summary(lm(true_status ~ predictor))$coefficients[1, 1] # slope and intercept of lm(true_status ~ predictor)
  slope <- summary(lm(true_status ~ predictor))$coefficients[2, 1]
  r_squared <- summary(lm(true_status ~ predictor))$r.squared

  return(c(slope = slope, inter = intercept, r2 = r_squared))
}

assess_performance_dichot <- function(true_status, predictor, K) {
  # for dichotomous type of outcome, so e.g. case/control
  # true_status and predictor are both vectors. true_status is a vector of cases (1) and controls (0), whereas predictor may contain continuous values e.g. probabilities
  model <- summary(lm(true_status ~ predictor))
  intercept <- model$coefficients[1, 1] # slope and intercept of lm(true_status ~ predictor)
  slope <- model$coefficients[2, 1]
  r2o <- model$r.squared
  P <- sum(true_status) / length(true_status)
  r2l <- r2o_to_r2l(K, P, r2o)

  roc_obj <- roc(true_status, predictor)
  AUC <- auc(roc_obj)
  AUC.SE <- sqrt(var(roc_obj))

  # Error correction: if f_ICI fails, ICI becomes NA
  ICI <- try(f_ICI(Y = true_status, P = predictor), silent = TRUE)
  if (class(ICI) == "try-error") {
    ICI <- NA
  }

  # ICI <- f_ICI(Y = true_status, P = predictor)

  return(c(slope = slope, inter = intercept, r2l = as.numeric(r2l), r2o = r2o, AUC = as.numeric(AUC), AUC.SE = AUC.SE, ICI = ICI))
}


# Data prep
# Function to compute AUC and AUC.SE for a combined model
assess_performance_flexibly <- function(data, coding_type = "DEP_simple_with_ANX_simple",
                                        outcome = "DEP_index", predictors = c("PAFGRSplus", "PRS")) {
  df <- data |>
    filter(coding == coding_type) |>
    pivot_wider(names_from = indicator, values_from = value)

  formula_str <- paste(outcome, "~", paste(predictors, collapse = " + "))
  model <- glm(as.formula(formula_str), data = df, family = "binomial")
  pred_probs <- predict(model, type = "response")

  roc_obj <- roc(df[[outcome]], pred_probs)
  AUC <- auc(roc_obj)
  AUC.SE <- sqrt(pROC::var(roc_obj))

  results <- list(
    coding_type = coding_type,
    predictors = paste(predictors, collapse = " + "),
    AUC = as.numeric(AUC),
    SE = as.numeric(AUC.SE),
    coeff = summary(model)$coefficients[predictors, "Estimate"],
    coeff.SE = summary(model)$coefficients[predictors, "Std. Error"],
    pvalue = summary(model)$coefficients[predictors, "Pr(>|z|)"]
  )

  return(list(results = results, model = model, roc_obj = roc_obj))
}


cor_test_against_value <- function(x, y, rho0 = 1, alpha = 0.05) {
  # Function to perform a correlation test against a specified value (rho0)
  # Written by Claude
  # x: numeric vector
  # y: numeric vector
  # rho0: value to test against (default is 1)
  # alpha: significance level (default is 0.05)

  # Calculate sample correlation
  r <- cor(x, y)
  n <- length(x)

  # Special handling for rho0 = 1
  if (rho0 == 1) {
    # Use a value very close to 1 instead
    rho0 <- 0.9999
    warning("Using rho0 = 0.9999 instead of 1 to avoid numerical issues")
  }

  # Fisher's z-transformation
  z_r <- 0.5 * log((1 + r) / (1 - r))
  z_rho0 <- 0.5 * log((1 + rho0) / (1 - rho0))

  se <- 1 / sqrt(n - 3)
  z_stat <- (z_r - z_rho0) / se

  # More precise p-value calculation
  p_value <- 2 * pnorm(-abs(z_stat))
  # Results
  list(
    sample_cor = r,
    n_complete = n,
    se = se,
    test_statistic = z_stat,
    p_value = p_value,
    significant = p_value < alpha
  )
}



draw_heatmap <- function(cor_df, title = NULL, subtitle = NULL, legend_title = "Correlation") {
  # Draws a lower-triangle heatmap (e.g. from a correlation matrix).
  # Inputs:
  # - cor_df: a symmetric matrix (e.g., correlation matrix)
  # - title: optional plot title
  # - subtitle: optional plot subtitle
  # Output:
  # - a ggplot2 heatmap object

  # Define variable order to align heatmap axes
  vars_order <- colnames(cor_df)

  # Reshape the matrix to long format and set proper factor levels
  corr_long <- as.data.frame(as.table(cor_df)) |>
    rename(Var1 = Var1, Var2 = Var2, Correlation = Freq) |>
    mutate(
      Correlation = as.numeric(Correlation),
      Var1 = factor(Var1, levels = vars_order),
      Var2 = factor(Var2, levels = rev(vars_order)), # reverse Y for top-left to bottom-right diagonal
      # Mask upper triangle (above diagonal) by setting to NA
      Correlation = ifelse(
        as.numeric(Var1) + as.numeric(Var2) <= length(vars_order) + 1,
        Correlation,
        NA_real_
      )
    )

  # Create heatmap
  ggplot(corr_long, aes(x = Var1, y = Var2, fill = Correlation)) +
    geom_tile(color = "white", linewidth = 1.5) +
    geom_text(
      aes(label = ifelse(is.na(Correlation), "", round(Correlation, 3))),
      color = "black", size = 9 / .pt
    ) +
    scale_fill_viridis(na.value = "white", direction = -1, begin = 0.65, end = 1, option = "G", name = legend_title) +
    labs(
      x = NULL, y = NULL
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(size = 9, angle = 45, hjust = 1),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 9),
      plot.subtitle = element_text(size = 9),
      axis.title = element_text(size = 9, face = "bold"),
      legend.position = c(0.85, 0.8), # adjust as needed
      legend.justification = "center", # or "right" / "top" for fine control
      legend.background = element_rect(fill = "white", color = "grey80"),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 9)
    )
}


# Function to flip the column names # From ChatGPT
flip_colnames <- function(name) {
  parts <- str_split(name, "_", simplify = TRUE)
  if (length(parts) == 2) {
    return(paste(parts[2], parts[1], sep = "_"))
  }
  return(name)
}


plot_AUC <- function(df, groups, title) {
  # Function to generate AUC plot with error bars
  # Input: df (data frame with AUC and SE), title (title of the plot as string)
  ggplot(data = df) +
    geom_errorbar(aes(x = groups, ymin = AUC - SE, ymax = AUC + SE), width = .2, position = position_nudge(-.22)) +
    geom_point(aes(x = groups, y = AUC), colour = "#09ADC9", size = 6, shape = 18, position = position_nudge(-.22)) +
    ylim(0.55, 0.68) +
    ylab("AUC") +
    xlab("") +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, size = 12, vjust = 1, hjust = 1),
      axis.text.y = element_text(size = 12),
      title = element_text(size = 14)
    ) +
    ggtitle(title)
}

