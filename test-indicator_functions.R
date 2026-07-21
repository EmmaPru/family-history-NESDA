# This script 
# contains tests for functions defined in indicator_functions.R, namely:
# FH_plusminus, case_proportion, apply_pafgrs, FLS, score_FH


install.packages("testthat")
library(testthat)

# Source the functions that are being tested here
source("Scripts/indicator_functions.R") # note this sources also setup.R

# Test FH_plusminus function
test_that("FH_plusminus correctly indicates presence or absence of a disorder", {
  
  # case: incorrect input type for FH, expect error
  expectation_zero <- c(mom_autism = "0", dad_autism = 0)
  expect_error(FH_plusminus(expectation_zero, disorders = c("autism")))
  
  # case: no disorders, expect 0
  expectation_one <- c(mom_autism = 0, dad_autism = 0)
  expect_equal(FH_plusminus(expectation_one, disorders = c("autism")), 0)
  
  # case: some disorders, expect 1
  expectation_two <- c(mother_anxiety = 0, father_schizophrenia = 1)
  expect_equal(FH_plusminus(expectation_two, disorders = c("anxiety", "schizophrenia")), 1)
  
  # case: all disorders, expect 1
  expectation_three <- c(sister_bipolar = 1, brother_bipolar = 1)
  expect_equal(FH_plusminus(expectation_three, disorders = c("bipolar")), 1)
  
  # case: missing values, expect 0
  expectation_four <- c(aunt_depression = NA)
  expect_equal(FH_plusminus(expectation_four, disorders = c("depression")), 0)
  
  # case: some missing values and some cases, expect 1
  expectation_five <- c(aunt_depression = NA, uncle_depression = 1, cousin_depression = 0)
  expect_equal(FH_plusminus(expectation_five, disorders = c("depression")), 1)
  
})


# Test case_proportion function
test_that("case_proportion calculates proportions correctly", {
  # Case 0: bad input, mismatch of disorders and FH
  FH_0 <- c(sib2_ANX = 0, sib2_DEP = 1, sib1_ANX = 1)
  expect_error(case_proportion(FH_0, family_size = 2, method = c("sparse", "dense"), disorders = c("SCZ", "DEP")))
  
  # Case 1: small family, no missings
  FH_1 <- c(sib2_ANX = 0, sib2_DEP = 1, sib1_ANX = 1, sib1_DEP = 0)
  output1 <- case_proportion(FH_1, family_size = 2, method = c("sparse", "dense"), disorders = c("ANX", "DEP"))
  expectation1 <- c("proportion_sparse" = 0.5, "proportion_dense" = 1)
  expect_equal(output1, expectation1)
  
  # Case 2: missings
  FH_2 <- c(sib2_ANX = NA, sib2_DEP = NA, sib1_ANX = 1, sib1_DEP = NA)
  output2 <- case_proportion(FH_2, family_size = 2, method = c("sparse", "dense"), disorders = c("ANX", "DEP"))
  expectation2 <- c("proportion_sparse" = 0.25, "proportion_dense" = 0.5)
  expect_equal(output2, expectation2)
  
  # Case 3: FH with extra columns
  FH3 <- c(sib2_ANX = 0, sib2_DEP = 1, sib1_ANX = 1, sib1_DEP = 0, sib1_SCZ = 1, sib1_age = 45)
  output3 <- case_proportion(FH3, family_size = 2, method = c("sparse", "dense"), disorders = c("ANX", "DEP"))
  expectation3 <- c("proportion_sparse" = 0.5, "proportion_dense" = 1)
  expect_equal(output3, expectation3)
})



# Test apply_pafgrs function
test_that("Testing that pafgrs liabilities are greater when more sibs are affected", {
  # Set constant arguments
  disorder_thresholds <- disorder_thresholds # as loaded by setup.R
  Ks <- Ks[c("DEP")]
  h2ls <- h2ls[c("DEP", "ANX")]
  sexes <- c(sib1 = "F", sib2 = "F", sib3 = "F")
  
  # Test whether more affected sibs lead to greater liability
  # ages is NA for all
  FH_a <- c(sib1_DEP = 1, sib2_DEP = 0, sib3_DEP = NA)
  res_a <- apply_pafgrs(FH = FH_a, Ks = Ks, h2ls = h2ls, sexes = sexes, rgs = rg_SCZ.BIP.DEP.ANX, threshold_list = disorder_thresholds, index_disorder = "DEP")
  FH_b <- c(sib1_DEP = 1, sib2_DEP = NA, sib3_DEP = NA)
  res_b <- apply_pafgrs(FH = FH_b, Ks = Ks, h2ls = h2ls, sexes = sexes, rgs = rg_SCZ.BIP.DEP.ANX, threshold_list = disorder_thresholds, index_disorder = "DEP")
  FH_c <- c(sib1_DEP = 1, sib2_DEP = 1, sib3_DEP = NA)
  res_c <- apply_pafgrs(FH = FH_c, Ks = Ks, h2ls = h2ls, sexes = sexes, rgs = rg_SCZ.BIP.DEP.ANX,threshold_list = disorder_thresholds, index_disorder = "DEP")
  FH_d <- c(sib1_DEP = 1, sib2_DEP = 1, sib3_DEP = 1)
  res_d <- apply_pafgrs(FH = FH_d, Ks = Ks, h2ls = h2ls, sexes = sexes, rgs = rg_SCZ.BIP.DEP.ANX,threshold_list = disorder_thresholds, index_disorder = "DEP")
  
  expect_true(res_a$liability["postM"] < res_b$liability["postM"])
  expect_true(res_b$liability["postM"] < res_c$liability["postM"])
  expect_true(res_c$liability["postM"] < res_d$liability["postM"])
})

test_that("Testing that pafgrs liabilities are greater when sibs are affected also with helper trait", {
  # Set constant arguments
  disorder_thresholds <- disorder_thresholds # as loaded by setup.R
  Ks <- Ks[c("DEP", "ANX")]
  h2ls <- h2ls[c("DEP", "ANX")]
  sexes <- c(sib1 = "F")
  
  # Test whether more affected sibs lead to greater liability
  FH_a <- c(sib1_DEP = 0, sib1_ANX = NA)
  res_a <- apply_pafgrs(FH = FH_a, Ks = Ks, h2ls = h2ls, sexes = sexes, threshold_list = disorder_thresholds, index_disorder = "DEP")
  FH_b <- c(sib1_DEP = 0, sib1_ANX = 1)
  res_b <- apply_pafgrs(FH = FH_b, Ks = Ks, h2ls = h2ls, sexes = sexes, threshold_list = disorder_thresholds, index_disorder = "DEP")
  FH_c <- c(sib1_DEP = 1, sib1_ANX = NA)
  res_c <- apply_pafgrs(FH = FH_c, Ks = Ks, h2ls = h2ls, sexes = sexes, threshold_list = disorder_thresholds, index_disorder = "DEP")
  FH_d <- c(sib1_DEP = 1, sib1_ANX = 1)
  res_d <- apply_pafgrs(FH = FH_d, Ks = Ks, h2ls = h2ls, sexes = sexes, threshold_list = disorder_thresholds, index_disorder = "DEP")
  expect_true(res_a$liability["postM"] < res_b$liability["postM"])
  expect_true(res_b$liability["postM"] < res_c$liability["postM"])
  expect_true(res_c$liability["postM"] < res_d$liability["postM"])
})

test_that("Testing that pafgrs liabilities are responding correctly to age", {
  # Set constant arguments
  disorder_thresholds <- disorder_thresholds # as loaded by setup.R
  Ks <- Ks[c("DEP")]
  h2ls <- h2ls[c("DEP", "ANX")]
  sexes <- c(sib1 = "F")
  
  # Unaffecteds have a smaller, more negative, liability when older
  FH_a <- c(sib1_DEP = 0)
  age_a <- c(sib1 = 20)
  res_a <- apply_pafgrs(FH = FH_a, Ks = Ks, h2ls = h2ls, sexes = sexes, ages = age_a, threshold_list = disorder_thresholds, index_disorder = "DEP")
  FH_b <- c(sib1_DEP = 0)
  age_b <- c(sib1 = 40)
  res_b <- apply_pafgrs(FH = FH_b, Ks = Ks, h2ls = h2ls, sexes = sexes, ages = age_b, threshold_list = disorder_thresholds, index_disorder = "DEP")
  expect_true(res_a$liability["postM"] < 0)
  expect_true(res_a$liability["postM"] > res_b$liability["postM"])
  
  # Affecteds have a smaller liability when older
  FH_c <- c(sib1_DEP = 1)
  age_c <- c(sib1 = 20)
  res_c <- apply_pafgrs(FH = FH_c, Ks = Ks, h2ls = h2ls, sexes = sexes, ages = age_c, threshold_list = disorder_thresholds, index_disorder = "DEP")
  FH_d <- c(sib1_DEP = 1)
  age_d <- c(sib1 = 40)
  res_d <- apply_pafgrs(FH = FH_d, Ks = Ks, h2ls = h2ls, sexes = sexes, ages = age_d, threshold_list = disorder_thresholds, index_disorder = "DEP")
  expect_true(res_d$liability["postM"] > 0)
  expect_true(res_c$liability["postM"] > res_d$liability["postM"])
})


# Test FLS function
test_that("FLS function runs and score increases when affected sib is added", {
  # Assuming values for K_relative, K_sporadic, AO_lower, AO_upper, ppt_age, disorder_FLS 
  # These values should be set according to real use case
  K_relative = K_relative
  K_sporadic = K_sporadic
  AO_lower = AO_lower
  AO_upper = AO_upper
  ppt_age = c(sib1 = 30, sib2 = 35, sib3 = 32)
  disorder_FLS = "DEP"
  
  # 3 siblings with status 1, 0 and NA respectively
  FH <- c(sib1_DEP = 1, sib2_DEP = 0, sib3_DEP = NA)
  siblings_1_0_NA = FLS(FH, K_relative, K_sporadic, AO_lower, AO_upper, ppt_age, disorder_FLS)
  
  # Add one more sibling with status 1: FH_updated
  FH_updated <- c(FH, sib4_DEP = 1)
  ppt_age_updated <- c(ppt_age, sib4 = 28)
  siblings_1_0_NA_plus_one <- FLS(FH_updated, K_relative, K_sporadic, AO_lower, AO_upper, ppt_age_updated, disorder_FLS)
  
  # Add Anxiety
  disorder_FLS <- "DEP.ANX"
  FH_new <- c(FH, sib1_ANX = 1, sib2_ANX = 1, sib3_ANX = 0)
  three_sibs_DEP.ANX <- FLS(FH_new, K_relative, K_sporadic, AO_lower, AO_upper, ppt_age, disorder_FLS)
  
  # Expectations
  expect_true(is.numeric(siblings_1_0_NA))
  expect_true(siblings_1_0_NA < siblings_1_0_NA_plus_one, info = "Adding a sibling with status 1 should increase FLS")
  expect_true(siblings_1_0_NA < three_sibs_DEP.ANX, info = "Adding anxiety should increase FLS")
})


# Test score_FH function
# This function calculates a selection of indicators for a dataframe with one family per row
# 
# Expectations:
# - The function should return a dataframe with the same number of rows as the input dataframe
# - The dataframe should contain the columns included in methods argument
# - For any row of the dataframe, the individual indicator function gives the same result as score_FH for the respective method

# Test case_proportion function
test_that("case_proportion calculates proportions correctly", {
  # Case 0: bad input, mismatch of disorders and FH
  #FH_0 <- c(sib2_ANX = 0, sib2_DEP = 1, sib1_ANX = 1)
  #expect_error(score_FH(FH_0, family_size = 2, method = c("sparse", "dense"), disorders = c("SCZ", "DEP")))
  
  # Case 1: small family, no missings
  FH_1 <- c(sib2_ANX = 0, sib2_DEP = 1, sib1_ANX = 1, sib1_DEP = 0)
  output1 <- score_FH(FH_1, family_size = 2, method = c("sparse", "dense"), disorders = c("ANX", "DEP"))
  expectation1 <- c("proportion_sparse" = 0.5, "proportion_dense" = 1)
  expect_equal(output1, expectation1)
  
  # Case 2: missings
  FH_2 <- c(sib2_ANX = NA, sib2_DEP = NA, sib1_ANX = 1, sib1_DEP = NA)
  output2 <- case_proportion(FH_2, family_size = 2, method = c("sparse", "dense"), disorders = c("ANX", "DEP"))
  expectation2 <- c("proportion_sparse" = 0.25, "proportion_dense" = 0.5)
  expect_equal(output2, expectation2)
  
  # Case 3: FH with extra columns
  FH3 <- c(sib2_ANX = 0, sib2_DEP = 1, sib1_ANX = 1, sib1_DEP = 0, sib1_SCZ = 1, sib1_age = 45)
  output3 <- case_proportion(FH3, family_size = 2, method = c("sparse", "dense"), disorders = c("ANX", "DEP"))
  expectation3 <- c("proportion_sparse" = 0.5, "proportion_dense" = 1)
  expect_equal(output3, expectation3)
})

