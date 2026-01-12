# Load packages
# packages in alphabetical order
packages <- c("stringr", "testit")

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

source("Scripts/setup.R")

# The general idea of the individual indicator functions is that they take as main input data one row containing all relevant diagnoses (may be one or multiple disorders)
# So there usually need to be some input preparation steps before-hand, isolating the relevant columns
# The results should not be impacted by columns with relevant names that contain NA (for convenience because we may have one dataframe for different family sizes)
# For PAFGRS, NAs are okay.
# For FLS NAs are okay because they get ignored in the product of likelihood ratios.
# For FH plusminus NAs are okay because they get ignored in the sum of cases.
# For proportion NAs are a bit of a problem because we explicitly need family size information. I solve this by adding explicit family size input to this function.

# Function to give indicator of presence or absence of an psychiatric disorder in family members
FH_plusminus <- function(FH = NULL, disorders = NULL) {
  # FH is a named vector (for one individual) of the disorder status of relatives, excluding index individual. 0 is unaffected, 1 is affected
  # naming format fammember1_disorderA, fammember1_disorderB, fammember2_disorderA etc., e.g. FH <- c(sib2_ANX = 0, sib2_DEP = 1)
  # disorders is a character vector with the names of the disorders in the FH data

  # Simple checks of input format
  assert("FH is not in the correct format. Please provide a named vector of the disorder status of relatives.", {
    is.numeric(FH)
    !is.null(FH)
    is.vector(FH)
    is.character(names(FH))
  })

  assert(
    "disorders is not in the correct format. Please provide a character vector with the names of the disorders in the FH data.",
    is.character(disorders),
    all(disorders %in% str_split_i(names(FH), "_", 2))
  ) # check if all disorders are in the FH data

  # isolate the relevant columns
  fam_members <- unique(str_split_i(names(FH), "_", 1))
  disorder_data <- FH[paste0(rep(fam_members, each = length(disorders)), "_", disorders)]

  # Calculate indicator
  plusminus <- as.numeric(sum(disorder_data, na.rm = TRUE) > 0) # is the sum larger than 0

  return(plusminus)
}



case_proportion <- function(FH, family_size = length(unique(str_split_i(names(FH), "_", 1))), method = c("sparse", "dense"), disorders) {
  # Function to get proportional family history indicator
  # sparse version calculates case proportion of diagnoses, dense version calculates proportion of persons affected.
  # methods sparse vs dense give different results when multiple disorders are included. for instance,

  # if a person has 2 affected siblings with different disorders, the sparse version would count 2 cases, the dense version would count 1 case.

  # FH is a named vector (for one individual) of the disorder status of relatives, excluding index individual. 0 is unaffected, 1 is affected
  # naming format fammember1_disorderA, fammember1_disorderB, fammember2_disorderA etc., e.g. FH <- c(sib2_ANX = 0, sib2_DEP = 1)
  # family_size is a single integer. as default use number of fammembers as extracted from column names
  # method is a character vector with values "sparse"" and "dense". Default is both.
  # disorders is a character vector with the names of the disorders in the FH data

  # Assert fundamental assumptions about input format
  # assert("FH is not in the correct format. Please provide a named vector of the disorder status of relatives.",
  #        !is.null(FH), is.character(names(FH)))

  assert(
    "family_size is not in the correct format. Please provide a single integer.",
    is.numeric(family_size), length(family_size) == 1,
    family_size >= 1
  ) # family size should be at least 1

  assert(
    "method is not in the correct format. Please provide a character vector with values 'sparse' and/or 'dense'.",
    is.character(method), all(method %in% c("sparse", "dense"))
  )

  assert(
    "disorders is not in the correct format. Please provide a character vector with the names of the disorders in the FH data.",
    is.character(disorders),
    all(disorders %in% str_split_i(names(FH), "_", 2))
  ) # check if all disorders are in the FH data

  # isolate the relevant columns
  fam_members <- unique(str_split_i(names(FH), "_", 1))
  disorder_data <- FH[paste0(rep(fam_members, each = length(disorders)), "_", disorders)]


  # Calculate FH indicators
  result <- c() # initialize result vector
  # Sparse version: calculate proportion of diagnoses
  if ("sparse" %in% method) {
    affected_nr <- sum(as.numeric(disorder_data), na.rm = TRUE) # sum of affected relatives
    nr_possible_diagnoses <- as.numeric(family_size) * length(disorders) # number of possible diagnoses
    result["proportion_sparse"] <- affected_nr / nr_possible_diagnoses
  }
  # Dense version: calculate proportion of persons affected
  if ("dense" %in% method) {
    FH_dense <- c()
    for (i in fam_members) {
      their_columns <- disorder_data[grepl(i, names(disorder_data))] # get all columns for one family member
      FH_dense[i] <- as.numeric(sum(their_columns, na.rm = TRUE) > 0) # for each family member, add 1 if they're affected
    }
    affected_nr <- sum(FH_dense, na.rm = TRUE) # sum of affected relatives
    result["proportion_dense"] <- affected_nr / family_size
  }

  return(result)
}



apply_pafgrs <- function(FH, Ks, h2ls, rgs = rg_SCZ.BIP.DEP.ANX, ages = NULL, sexes = NULL, threshold_list = disorder_thresholds, index_disorder = "DEP") {
  # Function to calculate pafgrs indicator
  # Input:
  # FH is a named vector (for one individual) of the disorder status of relatives, excluding index individual. 0 is unaffected, 1 is affected
  # naming format fammember1_disorderA, fammember1_disorderB, fammember2_disorderA etc., e.g. FH <- c(sib2_ANX = 0, sib2_DEP = 1)
  # Ks is a named vector of population prevalences in the format of c(disorderA = x, disorderB = y). it contains only the disorders relevant for prediction, i.e. index and helper traits
  # h2ls is a named vector of heritabilities in the format of c(disorderA = x, disorderB = y)
  # rgs is a named matrix of genetic correlations, for ANX/DEP/SCZ/BIP it is loaded by setup.R
  # ages is a named vector of ages of family members in the format of c(fammember1 = x, fammember2 = y)
  # sexes is a named vector of sexes of family members in the format of c(fammember1 = x, fammember2 = y)
  # thresholds is a list of age- and sex-specific thresholds for each disorder in the format of list(disorderA = list(F = c(age1 = x, age2 = y), M = c(age1 = x, age2 = y))
  # index_disorder is the disorder for which the PAFGRS liability is calculated (not: possible helper traits) as a character string, default "DEP"

  if (is.null(ages) & !is.null(sexes)) {
    ages <- rep(NA, length(sexes))
  } # if age argument is missing, create age vector with NAs only

  # Set fundamental unit of data: family members
  fam_members <- unique(str_split_i(names(FH), "_", 1)) # all family members, in the order in which they appear in FH
  disorders <- names(Ks)

  # Prepare inputs for pafgrs function

  # select from FH only those columns that contain the relevant disorders. if a disorder is not there in FH for a certain family member, don't include it.
  # e.g. if the family member has no ANX diagnosis, don't include ANX in the data frame
  disorder_data <- FH

  # thr
  family_thresholds <- c() # initialize threshold vector
  # if sex is supplied, assume all arguments are and build personalized threshold vector
  if (!(is.null(sexes))) {
    # select appropriate thresholds from thresholds_list using sex and age
    for (i in fam_members) {
      # set sex
      sex <- as.character(sexes[i]) # for now not setting this up for missing sex info, as this is never the case in NESDA data

      # set age
      if (is.na(ages[i])) { # if age is NA, select from final position of threshold list
        age <- 141
        print(paste0("For family member ", i, " argument 'ages' is NA. Calculating threshold from sex-specific prevalence."))
      } else { # if age is given, select from age-specific threshold
        age <- as.numeric(ages[i])
      }

      names <- paste0(i, "_", disorders)
      # select threshold for each disorder
      family_thresholds[names] <- sapply(disorders, function(x) threshold_list[[x]][[sex]][age])
    }
  } else { # if sex of participants is missing, calculate shared threshold from population prevalence
    print(paste0("For this participant, argument 'sexes' is missing. Calculating threshold from population prevalence."))
    family_thresholds <- qnorm(1 - Ks)
  }
  
  # covmat
  all_fammembers <- c("index", fam_members)
  h2ls <- h2ls[disorders]
  rg <- rgs[disorders, disorders]
  sigma <- build_initial(fam_members = all_fammembers, h2ls = h2ls, rg = rg)$sigma

  covmat_selectors <- c(
    paste0("g_index_", index_disorder), # genetic liability of index individual
    paste0("l_", names(disorder_data))
  ) # full liability of family members

  covmat <- sigma[covmat_selectors, covmat_selectors]

  # Calculate PAFGRS
  pafgrs <- pa_fgrs(rel_status = disorder_data, thr = family_thresholds, covmat = covmat)

  return(list(FH_data = disorder_data, thresholds = family_thresholds, covmat = covmat, liability = pafgrs))
}


FLS <- function(FH, K_relative, K_sporadic, AO_lower, AO_upper, ppt_age, disorder_FLS) {
  # FLS function
  # calculate FLS indicator with formula and values exactly like previous paper
  # Input:
  # FH is a named vector (for one individual) of the disorder status of relatives, excluding index individual. 0 is unaffected, 1 is affected
  # naming format fammember1_disorderA, fammember1_disorderB, fammember2_disorderA etc., e.g. FH <- c(sib2_ANX = 0, sib2_DEP = 1)
  # K_relative, K_sporadic, AO_lower, AO_upper are named numeric vectors with the values of the FLS formula parameters for each disorder. they are defined in setup.R
  # ppt_age is a named vector of ages of family members in the format of c(fammember1 = x, fammember2 = y)
  # disorder_FLS is a character string of the disorder name, currently only "DEP" is supported
  fam_members <- unique(str_split_i(names(FH), "_", 1))

  # Set parameters for the algorithm
  K_relative <- K_relative[disorder_FLS]
  K_sporadic <- K_sporadic[disorder_FLS]
  AO_lower <- AO_lower[disorder_FLS]
  AO_upper <- AO_upper[disorder_FLS]

  FH_extra <- FH

  # Build cross-disorder option
  if (disorder_FLS == "DEP.ANX") {
    for (i in fam_members) {
      # if a family member has status 1 for either DEP or ANX, they are considered to have DEP.ANX
      relevant_columns <- c(paste0(i, "_", "DEP"), paste0(i, "_", "ANX"))
      FH_extra[paste0(i, "_DEP.ANX")] <- as.numeric(sum(FH[relevant_columns], na.rm = TRUE) > 0)
    }
  }

  LR <- c()
  for (i in fam_members) {
    disorder_data <- FH_extra[paste0(i, "_", disorder_FLS)]
    age <- ppt_age[i]

    if (is.na(disorder_data) | is.na(age)) { # if either status or age is missing, FLS will be missing
      LR[i] <- NA
    } else if (disorder_data == 1) { # if the relative is affected
      LR[i] <- K_relative / K_sporadic
    } else if (disorder_data == 0) { # if the relative is unaffected
      LR[i] <- (1 - (K_relative * (age - AO_lower) / (AO_upper - AO_lower))) / (1 - (K_sporadic * (age - AO_lower) / (AO_upper - AO_lower)))
    }
  }

  FLS <- log10(prod(unlist(LR), na.rm = TRUE))

  return(FLS)
}

score_FH <- function(FH_dataframe, methods = c("plusminus", "proportion", "proportion_dense", "FLS", "PAFGRS", "PAFGRSplus"),
                     disorders = NULL, disorder_FLS = NULL, index_disorder = NULL,
                     family_sizes = NULL, ages = NULL, sexes = NULL,
                     Ks = NULL, h2ls = NULL, rgs = NULL, threshold_list = NULL, K_relative = NULL, K_sporadic = NULL, AO_lower = NULL, AO_upper = NULL) {
  # Function to calculate multiple family history indicators for all rows in a given dataset
  # FH_dataframe is a dataframe with family history data, each row is one index individual's family, each column is one family member/diagnosis combination
  # the column name format of FH_dataframe should be fammember1_disorderA, fammember1_disorderB, fammember2_disorderA etc.
  # methods is a string vector indicating which methods to apply to the FH data, choose from c("plusminus", "proportion", "proportion_dense", "FLS", "LT-FH", "PAFGRS")
  #
  #
  # Optional input arguments
  # Required for plusminus:
  # disorders is vector of disorder names in the data, index plus helper traits, e.g. c("DEP", "ANX")
  #
  # Required for proportion:
  # disorders is vector of disorder names in the data, index plus helper traits, e.g. c("DEP", "ANX")
  # family_sizes is a numeric vector. as default use number of fammembers as extracted from column names
  #
  # Required for FLS:
  # For method FLS, required parameters are K_relative, K_sporadic, AO_lower, AO_upper. the format should be K_relative = c(disorderA = 0.5, disorderB = 0.4)
  # disorder_FLS, a character string of the disorder name, currently only "DEP" is supported
  # and ages (named numeric vector of ages of family with naming format fammember1 = x)
  #
  # Required for PAFGRS:
  # Ks is a named vector of population prevalences in the format of c(disorderA = x, disorderB = y)
  # h2ls is a named vector of heritabilities in the format of c(disorderA = x, disorderB = y)
  # rgs is the genetic correlation between disorders, rg = 1 when there is only 1 disorder
  # index_disorder, the name of the disorder to be predicted as a character string
  # For PAFGRS with age, required inputs are disorder_thresholds (pre-calculated list of age- and disorder-specific thresholds),
  # ages (named numeric vector of ages of family with naming format fammember1) and
  # sexes (named character vector with naming format fammember1 = "M/F")
  #
  # to calculate all the indicators for a full dataframe, some arguments need to be dataframes with the same number of rows as FH_dataframe,
  # namely family_sizes, ages, sexes. so each vector described above should be a row of the dataframe
  # another class of function-specific arguments, namely Ks, h2ls, rg, threshold_list, K_relative, K_sporadic, AO_lower, AO_upper are disorder-specific parameters found in literature and loaded in the correct format by setup.R
  # disorders, disorder_FLS, index_disorder, are prediction-specific so they may differ between different applications of the function but are the same for the wrapper function as for the specific functions within
  # we select from parameters based on prediction-specific arguments

  if (is.null(dim(FH_dataframe))) {
    FH_dataframe <- as.data.frame(cbind(FH_dataframe))
  }

  # disease_columns <- colnames(FH_dataframe)[grepl(disorders, colnames(FH_dataframe)) & !grepl("index", colnames(FH_dataframe))] # all disease columns from data except index one
  # age_columns <- colnames(FH_dataframe)[grepl("age", colnames(FH_dataframe))] # age columns
  # sex_columns <- colnames(FH_dataframe)[grepl("sex", colnames(FH_dataframe))] # sex columns

  indicators <- list()

  if ("plusminus" %in% methods) {
    assert("One or more of the required arguments for plusminus method are missing.", !is.null(disorders))
    indicators[["plusminus"]] <- apply(FH_dataframe, 1, function(x) FH_plusminus(x, disorders = disorders))
  }

  if ("proportion" %in% methods) {
    assert("One or more of the required arguments for proportion method are missing.", !is.null(disorders), !is.null(family_sizes))

    prop_sparse <- c()
    for (i in 1:nrow(FH_dataframe)) {
      FH <- FH_dataframe[i, ]
      names(FH) <- colnames(FH_dataframe)

      family_size <- family_sizes[i]

      prop_sparse[i] <- case_proportion(FH = FH, family_size = family_size, method = "sparse", disorders = disorders)
    }
    indicators[["proportion"]] <- prop_sparse
  }

  if ("proportion_dense" %in% methods) {
    assert("One or more of the required arguments for proportion_dense method are missing.", !is.null(disorders), !is.null(family_sizes))

    prop_dense <- c()
    for (i in 1:nrow(FH_dataframe)) {
      FH <- FH_dataframe[i, ]
      names(FH) <- colnames(FH_dataframe)

      family_size <- family_sizes[i]

      prop_dense[i] <- case_proportion(FH = FH, family_size = family_size, method = "dense", disorders = disorders)
    }
    indicators[["proportion_dense"]] <- prop_dense
  }

  if ("FLS" %in% methods) {
    assert("One or more of the required arguments for FLS method are missing.", !is.null(K_relative), !is.null(K_sporadic), !is.null(AO_lower), !is.null(AO_upper), !is.null(ages), !is.null(disorder_FLS))

    FLS <- c()
    for (j in 1:nrow(FH_dataframe)) {
      FH <- FH_dataframe[j, ]
      names(FH) <- colnames(FH_dataframe)

      age_vector <- ages[j, ]
      names(age_vector) <- colnames(ages)

      FLS[j] <- FLS(FH = FH, K_relative = K_relative, K_sporadic = K_sporadic, AO_lower = AO_lower, AO_upper = AO_upper, ppt_age = age_vector, disorder_FLS = disorder_FLS)
    }
    indicators[["FLS"]] <- FLS
  }

  if ("PAFGRS" %in% methods) {
    assert("One or more of the required arguments for PAFGRS method are missing.", !is.null(Ks), !is.null(h2ls), !is.null(rgs), !is.null(index_disorder))
    indicators[["PAFGRS"]] <- apply(FH_dataframe, 1, function(x) apply_pafgrs(FH = x, Ks = Ks, h2ls = h2ls, rgs = rg_SCZ.BIP.DEP.ANX, index_disorder = index_disorder)[["liability"]]["postM"])
  }

  if ("PAFGRSplus" %in% methods) {
    assert("One or more of the required arguments for PAFGRSplus method are missing.", !is.null(Ks), !is.null(h2ls), !is.null(rgs), !is.null(index_disorder), !is.null(ages), !is.null(sexes))

    mean_liab <- c()
    for (i in 1:nrow(FH_dataframe)) {
      FH <- FH_dataframe[i, ]
      names(FH) <- colnames(FH_dataframe)

      age_vector <- ages[i, ]
      names(age_vector) <- colnames(ages)

      sex_vector <- sexes[i, ]
      names(sex_vector) <- colnames(sexes)

      print(i)

      mean_liab[i] <- apply_pafgrs(FH = FH, Ks = Ks, h2ls = h2ls, ages = age_vector, sexes = sex_vector, threshold_list = disorder_thresholds, index_disorder = index_disorder)[["liability"]]["postM"]
    }
    indicators[["PAFGRSplus"]] <- mean_liab
  }

  indicators <- as.data.frame(do.call("cbind", indicators))

  return(indicators)
}
