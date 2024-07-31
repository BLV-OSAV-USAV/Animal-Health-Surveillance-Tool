
#------------------------------------------------------------------------------#
# Introduction
#------------------------------------------------------------------------------#

# see chapter 1 of the code documentation

################################################################################
### Load packages ##############################################################
################################################################################

packages <- c("tidyverse", "readxl", "dtplyr", "lubridate", "freedom", "sf",
              "openxlsx")

for (i in packages) {
  if(require(i, character.only = TRUE)) {
    print(paste(i, "is loaded correctly"))
  } else {
    print(paste("trying to install", i))
    install.packages(i, dependencies = TRUE)
    if(require(i, character.only = TRUE)) {
      print(paste(i, "installed and loaded"))
    } else {
      stop(paste("could not install", i))
    }
  }
}

remove(packages, i)

################################################################################
### Modify hse_finite ##########################################################
################################################################################

hse_finite_modified <- function(id,
                                n_tested,
                                N,
                                test_Se,
                                dp) {
  
  if (length(n_tested) != length(N)) {
    stop(paste("The length of the n_tested vector must be equal to the N",
               "vector. ie. you must describe both the number of animals",
               "tested in each group as well as how many animals are in",
               "each group.", sep = "\n"))
  }
  
  if (any(n_tested > N)) {
    stop("One of the URG has more subunits tested than in the population")
  }
  
  if (!(length(dp) == 1 || length(n_tested) == length(dp))) {
    stop(paste("The length of the n_tested vector must be equal to",
               "the dp vector. ie. you must describe both the number of",
               "animals tested in each group as well as the dp in",
               "each group.", sep = "\n"))
  }
  
  if (!(length(test_Se) == 1 || length(test_Se) == length(n_tested))) {
    stop("The length of test_Se must be either 1 or the length of n_tested")
  }
  
  if (!(length(id) == length(n_tested))) {
    stop(paste("Argument id (grouping variable) should be",
               "the same length as n_tested"))
  }
  
  A <- 1 - (n_tested * test_Se / N)
  
  B <- pmax(dp * N, 1) # Change to the original function to round to 1 if below 1
  
  df <- as.data.frame(1 - tapply(A ^ B, INDEX = id, FUN = "prod"))
  
  names(df) <- c("HSe")
  
  df$id <- rownames(df)
  
  df[, c("id", "HSe")]
}

################################################################################
### Modify hse #################################################################
################################################################################

hse_modified <- function(id,
                         n_tested,
                         N,
                         test_Se,
                         dp,
                         threshold = 0.1,
                         force = FALSE) {
  
  ## Ratio of animals tested in the herds
  ratio <- n_tested / N
  
  ## Check if this is more than expected
  if (any(ratio > 1) & !force) {
    problem <- id[ratio > 1]
    stop(paste("Greater than 100% of animals cannot be tested.",
               "This occurs in the following ids:",
               paste(problem, collapse = ", "),
               "To ignore this an default to infinite population",
               "for these herds, set force = TRUE", sep = "\n"))
  }
  
  ## Use the finite calculation for those with more than the threshold
  finite <- NULL
  index_finite <- (ratio > threshold) & (ratio <= 1)
  test_Se_finite <- test_Se
  if (length(test_Se) > 1) {
    test_Se_finite <- test_Se[index_finite]
  }
  dp_finite <- dp
  if (length(dp) > 1) {
    dp_finite <- dp[index_finite]
  }
  if (any(index_finite)) {
    finite <- hse_finite_modified(id[index_finite], # Change to the original function to use 'hse_finite_modified' and not 'hse_finite'
                                  n_tested[index_finite],
                                  N[index_finite],
                                  test_Se_finite,
                                  dp_finite)
    finite$method <- "finite"
  }
  if (all(index_finite)) {
    return(finite)
  }
  
  ## Otherwise use the infinite
  index_infinite <- !index_finite
  test_Se_infinite <- test_Se
  if (length(test_Se) > 1) {
    test_Se_infinite <- test_Se[index_infinite]
  }
  dp_infinite <- dp
  if (length(dp) > 1) {
    dp_infinite <- dp[index_infinite]
  }
  
  infinite <- freedom::hse_infinite(id[index_infinite],
                           n_tested[index_infinite],
                           test_Se_infinite,
                           dp_infinite)
  infinite$method <- "infinite"
  
  ## return the complete dataset
  rbind(finite,
        infinite)
}
