
#------------------------------------------------------------------------------#
# Calculations part I
#------------------------------------------------------------------------------#

# See chapter 4 of the code documentation

################################################################################
### Calculate the ARFs #########################################################
################################################################################

### Adjust the RRs -------------------------------------------------------------

coeff_corr_1 <- cattle_df |>
  lazy_dt() |>
  group_by(Iteration) |>
  summarise(coeff_corr_1 = mean(RR)) |>
  as_tibble()

coeff_corr_1_2nd_disease <- cattle_2nd_disease_df |>
  lazy_dt() |>
  group_by(Iteration) |>
  summarise(coeff_corr_1 = mean(RR)) |>
  as_tibble()

coeff_corr_2 <- cattle_df |>
  lazy_dt() |>
  group_by(Iteration) |>
  summarise(coeff_corr_2 = max(RR) * parameters$P_betH) |>
  as_tibble()

coeff_corr_2_2nd_disease <- cattle_2nd_disease_df |>
  lazy_dt() |>
  group_by(Iteration) |>
  summarise(coeff_corr_2 = max(RR) * parameters$P_betH) |>
  as_tibble()

coeff_corr <- full_join(coeff_corr_1, coeff_corr_2, by = "Iteration")
coeff_corr_2nd_disease <- full_join(coeff_corr_1_2nd_disease, coeff_corr_2_2nd_disease, by = "Iteration")

remove(coeff_corr_1, coeff_corr_2, coeff_corr_1_2nd_disease, coeff_corr_2_2nd_disease)

coeff_corr <- coeff_corr |>
  rowwise() |>
  mutate(Divisor = max(coeff_corr_1, coeff_corr_2)) |> # Taking only the max coeff_corr for each iteration
  select(Iteration, Divisor)

coeff_corr_2nd_disease <- coeff_corr_2nd_disease |>
  rowwise() |>
  mutate(Divisor = max(coeff_corr_1, coeff_corr_2)) |> # Taking only the max coeff_corr for each iteration
  select(Iteration, Divisor)

cattle_df <- cattle_df |>
  lazy_dt() |>
  left_join(coeff_corr |> lazy_dt(), by = "Iteration") |>
  mutate(ARR = RR / Divisor) |>
  select(TVD_Nummer, Iteration, ARR) |>
  as_tibble()


cattle_2nd_disease_df <- cattle_2nd_disease_df |>
  lazy_dt() |>
  left_join(coeff_corr_2nd_disease |> lazy_dt(), by = "Iteration") |>
  mutate(ARR = RR / Divisor) |>
  select(TVD_Nummer, Iteration, ARR) |>
  as_tibble()

remove(coeff_corr, coeff_corr_2nd_disease)

################################################################################
### Calculate the scenario tree ################################################
################################################################################

### Calculate CSe_random -------------------------------------------------------

# HSe_random Beef

Se_i <- tibble(Iteration = seq_len(parameters$n_iterations),
               Se = rpert(n = parameters$n_iterations,
                          x.min = parameters$Se_min,
                          x.max = parameters$Se_max,
                          x.mode = parameters$Se_mode))

HSe_random_beef <- hse_infinite(id = paste0("Random__Iteration_", Se_i$Iteration),
                                n_tested = rep(2,
                                               parameters$n_iterations),
                                test_Se = Se_i$Se,
                                dp = parameters$P_inH) |> 
  separate(col = id, into = c("id", "Iteration"), sep = "__")

HSe_random_beef$Iteration <- HSe_random_beef$Iteration |> 
  str_sub(start = 11L) |> 
  as.integer()

HSe_random_beef <- HSe_random_beef |> arrange(Iteration)

Se_i_2nd_disease <- tibble(Iteration = seq_len(parameters$n_iterations),
                           Se_2nd_disease = rpert(n = parameters$n_iterations,
                                                  x.min = parameters$Se_min,
                                                  x.max = parameters$Se_max,
                                                  x.mode = parameters$Se_mode))

Se_i <- Se_i |> left_join(Se_i_2nd_disease, by = "Iteration")

remove(Se_i_2nd_disease)

HSe_random_beef_2nd_disease <- hse_infinite(id = paste0("Random__Iteration_", Se_i$Iteration),
                                            n_tested = rep(2,
                                                           parameters$n_iterations),
                                            test_Se = Se_i$Se_2nd_disease,
                                            dp = parameters_2nd_disease$P_inH) |> 
  separate(col = id, into = c("id", "Iteration"), sep = "__")

HSe_random_beef_2nd_disease$Iteration <- HSe_random_beef_2nd_disease$Iteration |> 
  str_sub(start = 11L) |> 
  as.integer()

HSe_random_beef_2nd_disease <- HSe_random_beef_2nd_disease |> arrange(Iteration)

# CSe_random Beef

CSe_random_beef <- HSe_random_beef |> 
  mutate(CSe = 1 - (1 - parameters$P_betH * HSe) ^ (1/3 * parameters$n_random_beef))

CSe_random_beef_2nd_disease <- HSe_random_beef_2nd_disease |> 
  mutate(CSe = 1 - (1 - parameters_2nd_disease$P_betH * HSe) ^ (1/3 * parameters$n_random_beef))

remove(HSe_random_beef, HSe_random_beef_2nd_disease)

# HSe_random Dairy

Se_i$BTM_Se <- rpert(n = parameters$n_iterations,
                     x.min = parameters$BTM_Se_min,
                     x.max = parameters$BTM_Se_max,
                     x.mode = parameters$BTM_Se_mode)


Se_i$BTM_Se_2nd_disease <- rpert(n = parameters$n_iterations,
                                 x.min = parameters_2nd_disease$BTM_Se_min,
                                 x.max = parameters_2nd_disease$BTM_Se_max,
                                 x.mode = parameters_2nd_disease$BTM_Se_mode)

# CSe_random Dairy

CSe_random_dairy <- hse_infinite(id = paste0("Random__", Se_i$Iteration),
                                 n_tested = rep(parameters$n_random_dairy,
                                                parameters$n_iterations),
                                 test_Se = Se_i$BTM_Se,
                                 dp = parameters$P_betH) |> 
  separate(col = id, into = c("id", "Iteration"), sep = "__")

CSe_random_dairy_2nd_disease <- hse_infinite(id = paste0("Random__", Se_i$Iteration),
                                             n_tested = rep(parameters$n_random_dairy,
                                                            parameters$n_iterations),
                                             test_Se = Se_i$BTM_Se_2nd_disease,
                                             dp = parameters_2nd_disease$P_betH) |> 
  separate(col = id, into = c("id", "Iteration"), sep = "__")

CSe_random_dairy <- rename(CSe_random_dairy,`CSe` = `HSe`)

CSe_random_dairy_2nd_disease <- rename(CSe_random_dairy_2nd_disease,`CSe` = `HSe`)

### Calculate HSe_sentinel -----------------------------------------------------

# HSe_sentinel Beef

HSe_sentinel_beef <- hse_infinite(id = seq_len(parameters$n_iterations),
                                  n_tested = rep(parameters$n_sentinel_beef,
                                                 parameters$n_iterations),
                                  test_Se = Se_i$Se,
                                  dp = parameters$P_inH) |> 
  mutate(Iteration = as.integer(id)) |> 
  select(-id)

HSe_sentinel_beef_2nd_disease <- hse_infinite(id = seq_len(parameters$n_iterations),
                                              n_tested = rep(parameters$n_sentinel_beef,
                                                             parameters$n_iterations),
                                              test_Se = Se_i$Se_2nd_disease,
                                              dp = parameters_2nd_disease$P_inH) |> 
  mutate(Iteration = as.integer(id)) |> 
  select(-id) |> 
  rename(HSe_2nd_disease = HSe)

HSe_sentinel_beef <- HSe_sentinel_beef |> 
  left_join(HSe_sentinel_beef_2nd_disease, by = "Iteration")

remove(HSe_sentinel_beef_2nd_disease)

# HSe_sentinel Dairy

HSe_sentinel_dairy <- Se_i |> 
  select(Iteration, HSe = BTM_Se, HSe_2nd_disease = BTM_Se_2nd_disease)

################################################################################
### Reverse calculation to calculate CSe_sentinel_target #######################
################################################################################

### Calculate PIntro -----------------------------------------------------------

PIntro_i <- tibble(Iteration = c(1:parameters$n_iterations),
                   PIntro = rpert(n = parameters$n_iterations,
                                  x.min = parameters$PIntro_min,
                                  x.max = parameters$PIntro_max,
                                  x.mode = parameters$PIntro_mode)) |> 
  mutate(PriorPFree = prior_fr(post_fr = PFree_last, intro = PIntro))


PIntro_i_2nd_disease <- tibble(Iteration = c(1:parameters$n_iterations),
                               PIntro_2nd_disease = rpert(n = parameters$n_iterations,
                                                          x.min = parameters_2nd_disease$PIntro_min,
                                                          x.max = parameters_2nd_disease$PIntro_max,
                                                          x.mode = parameters_2nd_disease$PIntro_mode)) |> 
  mutate(PriorPFree_2nd_disease = prior_fr(post_fr = PFree_last_2nd_disease, intro = PIntro_2nd_disease))

PIntro_i <- PIntro_i |> 
  left_join(PIntro_i_2nd_disease, by = "Iteration")

remove(PIntro_i_2nd_disease)

### Calculate SSe_target -------------------------------------------------------

SSe_target <- (1 - PIntro_i$PriorPFree / parameters$targetPostPFree) / (1 - PIntro_i$PriorPFree)

SSe_target_2nd_disease <- (1 - PIntro_i$PriorPFree_2nd_disease / parameters_2nd_disease$targetPostPFree) / (1 - PIntro_i$PriorPFree_2nd_disease)

remove(PFree_last, PFree_last_2nd_disease)

### Calculate CSe_target -------------------------------------------------------

CSe_target <- 1 - sqrt(1 - SSe_target)

CSe_target_2nd_disease <- 1 - sqrt(1 - SSe_target_2nd_disease)

# CSe_target_sentinel_beef

CSe_target_sentinel_beef <- max(1 - (1 - CSe_target) / (1 - CSe_random_beef$CSe), 0)

CSe_target_sentinel_beef_2nd_disease <- max(1 - (1 - CSe_target_2nd_disease) / (1 - CSe_random_beef_2nd_disease$CSe), 0)


# CSe_target_sentinel_dairy

CSe_target_sentinel_dairy <- max(1 - (1 - CSe_target) / (1 - CSe_random_dairy$CSe), 0)

CSe_target_sentinel_dairy_2nd_disease <- max(1 - (1 - CSe_target_2nd_disease) / (1 - CSe_random_dairy_2nd_disease$CSe), 0)

remove(CSe_target, SSe_target, CSe_target_2nd_disease, SSe_target_2nd_disease)

################################################################################
### Calculation of the number of sentinels needed ##############################
################################################################################

### Selection of potential sentinels -------------------------------------------

cattle_averagedARF_df <- cattle_df |>
  lazy_dt() |> 
  group_by(TVD_Nummer) |> 
  summarise(averagedARF = median(ARR)) |> 
  filter(averagedARF >= 1) |> 
  ungroup() |> 
  mutate(random_order = runif(n = n(),
                              min = 1,
                              max = n())) |> 
  arrange(desc(averagedARF), random_order) |> 
  as_tibble() |> 
  select(-random_order)

cattle_averagedARF_df$TVD_Nummer <- as.character(cattle_averagedARF_df$TVD_Nummer)
good_sentinel_candidates$TVD_Nummer <- as.character(good_sentinel_candidates$TVD_Nummer)

cattle_sentinel_df <- cattle_averagedARF_df |>
  anti_join(exclude_from_sentinels, by = "TVD_Nummer") |> 
  inner_join(good_sentinel_candidates, by = "TVD_Nummer")

remove(cattle_averagedARF_df, good_sentinel_candidates)

beef_sentinel_df <- cattle_sentinel_df |> 
  filter(Component == "beef") |> 
  select(-Component)

dairy_sentinel_df <- cattle_sentinel_df |> 
  filter(Component == "dairy") |> 
  select(-Component)

beef_sentinel_df$Sentinel_position <- seq_len(nrow(beef_sentinel_df))

dairy_sentinel_df$Sentinel_position <- seq_len(nrow(dairy_sentinel_df))

remove(cattle_sentinel_df)

cattle_df$TVD_Nummer <- as.character(cattle_df$TVD_Nummer)
cattle_2nd_disease_df$TVD_Nummer <- as.character(cattle_2nd_disease_df$TVD_Nummer)

cattle_2nd_disease_df <- cattle_2nd_disease_df |> 
  rename(ARR_2nd_disease = ARR)

beef_sentinel_df <- beef_sentinel_df |> 
  left_join(cattle_df, by = "TVD_Nummer") |> 
  left_join(cattle_2nd_disease_df, by = c("TVD_Nummer", "Iteration"))

dairy_sentinel_df <- dairy_sentinel_df |> 
  left_join(cattle_df, by = "TVD_Nummer") |> 
  left_join(cattle_2nd_disease_df, by = c("TVD_Nummer", "Iteration"))

### Calculation of CSe_sentinel ------------------------------------------------

# Add HSe to selected sentinels

HSe_sentinel_beef <- left_join(HSe_sentinel_beef, beef_sentinel_df,
                               by = "Iteration")

HSe_sentinel_dairy <- left_join(HSe_sentinel_dairy, dairy_sentinel_df,
                                by = "Iteration")

# Select CSe_sentinel Beef

HSe_sentinel_beef <- HSe_sentinel_beef |> 
  lazy_dt() |> 
  mutate(Aggregate = 1 - (ARR * parameters$P_betH * HSe),
         Aggregate_2nd_disease = 1 - (ARR_2nd_disease * parameters_2nd_disease$P_betH * HSe_2nd_disease)) |> 
  arrange(Sentinel_position) |> 
  as_tibble()

# States the origin of and |> inside function:
#' @importFrom magrittr |>
Nbr_sentinel_fn <- function(sentinel_list, n_sentinels) {
  
  results <- sentinel_list |> 
    dplyr::filter(Sentinel_position <= n_sentinels) |> 
    dplyr::summarise(CSe = 1 - prod(.data$Aggregate),
                     CSe_2nd_disease = 1 - prod(.data$Aggregate_2nd_disease)) |> 
    dplyr::as_tibble()
  
  results$n_sentinels <- n_sentinels
  
  return(results)
}

CSe_sentinel_beef <- map_dfr(seq_len(HSe_sentinel_beef$Sentinel_position |> max()),
                             Nbr_sentinel_fn,
                             sentinel_list = HSe_sentinel_beef |> lazy_dt() |> group_by(Iteration)) |> 
  mutate(targetCSe = CSe_target_sentinel_beef,
         targetCSe_2nd_disease = CSe_target_sentinel_beef_2nd_disease) |> 
  rename(!! paste0("CSe_", parameters$disease) := CSe,
         !! paste0("CSe_", parameters_2nd_disease$disease) := CSe_2nd_disease,
         !! paste0("targetCSe_", parameters$disease) := targetCSe,
         !! paste0("targetCSe_", parameters_2nd_disease$disease) := targetCSe_2nd_disease) |> 
  select(Iteration, n_sentinels, CSe_EBL, targetCSe_EBL, CSe_IBR, targetCSe_IBR)

remove(HSe_sentinel_beef)

# Select CSe_sentinel Dairy

HSe_sentinel_dairy <- HSe_sentinel_dairy |> 
  lazy_dt() |> 
  mutate(Aggregate = 1 - (ARR * parameters$P_betH * HSe),
         Aggregate_2nd_disease = 1 - (ARR_2nd_disease * parameters_2nd_disease$P_betH * HSe_2nd_disease)) |> 
  arrange(Sentinel_position) |> 
  as_tibble()

CSe_sentinel_dairy <- map_dfr(seq_len(HSe_sentinel_dairy$Sentinel_position |> max()),
                              Nbr_sentinel_fn,
                              sentinel_list = HSe_sentinel_dairy |> lazy_dt() |> group_by(Iteration)) |> 
  mutate(targetCSe = CSe_target_sentinel_dairy,
         targetCSe_2nd_disease = CSe_target_sentinel_dairy_2nd_disease) |>
  rename(!! paste0("CSe_", parameters$disease) := CSe,
         !! paste0("CSe_", parameters_2nd_disease$disease) := CSe_2nd_disease,
         !! paste0("targetCSe_", parameters$disease) := targetCSe,
         !! paste0("targetCSe_", parameters_2nd_disease$disease) := targetCSe_2nd_disease) |> 
  select(Iteration, n_sentinels, CSe_EBL, targetCSe_EBL, CSe_IBR, targetCSe_IBR)

remove(HSe_sentinel_dairy, Nbr_sentinel_fn)
