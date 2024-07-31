
#------------------------------------------------------------------------------#
# Edit data
#------------------------------------------------------------------------------#

# See chapter 11 of the code documentation

################################################################################
### Calculate the adjusted risk ################################################
################################################################################

coeff_corr_1 <- riskCatHerds |>
  lazy_dt() |>
  group_by(Iteration) |>
  summarise(coeff_corr_1 = mean(RR)) |>
  as_tibble()

coeff_corr_2 <- riskCatHerds |>
  lazy_dt() |>
  group_by(Iteration) |>
  summarise(coeff_corr_2 = max(RR) * parameters$P_betH) |>
  as_tibble()

coeff_corr <- full_join(coeff_corr_1, coeff_corr_2, by = "Iteration")

remove(coeff_corr_1, coeff_corr_2)

coeff_corr$coeff_corr_3 <- 1 # Be sure that CoefCorr is always greater than 1

coeff_corr <- coeff_corr |>
  rowwise() |>
  mutate(Divisor = max(coeff_corr_1, coeff_corr_2, coeff_corr_3)) |>
  select(Iteration, Divisor)

riskCatHerds <- riskCatHerds |>
  lazy_dt() |>
  left_join(coeff_corr |> lazy_dt(), by = "Iteration") |>
  mutate(ARR = RR / Divisor) |>
  select(TVD_Nummer, Iteration, ARR) |>
  as_tibble()

remove(coeff_corr)

################################################################################
### Calculate the scenario tree ################################################
################################################################################

### Calculate CSe_sentinel -------------------------------------------------------

# Check sentinel are on ARR list

list_dairy_sentinel <- postSurv_dairy_df |> 
  filter(is_sentinel == TRUE) |> 
  pull(TVD_Nummer) |> 
  unique()

list_beef_sentinel <- postSurv_beef_df |> 
  filter(is_sentinel == TRUE) |> 
  pull(TVD_Nummer) |> 
  unique()

list_sentinel <- c(list_dairy_sentinel, list_beef_sentinel)

remove(list_dairy_sentinel, list_beef_sentinel)

test_sentinel <- list_sentinel[! list_sentinel %in% riskCatHerds$TVD_Nummer]

if (length(test_sentinel) != 0) {
  
  moveMissingSentinelToRandom <- askYesNo(msg = paste0("Some herds have no corresponding value ",
                                                       "in the list of ARRs generated during the planning.\n",
                                                       "Do you want to ignore these herds ",
                                                       "when calculating the CSe of the sentinel component ",
                                                       "and use them to calculate the CSe ",
                                                       "of the random component?\n",
                                                       "If you want to use them for the calculation ",
                                                       "of the CSe of the sentinel component, ",
                                                       "you must choose No, and then first create ",
                                                       "a new ARR list using the corresponding tool ",
                                                       "containing all sentinel herds."),
                                          prompts = getOption("askYesNo",
                                                              gettext(c("Yes", "No", "Cancel"))),
                                          default = TRUE)
  
  if (moveMissingSentinelToRandom == TRUE && !is.na(moveMissingSentinelToRandom)) {
    
    warning(paste0("The herds with TVD number ",
                   paste(test_sentinel, collapse = ", "),
                   " have no corresponding value in the list of ARRs ",
                   "generated during the planning.\n",
                   "You decided to compute the calculations ",
                   "of the CSe by considering these herds ",
                   "to be part of the random component instead of ",
                   "the sentinel component.\n",
                   "If you want to use them for the calculation of the CSe ",
                   "of the sentinel component, you must first ",
                   "a new ARR list using the corresponding tool ",
                   "containing all sentinel herds ",
                   "and run the evaluation tool again."))
    
    postSurv_dairy_df$is_sentinel[postSurv_dairy_df$TVD_Nummer %in% test_sentinel] <- FALSE
    postSurv_beef_df$is_sentinel[postSurv_beef_df$TVD_Nummer %in% test_sentinel] <- FALSE
    
  } else {
    
    stop(paste0("Execution halted. Please create a new ARR list ",
                "using the corresponding tool containing all sentinel herd ",
                "before trying the evaluation tool again."))
  }
  
  remove(moveMissingSentinelToRandom)
}

remove(list_sentinel, test_sentinel)

# HSe_sentinel Dairy

BTM_Se_i <- tibble(Iteration = c(1:parameters$n_iterations),
                   BTM_HSe = rpert(n = parameters$n_iterations,
                                   x.min = parameters$BTM_Se_min,
                                   x.max = parameters$BTM_Se_max,
                                   x.mode = parameters$BTM_Se_mode),
                   p_lactation = rpert(n = parameters$n_iterations,
                                       x.min = parameters$p_lactation_min,
                                       x.max = parameters$p_lactation_max,
                                       x.mode = parameters$p_lactation_mode))

HSe_dairy_sentinel <- postSurv_dairy_df |>
  filter(is_sentinel == TRUE) |> 
  select(TVD_Nummer) |> 
  crossing(Iteration = BTM_Se_i$Iteration) |> 
  left_join(postSurv_dairy_df, by = "TVD_Nummer") |> 
  left_join(BTM_Se_i, by = "Iteration") |> 
  mutate(HSe = case_when(n_sampling == 1 ~ BTM_HSe * p_lactation,
                         n_sampling > 1 ~ BTM_HSe)) |>
  select(TVD_Nummer, Iteration, HSe)

# HSe_sentinel Beef

Se_i <- tibble(Iteration = c(1:parameters$n_iterations),
               Se = rpert(n = parameters$n_iterations,
                          x.min = parameters$Se_min,
                          x.max = parameters$Se_max,
                          x.mode = parameters$Se_mode))

beef_sentinel_component <- postSurv_beef_df |> 
  filter(is_sentinel == TRUE) |> 
  select(TVD_Nummer) |> 
  crossing(Iteration = Se_i$Iteration) |> 
  left_join(postSurv_beef_df, by = "TVD_Nummer") |> 
  left_join(Se_i, by = "Iteration") |> 
  unite(col = "id", TVD_Nummer, Iteration, sep = "__")

HSe_beef_sentinel <- hse(id = beef_sentinel_component$id,
                         n_tested = beef_sentinel_component$n_tested,
                         N = beef_sentinel_component$n_animals,
                         test_Se = beef_sentinel_component$Se,
                         dp = parameters$P_inH,
                         threshold = 0.1) |> 
  separate(col = id, into = c("TVD_Nummer", "Iteration"), sep = "__", remove = FALSE)

HSe_beef_sentinel$Iteration <- HSe_beef_sentinel$Iteration |> as.integer()

HSe_beef_sentinel$TVD_Nummer <- HSe_beef_sentinel$TVD_Nummer |> as.integer()

HSe_beef_sentinel <- HSe_beef_sentinel |> arrange(Iteration)

remove(beef_sentinel_component)

# CSe_sentinel

HSe_dairy_sentinel <- HSe_dairy_sentinel |> 
  lazy_dt() |> 
  left_join(riskCatHerds |> 
              lazy_dt() |> 
              select(TVD_Nummer, ARR),
            by = "TVD_Nummer")

CSe_dairy_sentinel <- HSe_dairy_sentinel |> 
  group_by(Iteration) |> 
  summarise(CSe = 1 - prod(1 - parameters$P_betH * ARR * HSe)) |> 
  as_tibble()


HSe_beef_sentinel <- HSe_beef_sentinel |> 
  lazy_dt() |> 
  left_join(riskCatHerds |> 
              lazy_dt() |> 
              select(TVD_Nummer, ARR),
            by = "TVD_Nummer")

CSe_beef_sentinel <- HSe_beef_sentinel |> 
  group_by(Iteration) |> 
  summarise(CSe = 1 - prod(1 - parameters$P_betH * ARR * HSe)) |> 
  as_tibble()

remove(HSe_dairy_sentinel, HSe_beef_sentinel, riskCatHerds)

### Calculate CSe_random -------------------------------------------------------

# HSe_random Dairy

HSe_dairy_random <- postSurv_dairy_df |>
  filter(is_sentinel == FALSE) |>
  select(TVD_Nummer) |>
  crossing(Iteration = BTM_Se_i$Iteration) |> 
  left_join(postSurv_dairy_df, by = "TVD_Nummer") |> 
  left_join(BTM_Se_i, by = "Iteration") |> 
  mutate(HSe = case_when(n_sampling == 1 ~ BTM_HSe * p_lactation,
                         n_sampling > 1 ~ BTM_HSe)) |>
  select(TVD_Nummer, Iteration, HSe)

# HSe_random Beef

beef_random_component <- postSurv_beef_df |> 
  filter(is_sentinel == FALSE) |> 
  select(TVD_Nummer) |> 
  crossing(Iteration = Se_i$Iteration) |> 
  left_join(postSurv_beef_df, by = "TVD_Nummer") |> 
  left_join(Se_i, by = "Iteration") |> 
  unite(col = "id", TVD_Nummer, Iteration, sep = "__")

HSe_beef_random <- hse(id = beef_random_component$id,
                       n_tested = beef_random_component$n_tested,
                       N = beef_random_component$n_animals,
                       test_Se = beef_random_component$Se,
                       dp = parameters$P_inH,
                       threshold = 0.1) |> 
  separate(col = id, into = c("TVD_Nummer", "Iteration"), sep = "__", remove = FALSE)

HSe_beef_random$Iteration <- HSe_beef_random$Iteration |> as.integer()

HSe_beef_random$TVD_Nummer <- HSe_beef_random$TVD_Nummer |> as.integer()

HSe_beef_random <- HSe_beef_random |> arrange(Iteration)

remove(beef_random_component)

# CSe_random

CSe_dairy_random <- HSe_dairy_random |> 
  group_by(Iteration) |> 
  summarise(CSe = 1 - prod(1 - parameters$P_betH * HSe)) |> 
  as_tibble()

CSe_beef_random <- HSe_beef_random |> 
  group_by(Iteration) |> 
  summarise(CSe = 1 - prod(1 - parameters$P_betH * HSe)) |> 
  as_tibble()

remove(HSe_dairy_random, HSe_beef_random)

### Calculate aggregated CSe ---------------------------------------------------

CSe_dairy <- full_join(CSe_dairy_random |> rename(CSe_dairy_random = CSe),
                       CSe_dairy_sentinel |> rename(CSe_dairy_sentinel = CSe),
                       by = "Iteration") |>
  mutate(CSe_dairy = 1 - (1 - CSe_dairy_random) * (1 - CSe_dairy_sentinel)) |> 
  select(Iteration,
         CSe_dairy)

CSe_beef <- full_join(CSe_beef_random |> rename(CSe_beef_random = CSe),
                      CSe_beef_sentinel |> rename(CSe_beef_sentinel = CSe),
                      by = "Iteration") |>
  mutate(CSe_beef = 1 - (1 - CSe_beef_random) * (1 - CSe_beef_sentinel)) |> 
  select(Iteration,
         CSe_beef)

CSe_sentinel <- full_join(CSe_beef_sentinel |> rename(CSe_beef_sentinel = CSe),
                          CSe_dairy_sentinel |> rename(CSe_dairy_sentinel = CSe),
                          by = "Iteration") |>
  mutate(CSe_sentinel = 1 - (1 - CSe_beef_sentinel) * (1 - CSe_dairy_sentinel)) |> 
  select(Iteration,
         CSe_sentinel)

CSe_random <- full_join(CSe_beef_random |> rename(CSe_beef_random = CSe),
                        CSe_dairy_random |> rename(CSe_dairy_random = CSe),
                        by = "Iteration") |>
  mutate(CSe_random = 1 - (1 - CSe_beef_random) * (1 - CSe_dairy_random)) |> 
  select(Iteration,
         CSe_random)

### Calculate SSe --------------------------------------------------------------

SSe <- full_join(CSe_dairy_random |> rename(CSe_dairy_random = CSe),
                 CSe_dairy_sentinel |> rename(CSe_dairy_sentinel = CSe),
                 by = "Iteration") |>
  full_join(CSe_beef_random |> rename(CSe_beef_random = CSe),
            by = "Iteration") |> 
  full_join(CSe_beef_sentinel |> rename(CSe_beef_sentinel = CSe),
            by = "Iteration") |> 
  mutate(SSe = 1 - (1 - CSe_dairy_random) * (1 - CSe_dairy_sentinel) *
           (1 - CSe_beef_random) * (1 - CSe_beef_sentinel)) |> 
  left_join(CSe_dairy, by = "Iteration") |>
  left_join(CSe_beef, by = "Iteration") |>
  left_join(CSe_sentinel, by = "Iteration") |>
  left_join(CSe_random, by = "Iteration") |>
  select(Iteration, CSe_dairy_random,
         CSe_dairy_sentinel,
         CSe_beef_random,
         CSe_beef_sentinel,
         CSe_dairy,
         CSe_beef,
         CSe_sentinel,
         CSe_random,
         SSe)

remove(CSe_dairy_random,
       CSe_dairy_sentinel,
       CSe_beef_random,
       CSe_beef_sentinel,
       CSe_dairy,
       CSe_beef,
       CSe_sentinel,
       CSe_random)

################################################################################
### PostPFree ##################################################################
################################################################################

### Calculate PostPFree --------------------------------------------------------

PIntro_i <- tibble(Iteration = c(1:parameters$n_iterations),
                   PIntro = rpert(n = parameters$n_iterations,
                                  x.min = parameters$PIntro_min,
                                  x.max = parameters$PIntro_max,
                                  x.mode = parameters$PIntro_mode)) |> 
  mutate(PriorPFree = prior_fr(post_fr = PFree_last, intro = PIntro))

SSe <- left_join(SSe, PIntro_i, by = "Iteration")

PostPFree <- SSe |> 
  mutate(PostPFree = post_fr(prior_fr = PriorPFree, Se = SSe)) |> 
  select(-PIntro, -PriorPFree)

remove(SSe)

### Compare PostPFree ----------------------------------------------------------

Results_eval <- PostPFree |> 
  lazy_dt() |> 
  summarise(Median = median(PostPFree),
            Mean = mean(PostPFree),
            Quantile_5 = quantile(x = PostPFree, c(0.05)),
            Quantile_10 = quantile(x = PostPFree, c(0.1)),
            Quantile_25 = quantile(x = PostPFree, c(0.25)),
            Quantile_33 = quantile(x = PostPFree, c(0.33)),
            Proportion_above_targetPostPFree = sum(PostPFree >= parameters$targetPostPFree) / parameters$n_iterations) |> 
  as_tibble()

################################################################################
### Summary tables #############################################################
################################################################################

### Yearly summary table -------------------------------------------------------

summary_df <- tibble(Variables = NA, !! parameters$disease := NA) |> 
  add_row(Variables = "Untersuchte Tierhaltungen", !! parameters$disease := nrow(postSurv_dairy_df) + nrow(postSurv_beef_df)) |> 
  add_row(Variables = "Untersuchte Proben", !! parameters$disease := sum(postSurv_dairy_df$n_sampling) + sum(postSurv_beef_df$n_tested)) |> 
  add_row(Variables = "Erreichte Sicherheit", !! parameters$disease := median(PostPFree$SSe)) |> 
  add_row(Variables = "davon ML", !! parameters$disease := median(PostPFree$CSe_dairy)) |> 
  add_row(Variables = "davon NML", !! parameters$disease := median(PostPFree$CSe_beef)) |> 
  filter(! is.na(Variables))

### Overview table for the yearly report ---------------------------------------

summary_df_NML <- postSurv_beef_df %>% 
  ungroup() %>% 
  group_by(Surveillance_programme, is_sentinel) %>% 
  summarise(Number_farms = n(),
            Number_samples = sum(n_tested)) %>% 
  mutate(is_sentinel = case_when(is_sentinel == TRUE ~ "sentinel",
                                 TRUE ~ "random")) %>% 
  pivot_wider(names_from = is_sentinel, values_from = c(Number_farms, Number_samples))

summary_df_ML <- postSurv_dairy_df %>% 
  ungroup() %>% 
  group_by(is_sentinel) %>% 
  summarise(Number_farms = n(),
            Number_samples = sum(n_sampling)) %>% 
  mutate(is_sentinel = case_when(is_sentinel == TRUE ~ "sentinel",
                                 TRUE ~ "random")) %>% 
  pivot_wider(names_from = is_sentinel, values_from = c(Number_farms, Number_samples))
