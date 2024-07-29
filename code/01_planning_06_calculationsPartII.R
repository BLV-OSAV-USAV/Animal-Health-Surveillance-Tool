
#------------------------------------------------------------------------------#
# Calculations part II
#------------------------------------------------------------------------------#

# See chapter 6 of the code documentation

################################################################################
### Select the number of sentinels #############################################
################################################################################

### Ask if ready to go forward -------------------------------------------------

askYesNo(paste0("Intermediate results were exported in the session journal.\n",
                "Please take some time to review them, ",
                "and when you are ready to select the number of sentinels ",
                "for the components beef and dairy, ",
                "then please click on yes and answer the in-line questions."),
         prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel"))),
         default = FALSE)

### Enter number of sentinels -------------------------------------------------

n_sentinels_beef <- readline(paste0("How many sentinel herds should be sampled ",
                                    "in the beef component?")) |> 
  as.integer()

n_sentinels_dairy <- readline(paste0("How many sentinel herds should be sampled ",
                                     "in the dairy component?")) |> 
  as.integer()

if (n_sentinels_beef < 0 | n_sentinels_dairy < 0) {
  stop("The value attributed to n_sentinels must be a positive integer.")
}

################################################################################
### Calculate the scenario tree ################################################
################################################################################

### Extract CSe_sentinel -------------------------------------------------------

if (n_sentinels_dairy == 0) {
  
  CSe_sentinel_dairy_selected <- tibble(Iteration = seq_len(parameters$n_iterations),
                                        CSe_EBL = 0,
                                        CSe_IBR = 0,
                                        n_sentinels = 0)
  
} else if (n_sentinels_dairy > CSe_sentinel_dairy$n_sentinels |> max()) {
  
  n_sentinels_dairy <- CSe_sentinel_dairy$n_sentinels |> max()
  
  CSe_sentinel_dairy_selected <- CSe_sentinel_dairy |> 
    filter(n_sentinels == n_sentinels_dairy)
  
} else {
  
  CSe_sentinel_dairy_selected <- CSe_sentinel_dairy |> 
    filter(n_sentinels == n_sentinels_dairy)
}

if (n_sentinels_beef == 0) {
  
  CSe_sentinel_beef_selected <- tibble(Iteration = seq_len(parameters$n_iterations),
                                       CSe_EBL = 0,
                                       CSe_IBR = 0,
                                       n_sentinels = 0)
  
} else if (n_sentinels_beef > CSe_sentinel_beef$n_sentinels |> max()) {
  
  n_sentinels_beef <- CSe_sentinel_beef$n_sentinels |> max()
  
  CSe_sentinel_beef_selected <- CSe_sentinel_beef |> 
    filter(n_sentinels == n_sentinels_beef)
  
} else {
  
  CSe_sentinel_beef_selected <- CSe_sentinel_beef |> 
    filter(n_sentinels == n_sentinels_beef)
}

### Calculate SSe --------------------------------------------------------

CSe_random_beef$Iteration <- as.character(CSe_random_beef$Iteration)
CSe_random_dairy$Iteration <- as.character(CSe_random_dairy$Iteration)
CSe_random_beef_2nd_disease$Iteration <- as.character(CSe_random_beef_2nd_disease$Iteration)
CSe_random_dairy_2nd_disease$Iteration <- as.character(CSe_random_dairy_2nd_disease$Iteration)
CSe_sentinel_beef_selected$Iteration <- as.character(CSe_sentinel_beef_selected$Iteration)
CSe_sentinel_dairy_selected$Iteration <- as.character(CSe_sentinel_dairy_selected$Iteration)

SSe <- full_join(CSe_random_beef |> rename(!! paste0("CSe_", parameters$disease, "_random_beef") := CSe) |> select(-HSe),
                 CSe_random_dairy |> rename(!! paste0("CSe_", parameters$disease, "_random_dairy") := CSe),
                 by = c("Iteration", "id")) |> 
  full_join(CSe_random_beef_2nd_disease |> rename(!! paste0("CSe_", parameters_2nd_disease$disease, "_random_beef") := CSe) |> select(-HSe),
            by = c("Iteration", "id")) |> 
  full_join(CSe_random_dairy_2nd_disease |> rename(!! paste0("CSe_", parameters_2nd_disease$disease, "_random_dairy") := CSe),
            by = c("Iteration", "id")) |>  
  full_join(CSe_sentinel_beef_selected |> rename(CSe_IBR_sentinel_beef = CSe_IBR, CSe_EBL_sentinel_beef = CSe_EBL, n_sentinels_beef = n_sentinels),
            by = "Iteration") |> 
  full_join(CSe_sentinel_dairy_selected |> rename(CSe_IBR_sentinel_dairy = CSe_IBR, CSe_EBL_sentinel_dairy = CSe_EBL, n_sentinels_dairy = n_sentinels),
            by = "Iteration") |> 
  mutate(IBR_CSe_beef = 1 - (1 - CSe_IBR_random_beef) * (1 - CSe_IBR_sentinel_beef),
         IBR_CSe_dairy = 1 - (1 - CSe_IBR_random_dairy) * (1 - CSe_IBR_sentinel_dairy),
         IBR_CSe_random = 1 - (1 - CSe_IBR_random_beef) * (1 - CSe_IBR_random_dairy),
         IBR_CSe_sentinel = 1 - (1 - CSe_IBR_sentinel_beef) * (1 - CSe_IBR_sentinel_dairy),
         IBR_SSe = 1 - (1 - CSe_IBR_random_beef) * (1 - CSe_IBR_random_dairy) * (1 - CSe_IBR_sentinel_beef) * (1 - CSe_IBR_sentinel_dairy),
         EBL_CSe_beef = 1 - (1 - CSe_EBL_random_beef) * (1 - CSe_EBL_sentinel_beef),
         EBL_CSe_dairy = 1 - (1 - CSe_EBL_random_dairy) * (1 - CSe_EBL_sentinel_dairy),
         EBL_CSe_random = 1 - (1 - CSe_EBL_random_beef) * (1 - CSe_EBL_random_dairy),
         EBL_CSe_sentinel = 1 - (1 - CSe_EBL_sentinel_beef) * (1 - CSe_EBL_sentinel_dairy),
         EBL_SSe = 1 - (1 - CSe_EBL_random_beef) * (1 - CSe_EBL_random_dairy) * (1 - CSe_EBL_sentinel_beef) * (1 - CSe_EBL_sentinel_dairy)) |> 
  select(Iteration, n_sentinels_beef, n_sentinels_dairy,
         IBR_CSe_random_beef = CSe_IBR_random_beef, IBR_CSe_sentinel_beef = CSe_IBR_sentinel_beef, IBR_CSe_random_dairy = CSe_IBR_random_dairy, IBR_CSe_sentinel_dairy = CSe_IBR_sentinel_dairy,
         IBR_CSe_beef, IBR_CSe_dairy, IBR_CSe_random, IBR_CSe_sentinel, IBR_SSe,
         EBL_CSe_random_beef = CSe_EBL_random_beef, EBL_CSe_sentinel_beef = CSe_EBL_sentinel_beef, EBL_CSe_random_dairy = CSe_EBL_random_dairy, EBL_CSe_sentinel_dairy = CSe_EBL_sentinel_dairy,
         EBL_CSe_beef, EBL_CSe_dairy, EBL_CSe_random, EBL_CSe_sentinel, EBL_SSe)

remove(CSe_random_beef, CSe_random_dairy, CSe_random_beef_2nd_disease, CSe_random_dairy_2nd_disease, CSe_sentinel_beef_selected, CSe_sentinel_dairy_selected)

### Calculate PostPFree --------------------------------------------------------

PIntro_i$Iteration <- as.character(PIntro_i$Iteration)

PIntro_i <- PIntro_i |> 
  rename(!! paste0("PIntro_", parameters$disease) := PIntro,
         !! paste0("PIntro_", parameters_2nd_disease$disease) := PIntro_2nd_disease,
         !! paste0("PriorPFree_", parameters$disease) := PriorPFree,
         !! paste0("PriorPFree_", parameters_2nd_disease$disease) := PriorPFree_2nd_disease)

SSe <- left_join(SSe, PIntro_i, by = "Iteration")

PostPFree <- SSe |> 
  mutate(PostPFree_IBR = post_fr(prior_fr = PriorPFree_IBR, Se = IBR_SSe),
         PostPFree_EBL = post_fr(prior_fr = PriorPFree_EBL, Se = EBL_SSe)) |> 
  select(-PIntro_IBR, -PriorPFree_IBR, -PIntro_EBL, -PriorPFree_EBL)

remove(SSe)
