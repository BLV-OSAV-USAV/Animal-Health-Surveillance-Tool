
#------------------------------------------------------------------------------#
# Edit data
#------------------------------------------------------------------------------#

# See chapter 10 of the code documentation

################################################################################
### Merge the data #############################################################
################################################################################

### Merge the data -------------------------------------------------------------

postSurv_dairy_df <- left_join(postSurv_dairy_df, sentinels_dairy, by = "TVD_Nummer") |>
  left_join(n_animals_df, by = "TVD_Nummer")

postSurv_beef_df <- left_join(postSurv_beef_df, sentinels_beef, by = "TVD_Nummer") |>
  left_join(n_animals_df, by = "TVD_Nummer")

postSurv_dairy_df$n_animals <- postSurv_dairy_df$n_animals |> as.double()
postSurv_beef_df$n_animals <- postSurv_beef_df$n_animals |> as.double()

postSurv_dairy_df$n_animals[is.na(postSurv_dairy_df$n_animals) | is.null(postSurv_dairy_df$n_animals) | postSurv_dairy_df$n_animals == "NULL"] <- 1000

postSurv_beef_df$n_animals[is.na(postSurv_beef_df$n_animals) | is.null(postSurv_beef_df$n_animals) | postSurv_beef_df$n_animals == "NULL"] <- 1000

remove(sentinels_dairy, sentinels_beef)

postSurv_dairy_raw <- postSurv_dairy_df
postSurv_beef_raw <- postSurv_beef_df

################################################################################
### Check correctness of TVD numbers ###########################################
################################################################################

TVD_dairy_check <- is.na(postSurv_dairy_df$TVD_Nummer) |
  postSurv_dairy_df$TVD_Nummer < 1000000 |
  postSurv_dairy_df$TVD_Nummer > 9999999

TVD_beef_check <- is.na(postSurv_beef_df$TVD_Nummer) |
  postSurv_beef_df$TVD_Nummer < 1000000 |
  postSurv_beef_df$TVD_Nummer > 9999999

if (any(TVD_dairy_check) || any(TVD_beef_check)){
  
  if (any(TVD_dairy_check)) {
    
    TVD_problems_dairy_df <- postSurv_dairy_df |> 
      subset(TVD_dairy_check)
    
  } else TVD_problems_dairy_df <- tibble()
  
  if (any(TVD_beef_check)) {
    
    TVD_problems_beef_df <- postSurv_beef_df |> 
      subset(TVD_beef_check)
    
  } else TVD_problems_beef_df <- tibble()
  
  TVD_problems_df <- bind_rows(TVD_problems_dairy_df, TVD_problems_beef_df)
  
  TVD_problems_list <- TVD_problems_df |> pull(TVD_Nummer)
  
  path <- paste("results/evaluation/session_journal", parameters$disease, timestamp, sep = "/")
  
  dir.create(path = path, recursive = TRUE)
  
  write.xlsx(TVD_problems_df, paste(path, "TVD_problems.xlsx", sep = "/"))
  
  remove(path)
  
  deal_with_TVD_problems <- askYesNo(msg = paste0("Some herds (",
                                                  paste(TVD_problems_list, collapse = ", "),
                                                  ") have missing or false TVD number.\n",
                                                  "You can find information on these herds ",
                                                  "in the table \'TVD-problems.xlsx\' ",
                                                  "in the session results of this session.\n",
                                                  "You can choose to continue the calculations ",
                                                  "without correcting the data ",
                                                  "by choosing \'Yes\'.",
                                                  "However, please be aware that in this case ",
                                                  "all missing TVD Numbers will be considered ",
                                                  "as being one and only one herd.\n",
                                                  "Altenatively, choose \'No\' and correct ",
                                                  "the inputs before running this tool again."),
                                     prompts = getOption("askYesNo",
                                                         gettext(c("Yes", "No", "Cancel"))),
                                     default = FALSE)
  
  if (deal_with_TVD_problems == FALSE) {
    
    stop(paste0("You chose to stop the evaluation tool to correct ",
                "the TVD numbers of the herds.\n"))
    
  } else if (deal_with_TVD_problems != FALSE) {
    
    warning(paste0("You chose to continue the calculation ",
                   "even if some TVD numbers were false or missing.\n",
                   "Be aware that errors can happen during the calculation ",
                   "as all inputs from farms with missing TVD number ",
                   "will be considered as originating from ",
                   "one and only one herd."))
  }
  
  remove(deal_with_TVD_problems,
         TVD_problems_df,
         TVD_problems_list,
         TVD_problems_dairy_df,
         TVD_problems_beef_df)
}

remove(TVD_dairy_check, TVD_beef_check)

################################################################################
### Attribute herds to sentinel or random component ############################
################################################################################

postSurv_dairy_df$is_sentinel[postSurv_dairy_df$is_sentinel == "TS"] <- TRUE
postSurv_dairy_df$is_sentinel[postSurv_dairy_df$is_sentinel == "RS" | is.na(postSurv_dairy_df$is_sentinel)] <- FALSE

postSurv_beef_df$is_sentinel[postSurv_beef_df$is_sentinel == "TS"] <- TRUE
postSurv_beef_df$is_sentinel[postSurv_beef_df$is_sentinel == "RS" | is.na(postSurv_beef_df$is_sentinel)] <- FALSE

################################################################################
### Check if data collected at farm or slaughterhouse ##########################
################################################################################

postSurv_beef_df$Surveillance_programme[postSurv_beef_df$Surveillance_programme == "Nationales Tierseuchenprogramm: Probenahme in Tierhaltung"] <- "Tierhaltung"
postSurv_beef_df$Surveillance_programme[postSurv_beef_df$Surveillance_programme == "Nationales Tierseuchenprogramm: Probenahme am Schlachtbetrieb"] <- "Schlachtbetrieb"

################################################################################
### Group beef by herd instead of animal #######################################
################################################################################

postSurv_beef_df <- postSurv_beef_df |>
  group_by(TVD_Nummer) |> 
  distinct(Ear_tag, .keep_all = TRUE) |> 
  add_count(name = "n_tested") |> 
  select(TVD_Nummer, is_sentinel, n_animals, n_tested, Surveillance_programme) |>
  distinct(TVD_Nummer, is_sentinel, n_animals, n_tested,.keep_all = TRUE)

postSurv_beef_df$n_animals[postSurv_beef_df$n_animals < postSurv_beef_df$n_tested] <- postSurv_beef_df$n_tested[postSurv_beef_df$n_animals < postSurv_beef_df$n_tested]

################################################################################
### Add number of samples for BTM ##############################################
################################################################################

postSurv_dairy_df <- postSurv_dairy_df |>
  group_by(TVD_Nummer) |>
  add_count(name = "n_sampling") |>
  select(TVD_Nummer, is_sentinel, n_animals, n_sampling) |>
  distinct()

################################################################################
### Attribute relative risk to herds ###########################################
################################################################################

### Attribute risk category ----------------------------------------------------

risk_factors <- relative_risk_df$Abbreviation

for (RF in risk_factors) {
  
  risk_category_code_df <- risk_category_code_df |>
    mutate(!! RF := case_when(str_detect(Risk_factors, RF) | Risk_factors == "All_risk_factors" ~ TRUE,
                              TRUE ~ FALSE))
}

remove(RF)

risk_category_code_df <- risk_category_code_df |> 
  select(-Risk_factors) |>
  pivot_longer(cols = all_of(risk_factors), names_to = "RF_name", values_to = "RF_level")

RR_iterations <- tibble(Iteration = seq_len(parameters$n_iterations))

RR_i <- relative_risk_df |>
  select(Abbreviation) |>
  crossing(RR_iterations) |>
  left_join(relative_risk_df |> select(-Description),
            by = c("Abbreviation")) |>
  rowwise() |>
  mutate(RR = rpert(n = 1,
                    x.min = RR_min,
                    x.max = RR_max,
                    x.mode = RR_mode)) |>
  select(Abbreviation, Iteration, RR) |>
  rename(RF_name = Abbreviation) |>
  right_join(risk_category_code_df, by = c("RF_name"))

RR_i$RR[RR_i$RF_level == FALSE] <- 1

RR_i <- RR_i |>
  rename(RR_not_grouped = RR) |>
  group_by(Risk_category, Iteration) |>
  summarise(RR = prod(RR_not_grouped)) |> 
  ungroup()

remove(risk_category_code_df, relative_risk_df, risk_factors, RR_iterations)

### Replace with relative risk -------------------------------------------------

riskCatHerds <- riskCatHerds |>
  lazy_dt() |>
  rename(Risk_category = risk_category) |>
  left_join(RR_i |> lazy_dt(), by = c("Risk_category")) |>
  select(-Risk_category) |>
  as_tibble()
