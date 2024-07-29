
#------------------------------------------------------------------------------#
# Edit data
#------------------------------------------------------------------------------#

# See chapter 3 of the code documentation

################################################################################
### Calculate suitable sentinels for beef component ############################
################################################################################

### Remove BTM sentinels -------------------------------------------------------

good_candidates_beef <- good_candidates_beef |> 
  anti_join(good_sentinel_candidates, by = "TVD_Nummer")

### Filter for animals > 6 month -----------------------------------------------

good_candidates_beef$Geburtsdatum <- as_date(good_candidates_beef$Geburtsdatum)
good_candidates_beef$Todesdatum <- as_date(good_candidates_beef$Todesdatum)

good_candidates_beef <- good_candidates_beef |> 
  filter(Todesdatum - Geburtsdatum >= 180)

### Select suitable herds ------------------------------------------------------

good_candidates_beef <- good_candidates_beef |> 
  filter(`TVD Nummer AHB` %in% c(1836622, 1554489, 1285666, 1285673, 1620900, 1856453, 1176186)) |> 
  group_by(TVD_Nummer) |> 
  summarise(n_animals = n()) |> 
  filter(n_animals >= 1.25 * parameters$n_sentinel_beef) |> 
  mutate(Component = "beef") |> 
  select(TVD_Nummer, Component)

good_sentinel_candidates <- bind_rows(good_sentinel_candidates, good_candidates_beef)

remove(good_candidates_beef)

################################################################################
### Attribute risk #############################################################
################################################################################

### Calculate relative risk for 1st disease ------------------------------------

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

### Calculate relative risk for 2nd disease ------------------------------------

risk_factors <- relative_risk_2nd_disease_df$Abbreviation

for (RF in risk_factors) {
  
  risk_category_code_2nd_disease_df <- risk_category_code_2nd_disease_df |>
    mutate(!! RF := case_when(str_detect(Risk_factors, RF) | Risk_factors == "All_risk_factors" ~ TRUE,
                              TRUE ~ FALSE))
}

remove(RF)

risk_category_code_2nd_disease_df <- risk_category_code_2nd_disease_df |> 
  select(-Risk_factors) |>
  pivot_longer(cols = all_of(risk_factors), names_to = "RF_name", values_to = "RF_level")

RR_iterations <- tibble(Iteration = seq_len(parameters$n_iterations))

RR_i_2nd_disease <- relative_risk_2nd_disease_df |>
  select(Abbreviation) |>
  crossing(RR_iterations) |>
  left_join(relative_risk_2nd_disease_df |> select(-Description),
            by = c("Abbreviation")) |>
  rowwise() |>
  mutate(RR = rpert(n = 1,
                    x.min = RR_min,
                    x.max = RR_max,
                    x.mode = RR_mode)) |>
  select(Abbreviation, Iteration, RR) |>
  rename(RF_name = Abbreviation) |>
  right_join(risk_category_code_2nd_disease_df, by = c("RF_name"))

RR_i_2nd_disease$RR[RR_i_2nd_disease$RF_level == FALSE] <- 1

RR_i_2nd_disease <- RR_i_2nd_disease |>
  rename(RR_not_grouped = RR) |>
  group_by(Risk_category, Iteration) |>
  summarise(RR = prod(RR_not_grouped)) |> 
  ungroup()

remove(risk_category_code_2nd_disease_df, relative_risk_2nd_disease_df, risk_factors, RR_iterations)

### Attribute relative risks to the herds --------------------------------------

cattle_df_raw <- cattle_df
cattle_2nd_disease_df_raw <- cattle_2nd_disease_df

cattle_df <- cattle_df |>
  lazy_dt() |>
  rename(Risk_category = risk_category) |>
  left_join(RR_i |> lazy_dt(), by = c("Risk_category")) |>
  select(-Risk_category) |>
  as_tibble()

cattle_2nd_disease_df <- cattle_2nd_disease_df |>
  lazy_dt() |>
  rename(Risk_category = risk_category) |>
  left_join(RR_i_2nd_disease |> lazy_dt(), by = c("Risk_category")) |>
  select(-Risk_category) |>
  as_tibble()