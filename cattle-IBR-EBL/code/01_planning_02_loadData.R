
#------------------------------------------------------------------------------#
# Load data
#------------------------------------------------------------------------------#

# See chapter 2 of the code documentation

################################################################################
### Create timestamps ##########################################################
################################################################################

timestamp <- now() |> format("%Y-%m-%d_%H-%M-%S")

################################################################################
### Choose disease #############################################################
################################################################################

Disease <- c("IBR", "EBL")

Index <- menu(gettext(c("Infectious bovine rhinotracheitis (IBR)",
                        "Enzootic bovine leukosis (EBL)")),
              graphics = TRUE,
              title = "Choose the disease for which the planning
              of the surveillance programme should me made.")

################################################################################
### Load parameters ############################################################
################################################################################

parameters <- read_excel("~/Parameters/Planning_parameters.xlsx",
                         range = "B5:V7") |> 
  filter(disease == Disease[Index])


parameters_2nd_disease <- read_excel("~/Parameters/Planning_parameters.xlsx",
                                     range = "B5:V7") |> 
  filter(disease != Disease[Index])

remove(Disease, Index)



################################################################################
### Load risk data #############################################################
################################################################################

### Load risk category for the farms -------------------------------------------

if (parameters$disease == "IBR") {
  
  cattle_df <- read_excel("~/Cattle farms with risk factors IBR EBL.xlsx") |>
    lazy_dt() |>
    select(TVD_Nummer = TVDNumber,
           risk_category = riskCategoryIBR) |>
    as_tibble()
  
} else if (parameters$disease == "EBL") {
  
  cattle_df <- read_excel("~/Cattle farms with risk factors IBR EBL.xlsx") |>
    lazy_dt() |>
    select(TVD_Nummer = TVDNumber,
           risk_category = riskCategoryEBL) |>
    as_tibble()
}


if (parameters_2nd_disease$disease == "IBR") {
  
  cattle_2nd_disease_df <- read_excel("~/Cattle farms with risk factors IBR EBL.xlsx") |>
    lazy_dt() |>
    select(TVD_Nummer = TVDNumber,
           risk_category = riskCategoryIBR) |>
    as_tibble()
  
} else if (parameters_2nd_disease$disease == "EBL") {
  
  cattle_2nd_disease_df <- read_excel("~/Cattle farms with risk factors IBR EBL.xlsx") |>
    lazy_dt() |>
    select(TVD_Nummer = TVDNumber,
           risk_category = riskCategoryEBL) |>
    as_tibble()
}

### Load relative risk for risk categories -------------------------------------

risk_category_code_df <- read_excel("~/Parameters/relative_risk.xlsx",
                                    sheet = paste0(parameters$disease, "_risk_categories"))

relative_risk_df <- read_excel("~/Parameters/relative_risk.xlsx",
                               sheet = paste0(parameters$disease, "_relative_risk"))


risk_category_code_2nd_disease_df <- read_excel("~/Parameters/relative_risk.xlsx",
                                                sheet = paste0(parameters_2nd_disease$disease, "_risk_categories"))

relative_risk_2nd_disease_df <- read_excel("~/Parameters/relative_risk.xlsx",
                                           sheet = paste0(parameters_2nd_disease$disease, "_relative_risk"))

################################################################################
### Load suitable sentinels for the BTM component ##############################
################################################################################

good_sentinel_candidates <- read_excel("202110_202209_MP_Proben_Anzahl TM Okt2021_September2022.xlsx") |> 
  filter(AnzProben >= 12) |> 
  select(TVD_Nummer = TVDId) |> 
  mutate(Component = "dairy")

################################################################################
### Load suitable sentinels for the beef component #############################
################################################################################

good_candidates_beef <- read_excel("RiBeS_IBRProbanden_Schlachtungen_7nov2021bis7nov2022.xlsx") |> 
  rename(TVD_Nummer = `TVD Nummer HKB`)

################################################################################
### Load LastPFree #############################################################
################################################################################

PFree_last <- read.xlsx(paste("~/results/evaluation/official_results",
                              parameters$year - 1,
                              parameters$disease,
                              "results.xlsx",
                              sep = "/")) |> 
  select(Median) |> 
  pull() |> 
  as.double()


PFree_last_2nd_disease <- read.xlsx(paste("~/results/evaluation/official_results",
                                          parameters$year - 1,
                                          parameters_2nd_disease$disease,
                                          "results.xlsx",
                                          sep = "/")) |> 
  select(Median) |> 
  pull() |> 
  as.double()

################################################################################
### Load herds to exclude from sentinels #######################################
################################################################################

exclude_from_sentinels <- read_excel("~/parameters/exclude_from_sentinels.xlsx")

################################################################################
### Load canton of the farms ###################################################
################################################################################

canton_df <- read_excel("~/TVKplus_TVD-290_AUSWERTUNG_DATEN_stammdatenabfrabe_blv.xlsx") |> 
  select(TVD_Nummer = TVDNr, Kanton)
