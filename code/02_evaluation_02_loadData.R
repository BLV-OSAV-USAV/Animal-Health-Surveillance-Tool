
#------------------------------------------------------------------------------#
# Load data
#------------------------------------------------------------------------------#

# see chapter 9 of the code documentation

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
              title = "Choose the disease for which the surveillance programme
              should me evaluated.")

################################################################################
### Load parameters ############################################################
################################################################################

parameters <- read_excel("Parameters/Evaluation_parameters.xlsx",
                         range = "B5:S7") |> 
  filter(disease == Disease[Index])

remove(Disease, Index)

################################################################################
### Load surveillance data #####################################################
################################################################################

### Dairy component ------------------------------------------------------------

if (parameters$disease == "IBR") {
  
  lab_code <- "Infektiöse bovine Rhinotracheitis /Infektiöse pustulöse Vulvovaginitis"
  
} else if (parameters$disease == "EBL") {
  
  lab_code <- "Enzootische Leukose der Rinder"
}

postSurv_dairy_df <- read_excel("Auswertung_TM_IBR_EBL.xlsx",
                                sheet = "Einzelresultate",
                                skip = 3) |>
  select(Labor_ID = `Labor-ID`,
         Sample_ID = `Proben-ID`,
         Sampling_date = Probenahmedatum,
         TVD_Nummer = `Betrieb-ID`,
         Disease = UntersuchungVon,
         Info = `UntersuchungsParameter-ID`,
         Method = `Nachweis P Ebene`,
         Result = `Ergebnis Qualitativ P Ebene`,
         Result_date = `Laborergebnisdatum P Ebene`) |> 
  filter(Disease == lab_code)

postSurv_dairy_df$TVD_Nummer <- postSurv_dairy_df$TVD_Nummer |> as.double()

### Beef component -------------------------------------------------------------

postSurv_beef_df <- read_excel("Auswertung_BP_IBR_EBL.xlsx",
                               sheet = "Einzelresultate",
                               skip = 3) |>
  select(Labor_ID = `Labor-ID`,
         Sample_ID = `Proben-ID`,
         Sampling_date = Probenahmedatum,
         TVD_Nummer = `Betrieb-ID`,
         Ear_tag = `Tier-ID`,
         Disease = UntersuchungVon,
         Surveillance_programme = `Untersuchungsprogramm`,
         Info = `UntersuchungsParameter-ID`,
         Method = `Nachweis P Ebene`,
         Result = `Ergebnis Qualitativ P Ebene`,
         Result_date = `Laborergebnisdatum P Ebene`) |> 
  filter(Disease == lab_code)
postSurv_beef_df$TVD_Nummer <- postSurv_beef_df$TVD_Nummer |> as.double()

remove(lab_code)

################################################################################
### Load list of sentinels #####################################################
################################################################################

if (parameters$disease == "IBR") {
  
  sentinels_dairy <- read_excel("sentinelsML.xlsx") |>
    select(TVD_Nummer = TVD,
           is_sentinel = IBR.x,
           remark = Notizien)
  
  sentinels_beef <- read_excel("sentinelsNML.xlsx") |>
    select(TVD_Nummer = TVD,
           is_sentinel = IBR.x)
  
} else if (parameters$disease == "EBL") {
  
  sentinels_dairy <- read_excel("sentinelsML.xlsx") |>
    select(TVD_Nummer = TVD,
           is_sentinel = EBL.x,
           remark = Notizien)
  
  sentinels_beef <- read_excel("sentinelsNML.xlsx") |>
    select(TVD_Nummer = TVD,
           is_sentinel = EBL.x)
}

sentinels_dairy$TVD_Nummer <- sentinels_dairy$TVD_Nummer |> as.double()
sentinels_beef$TVD_Nummer <- sentinels_beef$TVD_Nummer |> as.double()

################################################################################
### Load risk data #############################################################
################################################################################

### Load risk category for the farms -------------------------------------------

if (parameters$disease == "IBR") {
  
  riskCatHerds <- read_excel("Cattle farms with risk factors IBR EBL.xlsx") |>
    lazy_dt() |>
    select(TVD_Nummer = TVDNumber,
           risk_category = riskCategoryIBR) |>
    as_tibble()
  
} else if (parameters$disease == "EBL") {
  
  riskCatHerds <- read_excel("Cattle farms with risk factors IBR EBL.xlsx") |>
    lazy_dt() |>
    select(TVD_Nummer = TVDNumber,
           risk_category = riskCategoryEBL) |>
    as_tibble()
}

riskCatHerds$TVD_Nummer <- riskCatHerds$TVD_Nummer |> as.double()

### Load relative risk for risk categories -------------------------------------

risk_category_code_df <- read_excel("Parameters/relative_risk.xlsx",
                                    sheet = paste0(parameters$disease, "_risk_categories"))

relative_risk_df <- read_excel("Parameters/relative_risk.xlsx",
                               sheet = paste0(parameters$disease, "_relative_risk"))

################################################################################
### Load number of animals per herd ############################################
################################################################################

n_animals_df <- read_excel("(BVD)_Berechnung_BVDReferenceCount.xlsx",
                           sheet = "Betriebsliste") |>
  lazy_dt() |>
  select(TVD_Nummer = `TVD Nummer`, n_animals = `Mittlere Anzahl Tiere`) |>
  as_tibble()

n_animals_df$TVD_Nummer <- n_animals_df$TVD_Nummer |> as.double()

################################################################################
### Load LastPFree #############################################################
################################################################################

PFree_last <- read.xlsx(paste("results/evaluation/official_results",
                              parameters$year - 1,
                              parameters$disease,
                              "results.xlsx",
                              sep = "/")) |> 
  select(Median) |> 
  pull() |> 
  as.double()
