
#------------------------------------------------------------------------------#
# Export results
#------------------------------------------------------------------------------#

# See chapter 12 of the code documentation

################################################################################
### Save sessions results ######################################################
################################################################################

random_iterations_export_df <- full_join(Se_i, BTM_Se_i, by = "Iteration") |> 
  full_join(PIntro_i %>% select(-PriorPFree),  by = "Iteration")

results <- list(Results = "Results_eval",
                PostPFree_i = "PostPFree",
                Samples_i = "random_iterations_export_df",
                Relative_risk_i = "RR_i",
                Parameters = "parameters",
                Summary_table = "summary_df",
                Dairy_surv_raw = "postSurv_dairy_raw",
                Beef_surv_raw = "postSurv_beef_raw",
                Dairy_surv = "postSurv_dairy_df",
                Beef_surv = "postSurv_beef_df",
                Overview_dairy = "summary_df_ML",
                Overview_beef = "summary_df_NML" )

export_results_fn <- function(index, results) {
  
  name <- names(results[index])
  table <- get(paste0(results[index]))
  
  path <- paste("results/evaluation/session_journal", parameters$disease, timestamp, sep = "/")
  
  dir.create(path = path, recursive = TRUE)
  
  write_excel_csv2(x = table,
                   file = paste0(paste(path, name, sep = "/"), ".csv"))
}

walk(seq_len(length(results)), export_results_fn, results = results)

remove(results, export_results_fn, PIntro_i, Se_i, BTM_Se_i)

################################################################################
### Save official results ######################################################
################################################################################

### Prepare the official results -----------------------------------------------

saveOfficialResults <- askYesNo(paste0("Do you really wand to run this chunk?\n",
                                       "If you click yes, it will remove ",
                                       "previous results if there were any.\n",
                                       "Run ONLY if you want to use the results ",
                                       "of this session to evaluate ",
                                       "the last surveillance programme.\n",
                                       "If you click no, resuls of this session ",
                                       "will still be available afterwards ",
                                       "under the session journal"),
                                prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel"))),
                                default = FALSE)

if (is.na(saveOfficialResults)) {
  stop(paste0("You decided to cancel the export of official data.\n",
              "This causes the tool to stop.\n",
              "If you want the tool to run until the end ",
              "but without exporting the results, ",
              "please choose `No`."))
}

### Export the official results ------------------------------------------------

if (saveOfficialResults == TRUE) {
  
  dir.create(paste("results/evaluation/official_results",
                   parameters$year,
                   parameters$disease,
                   "reproducibility",
                   sep = "/"),
             recursive = TRUE)
  
  write.xlsx(x = Results_eval,
             file = paste("results/evaluation/official_results",
                          parameters$year,
                          parameters$disease,
                          "results.xlsx",
                          sep = "/"))
  
  write.xlsx(x = PostPFree,
             file = paste("results/evaluation/official_results",
                          parameters$year,
                          parameters$disease,
                          "results_i.xlsx",
                          sep = "/"))
  
  write.xlsx(x = summary_df,
             file = paste("results/evaluation/official_results",
                          parameters$year,
                          parameters$disease,
                          "summary_table.xlsx",
                          sep = "/"))
  
  write.xlsx(x = parameters,
             file = paste("results/evaluation/official_results",
                          parameters$year,
                          parameters$disease,
                          "parameters.xlsx",
                          sep = "/"),
             overwrite = TRUE)
  
  write.xlsx(x = summary_df_NML,
             file = paste("results/evaluation/official_results",
                          parameters$year,
                          parameters$disease,
                          "overview_beef.xlsx",
                          sep = "/"),
             overwrite = TRUE)
  
  write.xlsx(x = summary_df_ML,
             file = paste("results/evaluation/official_results",
                          parameters$year,
                          parameters$disease,
                          "overview_dairy.xlsx",
                          sep = "/"),
             overwrite = TRUE)
  
  write_excel_csv2(x = random_iterations_export_df,
                   file = paste("results/evaluation/official_results",
                                parameters$year,
                                parameters$disease,
                                "reproducibility/samples_i.csv",
                                sep = "/"))
  
  write_excel_csv2(x = RR_i,
                   file = paste("results/evaluation/official_results",
                                parameters$year,
                                parameters$disease,
                                "reproducibility/relative_risk_i.csv",
                                sep = "/"))
  
  write_excel_csv2(x = postSurv_dairy_raw,
                   file = paste("results/evaluation/official_results",
                                parameters$year,
                                parameters$disease,
                                "reproducibility/postSurv_dairy_raw.csv",
                                sep = "/"))
  
  write_excel_csv2(x = postSurv_beef_raw,
                   file = paste("results/evaluation/official_results",
                                parameters$year,
                                parameters$disease,
                                "reproducibility/postSurv_beef_raw.csv",
                                sep = "/"))
  
  data <- loadWorkbook(paste0("Auswertung_TM_IBR_EBL.xlsx"))
  
  saveWorkbook(wb = data,
               file = paste("results/evaluation/official_results",
                            parameters$year,
                            parameters$disease,
                            "reproducibility/Auswertung_TM_IBR_EBL.xlsx",
                            sep = "/"), overwrite = TRUE)
  
  remove(data)
}
