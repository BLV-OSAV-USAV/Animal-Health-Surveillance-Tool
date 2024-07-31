
#------------------------------------------------------------------------------#
# Export final results
#------------------------------------------------------------------------------#

# See chapter 7 of the code documentation

################################################################################
### Save sessions results ######################################################
################################################################################

### Prepare the session results ------------------------------------------------

sentinel_beef <- beef_sentinel_df |> 
  filter(Sentinel_position <= n_sentinels_beef) |> 
  select(TVD_Nummer)

sentinel_dairy <- dairy_sentinel_df |> 
  filter(Sentinel_position <= n_sentinels_dairy) |> 
  select(TVD_Nummer)

results <- list(results = "PostPFree", sentinel_beef = "sentinel_beef", sentinel_dairy = "sentinel_dairy")

### Export the session results -------------------------------------------------

export_results_fn <- function(index, results) {
  
  name <- names(results[index])
  table <- get(paste0(results[index]))
  
  path <- paste("results/planning/session_journal", timestamp, sep = "/")
  
  dir.create(path = path, recursive = TRUE)
  
  write_excel_csv2(x = table,
                   file = paste0(paste(path, name, sep = "/"), ".csv"))
}

walk(seq_len(length(results)), export_results_fn, results = results)

remove(results, export_results_fn)

# ################################################################################
# ### Save official results ######################################################
# ################################################################################
# 
# ### Prepare the official results -----------------------------------------------
# 
# saveOfficialResults <- askYesNo(paste0("Do you really wand to run this chunk?\n",
#                                        "If you click yes, it will remove ",
#                                        "previous results if there were any.\n",
#                                        "Run ONLY if you want to use ",
#                                        "the results of this session ",
#                                        "to plan for next surveillance programme.\n",
#                                        "If you click no, results of this session ",
#                                        "will still be available afterwards ",
#                                        "under the session journal"),
#                                 prompts = getOption("askYesNo", gettext(c("Yes", "No", "Cancel"))),
#                                 default = FALSE)
# 
# if (is.na(saveOfficialResults)) {
#   stop(paste0("You decided to cancel the export of official data.\n",
#               "This causes the tool to stop.\n",
#               "If you want the tool to run until the end ",
#               "but without exporting the results, please choose `No`."))
# }
# 
# if (saveOfficialResults == TRUE) {
#   
#   if (interactive()) {
#     n_sentinels <- readline(paste0("How many sentinels should be sampled ",
#                                    "in the sentinel component?")) |> 
#       as.integer()
#   }
#   
#   if (n_sentinels <= 0) {
#     stop(paste0("The value attributed to n_sentinels must be an integer ",
#                 "comprised between 1 and ",
#                 max(Results_planning$n_sentinels),
#                 "."))
#   }
# }
# 
# if (saveOfficialResults == TRUE) {
#   
#   sentinels <- slice_head(poultry_sentinel_export_df, n = n_sentinels)
#   
#   sentinels$TVD_Nummer <- sentinels$TVD_Nummer |> as.double()
#   
#   results_n_sentinels <- Results_planning |> 
#     filter(n_sentinels == !!n_sentinels)
# }
# 
# ### Export the official results ------------------------------------------------
# 
# if (saveOfficialResults == TRUE) {
#   
#   dir.create(path = paste("results/planning/official_results",
#                           parameters$year,
#                           "reproducibility",
#                           sep = "/"),
#              recursive = TRUE)
#   
#   write.xlsx(x = sentinels,
#              file = paste("results/planning/official_results",
#                           parameters$year,
#                           "sentinels.xlsx",
#                           sep = "/"),
#              overwrite = TRUE)
#   
#   write.xlsx(x = results_n_sentinels,
#              file = paste("results/planning/official_results",
#                           parameters$year,
#                           "probability_of_freedom.xlsx",
#                           sep = "/"),
#              overwrite = TRUE)
#   
#   write.xlsx(x = parameters,
#              file = paste("results/planning/official_results",
#                           parameters$year,
#                           "parameters.xlsx",
#                           sep = "/"),
#              overwrite = TRUE)
#   
#   write_excel_csv2(x = random_iterations_export_df,
#                    file = paste("results/planning/official_results",
#                                 parameters$year,
#                                 "reproducibility/samples_i.csv",
#                                 sep = "/"))
#   
#   write_excel_csv2(x = poultry_ARF_export_df,
#                    file = paste("results/planning/official_results",
#                                 parameters$year,
#                                 "reproducibility/relative_risks.csv",
#                                 sep = "/"))
#   
#   data <- loadWorkbook("AI_ND_surveillance__poultry_data.xlsx")
#   
#   saveWorkbook(wb = data,
#                file = paste("results/planning/official_results",
#                             parameters$year,
#                             "reproducibility/AI_ND_surveillance__poultry_data.xlsx",
#                             sep = "/"),
#                overwrite = TRUE)
#   
#   remove(data)
# }
