
#------------------------------------------------------------------------------#
# Export intermediate results
#------------------------------------------------------------------------------#

# See chapter 5 of the code documentation

################################################################################
### Save sessions results ######################################################
################################################################################

### Prepare the session results ------------------------------------------------

canton_df$TVD_Nummer <- canton_df$TVD_Nummer |> as.character()

beef_sentinel_df <- beef_sentinel_df |> 
  left_join(canton_df, by = "TVD_Nummer") |> 
  select(TVD_Nummer, Kanton, averagedARF, Sentinel_position, Iteration, ARR, ARR_2nd_disease) |> 
  rename(!! paste0("ARR_", parameters$disease) := ARR,
         !! paste0("ARR_", parameters_2nd_disease$disease) := ARR_2nd_disease)

dairy_sentinel_df <- dairy_sentinel_df |> 
  left_join(canton_df, by = "TVD_Nummer") |> 
  select(TVD_Nummer, Kanton, averagedARF, Sentinel_position, Iteration, ARR, ARR_2nd_disease) |> 
  rename(!! paste0("ARR_", parameters$disease) := ARR,
         !! paste0("ARR_", parameters_2nd_disease$disease) := ARR_2nd_disease)

results <- list(interm_results_CSe_beef = "CSe_sentinel_beef",
                interm_results_CSe_dairy = "CSe_sentinel_dairy",
                list_sentinel_beef = "beef_sentinel_df",
                list_sentinel_dairy = "dairy_sentinel_df")

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
