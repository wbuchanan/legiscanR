#' @title Bill Ids from LegiScan Bill API Requests
#' @description Generates a data.frame object of ID elements for LegiScan Bill data
#' @param billobject A file/API request to the bill data
#' @return Returns a data.frame object of ID elements for LegiScan Bill data
#' @examples \dontrun{
#' # Pass an unprocessed/cleaned bill object to billids to clean up the
#' # identifiers in the API response
#' billobject_cleaner <- billids(billobject_raw)
#' }
#' @family Parsing and Cleaning LegiScan Data
#' @importFrom plyr llply
#' @importFrom magrittr %>%
#' @name billids
billids <- function(billobject) {

	# Create a list of identifiers from a the bill object passed to the function
	idList <- list(session_id = billobject[["session"]][[1]],
				   bill_id = billobject[["bill_id"]],
				   change_hash = billobject[["change_hash"]],
				   state = billobject[["state"]],
				   state_id = billobject[["state_id"]],
				   bill_number = billobject[["bill_number"]])

	# Clean up the null values
	idVars <- plyr::llply(idList, .fun = function(rmNulls) {

		# Replace length 0 vectors with blank character string
		if (length(rmNulls) == 0) rmNulls <- ""

		# else they retain the same value
		else rmNulls <- as.character(rmNulls)

		# Then pipe the output into a data.frame object
	}) %>% as.data.frame(stringsAsFactors = FALSE)

	# Return the cleaned up id variables
	return(idVars)

} # End of Function
