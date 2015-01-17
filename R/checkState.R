#' @title State validation
#' @description Checks for valid values of state parameter for legiscan methods
#' @param x The state value passed from the legiscan function
#' @return Returns a valid state object for inclusion in the URL specification
#' @examples
#' \dontrun{
#' state <- checkState(state)
#' }
#' @rdname checkState-helper
checkState <- function(x) {

	# Check to see if value is null
	if (is.null(x)) {

		# Return the null object
		return(x)

	# Ensure the state abbreviation specified was valid and only 1 state
	} else {

		if (!(toupper(x) %in% c(state.abb, "ALL", "DC", "US")) && length(x) == 1) {

			# If invalid state abbreviation supplied error out
			stop(paste0("Error: State abbreviation must be 'ALL' or be found in ",
						"lookuptables[['state']]$state_abbr dataset"))

		} else if ((toupper(x) %in% c(state.abb, "ALL", "DC", "US")) && length(x) > 1) {

			# If multiple states specified error out
			stop("Error: Can only supply a single state")

		} else {

			# If valid single state supplied add the URL definition to the state
			x <- paste0("&state=", toupper(x))

		} # End checking for valid state codes

		# Return valid state object
		return(x)

	} # End for non-null state variable

} # End check state function call
