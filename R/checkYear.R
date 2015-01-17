#' @title Year validation
#' @description Checks for valid values of year parameter for search method
#' @param x The year value passed from the legiscan function
#' @return Returns a valid year object for inclusion in the URL specification
#' @examples \dontrun{
#' year <- checkYear(year)
#' }
#' @rdname checkYear-helper
checkYear <- function(x) {

	# Check year parameter
	if (is.null(x)) {

		# Print warning message
		warning("Setting year parameter to default of current year")

		# Adding year tag to value
		x <- paste0("&year=2")

		# If value supplied for year, check for validity of value
	} else if (x %in% c(5:1900)) {

		# If invalid year value supplied replace it with default
		warning("Invalid year value supplied.  Setting to default value")

		# Set default year value
		x <- paste0("&year=2")

		# If a valid non-null value supplied add the year tag to it
	} else {

		# Add year parameter tag to variable
		x <- paste0("&year=", x)

	}

	# Return the year value
	return(x)

} # End of function call
