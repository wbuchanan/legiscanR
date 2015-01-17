#' @title Page validation
#' @description Checks for valid values of page parameter for search method
#' @param x The page value passed from the legiscan function
#' @return Returns a valid page object for inclusion in the URL specification
#' @examples \dontrun{
#' page <- checkPage(page)
#' }
#' @rdname checkPage-helper
checkPage <- function(x) {

	# Check page parameter
	if (is.null(x)) {

		# Print warning message
		warning("Setting page parameter to default (1st page)")

		# Adding page tag to value
		x <- paste0("&page=1")

	# If value was passed to the argument
	} else {

		# Add page parameter tag to variable
		x <- paste0("&page=", page)

	}

	# Return the page value
	return(x)

} # End of function call
