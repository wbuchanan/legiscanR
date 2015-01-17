#' @title Determine if request was sent as XML or JSON
#' @description
#' Checks the file heading to identify XML vs JSON content and appends the
#' appropriate class to the object.
#' @param x The result from the API call
#' @return Returns the result object with additional class attributes
#' @examples \dontrun{
#' # Use getURL from RCurl to make the API call
#' statelist <- getURL(requestURL)
#'
#' # Determine if statelist object is XML or JSON
#' statelist <- xjformat(statelist)
#' }
#' @family Parsing and Cleaning LegiScan Data
#' @name xjformat
xjformat <- function(x) {

	# If the file includes the XML signature add the XMLDocumentContent class
	# to it for work with the XML package
	if (grepl("(<?xml.*)", x)) class(x) <- c("XMLDocumentContent", "character")

	# Else treat as text
	else class(x) <- "character"

	# Return the object with the class modification(s)
	return(x)

} # End Function
