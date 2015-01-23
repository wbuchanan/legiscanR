#' @title Full Bill Text Retreival
#' @description
#' Used to retrieve the full text of legislation from LegiScan and/or the official
#' State URL provided by the LegiScan API call
#' @param data The texts data object produced internally from the parseBill method
#' @param link Argument defines where the function will attempt to retrieve the
#' text from
#' @return Returns the texts object with the full text of the bill(s) joined to
#' the data frame object
#' @family Parsing and Cleaning LegiScan Data
#' @name getFullText
#' @importFrom XML xpathApply htmlParse xmlValue
#' @importFrom httr http_status GET
#' @importFrom plyr llply
#' @importFrom dplyr as_data_frame bind_rows
#' @export getFullText
getFullText <- function(data, link = c("state_link", "url")) {

	# Generate a list of all of the state link elements
	linkList <- plyr::llply(data, FUN = function(x){
	  list(x[[link]])
	})
	  # Retrieve/Clean bill text if the URL request doesn't fail
	  cleanText <- plyr::llply(linkList, .fun = function(url) {
	  		if (httr::http_status(
	  				httr::GET(url))$message == "success: (200) OK") {
	  				paste(XML::xpathApply(XML::htmlParse(url),
	                    	"//p", XML::xmlValue), collapse = "\n")
	  		} else {
	  			"Could not retrieve full bill text from URL"
	  		}
	  	}) %>% dplyr::as_data_frame()
	  # Bind the full text to the rest of the test data
	  data <- dplyr::bind_rows(data, cleanText)

	  # Return text data frame with full text attached
	  return(data)

}
