##' @title Legiscan Bill Sponsor Data
##' @description Parses and arranges JSON output from
##' Legiscan master data downloads bills subdirectory
##' @param fileobject A JSON file object from the bills subdirectory
##' @return Creates a list object containing a data frame
##' for the progress of the bill
##' @export
##' @examples \donttest{
#' # Create directory tree object
#' directoryTree <- fileStructure("data/msHistorical/")
#'
#' # Build file list object
#' files <- fileLists(directoryTree)
#'
#' # Parse/clean up the Bill Sponsor data
#' billSponsors <- legiBillSpons(files[["bills"]][[10]][[12]])
#' }
#'
#' @import RJSONIO lubridate plyr dplyr magrittr
#' @export legiBillSpons
#' @family Parsing and Cleaning LegiScan Data
#' @name legiBillSpons
legiBillSpons <- function(fileobject) {

	# Parse the JSON object
	billobject <- billdata(eval(fileobject))

	# Named list of ID elements
	IDs <- billids(billobject)

	# Add the ID columns to the data frames and fill the required
	# number of records to rectangularize the data frame
	billSpons <- as.data.frame(dplyr::bind_cols(
					IDs[rep(seq_len(nrow(IDs)), nrow(billobject[["sponsors"]])), ],
					billobject[["sponsors"]]), stringsAsFactors = FALSE)

	# Return the parsed/cleaned object
	return(billSpons)

} # End of Function

