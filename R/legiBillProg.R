#' @title Legiscan Bill Progress Tracking Data
#' @description Parses and arranges JSON output from
#' Legiscan master data downloads bills subdirectory
#' @param fileobject A JSON file object from the bills subdirectory
#' @return Creates a list object containing a data frame
#' for the progress of the bill
#' @examples \donttest{
#' # Create directory object
#' directoryTree <- fileStructure("data/msHistoricalJSON/")
#'
#' # Create file list object
#' files <- fileLists(directoryTree)
#'
#' # Parse/clean the bill progress data from a LegiScan bill file
#' billProgress <- legiBillProg(files[["bills"]][[10]][[12]])
#' }
#'
#' @importFrom dplyr bind_cols
#' @export legiBillProg
#' @family Parsing and Cleaning LegiScan Data
#' @name legiBillProg
legiBillProg <- function(fileobject) {

	# Parse the JSON object
	billobject <- billdata(eval(fileobject))

	# Named list of ID elements
	IDs <- billids(billobject)

	# Add the ID columns to the data frames and fill the required
	# number of records to rectangularize the data frame
	billProg <- as.data.frame(dplyr::bind_cols(IDs[rep(seq_len(nrow(IDs)),
							  nrow(billobject[["progress"]])), ],
		  					  billobject[["progress"]]),
							  stringsAsFactors = FALSE)

	# Recode the numeric event codes as factors with labels
	#billProg$event <- factor(billProg$event,
	#					levels = legiscanLookupTables[["progress"]][["progress_event"]],
	#					labels = legiscanLookupTables[["progress"]][["progress_desc"]])

	# Return the parsed/cleaned object
	return(billProg)

} # End of Function

