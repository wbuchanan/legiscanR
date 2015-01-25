#' @title Legiscan Bill Historical Data
#' @description Parses and arranges JSON output from
#' Legiscan master data downloads bills subdirectory
#' @param fileobject A JSON file object from the bills subdirectory
#' @return Creates a list object containing a data frame
#' for the history of the bill
#' @examples \donttest{
#' # Create the directory tree in an object
#' directoryTree <- fileStructure("data/msHistoricalJSON/")
#'
#' # Store the file path/structure in a new object to reference files for parsing
#' files <- fileLists(directoryTree)
#'
#' # Parse the bill history data from a bill file
#' history <- legiBillHist(files[["bills"]][[10]][[12]])
#' }
#' @export legiBillHist
#' @importFrom dplyr bind_cols
#' @family Parsing and Cleaning LegiScan Data
#' @name legiBillHist
legiBillHist <- function(fileobject) {

	# Parse the JSON object
	billobject <- billdata(eval(fileobject))

	# Named list of ID elements
	IDs <- billids(billobject)

	# Add the ID columns to the data frames and fill the required
	# number of records to rectangularize the data frame
	billHist <- as.data.frame(dplyr::bind_cols(IDs[rep(seq_len(nrow(IDs)),
					  		  nrow(billobject[["history"]])), ],
							  billobject[["history"]]),
							  stringsAsFactors = FALSE)

	# Recode the numeric event codes as factors with labels
	#billProg$event <- factor(billProg$event,
	#			levels = legiscanLookupTables[["progress"]][["progress_event"]],
	#			labels = legiscanLookupTables[["progress"]][["progress_desc"]])

	# Return the parsed/cleaned object
	return(billHist)

# End of function call
}
