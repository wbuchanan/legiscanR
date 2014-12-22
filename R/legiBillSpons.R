##' @title Legiscan Bill Sponsor Data
##' @description Parses and arranges JSON output from
##' Legiscan master data downloads bills subdirectory
##' @param fileobject A JSON file object from the bills subdirectory
##' @return Creates a list object containing a data frame
##' for the progress of the bill
##' @export
##' @examples
#' require(legiscanR)
#' directoryTree <- fileStructure("data/msHistoricalJSON/")
#' files <- fileLists(directoryTree)
#' billSponsors <- legiBillSpons(files[["bills"]][[10]][[12]])

# Function to parse/clean JSON output from LegiScan API calls
legiBillSpons <- function(fileobject) {

	# Parse the JSON object
	billobject <- billdata(eval(fileobject))

	# Named list of ID elements
	IDs <- billids(billobject)

	# Add the ID columns to the data frames and fill the required
	# number of records to rectangularize the data frame
	billSpons <- as.data.frame(cbind(IDs[rep(seq_len(nrow(IDs)),
											nrow(billobject[["sponsors"]])), ],
									billobject[["sponsors"]]), stringsAsFactors = FALSE)

	# Return the parsed/cleaned object
	return(billSpons)

	# End of function call
}

