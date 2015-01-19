#' @title Legiscan Bill Metadata
#' @description Parses and arranges JSON output from
#' Legiscan master data downloads bills subdirectory
#' @param file A JSON file object from the bills subdirectory
#' @return Creates a list object containing a data frame
#' of metadata for an individual piece of legislation
#' @examples \donttest{
#' # Create directory object
#' directoryTree <- fileStructure("data/msHistorical/")
#'
#' # Create file list object
#' files <- fileLists(directoryTree)
#'
#' # Parse the meta data from the bill file
#' bills <- legiBillMeta(files[["bills"]][[1]][[1]])
#' }
#'
#' @importFrom RJSONIO fromJSON
#' @importFrom lubridate ymd
#' @importFrom plyr llply
#' @importFrom dplyr bind_cols
#' @importFrom magrittr %>%
#' @export legiBillMeta
#' @family Parsing and Cleaning LegiScan Data
#' @name legiBillMeta
legiBillMeta <- function(file) {

	# Parse the JSON object
	billobject <- billdata(file)

	# Create the session ID/name list
	session <- as.data.frame(billobject[["session"]], stringsAsFactors = FALSE)

	# Define named list with all of the single element values
	singleValueList <- list(bill_id = "bill_id", change_hash = "change_hash", url = "url",
							state_link = "state_link", completed = "completed",
							status = "status", status_date = "status_date", state = "state",
							state_id = "state_id", bill_number = "bill_number",
							bill_type = "bill_type", body = "body",
							body_id = "body_id", current_body = "current_body",
							current_body_id = "current_body_id", title = "title",
							description = "description", committee = "committee")

	# Build the list containing all the single element values
	billData <- plyr::llply(singleValueList, FUN = function(singles){
		as.list(billobject[[singles]])
	})

	# Pull variable names from the name elements fo the billData list object
	bdnames <- names(billData)

	# Replace any NULL values with NA values
	billData <- plyr::llply(billData, FUN = function(rmNulls) {
		if (length(rmNulls) == 0) rmNulls <- ""
		else rmNulls <- rmNulls
	}) %>% as.data.frame(stringsAsFactors = FALSE)

	# Attach original variable names to new data frame
	names(billData) <- bdnames

	# Recast the date values as dates
	billData$status_date <- lubridate::ymd(billData$status_date)

	# Create bill metadata data frame object
	billData <- dplyr::bind_cols(session[1], as.data.frame(billData,
											stringsAsFactors = FALSE))

	# Return the bill meta data object
	return(billData)

} # End of Function
