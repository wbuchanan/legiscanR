#' @title Legiscan Voting Data
#' @description Parses and arranges XML dumps from
#' Legiscan master data dumps vote subdirectory
#' @param file An XML file object from the vote subdirectory
#' @return Creates a list object containing a data frame
#' for voting metadata and individual legislators' votes
#' @examples \donttest{
#' # Create directory object
#' directoryTree <- fileStructure("data/msHistoricalJSON/")
#'
#' # Create file list object
#' files <- fileLists(directoryTree)
#'
#' # Parse/clean the voting records data from a LegiScan votes file
#' votes <- legiscanVotes(files[["votes"]])
#' }
#'
#' @importFrom XML xmlRoot xmlParse xmlToList xmlToDataFrame
#' @importFrom lubridate ymd
#' @importFrom dplyr bind_cols
#' @export legiscanVotes
#' @family Parsing and Cleaning LegiScan Data
#' @name legiscanVotes
legiscanVotes <- function(file) {

	# Create a timestamp when the function begins
	parseTime <- as.data.frame(lubridate::now()) %>% dplyr::as_data_frame()
	names(parseTime) <- "parse_timestamp"

	# Parse the XML tree
	voteobject <- XML::xmlRoot(XML::xmlParse(file))

	# Parse the metadata from the voting records
	metaParsed <- XML::xmlToList(voteobject[["roll_call"]])

	# Transform the shape to make it more compatible for a data frame
	meta <- as.data.frame(metaParsed[-10], stringsAsFactors = FALSE)

	# Replace date with date formatted value
	meta$date <- lubridate::ymd(meta$date)

	# Make sure numeric values for overall counts are stored as numeric data
	meta[, c(5:9)] <- as.numeric(meta[, c(5:9)])

	# Extract the individual voting records of legislators in separate data frame
	votes <- XML::xmlToDataFrame(voteobject[["roll_call"]][["votes"]],
							stringsAsFactors = FALSE)

	voteID <- meta[, c(1:3)]

	# Add the roll call, bill IDs, and date to the votes table
	votes <- dplyr::bind_cols(voteID[rep(seq_len(nrow(voteID)),
			  				nrow(votes)), ], votes,
							parseTime[rep(seq_len(1), nrow(votes)), ])

	# Store both data frames in a single list object
	votesOnBill <- list(meta = meta, records = votes)

	# Return the list obejct
	return(votesOnBill)

} # End of Function


