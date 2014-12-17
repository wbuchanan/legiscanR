##' @title Legiscan Voting Data
##' @description Parses and arranges XML dumps from 
##' Legiscan master data dumps vote subdirectory
##' @param file An XML file object from the vote subdirectory
##' @return Creates a list object containing a data frame 
##' for voting metadata and individual legislators' votes
##' @export
##' @examples
#' require(legiscanR)
#' directoryTree <- fileStructure("data/msHistorical/")
#' files <- fileLists(directoryTree)
#' votes <- legiscanVotes(files[["votes"]])

# Function to create a votes object with the data from individual voting records files
legiscanVotes <- function(file) {
	
	# Parse the XML tree
	voteobject <- xmlRoot(xmlParse(file))
	
	# Parse the metadata from the voting records
	metaParsed <- xmlToList(voteobject[["roll_call"]])
	
	# Transform the shape to make it more compatible for a data frame
	meta <- as.data.frame(metaParsed[-10], stringsAsFactors = FALSE)
	
	# Replace date with date formatted value
	meta$date <- ymd(meta$date)
	
	# Make sure numeric values for overall counts are stored as numeric data
	meta[, c(5:9)] <- as.numeric(meta[, c(5:9)])
	
	# Extract the individual voting records of legislators in separate data frame
	votes <- xmlToDataFrame(voteobject[["roll_call"]][["votes"]], 
							stringsAsFactors = FALSE)
	
	# Add the roll call, bill IDs, and date to the votes table
	votes <- cbind(meta[, c(1:3)], votes)
	
	# Store both data frames in a single list object
	votesOnBill <- list(meta = meta, records = votes)
	
	# Return the list obejct
	return(votesOnBill)
	
# End function call    
}


