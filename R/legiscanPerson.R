##' @title Legiscan Legislator Data
##' @description Parses and arranges XML dumps from 
##' Legiscan master data dumps people subdirectory
##' @param file An XML file object from the people subdirectory
##' @return Creates a list object containing a data frame 
##' for identifying individual legislators
##' @export
##' @examples
#' require(legiscanR)
#' directoryTree <- fileStructure("data/msHistorical/")
#' files <- fileLists(directoryTree)
#' people <- legiscanVotes(files[["people"]])

# Function to create Person object as a data frame
legiscanPerson <- function(file) {

	# Parse XML Tree
	xmlobject <- xmlRoot(xmlParse(file))
	
	# Initial parsing of the data object
	initialPerson <- xmlToList(xmlobject[["person"]])
	
	# Need to replace NULL values with NA for consistency in the data storage
	withOutNulls <- lapply(initialPerson, FUN = function(cleanNulls) {
	
		# If value is null replace it with generic missing value
		if (is.null(cleanNulls)) cleanNulls <- NA
		
		# If there is currently a value in that slot retain it
		else cleanNulls <- cleanNulls
	
	})
	
	# Create the data frame row for the person
	finalPerson <- as.data.frame(withOutNulls, stringsAsFactors = FALSE)
	
	# Return the cleaned/formatted object
	return(finalPerson)

# End Function call    
}
