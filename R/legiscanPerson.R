#' @title Legiscan Legislator Data
#' @description Parses and arranges XML dumps from
#' Legiscan master data dumps people subdirectory
#' @param file An XML file object from the people subdirectory
#' @return Creates a list object containing a data frame
#' for identifying individual legislators
#' @examples \donttest{
#' # Build directory structure from master data downloads
#' directoryTree <- fileStructure("data/msHistorical/")
#'
#' # Create object with all file references stored in it
#' files <- fileLists(directoryTree)
#'
#' # Pass the function a bill object for processing
#' people <- legiscanPerson(files[["people"]])
#' }
#' @family Parsing and Cleaning LegiScan Data
#' @name legiscanPerson
#' @import XML plyr
#' @export legiscanPerson
legiscanPerson <- function(file) {

	# Parse XML Tree
	xmlobject <- XML::xmlRoot(XML::xmlParse(file))

	# Initial parsing of the data object
	initialPerson <- XML::xmlToList(xmlobject[["person"]])

	# Need to replace NULL values with NA for consistency in the data storage
	withOutNulls <- plyr::llply(initialPerson, FUN = function(cleanNulls) {

		# If value is null replace it with generic missing value
		if (is.null(cleanNulls)) cleanNulls <- NA

		# If there is currently a value in that slot retain it
		else cleanNulls <- cleanNulls

	})

	# Create the data frame row for the person
	finalPerson <- as.data.frame(withOutNulls, stringsAsFactors = FALSE)

	# Return the cleaned/formatted object
	return(finalPerson)

} # End of Function
