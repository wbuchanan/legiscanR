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
#' @importFrom XML xmlRoot xmlParse xmlToList
#' @importFrom plyr llply
#' @importFrom dplyr as_data_frame bind_cols
#' @importFrom lubridate now
#' @export legiscanPerson
legiscanPerson <- function(file) {
	
	# Create a timestamp when the function begins
	parseTime <- as.data.frame(lubridate::now()) %>% dplyr::as_data_frame()
	names(parseTime) <- "parse_timestamp"

	# Parse XML Tree
	xmlobject <- XML::xmlRoot(XML::xmlParse(file))[["person"]]

	# Initial parsing of the data object
	finalPerson <- XML::xmlToList(xmlobject) %>%
		  				plyr::llply(.fun = function(x) {
		  					ifelse(is.null(x), NA, x)
		  				}) %>% dplyr::as_data_frame() 
	
	finalPerson <- dplyr::bind_cols(finalPerson, parseTime)
	
	# Correct casting of variables
	correctCasting <- list(people_id = "people_id", role_id = "role_id", 
						   ftm_id = "ftm_id", committee_id = "committee_id", 
						   party_id = "party_id", ftm_eid = "ftm_eid")
	for (i in correctCasting) {
		finalPerson[, i] <- as.numeric(finalPerson[, i])
	}

	# Return the cleaned/formatted object
	return(finalPerson)

} # End of Function
