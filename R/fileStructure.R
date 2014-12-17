##' @title Build Directory Tree Object for Legiscan Data Downloads
##' @description Creates an object to store all of the directory/file information
##' required to build the fileLists object and parse the legiscan data
##' @param filepath The file path to the root directory of the legiscan data
##' @return Creates a list object containing three nested lists for each 
##' directory in the path
##' bills = a list with the bills subdirectory file path and all filenames
##' people = a list with the people subdirectory file path and all filenames
##' votes = a list with the vote subdirectory file path and all filenames
##' @export
##' @examples
#' require(legiscanR)
#' directoryTree <- fileStructure("data/msHistorical/")

# Function to build a file structure object to get/build historical data
fileStructure <- function(filepath) {

	# Move to location where historical data are stored
	setwd(filepath)
	
	# Identify the directory path to each of the sessions
	sessionRoot <- split(paste0(filepath, list.files(), "/"), 
						 list(list.files()))
	
	# Identify the different types of file classes
	fileClass <- list(bills = "bill/", votes = "vote/", people = "people/")
	
	# Build a list object with the following structure:
	# Object
	# -----Session Name
	# ----------File Class
	# ---------------File Path
	# ---------------File Name
	fileList <- lapply(names(sessionRoot), FUN = function(x){
		# Store the file path/name in the object x which identifies the session
		x <- 	lapply(fileClass, FUN = function(y){
					# Create a list object containing the file path and file name 
					# based on the file class type (e.g., bills, people, or voting)
					list(filePaths= paste0(sessionRoot[[x]], y),
						 fileNames= list.files(path = paste0(sessionRoot[[x]], y)))
				})
	})
	
	# Assign names to the branches of the fileStructure object to identify the sessions
	names(fileList) <- names(sessionRoot)
	
	# Remove objects no longer needed
	rm(fileClass, sessionRoot)
	
	# Return the file structure object
	return(fileList)

# End function call
}

