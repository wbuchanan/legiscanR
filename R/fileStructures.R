##' @title Build Directory Tree Object for Legiscan Data Downloads
##' @description Creates an object to store all of the directory/file information
##' required to build the fileLists object and parse the legiscan data
##' @param filepath The file path to the root directory of the legiscan data
##' @return Creates a list object containing three nested lists for each
##' directory in the path
#' @details \itemize{
#' 	\item{"bills"}{list of full filepaths/names for all bills XML/JSON files}
#' 	\item{"people"}{list of full filepaths/names for all people XML/JSON files}
#' 	\item{"votes"}{list of full filepaths/names for all vote  XML/JSON files}
#' 	}
#' @export fileStructure
#' @importFrom plyr llply
#' @name fileStructure
#' @rdname fileStructure-Archive_Structure
#' @family Parsing and Cleaning LegiScan Data
#' @family Legiscan Data Downloads
#' @examples \donttest{
#' # Build a directory tree object
#' directoryTree <- fileStructure("data/msHistorical/")
#' }

# Function to build a file structure object to get/build historical data
fileStructure <- function(filepath) {

  # Store current directory; move to directory specified in function call
  curdir <- getwd(); setwd(filepath)

	# Make sure file path ends with slash
	if (grepl("/$", filepath) == FALSE) filepath <- paste0(filepath, "/")

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
	fileList <- plyr::llply(names(sessionRoot), FUN = function(x){
		# Store the file path/name in the object x which identifies the session
		# Create a list object containing the file path and file name
		# based on the file class type (e.g., bills, people, or voting)
		x <- 	plyr::llply(fileClass, FUN = function(y){
					list(filePaths= paste0(sessionRoot[[x]], y),
						 fileNames= list.files(path = paste0(sessionRoot[[x]], y)))
				})
	})

	# Assign names to the branches of the fileStructure object to identify the sessions
	names(fileList) <- names(sessionRoot)

	# Remove objects no longer needed
	rm(fileClass, sessionRoot)

  	# Move back to original directory
  	setwd(curdir)

	# Return the file structure object
	return(fileList)

} # End of Function

