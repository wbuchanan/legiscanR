#' @title Build file listing for Legiscan data
#' @description Creates an object used to pass filenames to other legiscan functions
#' @param fileobject The object produced by the returned value from fileStructure()
#' @return Creates a list object containing three nested lists for each
#' legislative session
#' @details \itemize{
#' 	\item{"bills"}{list of full filepaths/names for all bills XML/JSON files}
#' 	\item{"people"}{list of full filepaths/names for all people XML/JSON files}
#' 	\item{"votes"}{list of full filepaths/names for all vote  XML/JSON files}
#' 	}
#' @export fileLists
#' @importFrom plyr llply
#' @name fileLists
#' @rdname fileLists-Archive_Structure
#' @family Parsing and Cleaning LegiScan Data
#' @family Legiscan Data Downloads
#' @examples \donttest{
#' # Create an object to store the directory tree
#' directoryTree <- fileStructure("data/msHistorical/")
#'
#' # Build a file object
#' files <- fileLists(directoryTree)
#' }

# Function to build individual session named lists of the files
fileLists <- function(fileobject) {

	# Parse all of the session named people lists
	peoples <- plyr::llply(fileobject, .fun = function(x) { x[[3]] })

	# Create people files object with all of the full file paths/names
	# To the peoples data
	peopleFiles <- plyr::llply(peoples, .fun = function(x) {
		as.list(outer(x[[1]], x[[2]], FUN = paste0))
	})

	# Parse all of the session named voting records lists
	votes <- plyr::llply(fileobject, .fun = function(x) { x[[2]] })

	# Create votes files object with all of the full file paths/names
	# To the voting records data
	voteFiles <- plyr::llply(votes, .fun = function(x) {
		as.list(outer(x[[1]], x[[2]], FUN = paste0))
	})

	# Parse all of the session named bill lists
	bills <- plyr::llply(fileobject, .fun = function(x) { x[[1]] })

	# Create bill files object with all of the full file paths/names
	# To the bill data
	billFiles <- plyr::llply(bills, .fun = function(x) {
		as.list(outer(x[[1]], x[[2]], FUN = paste0))
	})

	# Store all of the lists in a single list object
	theFiles <- list(bills = billFiles,
					 votes = voteFiles,
	             	 people = peopleFiles)

	# Return the list object
	return(theFiles)

} # End of Function
