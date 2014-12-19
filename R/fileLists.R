##' @title Build file listing for Legiscan data
##' @description Creates an object used to pass filenames to other legiscan functions
##' @param fileobject The object produced by the returned value from fileStructure()
##' @return Creates a list object containing three nested lists for each 
##' legislative session
##' bills = a list of full filepaths/names for all bills XML files
##' people = a list of full filepaths/names for all people XML filess
##' votes = a list of full filepaths/names for all vote  XML filess
##' @export
##' @examples
#' require(legiscanR)
#' directoryTree <- fileStructure("data/msHistorical/")
#' files <- fileLists(directoryTree)

# Function to build individual session named lists of the files
fileLists <- function(fileobject) {
  
	# Parse all of the session named people lists
	peoples <- lapply(fileobject, FUN = function(x) {
		x[[3]] 
	})
	
	# Create people files object with all of the full file paths/names
	# To the peoples data
	peopleFiles <- lapply(peoples, FUN = function(x) {
		as.list(outer(x[[1]], x[[2]], FUN = paste0))
	})
	
	# Parse all of the session named voting records lists
	votes <- lapply(fileobject, FUN = function(x) {
		x[[2]] 
	})
	
	# Create votes files object with all of the full file paths/names
	# To the voting records data
	voteFiles <- lapply(votes, FUN = function(x) {
		as.list(outer(x[[1]], x[[2]], FUN = paste0))
	})
	
	# Parse all of the session named bill lists
	bills <- lapply(fileobject, FUN = function(x) {
		x[[1]] 
	})
	
	# Create bill files object with all of the full file paths/names
	# To the bill data
	billFiles <- lapply(bills, FUN = function(x) {
		as.list(outer(x[[1]], x[[2]], FUN = paste0))
	})
	
	# Store all of the lists in a single list object
	theFiles <- list(bills = billFiles, votes = voteFiles, 
	             	 people = peopleFiles)
	
	# Return the list object
	return(theFiles)
	
# End of function call    
}
