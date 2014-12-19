##' @title Legiscan Legislator Data
##' @description Parses and arranges JSON data from 
##' Legiscan master data downloads people subdirectory
##' @param file A JSON file object from the people subdirectory
##' @return Creates a list object containing a data frame 
##' for identifying individual legislators
##' @export
##' @examples
#' require(legiscanR)
#' directoryTree <- fileStructure("data/msHistorical/")
#' files <- fileLists(directoryTree)
#' people <- legiscanVotes(files[["people"]])

# Function to create Person object as a data frame
legiPersonJSON <- function(fileobject) {
  
  # Parse JSON data
  person <- fromJSON(fileobject)[["person"]]
    
  # Return the cleaned/formatted object
  return(as.data.frame(person, stringsAsFactors = FALSE))
  
  # End Function call    
}
