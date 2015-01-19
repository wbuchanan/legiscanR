#' @title Legiscan Legislator Data
#' @description Parses and arranges JSON data from
#' Legiscan master data downloads people subdirectory
#' @param file A JSON file object from the people subdirectory
#' @return Creates a list object containing a data frame
#' for identifying individual legislators
#' @examples \donttest{
#' # Create directory object
#' directoryTree <- fileStructure("data/msHistoricalJSON/")
#'
#' # Create file list object
#' files <- fileLists(directoryTree)
#'
#' # Parse/clean the full bill text data from a LegiScan bill file
#' aLegislator <- legiPersonJSON(files[["people"]][[10]][[12]])
#' }
#'
#' @importFrom RJSONIO fromJSON
#' @importFrom magrittr %>%
#' @export legiPersonJSON
#' @family Parsing and Cleaning LegiScan Data
#' @name legiPersonJSON
legiPersonJSON <- function(file) {

  # Parse JSON data
  RJSONIO::fromJSON(file)[["person"]] %>%
  	as.data.frame(stringsAsFactors = FALSE) %>%
  		return()

  # Return the cleaned/formatted object
  # return(as.data.frame(person, stringsAsFactors = FALSE))

} # End of Function
