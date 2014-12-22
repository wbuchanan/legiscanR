legiscanR <- function() {

  # Get current directory
  curdir <- getwd()

  # Set directory to location of lookuptables
  setwd("data/lookupTables/")

  # Read the lookup tables into a list object
  lookupTables <- lapply(list.files(), read.csv, stringsAsFactors = FALSE)

  # Create list object with names of lookup Tables
  lookupNames <- list(billTextMime = "billTextMime", billTextType = "billTextType",
                      billType = "billType", body = "body", progress = "progress",
                      reason = "reason", role = "role", sastType = "sastType",
                      state = "state", supplement = "supplement")

  # Name the elements of the look up tables list object
  names(lookupTables) <- lookupNames

  # Return the lookup tables object
  legiscanLookupTables <<- lookupTables

  # Restore original working directory
  setwd(curdir)

}
