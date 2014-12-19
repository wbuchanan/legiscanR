##' @title Legiscan Bill Data
##' @description Parses and arranges XML output from 
##' Legiscan master data dumps vote subdirectory
##' @param file An XML file object from the vote subdirectory
##' @return Creates a list object containing a data frame 
##' for voting metadata and individual legislators' votes
##' @export
##' @examples
#' require(legiscanR)
#' directoryTree <- fileStructure("data/msHistorical/")
#' files <- fileLists(directoryTree)
#' bills <- legiscanBill(files[["votes"]][[1]][[1]])

# Function to parse/clean XML output from LegiScan API calls
legiscanBill <- function(file) {
  
  # Need to make sure the XML and lubridate libraries are loaded when function is called
  # may move these to be loaded when the package is loaded
  # require(XML); require(lubridate); require(plyr); require(dplyr)
  
  # Create a bill object with the parsed XML content
  bill <- xmlRoot(xmlParse(file))
  
  # Extract the bill elements from the bill object to be further processed
  billobject <- bill[["bill"]]
  
  # Create the session ID/name list
  session <- as.data.frame(xmlToList(billobject[["session"]]), 
                           stringsAsFactors = FALSE)
  
  # Define named list with all of the single element values
  singleValueList <- list(bill_id = "bill_id", change_hash = "change_hash", url = "url", 
                          state_link = "state_link", completed = "completed", 
                          status = "status", status_date = "status_date", state = "state",
                          state_id = "state_id", bill_number = "bill_number", 
                          bill_type = "bill_type", body = "body",
                          body_id = "body_id", current_body = "current_body", 
                          current_body_id = "current_body_id", title = "title", 
                          description = "description", committee = "committee")
  
  # Build the list containing all the single element values
  billData <- lapply(singleValueList, FUN = function(singles){
    xmlToList(billobject[[singles]])
  })
  
  # Replace any NULL values with NA values
  billData <- lapply(billData, FUN = function(rmNulls) {
    if (is.null(rmNulls)) rmNulls <- ""
    else rmNulls <- rmNulls
  })
  
  # Recast the date values as dates
  billData$status_date <- ymd(billData$status_date)
  
  # Create bill metadata data frame object
  billData <- cbind(session[1], as.data.frame(billData, 
                                              stringsAsFactors = FALSE))
  
  # Create the bill history object (contains multiple lists)
  history <- xmlApply(billobject[["history"]], xmlToList)
#                      FUN = function(x){
#                        list(
#                          date = ymd(xmlValue(x[["date"]])),
#                          action = xmlValue(x[["action"]]),
#                          chamber = xmlValue(x[["chamber"]]),
#                          chamber_id = xmlValue(x[["chamber_id"]]),
#                          importance = xmlValue(x[["importance"]])
#                        )
#                        
#                      }
#  )
  
  # ID References for history table
  historyIDs <- cbind(billData[c(1, 2, 11, 10)])
  
  # Convert the history object to a data frame and prevent NULL values from causing problems
  history <- rbind.fill(lapply(history, function(f) {
    as.data.frame(Filter(Negate(is.null), f), stringsAsFactors = FALSE)
  }))
  
  # Replicate the number of rows of history IDs needed to file the history data frame
  historyIDs <- historyIDs[rep(seq_len(nrow(historyIDs)), nrow(history)), ]
  
  # Add ID references to bill history dataframe
  history <- cbind(historyIDs, history)    
  
  # Create the bill progress object (contains multiple lists)
  progress <- xmlApply(billobject[["progress"]], xmlToList)
#                       FUN = function(x){
#                         list(
#                           date = ymd(xmlValue(x[["date"]])),
#                           event = xmlValue(x[["event"]])
#                         )
#                       }
#  )
  
  # Convert the bill progress object to a data frame and prevent NULL values from causing problems
  progress <- rbind.fill(lapply(progress, function(f) {
    as.data.frame(Filter(Negate(is.null), f), stringsAsFactors = FALSE)
  }))
  
  # ID References for progress table
  progressIDs <- as.data.frame(billData[c(1, 2, 11, 10)], stringsAsFactors = FALSE)
  
  # Replicate the number of rows of progress IDs needed to file the progress data frame
  progressIDs <- progressIDs[rep(seq_len(nrow(progressIDs)), nrow(progress)), ]
  
  # Add ID references to progress object
  progress <- cbind(progressIDs, progress)
  
  # Create the bill sponsors object (also contains multiple lists)
  sponsors <- xmlApply(billobject[["sponsors"]], xmlToList)
#                       FUN = function(x){
#                         list(
#                           people_id = xmlValue(x[["people_id"]]),
#                           name = xmlValue(x[["name"]]),
#                           role_id = xmlValue(x[["role_id"]]),
#                           ftm_id = xmlValue(x[["ftm_id"]]),
#                           sponsor_type_id = xmlValue(x[["sponsor_type_id"]]),
#                           sponsor_order = xmlValue(x[["sponsor_order"]])
#                         )
#                       }
#  )
  
  # ID References for sponsors tables
  sponsorIDs <- cbind(billData[c(1, 2, 11, 10)])
  
  # Convert the bill sponsors object to a data frame and prevent NULL values from causing problems
  sponsors <- rbind.fill(lapply(sponsors, function(f) {
    as.data.frame(Filter(Negate(is.null), f), stringsAsFactors = FALSE)
  }))
  
  # Replicate the number of rows of sponsor IDs needed to file the sponsors data frame
  sponsorIDs <- sponsorIDs[rep(seq_len(nrow(sponsorIDs)), nrow(sponsors)), ]
  
  # Add ID references to sponsors table
  sponsors <- cbind(sponsorIDs, sponsors)
  
  if (!is.null(billobject[["texts"]])) {
    
    # Create the bill text object (will need to pass 
    # the state_link slot to separate function to retrieve text)
    texts <- xmlApply(billobject[["texts"]], xmlToList)
#                      FUN = function(x){
#                        list(
#                          doc_id = xmlValue(x[["doc_id"]]),
#                          date = ymd(xmlValue(x[["date"]])),
#                          type = xmlValue(x[["type"]]),
#                          mime = xmlValue(x[["mime"]]),
#                          url = xmlValue(x[["url"]]),
#                          state_link = xmlValue(x[["state_link"]])
#                        )
#                      }
#    )
    
    # Generate a list of all of the state link elements
    linkLists <- lapply(texts, FUN = function(x){
      list(x[["state_link"]])
    })
    
    # Retrieve, parse, and clean the text of the bills
    cleanText <- lapply(linkLists, FUN = function(links) {
      tryCatch(paste(xpathApply(htmlParse(links), 
                                "//p", xmlValue), collapse = "\n"),
               error = function(e) {
                 list(c("drop me"), 
                      c("Error loading the bill text"))
               })
    })
    
    # Create data table with the text data
    fullText <- ldply(cleanText, rbind)
    
    # Convert text back to character vector
    fullText[, 2] <- toString(fullText[, 2])
    
    # Assign a name to the cleaned full text data table
    names(fullText) <- c("drop", "full_bill_text")
    
    # Remove the ID column generated by dplyr
    fullText <- fullText[, 2]
    
    # Convert the bill texts object to a data frame and prevent NULL values from causing problems
    texts <- rbind.fill(lapply(texts, function(f) {
      as.data.frame(Filter(Negate(is.null), f), stringsAsFactors = FALSE)
    }))
    
  } else {
    
    # Create NULL Texts object
    texts <- as.data.frame(cbind(doc_id = NA, date = NA,
                                 type = NA, mime = NA, url = NA, 
                                 state_link = NA))
  }
  
  # ID References for text tables
  textIDs <- cbind(billData[c(1, 2, 10, 11, 14, 15, 16)])
  
  # Copy the appropriate number of rows for the text IDs object
  textIDs <- textIDs[rep(seq_len(nrow(textIDs)), nrow(texts)), ]
  
  # Add ID references to text tables
  texts <- cbind(textIDs, texts, full_bill_text = fullText, 
                 stringsAsFactors = FALSE)
  
  # ID References for votes tables
  voteIDs <- cbind(billData[c(1, 2, 10, 11)])
  
  # Check to see if the votes object is null
  if (!is.null(billobject[["votes"]])) {
    
    # Create the bill voting records object 
    # Note: pass the state_link slot to the votingRecords function to retrieve and parse the
    # pdf formatted versions of the voting records from the state's website
    votes <- xmlApply(billobject[["votes"]], xmlToList)
#                      FUN = function(x){
#                        list(
#                          roll_call_id = xmlValue(x[["roll_call_id"]]),
#                          date = ymd(xmlValue(x[["date"]])),
#                          passed = xmlValue(x[["passed"]]),
#                          desc = xmlValue(x[["desc"]]),
#                          url = xmlValue(x[["url"]]),
#                          state_link = xmlValue(x[["state_link"]])
#                        )
#                      }
#                    )
        
    # Convert the bill votes object to a data frame and prevent NULL values from causing problems
    votes <- rbind.fill(lapply(votes, function(f) {
      as.data.frame(Filter(Negate(is.null), f), stringsAsFactors = FALSE)
    }))
        
  } else {
    
    # Create a Null data frame for the broken case
    votes <- as.data.frame(cbind(roll_call_id = NA, date = NA, passed = NA,
                                 desc = NA, url = NA, state_link = NA), 
                           stringsAsFactors = FALSE)
    
  }
    
  # Copy the appropriate number of rows for the vote IDs object
  voteIDs <- voteIDs[rep(seq_len(nrow(voteIDs)), nrow(votes)), ]

  # Add the ID References to the votes table
  votes <- cbind(voteIDs, votes)
  
  # Store all of the data frames in a list object that will be returned at 
  # the end of the function call
  legiBill <- list(billMetaData = billData, billHistory = history, 
                   billProgress = progress, billSponsors = sponsors, 
                   billText = texts, billVoteOutcomes = votes)  
  
  # Return the list of dataframes with all of the bill data
  return(legiBill)
  
  # End function call    
}
