#' @title Legiscan Bill Data
#' @description
#' Parses and arranges XML output from
#' Legiscan master data dumps vote subdirectory
#' @param file An XML file object from the bill subdirectory of the master download
#' @return Creates a list object containing a several data.frame objects.
#' The objects include the bill's meta data, status, history, text, and
#' vote histories.
#' @examples \donttest{
#' # Build directory structure from master data downloads
#' directoryTree <- fileStructure("data/msHistorical/")
#'
#' # Create object with all file references stored in it
#' files <- fileLists(directoryTree)
#'
#' # Pass the function a bill object for processing
#' bills <- legiscanBill(files[["bills"]][[1]][[1]])
#' }
#' @family Parsing and Cleaning LegiScan Data
#' @name legiscanBill
#' @import XML lubridate plyr dplyr
#' @export legiscanBill
legiscanBill <- function(file) {

  # Need to make sure the XML and lubridate libraries are loaded
  # when function is called
  # may move these to be loaded when the package is loaded
  # require(XML); require(lubridate); require(plyr); require(dplyr)

  # Create a bill object with the parsed XML content
  bill <- XML::xmlRoot(XML::xmlParse(file))

  # Extract the bill elements from the bill object to be further processed
  billobject <- bill[["bill"]]

  # Create the session ID/name list
  session <- as.data.frame(XML::xmlToList(billobject[["session"]]),
                           stringsAsFactors = FALSE)

  # Define named list with all of the single element values
  singleValueList <- list(bill_id = "bill_id", change_hash = "change_hash",
  						url = "url", state_link = "state_link",
  						completed = "completed", status = "status",
  						status_date = "status_date", state = "state",
  						state_id = "state_id", bill_number = "bill_number",
  						bill_type = "bill_type", body = "body",
  						body_id = "body_id", current_body = "current_body",
  						current_body_id = "current_body_id", title = "title",
  						description = "description", committee = "committee")

  # Build the list containing all the single element values
  billData <- plyr::llply(singleValueList, FUN = function(singles){
  	XML::xmlToList(billobject[[singles]])
  })

  # Replace any NULL values with blank character strings
  billData <- plyr::llply(billData, FUN = function(rmNulls) {
    if (is.null(rmNulls)) rmNulls <- ""
    else rmNulls <- rmNulls
  })

  # Recast the date values as dates
  billData$status_date <- lubridate::ymd(billData$status_date)

  # Create bill metadata data frame object
  billData <- dplyr::bind_cols(session[1], as.data.frame(billData, stringsAsFactors = FALSE))

  # Create the bill history object (contains multiple lists)
  history <- XML::xmlApply(billobject[["history"]], XML::xmlToList)
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
  historyIDs <- dplyr::bind_cols(billData[c(1, 2, 11, 10)])

  # Convert the history object to a data frame and prevent
  # NULL values from causing problems
  history <- dplyr::bind_rows(plyr::llply(history, function(f) {
    as.data.frame(Filter(Negate(is.null), f), stringsAsFactors = FALSE)
  }))

  # Replicate the number of rows of history IDs needed to file the history data frame
  historyIDs <- historyIDs[rep(seq_len(nrow(historyIDs)), nrow(history)), ]

  # Add ID references to bill history dataframe
  history <- dplyr::bind_cols(historyIDs, history)

  # Create the bill progress object (contains multiple lists)
  progress <- XML::xmlApply(billobject[["progress"]], XML::xmlToList)
#                       FUN = function(x){
#                         list(
#                           date = ymd(xmlValue(x[["date"]])),
#                           event = xmlValue(x[["event"]])
#                         )
#                       }
#  )

  # Convert the bill progress object to a data frame and
  # prevent NULL values from causing problems
  progress <- dplyr::bind_rows(plyr::llply(progress, function(f) {
    as.data.frame(Filter(Negate(is.null), f), stringsAsFactors = FALSE)
  }))

  # ID References for progress table
  progressIDs <- as.data.frame(billData[c(1, 2, 11, 10)], stringsAsFactors = FALSE)

  # Replicate the number of rows of progress IDs needed to file the progress data frame
  progressIDs <- progressIDs[rep(seq_len(nrow(progressIDs)), nrow(progress)), ]

  # Add ID references to progress object
  progress <- dplyr::bind_cols(progressIDs, progress)

  # Create the bill sponsors object (also contains multiple lists)
  sponsors <- XML::xmlApply(billobject[["sponsors"]], XML::xmlToList)
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
  sponsorIDs <- dplyr::bind_cols(billData[c(1, 2, 11, 10)])

  # Convert the bill sponsors object to a data frame and
  # prevent NULL values from causing problems
  sponsors <- dplyr::bind_rows(plyr::llply(sponsors, function(f) {
    as.data.frame(Filter(Negate(is.null), f), stringsAsFactors = FALSE)
  }))

  # Replicate the number of rows of sponsor IDs needed to file the sponsors data frame
  sponsorIDs <- sponsorIDs[rep(seq_len(nrow(sponsorIDs)), nrow(sponsors)), ]

  # Add ID references to sponsors table
  sponsors <- dplyr::bind_cols(sponsorIDs, sponsors)

  if (!is.null(billobject[["texts"]])) {

    # Create the bill text object (will need to pass
    # the state_link slot to separate function to retrieve text)
    texts <- XML::xmlApply(billobject[["texts"]], XML::xmlToList)
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
    linkLists <- plyr::llply(texts, FUN = function(x){
      list(x[["state_link"]])
    })

    # Retrieve, parse, and clean the text of the bills
    cleanText <- plyr::llply(linkLists, FUN = function(links) {
      tryCatch(paste(XML::xpathApply(XML::htmlParse(links),
                                "//p", XML::xmlValue), collapse = "\n"),
               error = function(e) {
                 list(c("drop me"),
                      c("Error loading the bill text"))
               })
    })

    # Create data table with the text data
    fullText <- plyr::ldply(cleanText, dplyr::bind_rows)

    # Convert text back to character vector
    fullText[, 2] <- toString(fullText[, 2])

    # Assign a name to the cleaned full text data table
    names(fullText) <- c("drop", "full_bill_text")

    # Remove the ID column generated by dplyr
    fullText <- fullText[, 2]

    # Convert the bill texts object to a data frame and prevent NULL
	# values from causing problems
    texts <- dplyr::bind_rows(plyr::llply(texts, function(f) {
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
  texts <- dplyr::bind_cols(textIDs, texts, full_bill_text = fullText,
                 stringsAsFactors = FALSE)

  # ID References for votes tables
  voteIDs <- dplyr::bind_cols(billData[c(1, 2, 10, 11)])

  # Check to see if the votes object is null
  if (!is.null(billobject[["votes"]])) {

    # Create the bill voting records object
    # Note: pass the state_link slot to the votingRecords function to
  	# retrieve and parse the pdf formatted versions of the voting
  	# records from the state's website
    votes <- XML::xmlApply(billobject[["votes"]], XML::xmlToList)
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

    # Convert the bill votes object to a data frame and prevent
	# NULL values from causing problems
    votes <- dplyr::bind_rows(plyr::llply(votes, function(f) {
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
  votes <- dplyr::bind_cols(voteIDs, votes)

  # Store all of the data frames in a list object that will be returned at
  # the end of the function call
  legiBill <- list(billMetaData = billData, billHistory = history,
                   billProgress = progress, billSponsors = sponsors,
                   billText = texts, billVoteOutcomes = votes)

  # Return the list of dataframes with all of the bill data
  return(legiBill)

} # End of Function
