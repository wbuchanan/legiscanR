#' @title LegiScan Parser Methods - parseStates
#' @description Method for parsing LegiScan API calls to getStateList
#' @family LegiScan Parser Methods
#' @docType methods
#' @examples \dontrun{
#' # Create object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Get the state list for the user
#' theStates <- stateList(myLegiScan)
#'
#' # Parse the XML/JSON formatted data into a data frame
#' parseStates(theStates)
#'
#' # Or parse into a list
#' parseStates(theStates, dataframe = FALSE)
#' }
#' @importFrom XML xmlRoot xmlParse xmlToDataFrame xmlToList
#' @export parseStates
#' @rdname parseStates-methods
#' @aliases parseStates,XMLDocumentContent,logical-method
setMethod(f = "parseStates",
		  signature("XMLDocumentContent", "logical"),
		  definition = function(rawStateList, dataframe = TRUE) {

				  if (dataframe == TRUE) {
					  # Generate data frame
					  parsed <- XML::xmlRoot(XML::xmlParse(
					  					  rawStateList))[["states"]] %>%
				  			 	XML::xmlToDataFrame(stringsAsFactors = FALSE)
				  } else {
				  	parsed <- XML::xmlRoot(XML::xmlParse(
				  						  rawSessionList))[["states"]] %>%
				  			  XML::xmlToList()
				  }

				  # Return the cleaner/parsed data
				  return(parsed)
		  })

#' @family LegiScan Parser Methods
#' @docType methods
#' @importFrom RJSONIO isValidJSON fromJSON
#' @importFrom plyr ldply
#' @export parseStates
#' @rdname parseStates-methods
#' @aliases parseStates,character,logical-method
setMethod(f = "parseStates",
		  signature("character", "logical"),
		  definition = function(rawStateList, dataframe = TRUE) {

		  	# Validate the JSON
		  	if (!(RJSONIO::isValidJSON(rawStateList))) {
		  		warning(cat(paste("JSON object is not valid",
		  				  "will still attempt to parse", sep = "\n")))
		  	}

		  	if (dataframe == TRUE) {
		  		# Parse the JSON data into a Data frame
		  		parsed <- RJSONIO::fromJSON(rawStateList, nullValue = "null",
		  									simplify = Strict, asText = TRUE,
		  									simplifyWithNames = TRUE)[["states"]] %>%
		  									plyr::ldply(as.data.frame)
		  	} else {

		  		# Parse the JSON data into a list
		  		parsed <- RJSONIO::fromJSON(rawStateList, nullValue = "null",
		  									simplify = Strict, asText = TRUE,
		  									simplifyWithNames = TRUE)[["states"]]
		  	}

		  	# Return the data
		  	return(parsed)

		  })


#' @title LegiScan Parser Methods - parseSessions
#' @description Method for parsing LegiScan API calls to getSessionList
#' @family LegiScan Parser Methods
#' @docType methods
#' @examples \dontrun{
#' # Create object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Get the session list for Mississippi
#' theSessions <- sessionList(myLegiScan, "MS")
#'
#' # Parse the XML/JSON formatted data into a data frame
#' parseSessions(theSessions)
#'
#' # Or parse into a list
#' parseSessions(theSessions, dataframe = FALSE)
#' }
#' @importFrom XML xmlRoot xmlParse xmlToDataFrame xmlToList
#' @export parseSessions
#' @rdname parseSessions-methods
#' @aliases parseSessions,XMLDocumentContent,logical-method
setMethod(f = "parseSessions",
		  signature("XMLDocumentContent", "logical"),
		  definition = function(rawSessionList, dataframe = TRUE) {

		  	if (dataframe == TRUE) {
		  		# Generate data frame
		  		parsed <- XML::xmlRoot(XML::xmlParse(
		  			rawSessionList))[["sessions"]] %>%
		  			XML::xmlToDataFrame(stringsAsFactors = FALSE)
		  	} else {
		  		parsed <- XML::xmlRoot(XML::xmlParse(
		  			rawSessionList))[["sessions"]] %>%
		  			XML::xmlToList()
		  	}

		  	# Return the cleaner/parsed data
		  	return(parsed)
		  })

#' @family LegiScan Parser Methods
#' @docType methods
#' @importFrom RJSONIO isValidJSON fromJSON
#' @importFrom plyr ldply
#' @export parseSessions
#' @rdname parseSessions-methods
#' @aliases parseSessions,character,logical-method
setMethod(f = "parseSessions",
		  signature("character", "logical"),
		  definition = function(rawSessionList, dataframe = TRUE) {

		  	# Validate the JSON
		  	if (!(RJSONIO::isValidJSON(rawSessionList))) {
		  		warning(cat(paste("JSON object is not valid",
		  						  "will still attempt to parse", sep = "\n")))
		  	}

		  	if (dataframe == TRUE) {
		  		# Parse the JSON data into a Data frame
		  		parsed <- RJSONIO::fromJSON(rawSessionList, nullValue = "null",
		  									simplify = Strict, asText = TRUE,
		  									simplifyWithNames = TRUE)[["sessions"]] %>%
		  			plyr::ldply(as.data.frame)
		  	} else {

		  		# Parse the JSON data into a list
		  		parsed <- RJSONIO::fromJSON(rawSessionList, nullValue = "null",
		  									simplify = Strict, asText = TRUE,
		  									simplifyWithNames = TRUE)[["sessions"]]
		  	}

		  	# Return the data
		  	return(parsed)

		  })

#' @title LegiScan Parser Methods - parseMasterList
#' @description Method for parsing LegiScan API calls to getMasterList
#' @family LegiScan Parser Methods
#' @docType methods
#' @examples \dontrun{
#' # Create object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Get the master list for Mississippi
#' theMasterList <- masterList(myLegiScan, "MS")
#'
#' # Parse the XML/JSON formatted data into a data frame
#' parseMasterList(theMasterList)
#'
#' # Or parse into a list
#' parseMasterList(theMasterList, dataframe = FALSE)
#'
#' # R
#' }
#' @importFrom XML xmlRoot xmlParse xmlToDataFrame
#' @importFrom plyr ldply
#' @importFrom dplyr bind_cols
#' @export parseMasterList
#' @rdname parseMasterList-methods
#' @aliases parseMasterList,XMLDocumentContent,logical,logical-method
setMethod(f = "parseMasterList",
		  signature("XMLDocumentContent", "logical", "logical"),
		  definition = function(rawMasterList, dataframe = FALSE, archive) {

		  	# Master list files are a bit more complex, there is a small list used to ID
	  		# the session followed by a list of bills.
	  		# First get the session ID data into the data.frame
	  		# Then put the bills in a separate data frame
	  		# Replicate the correct # of rows of session IDs for the bills and bind them
	  		# together.  The order doesn't matter since all bills will be from the same
	  		# legislative session.
		  	sessionIDs <- XML::xmlRoot(
		  					XML::xmlParse(rawMasterList))[["masterlist"]][[1]] %>%
		  					XML::xmlToList()
		  	sessionIDs <- dplyr::as_data_frame(sessionIDs) %>%
		  				  dplyr::as.tbl(stringsAsFactors = FALSE)

		  	bills <- XML::xmlRoot(
		  					XML::xmlParse(rawMasterList))[["masterlist"]][-1] %>%
		  					XML::xmlToDataFrame(stringsAsFactors = FALSE)

		  	# Coerce character dates to POSIX dates
	  		bills$status_date <- lubridate::ymd(bills$status_date)
	  		bills$last_action_date <- lubridate::ymd(bills$last_action_date)

		  	# Need to coerce bill_ids to numeric from XML data, but cast as numeric in JSON
		  	bills$bill_id <- as.numeric(bills$bill_id)

		  	# Build the master list data object including the session ID data
		  	parsed <- sessionIDs[rep(seq_len(nrow(sessionIDs)), nrow(bills)), ] %>%
		  									dplyr::bind_cols(bills)

	  		# Coerce to data frame if the dataframe argument is set to TRUE
			if (dataframe == TRUE) parsed <- as.data.frame(parsed, stringsAsFactors = FALSE)

	  		# Check archive option
	  		if (archive == TRUE) {

	  			# Save an archive file
	  			saveRDS(parsed, file = paste0("MasterListArchive", Sys.time(), ".Rds"))

	  			# Return data object
	  			return(parsed)

			} else {

	  			# Return data object
	  			return(parsed)

			}
		  })


#' @family LegiScan Parser Methods
#' @docType methods
#' @importFrom RJSONIO fromJSON
#' @importFrom plyr ldply
#' @importFrom dplyr bind_cols
#' @export parseMasterList
#' @rdname parseMasterList-methods
#' @aliases parseSessions,character,logical,logical-method
setMethod(f = "parseMasterList",
		  signature("character", "logical", "logical"),
		  definition = function(rawMasterList, dataframe = FALSE, archive = TRUE) {

			  	# Validate the JSON
			  	if (!(RJSONIO::isValidJSON(rawMasterList, asText = TRUE))) {
			  		warning(cat(paste("JSON object is not valid",
			  				  "will still attempt to parse", sep = "\n")))
			  	}

		  		# Explanatory comments are above in the method for XML files.
		  		# No changes to the method beyond using a different method to
		  		# interpret the data (e.g., XML v JSON)
		  		sessionIDs <- RJSONIO::fromJSON(rawMasterList, simplify = Strict,
  								asText = TRUE, nullValue = "null",
  								simplifyWithNames = TRUE)[["masterlist"]][1] %>%
  								plyr::ldply(as.data.frame, stringsAsFactors = FALSE)

		  		bills <- RJSONIO::fromJSON(rawMasterList, simplify = Strict,
  							   asText = TRUE, nullValue = "null",
  							   simplifyWithNames = TRUE)[["masterlist"]][-1] %>%
  							   plyr::ldply(as.data.frame, stringsAsFactors = FALSE)

		  		bills$status_date <- lubridate::ymd(bills$status_date)

		  		bills$last_action_date <- lubridate::ymd(bills$last_action_date)

		  		parsed <- sessionIDs[rep(seq_len(nrow(sessionIDs)), nrow(bills)), -1] %>%
		  									dplyr::bind_cols(bills[, -1])

				if (dataframe == TRUE) parsed <- as.data.frame(parsed,
														stringsAsFactors = FALSE)

		  		if (archive == TRUE) {
		  			saveRDS(parsed, file = paste0("MasterListArchive", Sys.time(), ".Rds"))
		  			return(parsed)
				} else {
					return(parsed)
				}

		  })

#' @title LegiScan Parser Methods - parseBill
#' @description Method for parsing ID data from LegiScan API calls to getBill
#' @family LegiScan Parser Methods
#' @docType methods
#' @examples \dontrun{
#' # Create object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Get the master list to get bill IDs
#' master <- masterList(myLegiScan, state = "MS")
#'
#' # Get the bill data for the first bill in the session
#' theBill <- bill(myLegiScan, master[1,3])
#'
#' # Parse the XML/JSON formatted data into a data frame
#' parseBillIDs(theBill)
#'
#' }
#' @return Returns multiple data frame objects if dataframe = TRUE or returns a
#' list of data frames if the dataframe = FALSE
#' @importFrom XML xmlRoot xmlParse xmlToDataFrame xmlToList
#' @export parseBill
#' @rdname parseBill-methods
#' @aliases parseBill,XMLDocumentContent,logical,character-method
setMethod(f = "parseBill",
		  signature("XMLDocumentContent", "logical", "character"),
		  definition = function(rawBill, dataframe = FALSE, fullText = "") {

	  	# Check values of the fullText parameter
		if (!(fullText %in% c("", "state_link", "url"))) {
			warning(cat(paste('Error: fullText not in "", state_link, or url',
				'will not attempt to retrieve the full bill text', sep = "\n")))
		}

	  	# Create a parsed bill object
	  	billobject <- XML::xmlRoot(XML::xmlParse(rawBill))[["bill"]]

	  	# Parse the XML into a List object and replace higher level
	  	# NULL values with NA
	  	billData <- XML::xmlToList(billobject) %>%
		  				plyr::llply(.fun = function(x) {
		  					ifelse(is.null(x), NA, x)
		  				})

	  	# Object storing names of metadata elements
	  	meta <- list(bill_id = "bill_id", change_hash = "change_hash",
  					status_date = "status_date", state_id = "state_id",
	  				state = "state", bill_number = "bill_number",
	  				url = "url", state_link = "state_link",
	  				completed = "completed", status = "status",
  					bill_type = "bill_type", body = "body",
  					body_id = "body_id", current_body = "current_body",
  					current_body_id = "current_body_id", title = "title",
  					description = "description", committee = "committee")

	  	# Pull out a meta data record that will also provide the
	  	# ID data for subsequent objects
	  	billMeta <- as_data_frame(billobject[unlist(meta)])

	  	# Adjust casting of the date value so it will be pushed into
	  	# subsequent data frame objects
	  	billMeta$status_date <- lubridate::ymd(billMeta$status_date)
	  	billMeta$bill_id <- as.numeric(billMeta$bill_id)
	  	billMeta$state_id <- as.numeric(billMeta$state_id)
	  	billMeta$completed <- as.numeric(billMeta$completed)
	  	billMeta$status <- as.numeric(billMeta$status)
	  	billMeta$body_id <- as.numeric(billMeta$body_id)
	  	billMeta$current_body_id <- as.numeric(billMeta$current_body_id)

	  	# Create a vector  of Bill ID data
	  	billID <- billMeta[c(1:6)]

	  	# Build data frame objects for the bill sponsors,
	  	# history, progress, committees, and texts
		sponsors <- XML::xmlApply(billobject[["sponsors"]], XML::xmlToList) %>%
						plyr::llply(.fun = function(x) {
							plyr::llply(x, .fun = function(y) {
								ifelse(is.null(y), NA, y)
							}) %>%
							dplyr::as_data_frame()
						}) %>% dplyr::bind_rows()

		history <-  XML::xmlApply(billobject[["history"]], XML::xmlToList) %>%
						plyr::llply(.fun = function(x) {
							plyr::llply(x, .fun = function(y) {
								ifelse(is.null(y), NA, y)
							}) %>%
							dplyr::as_data_frame()
						}) %>% dplyr::bind_rows()

		committees <- XML::xmlApply(billobject[["committee"]], XML::xmlToList) %>%
						plyr::llply(.fun = function(x) {
							ifelse(is.null(x), NA, x)
						}) %>% dplyr::as_data_frame() %>%
						dplyr::bind_rows()

		texts <- 	XML::xmlApply(billobject[["texts"]], XML::xmlToList) %>%
						plyr::llply(.fun = function(x) {
							plyr::llply(x, .fun = function(y) {
								ifelse(is.null(y), NA, y)
							}) %>%
							dplyr::as_data_frame()
						}) %>% dplyr::bind_rows()

		votes <- plyr::llply(billobject[["votes"]], .fun = function(x) {
						plyr::llply(x, .fun = function(y) {
								ifelse(is.null(y) | nchar(y) == 0, NA, y)
							}) %>%
						dplyr::as_data_frame()
					}) %>% dplyr::bind_rows()

		progress <- XML::xmlApply(billobject[["progress"]], XML::xmlToList) %>%
					plyr::llply(.fun = function(x) {
						plyr::llply(x, .fun = function(y) {
							ifelse(is.null(y), NA, y)
						}) %>%
						dplyr::as_data_frame()
					}) %>% dplyr::bind_rows()

	  	# Add ID vector to each of the data objects
  	  	sponsors <- bind_cols(billID[rep(seq_len(nrow(billID)),
  						nrow(sponsors)), ], sponsors)
  	  	history <- bind_cols(billID[rep(seq_len(nrow(billID)),
  						nrow(history)), ], history)
  	  	committees <- bind_cols(billID[rep(seq_len(nrow(billID)),
  						nrow(committees)), ], committees)
  	  	progress <- bind_cols(billID[rep(seq_len(nrow(billID)),
  						nrow(progress)), ], progress)
  	  	votes <- bind_cols(billID[rep(seq_len(nrow(billID)),
  						nrow(votes)), ], votes)
  	  	texts <- bind_cols(billID[rep(seq_len(nrow(billID)),
  						nrow(texts)), ], texts)

	  	# Check for retreival value for full bill texts
	  	if (fullText %in% c("state_link", "url")) {
		  	# Generate a list of all of the state link elements
		    linkList <- plyr::llply(texts, FUN = function(x){
		      list(x[[fullText]])
		    })
	  		# Retrieve/Clean bill text if the URL request doesn't fail
	  		cleanText <- plyr::llply(linkList, .fun = function(link) {
		  			if (httr::http_status(
		  					httr::GET(link))$message == "success: (200) OK") {
		  					paste(XML::xpathApply(XML::htmlParse(links),
                                	"//p", XML::xmlValue), collapse = "\n")
		  			} else {
		  				"Could not retrieve full bill text from URL"
		  			}
	  			}) %>% dplyr::as_data_frame()
	  		# Bind the full text to the rest of the test data
	  		texts <- dplyr::bind_rows(texts, cleanText)
	  	}

	  	# Reformat date/numeric values for data frames that include dates
		committees$committee_id <- as.numeric(committees$committee_id)
	  	history$date <- lubridate::ymd(history$date)
	  	history$chamber_id <- as.numeric(history$chamber_id)
	  	history$importance <- as.numeric(history$importance)
	  	sponsorIDs <- list("people_id", "party_id", "role_id", "ftm_eid",
	  					  "sponsor_type_id", "sponsor_order",
	  					  "committee_sponsor", "committee_id")
		for (x in sponsorIDs) {
	  		sponsors[[x]] <- as.numeric(sponsors[[x]])
		}
	  	progress$date <- lubridate::ymd(progress$date)
	  	progress$event <- as.numeric(progress$event)
	  	texts$doc_id <- as.numeric(texts$doc_id)
	  	texts$date <- lubridate::ymd(texts$date)
		votes$date <- lubridate::ymd(votes$date)
		votes$roll_call_id <- as.numeric(votes$roll_call_id)
		votes$passed <- as.numeric(votes$passed)

	  	# Determine what type of object to return
	  	if (dataframe == TRUE) {
	  		# Return individual data frame objects
	  		billmeta <<- billMeta; sponsors <<- sponsors; texts <<- texts;
	  		progress <<- progress; committees <<- committees; votes <<- votes
	  	} else {
			# Pack all of the data frames into a single list object that
	  		# would be returned at the end of the method call
	  		theData <- list(billmeta = billMeta, sponsors = sponsors,
	  						texts = texts, progress = progress,
	  						committees = committees, votes = votes)
	  		return(theData)
	  	}
	}
)


#' @family LegiScan Parser Methods
#' @docType methods
#' @return Returns multiple data frame objects if dataframe = TRUE or returns a
#' list of data frames if the dataframe = FALSE
#' @importFrom RJSONIO isValidJSON fromJSON
#' @importFrom lubridate ymd
#' @importFrom plyr llply
#' @importFrom dplyr as_data_frame bind_rows bind_cols
#' @importFrom httr http_status GET
#' @importFrom XML xpathApply htmlParse xmlValue
#' @export parseBill
#' @rdname parseBill-methods
#' @aliases parseBill,character,logical,character-method
setMethod(f = "parseBill",
		  signature("character", "logical", "character"),
		  definition = function(rawBill, dataframe = FALSE, fullText = "") {

	  	# Check values of the fullText parameter
		if (!(fullText %in% c("", "state_link", "url"))) {
			warning(cat(paste('Error: fullText not in "", state_link, or url',
				'will not attempt to retrieve the full bill text', sep = "\n")))
		}

		# Validate the JSON
	  	if (!(RJSONIO::isValidJSON(rawBill, asText = TRUE))) {
	  		warning(cat(paste("JSON object is not valid",
	  				  "will still attempt to parse", sep = "\n")))
	  	}

	  	# Create a parsed bill object
	  	billobject <- RJSONIO::fromJSON(rawBill, asText = TRUE,
	  									nullValue = NA)[["bill"]]

	  	# Object storing names of metadata elements
	  	meta <- list(bill_id = "bill_id", change_hash = "change_hash",
  					status_date = "status_date", state_id = "state_id",
	  				state = "state", bill_number = "bill_number",
	  				url = "url", state_link = "state_link",
	  				completed = "completed", status = "status",
  					bill_type = "bill_type", body = "body",
  					body_id = "body_id", current_body = "current_body",
  					current_body_id = "current_body_id", title = "title",
  					description = "description", committee = "committee")

	  	# Pull out a meta data record that will also provide the
	  	# ID data for subsequent objects
	  	billMeta <- as_data_frame(billobject[unlist(meta)])

	  	# Adjust casting of the date value so it will be pushed into
	  	# subsequent data frame objects
	  	billMeta$status_date <- lubridate::ymd(billMeta$status_date)

	  	# Create a vector  of Bill ID data
	  	billID <- billMeta[c(1:6)]

	  	# Build data frame objects for the bill sponsors,
	  	# history, progress, committees, and texts
		sponsors <- plyr::llply(billobject[["sponsors"]], .fun = function(x) {
						plyr::llply(x, .fun = function(y) {
								ifelse(is.null(y) | nchar(y) == 0, NA, y)
							}) %>%
						dplyr::as_data_frame()
					}) %>% dplyr::bind_rows()

		history <-  plyr::llply(billobject[["history"]], .fun = function(x) {
						plyr::llply(x, .fun = function(y) {
								ifelse(is.null(y) | nchar(y) == 0, NA, y)
							}) %>%
						dplyr::as_data_frame()
					}) %>% dplyr::bind_rows()

		committees <- 	plyr::llply(billobject[["committee"]], .fun = function(x) {
							ifelse(is.null(x) | nchar(x) == 0, NA, x)
						}) %>% dplyr::as_data_frame() %>%
						dplyr::bind_rows()

		texts <- 	plyr::llply(billobject[["texts"]], .fun = function(x) {
						plyr::llply(x, .fun = function(y) {
								ifelse(is.null(y) | nchar(y) == 0, NA, y)
							}) %>%
						dplyr::as_data_frame()
					}) %>% dplyr::bind_rows()

		votes <- 	plyr::llply(billobject[["votes"]], .fun = function(x) {
						plyr::llply(x, .fun = function(y) {
								ifelse(is.null(y) | nchar(y) == 0, NA, y)
							}) %>%
						dplyr::as_data_frame()
					}) %>% dplyr::bind_rows()

		progress <- plyr::llply(billobject[["progress"]], .fun = function(x) {
						plyr::llply(x, .fun = function(y) {
								ifelse(is.null(y) | nchar(y) == 0, NA, y)
							}) %>%
						dplyr::as_data_frame()
					}) %>% dplyr::bind_rows()

	  	# Add ID vector to each of the data objects
  	  	sponsors <- bind_cols(billID[rep(seq_len(nrow(billID)),
  						nrow(sponsors)), ], sponsors)
  	  	history <- bind_cols(billID[rep(seq_len(nrow(billID)),
  						nrow(history)), ], history)
  	  	committees <- bind_cols(billID[rep(seq_len(nrow(billID)),
  						nrow(committees)), ], committees)
  	  	progress <- bind_cols(billID[rep(seq_len(nrow(billID)),
  						nrow(progress)), ], progress)
  	  	votes <- bind_cols(billID[rep(seq_len(nrow(billID)),
  						nrow(votes)), ], votes)
  	  	texts <- bind_cols(billID[rep(seq_len(nrow(billID)),
  						nrow(texts)), ], texts)

	  	# Check for retreival value for full bill texts
	  	if (fullText %in% c("state_link", "url")) {
		  	# Generate a list of all of the state link elements
		    linkList <- plyr::llply(texts, FUN = function(x){
		      list(x[[fullText]])
		    })
	  		# Retrieve/Clean bill text if the URL request doesn't fail
	  		cleanText <- plyr::llply(linkList, .fun = function(link) {
		  			if (httr::http_status(
		  					httr::GET(link))$message == "success: (200) OK") {
		  					paste(XML::xpathApply(XML::htmlParse(links),
                                	"//p", XML::xmlValue), collapse = "\n")
		  			} else {
		  				"Could not retrieve full bill text from URL"
		  			}
	  			}) %>% dplyr::as_data_frame()
	  		# Bind the full text to the rest of the test data
	  		texts <- dplyr::bind_rows(texts, cleanText)
	  	}

	  	# Reformat date/numeric values for data frames that include dates
		committees$committee_id <- as.numeric(committees$committee_id)
	  	history$date <- lubridate::ymd(history$date)
	  	history$chamber_id <- as.numeric(history$chamber_id)
	  	history$importance <- as.numeric(history$importance)
	  	people$committee_id <- as.numeric(people$committee_id)
	  	progress$date <- lubridate::ymd(progress$date)
	  	progress$event <- as.numeric(progress$event)
	  	texts$doc_id <- as.numeric(texts$doc_id)
	  	texts$date <- lubridate::ymd(texts$date)
		votes$date <- lubridate::ymd(votes$date)
		votes$roll_call_id <- as.numeric(votes$roll_call_id)
		votes$passed <- as.numeric(votes$passed)

	  	# Determine what type of object to return
	  	if (dataframe == TRUE) {
	  		# Return individual data frame objects
	  		billmeta <<- billMeta; sponsors <<- sponsors; texts <<- texts;
	  		progress <<- progress; committees <<- committees; votes <<- votes
	  	} else {
			# Pack all of the data frames into a single list object that
	  		# would be returned at the end of the method call
	  		theData <- list(billmeta = billMeta, sponsors = sponsors,
	  						texts = texts, progress = progress,
	  						committees = committees, votes = votes)
	  		return(theData)
	  	}
	}
)


#' @title LegiScan Parser Methods - parseBillText
#' @description Method for parsing LegiScan API calls to getBillText
#' @family LegiScan Parser Methods
#' @docType methods
#' @examples \dontrun{
#' # Create object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Get the master list for Mississippi
#' theMasterList <- masterList(myLegiScan, "MS")
#'
#' # Parse the XML/JSON formatted data into a data frame
#' cleanerMasterList <- parseMasterList(theMasterList)
#'
#' # Get A bill (bill_id is in the 3rd Column of the master list)
#' aBill <- bill(myLegiScan, cleanerMasterList[1, 3])
#'
#' # Parse the bill data
#' cleanerBill <- cleanBill(aBill, dataframe = FALSE, text = 'state_link')
#'
#' # Retrieve the Bill Text from the API call
#' legiscanBillText <- billText(myLegiScan, cleanerBill[["texts"]][["doc_id"]])
#'
#' # Store the API response in new object
#' aBillText <- legiscanBillText[[1]]
#'
#' # Parse the bill text response from LegiScan
#' cleanedBillText <- parseBillText(aBillText)
#'
#' }
#' @note The getBillText API call method returns a binary MIME object with any
#' of several document format types.  While this may be of interest, it is much
#' easier to retrieve the text data from the cleanBill function with an option
#' specified for the text argument.
#' @importFrom XML xmlRoot xmlParse xmlToList
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd
#' @export parseBillText
#' @rdname parseBillText-methods
#' @aliases parseBillText,XMLDocumentContent-method
setMethod(f = "parseBillText",
		  signature("XMLDocumentContent"),
		  definition = function(theBillText) {

		  	# Parse the API call response from the billText method
		  	billText <- XML::xmlRoot(XML::xmlParse(theBillText)) %>%
		  				XML::xmlToList %>% unlist(recursive = FALSE)

		  	# Clean up the names of the elements
		  	billText <- gsub("text.", "", names(billText))

		  	# Clean up the casting of the variables
		  	billText$date <- lubridate::ymd(billText$date)
		  	billText$doc_id <- as.numeric(billText$doc_id)
		  	billText$bill_id <- as.numeric(billText$bill_id)

		  	# Return the cleaner/parsed data [omit the HTTP status response]
		  	return(billText[-1])

		  })

#' @family LegiScan Parser Methods
#' @docType methods
#' @importFrom RJSONIO isValidJSON fromJSON
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd
#' @export parseBillText
#' @rdname parseBillText-methods
#' @aliases parseBillText,character-method
setMethod(f = "parseBillText",
		  signature("character"),
		  definition = function(theBillText) {

		  	# Validate the JSON
		  	if (!(RJSONIO::isValidJSON(theBillText))) {
		  		warning(cat(paste("JSON object is not valid",
		  				"will still attempt to parse", sep = "\n")))
		  	}

		  	# Parse the JSON data into a Data frame
	  		billText <- RJSONIO::fromJSON(theBillText, nullValue = NA,
	  									simplify = Strict, asText = TRUE,
	  									simplifyWithNames = TRUE) %>%
						unlist(recursive = FALSE)

		  	# Clean up the names of the elements
		  	billText <- gsub("text.", "", names(billText))

		  	# Clean up the casting of the variables
		  	billText$date <- lubridate::ymd(billText$date)
		  	billText$doc_id <- as.numeric(billText$doc_id)
		  	billText$bill_id <- as.numeric(billText$bill_id)

		  	# Return the cleaner/parsed data [omit the HTTP status response]
		  	return(billText[-1])

		  })


#' @title LegiScan Parser Methods - parseAmendment
#' @description Method for parsing LegiScan API calls to getAmendment
#' @family LegiScan Parser Methods
#' @docType methods
#' @examples \dontrun{
#' # Create object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Get the master list for Mississippi
#' theMasterList <- masterList(myLegiScan, "MS")
#'
#' # Parse the XML/JSON formatted data into a data frame
#' cleanerMasterList <- parseMasterList(theMasterList)
#'
#' # Get A bill (bill_id is in the 3rd Column of the master list)
#' aBill <- bill(myLegiScan, cleanerMasterList[1, 3])
#'
#' # Parse the bill data
#' cleanerBill <- cleanBill(aBill, dataframe = FALSE, text = '')
#'
#' # Retrieve the Amendment from the API call
#' anAmendment <- amendment(myLegiScan, cleanerBill[["amendments"]][["amendment_id"]])
#'
#' # Parse the bill text response from LegiScan
#' cleanedAmendment <- parseAmendment(anAmendment)
#'
#' }
#' @note The getAmendment API call method returns a binary MIME object with any
#' of several document format types.  Amendments and Supplements are not supported
#' in all states.
#' @importFrom XML xmlRoot xmlParse xmlToList
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd
#' @export parseAmendment
#' @rdname parseAmendment-methods
#' @aliases parseAmendment,XMLDocumentContent-method
setMethod(f = "parseAmendment",
		  signature("XMLDocumentContent"),
		  definition = function(theAmendment) {

		  	# Parse the API call response from the amendment method
		  	anAmendment <- XML::xmlRoot(XML::xmlParse(theAmendment)) %>%
		  				XML::xmlToList %>% unlist(recursive = FALSE)

		  	# Clean up the names of the elements
		  	anAmendment <- gsub("amendment.", "", names(anAmendment))

		  	# Clean up the casting of the variables
		  	anAmendment$date <- lubridate::ymd(anAmendment$date)
		  	anAmendment$amendment_id <- as.numeric(anAmendment$amendment_id)
		  	anAmendment$bill_id <- as.numeric(anAmendment$bill_id)
			anAmendment$adopted <- as.numeric(anAmendment$adopted)

		  	# Return the cleaner/parsed data [omit the HTTP status response]
		  	return(anAmendment[-1])

		  })

#' @family LegiScan Parser Methods
#' @docType methods
#' @importFrom RJSONIO isValidJSON fromJSON
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd
#' @export parseAmendment
#' @rdname parseAmendment-methods
#' @aliases parseAmendment,character-method
setMethod(f = "parseAmendment",
		  signature("character"),
		  definition = function(theAmendment) {

		  	# Validate the JSON
		  	if (!(RJSONIO::isValidJSON(theAmendment))) {
		  		warning(cat(paste("JSON object is not valid",
		  				"will still attempt to parse", sep = "\n")))
		  	}

		  	# Parse the JSON data into a Data frame
	  		anAmendment <- RJSONIO::fromJSON(anAmendment, nullValue = NA,
	  									simplify = Strict, asText = TRUE,
	  									simplifyWithNames = TRUE) %>%
						unlist(recursive = FALSE)

		  	# Clean up the names of the elements
		  	anAmendment <- gsub("amendment.", "", names(anAmendment))

		  	# Clean up the casting of the variables
		  	anAmendment$date <- lubridate::ymd(anAmendment$date)
		  	anAmendment$amendment_id <- as.numeric(anAmendment$amendment_id)
		  	anAmendment$bill_id <- as.numeric(anAmendment$bill_id)
			anAmendment$adopted <- as.numeric(anAmendment$adopted)

		  	# Return the cleaner/parsed data [omit the HTTP status response]
		  	return(anAmendment[-1])

		  })


#' @title LegiScan Parser Methods - parseSupplement
#' @description Method for parsing LegiScan API calls to getSupplement
#' @family LegiScan Parser Methods
#' @docType methods
#' @examples \dontrun{
#' # Create object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Get the master list for Mississippi
#' theMasterList <- masterList(myLegiScan, "MS")
#'
#' # Parse the XML/JSON formatted data into a data frame
#' cleanerMasterList <- parseMasterList(theMasterList)
#'
#' # Get A bill (bill_id is in the 3rd Column of the master list)
#' aBill <- bill(myLegiScan, cleanerMasterList[1, 3])
#'
#' # Parse the bill data
#' cleanerBill <- cleanBill(aBill, dataframe = FALSE, text = '')
#'
#' # Retrieve the Supplement from the API call
#' aSupplement <- supplement(myLegiScan, cleanerBill[["supplements"]][["supplement_id"]])
#'
#' # Parse the bill text response from LegiScan
#' cleanedSupplement <- parseSupplement(aSupplement)
#'
#' }
#' @note The getSupplement API call method returns a binary MIME object with any
#' of several document format types.  Amendments and supplements are not supported
#' in all states.
#' @importFrom XML xmlRoot xmlParse xmlToList
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd
#' @export parseSupplement
#' @rdname parseSupplement-methods
#' @aliases parseSupplement,XMLDocumentContent-method
setMethod(f = "parseSupplement",
		  signature("XMLDocumentContent"),
		  definition = function(theSupplement) {

		  	# Parse the API call response from the supplement method
		  	aSupplement <- XML::xmlRoot(XML::xmlParse(theSupplement)) %>%
		  				XML::xmlToList %>% unlist(recursive = FALSE)

		  	# Clean up the names of the elements
		  	aSupplement <- gsub("supplement.", "", names(aSupplement))

		  	# Clean up the casting of the variables
		  	aSupplement$date <- lubridate::ymd(aSupplement$date)
		  	aSupplement$supplement_id <- as.numeric(aSupplement$supplement_id)
		  	aSupplement$bill_id <- as.numeric(aSupplement$bill_id)
			aSupplement$type_id <- as.numeric(aSupplement$type_id)

		  	# Return the cleaner/parsed data [omit the HTTP status response]
		  	return(aSupplement[-1])

		  })

#' @family LegiScan Parser Methods
#' @docType methods
#' @importFrom RJSONIO isValidJSON fromJSON
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd
#' @export parseSupplement
#' @rdname parseSupplement-methods
#' @aliases parseSupplement,character-method
setMethod(f = "parseSupplement",
		  signature("character"),
		  definition = function(theSupplement) {

		  	# Validate the JSON
		  	if (!(RJSONIO::isValidJSON(theSupplement))) {
		  		warning(cat(paste("JSON object is not valid",
		  				"will still attempt to parse", sep = "\n")))
		  	}

		  	# Parse the JSON data into a Data frame
	  		aSupplement <- RJSONIO::fromJSON(theSupplement, nullValue = NA,
	  									simplify = Strict, asText = TRUE,
	  									simplifyWithNames = TRUE) %>%
						unlist(recursive = FALSE)

		  	# Clean up the names of the elements
		  	aSupplement <- gsub("supplement.", "", names(aSupplement))

		  	# Clean up the casting of the variables
		  	aSupplement$date <- lubridate::ymd(aSupplement$date)
		  	aSupplement$supplement_id <- as.numeric(aSupplement$supplement_id)
		  	aSupplement$bill_id <- as.numeric(aSupplement$bill_id)
			aSupplement$type_id <- as.numeric(aSupplement$type_id)

		  	# Return the cleaner/parsed data [omit the HTTP status response]
		  	return(aSupplement[-1])

		  })


#' @title LegiScan Parser Methods - parseRollCall
#' @description Method for parsing LegiScan API calls to getRollCall
#' @family LegiScan Parser Methods
#' @docType methods
#' @examples \dontrun{
#' # Create object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Get the master list for Mississippi
#' theMasterList <- masterList(myLegiScan, "MS")
#'
#' # Parse the XML/JSON formatted data into a data frame
#' cleanerMasterList <- parseMasterList(theMasterList)
#'
#' # Get A bill (bill_id is in the 3rd Column of the master list)
#' aBill <- bill(myLegiScan, cleanerMasterList[1, 3])
#'
#' # Parse the bill data
#' cleanerBill <- cleanBill(aBill, dataframe = FALSE, text = '')
#'
#' # Retrieve the RollCall from the API call
#' aRollCall <- rollCall(myLegiScan, cleanerBill[["votes"]][["roll_call_id"]])
#'
#' # Parse the bill text response from LegiScan
#' cleanedRollCall <- parseRollCall(aRollCall)
#'
#' }
#' @note The getRollCall API call method returns a binary MIME object with any
#' of several document format types.  Amendments and rollCalls are not supported
#' in all states.
#' @importFrom XML xmlRoot xmlParse xmlToList
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd
#' @export parseRollCall
#' @rdname parseRollCall-methods
#' @aliases parseRollCall,XMLDocumentContent,logical-method
setMethod(f = "parseRollCall",
		  signature("XMLDocumentContent", "logical"),
		  definition = function(theRollCall, dataframe = FALSE) {

		  	# Parse the API call response from the rollCall method
		  	aRollCall <- XML::xmlRoot(XML::xmlParse(theRollCall))[["roll_call"]] %>%
		  				XML::xmlToList

		  	# Object storing names of metadata elements
	  		metaVote <- list(roll_call_id = "roll_call_id", bill_id = "bill_id",
	  					date = "date", desc = "desc", yea = "yea", nay = "nay",
	  					nv = "nv", absent = "absent", passed = "passed")

		  	# Pull out a meta data record that will also provide the
		  	# ID data for subsequent objects
		  	voteMeta <- as_data_frame(aRollCall[unlist(metaVote)])

		  	# Check casting of voting meta data
		  	voteMeta$date <- lubridate::ymd(voteMeta$date)
		  	for (i in names(metaVote[-c(3, 4)])) {
		  		voteMeta[[i]] <- as.numeric(voteMeta[[i]])
		  	}

		  	# Create vector of ID variables to attach to the individual voting
		  	# records
		  	voteID <- voteMeta[c(1:3)]

		  	# Parse the individual legislator votes
		  	indVotes <- plyr::llply(aRollCall[["votes"]], .fun = function(x) {
						plyr::llply(x, .fun = function(y) {
								ifelse(is.null(y) | nchar(y) == 0, NA, y)
							}) %>%
						dplyr::as_data_frame()
					}) %>% dplyr::bind_rows()

  	  	  	indVotes <- bind_cols(voteID[rep(seq_len(nrow(voteID)),
  				nrow(indVotes)), ], indVotes)

		  	# Check the casting of the individual votes
		  	indVotes$people_id <- as.numeric(indVotes$people_id)
		  	indVotes$vote_id <- as.numeric(indVotes$vote_id)

		  	# Determine what type of object to return
	  		if (dataframe == TRUE) {
	  			# Return individual data frame objects
	  			voteSummary <<- voteMeta; individualVotes <<- indVotes
	  		} else {
	  			voteData <- list(voteSummary = voteMeta, individualVotes = indVotes)
	  			return(voteData)
	  		}

		  })

#' @family LegiScan Parser Methods
#' @docType methods
#' @importFrom RJSONIO isValidJSON fromJSON
#' @importFrom magrittr %>%
#' @importFrom lubridate ymd
#' @export parseRollCall
#' @rdname parseRollCall-methods
#' @aliases parseRollCall,character,logical-method
setMethod(f = "parseRollCall",
		  signature("character", "logical"),
		  definition = function(theRollCall, dataframe = FALSE) {

		  	# Validate the JSON
		  	if (!(RJSONIO::isValidJSON(theRollCall))) {
		  		warning(cat(paste("JSON object is not valid",
		  				"will still attempt to parse", sep = "\n")))
		  	}

		  	# Parse the JSON data into a Data frame
	  		aRollCall <- RJSONIO::fromJSON(theRollCall, asText = TRUE,
	  									nullValue = NA)[["roll_call"]]

			# Object storing names of metadata elements
	  		metaVote <- list(roll_call_id = "roll_call_id", bill_id = "bill_id",
	  					date = "date", desc = "desc", yea = "yea", nay = "nay",
	  					nv = "nv", absent = "absent", passed = "passed")

		  	# Pull out a meta data record that will also provide the
		  	# ID data for subsequent objects
		  	voteMeta <- as_data_frame(aRollCall[unlist(metaVote)])

		  	# Check casting of voting meta data
		  	voteMeta$date <- lubridate::ymd(voteMeta$date)
		  	for (i in names(metaVote[-c(3, 4)])) {
		  		voteMeta[[i]] <- as.numeric(voteMeta[[i]])
		  	}

		  	# Create vector of ID variables to attach to the individual voting
		  	# records
		  	voteID <- voteMeta[c(1:3)]

		  	# Parse the individual legislator votes
		  	indVotes <- plyr::llply(aRollCall[["votes"]], .fun = function(x) {
						plyr::llply(x, .fun = function(y) {
								ifelse(is.null(y) | nchar(y) == 0, NA, y)
							}) %>%
						dplyr::as_data_frame()
					}) %>% dplyr::bind_rows()

  	  	  	indVotes <- bind_cols(voteID[rep(seq_len(nrow(voteID)),
  				nrow(indVotes)), ], indVotes)

		  	# Check the casting of the individual votes
		  	indVotes$people_id <- as.numeric(indVotes$people_id)
		  	indVotes$vote_id <- as.numeric(indVotes$vote_id)

		  	# Determine what type of object to return
	  		if (dataframe == TRUE) {
	  			# Return individual data frame objects
	  			voteSummary <<- voteMeta; individualVotes <<- indVotes
	  		} else {
	  			voteData <- list(voteSummary = voteMeta, individualVotes = indVotes)
	  			return(voteData)
	  		}

		  })


#' @title LegiScan Parser Methods - parseSponsor
#' @description Method for parsing LegiScan API calls to getSponsor
#' @family LegiScan Parser Methods
#' @docType methods
#' @examples \dontrun{
#' # Create object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Get the master list for Mississippi
#' theMasterList <- masterList(myLegiScan, "MS")
#'
#' # Parse the XML/JSON formatted data into a data frame
#' cleanerMasterList <- parseMasterList(theMasterList)
#'
#' # Get A bill (bill_id is in the 3rd Column of the master list)
#' aBill <- bill(myLegiScan, cleanerMasterList[1, 3])
#'
#' # Parse the bill data
#' cleanerBill <- cleanBill(aBill, dataframe = FALSE, text = '')
#'
#' # Retrieve the Sponsor from the API call
#' aSponsor <- sponsor(myLegiScan, cleanerBill[["sponsors"]][["people_id"]])
#'
#' # Parse the sponsor response from LegiScan
#' legislator <- parseSponsor(aSponsor)
#'
#' }
#' @importFrom XML xmlRoot xmlParse xmlToList
#' @importFrom magrittr %>%
#' @export parseSponsor
#' @rdname parseSponsor-methods
#' @aliases parseSponsor,XMLDocumentContent,logical-method
setMethod(f = "parseSponsor",
		  signature("XMLDocumentContent", "logical"),
		  definition = function(theSponsor, dataframe = FALSE) {

		  	# Parse the API call response from the rollCall method
		  	aSponsor <- XML::xmlRoot(XML::xmlParse(theSponsor))[["person"]] %>%
		  				XML::xmlToList

		  	# Object storing names of metadata elements
	  		legislatorIDs <- list(people_id = "people_id", ftm_id = "ftm_id",
	  					role_id = "role_id", committee_id = "committee_id")

		  	# Pull out a meta data record that will also provide the
		  	# ID data for subsequent objects
		  	legislator <- as_data_frame(aSponsor[unlist(legislatorIDs)])

		  	# Check casting of voting meta data
		  	for (i in names(legislatorIDs)) {
		  		legislator[[i]] <- as.numeric(legislator[[i]])
		  	}

		  	# Determine what type of object to return
	  		if (dataframe == TRUE) {
	  			# Return data.frame object
	  			return(as.data.frame(legislator))
	  		} else {
	  			return(legislator)
	  		}

		  })

#' @family LegiScan Parser Methods
#' @docType methods
#' @importFrom RJSONIO isValidJSON fromJSON
#' @importFrom magrittr %>%
#' @export parseSponsor
#' @rdname parseSponsor-methods
#' @aliases parseSponsor,character,logical-method
setMethod(f = "parseSponsor",
		  signature("character", "logical"),
		  definition = function(theSponsor, dataframe = FALSE) {

		  	# Validate the JSON
		  	if (!(RJSONIO::isValidJSON(theSponsor))) {
		  		warning(cat(paste("JSON object is not valid",
		  				"will still attempt to parse", sep = "\n")))
		  	}

		  	# Parse the JSON data into a Data frame
	  		aSponsor <- RJSONIO::fromJSON(theSponsor, asText = TRUE,
	  									nullValue = NA)[["person"]]

		  	# Object storing names of metadata elements
	  		legislatorIDs <- list(people_id = "people_id", ftm_id = "ftm_id",
	  					role_id = "role_id", committee_id = "committee_id")

		  	# Pull out a meta data record that will also provide the
		  	# ID data for subsequent objects
		  	legislator <- as_data_frame(aSponsor[unlist(legislatorIDs)])

		  	# Check casting of voting meta data
		  	for (i in names(legislatorIDs)) {
		  		legislator[[i]] <- as.numeric(legislator[[i]])
		  	}

		  	# Determine what type of object to return
	  		if (dataframe == TRUE) {
	  			# Return data.frame object
	  			return(as.data.frame(legislator))
	  		} else {
	  			return(legislator)
	  		}

		  })
