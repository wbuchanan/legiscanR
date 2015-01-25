#' @title LegiScan Archival Bill Data
#' @description
#' Parses and arranges XML output from LegiScan master data downloads and stores the
#' results in a list of data frames.
#' @param rawBill An XML file object from the bill subdirectory of the master download
#' @param fullText A character vector identifying where to retrieve the full text of the
#' bills (e.g., LegiScan site 'url' or official sites 'state_link')
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
#'
#' # When working with archive files, the process is fairly computationally intense due to
#' # the large volume of I/O operations.  However, this is a process that may only need to be
#' # run on an annual/semi-annual basis as a means of building data to load into a data warehouse.
#' # To parse the entire archive you can do:
#' library(doMC)
#' registerDoMC(cores = 8)
#' bills <- list()
#' for (i in names(files[["bills"]])) {
#' 		# Builds a large list of data frame objects
#' 		bills[[i]] <- plyr::llply(files[["bills"]][[i]], legiscanBill, .parallel = TRUE)
#' }
#'
#' }
#' @family Parsing and Cleaning LegiScan Data
#' @name legiscanBill
#' @importFrom XML xmlRoot xmlParse xmlToList xmlApply xpathApply xmlValue htmlParse
#' @importFrom lubridate ymd now
#' @importFrom plyr llply ldply
#' @importFrom dplyr bind_cols bind_rows as_data_frame full_join
#' @importFrom httr http_status GET
#' @export legiscanBill
legiscanBill <- function(rawBill, fullText = c("state_link", "url")) {

	# Create a timestamp when the function begins
	parseTime <- as.data.frame(lubridate::now()) %>% dplyr::as_data_frame()
	names(parseTime) <- "parse_timestamp"

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
  	billMeta <- as_data_frame(billData[unlist(meta)])

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

	billMeta <- dplyr::bind_cols(billMeta, parseTime[rep(seq_len(1),
			nrow(billMeta)), ])
	theData <- list(billMeta = billMeta)

	if (!is.null(billobject[["sponsors"]])) {

	  	# Build data frame objects for the bill sponsors,
	  	# history, progress, committees, and texts
		sponsors <- XML::xmlApply(billobject[["sponsors"]], XML::xmlToList) %>%
						plyr::llply(.fun = function(x) {
							plyr::llply(x, .fun = function(y) {
								ifelse(is.null(y), NA, y)
							}) %>%
							dplyr::as_data_frame()
						}) %>% dplyr::bind_rows()

  	  	sponsors <- dplyr::bind_cols(billID[rep(seq_len(nrow(billID)),
			  				nrow(sponsors)), ], sponsors,
							parseTime[rep(seq_len(1), nrow(sponsors)), ])

	  	sponsorIDs <- list("people_id", "role_id", "ftm_id",
  			  "sponsor_type_id", "sponsor_order")
		for (x in sponsorIDs) {
	  		sponsors[[x]] <- as.numeric(sponsors[[x]])
		}

	  	theData[["sponsors"]] <- sponsors

	} else {
		sponsors <- NULL
	}
	if (!is.null(billobject[["history"]])) {
		history <-  XML::xmlApply(billobject[["history"]], XML::xmlToList) %>%
					plyr::llply(.fun = function(x) {
						plyr::llply(x, .fun = function(y) {
							ifelse(is.null(y), NA, y)
						}) %>%
						dplyr::as_data_frame()
					}) %>% dplyr::bind_rows()

  	  	history <- dplyr::bind_cols(billID[rep(seq_len(nrow(billID)),
  					nrow(history)), ], history,
    					parseTime[rep(seq_len(1), nrow(history)), ])

	  	history$date <- lubridate::ymd(history$date)
	  	history$chamber_id <- as.numeric(history$chamber_id)
	  	history$importance <- as.numeric(history$importance)

		theData[["history"]] <- history

	} else {
		history <- NULL
	}
	if (!is.null(billobject[["committees"]])) {

		committees <- XML::xmlApply(billobject[["committee"]], XML::xmlToList) %>%
					plyr::llply(.fun = function(x) {
						ifelse(is.null(x), NA, x)
					}) %>% dplyr::as_data_frame() %>%
					dplyr::bind_rows()

  	  	committees <- dplyr::bind_cols(billID[rep(seq_len(nrow(billID)),
  					nrow(committees)), ], committees,
    					parseTime[rep(seq_len(1), nrow(committees)), ])

	  	theData[["committees"]] <- committees

	} else {
		committees <- NULL
	}
	if (!is.null(billobject[["texts"]])) {

		texts <- 	XML::xmlApply(billobject[["texts"]], XML::xmlToList) %>%
					plyr::llply(.fun = function(x) {
						plyr::llply(x, .fun = function(y) {
							ifelse(is.null(y), NA, y)
						}) %>%
						dplyr::as_data_frame()
					}) %>% dplyr::bind_rows()

		texts <- dplyr::bind_cols(billID[rep(seq_len(nrow(billID)),
  					nrow(texts)), ], texts,
    					parseTime[rep(seq_len(1), nrow(texts)), ])

		# Check for retreival value for full bill texts
	  	if (fullText %in% c("state_link", "url")) {
		  	# Generate a list of all of the state link elements
		    linkList <- as.list(texts[[fullText]])
	  		names(linkList) <- rep("full_bill_text", length(linkList))
	  		# Retrieve/Clean bill text if the URL request doesn't fail
	  		cleanText <- plyr::llply(linkList, .fun = function(link) {
		  			if (httr::http_status(
		  					httr::GET(link))$message == "success: (200) OK") {
		  					paste(XML::xpathApply(XML::htmlParse(link),
                                	"//p", XML::xmlValue), collapse = "\n")
		  			} else {
		  				"Could not retrieve full bill text from URL"
		  			}
	  			}) %>% dplyr::as_data_frame()

	  		cleanText <- dplyr::bind_cols(billID[rep(seq_len(nrow(billID)),
	  						nrow(cleanText)), ], cleanText)

	  		# Bind the full text to the rest of the test data
	  		texts <- dplyr::full_join(texts, cleanText)
	  	}

		texts$doc_id <- as.numeric(texts$doc_id)
	  	texts$date <- lubridate::ymd(texts$date)

	  	theData[["texts"]] <- texts

	} else {
		texts <- NULL
	}
	if (!is.null(billobject[["votes"]])) {
		votes <- XML::xmlApply(billobject[["progress"]], XML::xmlToList) %>%
				plyr::llply(.fun = function(x) {
					plyr::llply(x, .fun = function(y) {
							ifelse(is.null(y) | nchar(y) == 0, NA, y)
						}) %>%
					dplyr::as_data_frame()
				}) %>% dplyr::bind_rows()

  	  	votes <- dplyr::bind_cols(billID[rep(seq_len(nrow(billID)),
  						nrow(votes)), ], votes,
  	  					parseTime[rep(seq_len(1), nrow(votes)), ])
		votes$date <- lubridate::ymd(votes$date)

	  	theData[["votes"]] <- votes

	} else {
		votes <- NULL
	}
	if (!is.null(billobject[["progress"]])) {

		progress <- XML::xmlApply(billobject[["progress"]], XML::xmlToList) %>%
				plyr::llply(.fun = function(x) {
					plyr::llply(x, .fun = function(y) {
						ifelse(is.null(y), NA, y)
					}) %>%
					dplyr::as_data_frame()
				}) %>% dplyr::bind_rows()

		progress <- dplyr::bind_cols(billID[rep(seq_len(nrow(billID)),
  					nrow(progress)), ], progress,
    					parseTime[rep(seq_len(1), nrow(progress)), ])
	  	progress$date <- lubridate::ymd(progress$date)
	  	progress$event <- as.numeric(progress$event)

		theData[["progress"]] <- progress

	} else {
		progress <- NULL
	}

  	return(theData)
}
