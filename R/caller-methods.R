#' @title LegiScan Caller Methods - stateList
#' @description A method to retrieve the list of States the user is authorized
#' to access from LegiScan
#' @return Returns State List from LegiScan API method getStateList
#' @docType methods
#' @examples \dontrun{
#' # Create new LegiScan object
#' myLegiScan <- legiscanR()
#'
#' # Retrieve the State list available from the LegiScan API
#' stateList(myLegiScan)
#' }
#' @family LegiScan API Caller Methods
#' @importFrom RCurl getURL
#' @export stateList
#' @rdname stateList-methods
#' @aliases stateList,LegiScan-method
setMethod(f = "stateList",
		  signature("LegiScan"),
		  definition = function(legiscan) {

		  	# Construct the URL for the API Call
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan), "&op=getStateList")

		  	# Use getURL from RCurl to make the API call and Assign appropriate class
		  	statelist <- getURL(requestURL); statelist <- xjformat(statelist)

		  	# Put calling URL and results into single list object
		  	results <- list(getStateList = statelist, url = requestURL)

		  	# Return the data object
		  	return(results)

		  })

#' @title LegiScan Caller Methods - sessionList
#' @description Generic method for the LegiScan getSessionList API call
#' @family LegiScan API Caller Methods
#' @docType methods
#' @examples \dontrun{
#' # Create object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Get the session list for Mississippi
#' sessionList(myLegiScan, "MS")
#' }
#' @importFrom RCurl getURL
#' @export sessionList
#' @rdname sessionList-methods
#' @aliases sessionList,LegiScan,character-method
setMethod(f = "sessionList",
		  signature("LegiScan", "character"),
		  definition = function(legiscan, state) {

		  	# Validate the value of the State variable
		  	state <- checkState(state)

		  	# Construct the API request URL
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan),
		  						 "&op=getSessionList", state)

		  	# Create sessionlist object with result of API call
		  	sessionlist <- getURL(requestURL); sessionlist <- xjformat(sessionlist)

		  	# Put calling URL and results into single list object
		  	results <- list(getSessionList = sessionlist, url = requestURL)

		  	# Return the data to the user
		  	return(results)

		  })

#' @title LegiScan Caller Methods - masterList
#' @description
#' Method to retrieve master list of bills introduced in the current legislative
#' session.  The response also provides a checksum for tracking changes to the
#' text of bills, as well as bill_id and billnumber identifiers.
#' @family LegiScan API Caller Methods
#' @docType methods
#' @examples \dontrun{
#' # Construct LegiScan Class Object
#' myLegiScan <- legiscanR(format = "XML")
#'
#' # Request the masterlist for Little Rhody
#' masterList(myLegiScan, "RI")
#' }
#' @details
#' The getMasterList API call has two distinct methods available.  The method
#' which LegiScan recommends, is to make master list requests using the state
#' abbreviation.  However, a second method is available which will select the
#' data based on the session id number assigned by LegiScan.
#' @importFrom RCurl getURL
#' @export masterList
#' @rdname masterList-methods
#' @aliases masterList,LegiScan,character,missing-method
setMethod(f = "masterList",
		  signature = c("LegiScan", "character", "missing"),
		  definition = function(legiscan, state, id) {

	  		# Validate the value of the State variable
	  		state <- checkState(state)

		  	# Build the URL for the API call
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan),
		  						 "&op=getMasterList", state)

		  	# Submit the request to the API
		  	masterlist <- RCurl::getURL(requestURL)
	  		masterlist <- xjformat(masterlist)

	  		# Put calling URL and results into single list object
	  		results <- list(getMasterList = masterlist, url = requestURL)

	  		# Return the data/results to the user
		  	return(results)

		})


#' @title LegiScan Caller Methods - masterList
#' @description
#' Method to retrieve master list of bills introduced in the current legislative
#' session.  The response also provides a checksum for tracking changes to the
#' text of bills, as well as bill_id and billnumber identifiers.
#' @family LegiScan API Caller Methods
#' @importFrom RCurl getURL
#' @export masterList
#' @rdname masterList-methods
#' @aliases masterList,LegiScan,missing,missing-method
setMethod(f = "masterList",
		  signature = c("LegiScan", "missing", "missing"),
		  definition = function(legiscan, state, id) {

		  	# Validate class of id variable
		  	if (!is.numeric(id)) {

		  		# Error out of non-numeric id value
		  		stop("Error: The sessionID argument was non-numeric")

		  	} else {

		  		# create session_id parameter and ID string
		  		session_id <- paste0("&id=", id)

		  	}

		  	# Build the URL for the API call
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan),
		  						 "&op=getMasterList", session_id)

			# Make the API call
		  	masterlist <- RCurl::getURL(requestURL)
			masterlist <- xjformat(masterlist)

			# Put calling URL and results into single list object
			results <- list(getMasterList = masterlist, url = requestURL)

			# Return the data from the API Call
			return(results)

		})

#' @title LegiScan Caller Methods - bill
#' @description
#' Detailed data on specific bill including - but not limited to: sponsors,
#' full text (doc_id) identifier, and voting records identifiers
#' @family LegiScan API Caller Methods
#' @importFrom RCurl getURL
#' @export bill
#' @rdname bill-methods
#' @aliases bill,LegiScan,numeric,missing,missing-method
setMethod(f = "bill",
		  signature("LegiScan", "numeric", "missing", "missing"),
		  definition = function(legiscan, id, state, billnumber) {

		  	# Validate the class of the id object
		  	if (!is.numeric(id)) {

		  		# Error out if non-numeric
		  		stop("Error: The sessionID argument was non-numeric")

		  	} else {

		  		# create session_id parameter and ID string
		  		bill_id <- paste0("&id=", id)

		  	}

		  	# Construction API Request URL
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan),
		  						 "&op=getBill", bill_id)

		  	# Make call to the API and assign the correct class
		  	theBill <- RCurl::getURL(requestURL); theBill <- xjformat(theBill)

		  	# Put calling URL and results into single list object
		  	results <- list(getBill = theBill, url = requestURL)

		  	# Return the data retrieved from the API call
		  	return(results)

		  })


#' @title LegiScan Caller Methods - bill
#' @description
#' Detailed data on specific bill including - but not limited to: sponsors,
#' full text (doc_id) identifier, and voting records identifiers
#' @family LegiScan API Caller Methods
#' @importFrom RCurl getURL
#' @export bill
#' @rdname bill-methods
#' @aliases bill,LegiScan,missing,character,numeric-method
setMethod(f = "bill",
		  signature("LegiScan", "missing", "character", "numeric"),
		  definition = function(legiscan, id, state, billnumber) {

		  	# Validate value of id parameter
		  	if (!is.numeric(id)) {

		  		# If non-numeric Error out of program
		  		stop("Error: The sessionID argument was non-numeric")

		  	} else {

		  		# create session_id parameter and ID string
		  		billnumber <- paste0("&bill=", id)

		  	}

		  	# Validate the value of the State variable
	  		state <- checkState(state)

		  	# Build the URL for the API call
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan),
		  						 "&op=getBill", state, billnumber)

		  	# Send request to the API
		  	theBill <- RCurl::getURL(requestURL); theBill <- xjformat(theBill)

		  	# Put calling URL and results into single list object
		  	results <- list(getBill = theBill, url = requestURL)

		  	# Return the data retrieved from the API request
		  	return(results)

		  })

#' @title LegiScan Caller Methods - billText
#' @description
#' Retrieves the full bill text, metadata (e.g., draft revision number, etc...),
#' and MIME type for the text.  The text is encoded in base64.
#' @family LegiScan API Caller Methods
#' @importFrom RCurl getURL
#' @export billText
#' @rdname billText-methods
#' @aliases billText,LegiScan,numeric-method
setMethod(f = "billText",
		  signature("LegiScan", "numeric"),
		  definition = function(legiscan, id) {

		  	# Ensure id parameter is numeric
		  	if (!is.numeric(id)) {

		  		# Error out on non-numeric ID value
		  		stop("Error: The doc_id supplied to the id argument was non-numeric")

		  	} else {

		  		# create doc_id parameter and ID string
		  		doc_id <- paste0("&id=", id)

		  	}

		  	# Build URL for API Call
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan),
		  						 "&op=getBillText", doc_id)

		  	# Request data from the API
		  	theBillText <- RCurl::getURL(requestURL)
		  	theBillText <- xjformat(theBillText)

		  	# Put calling URL and results into single list object
		  	results <- list(getBillText = theBillText, url = requestURL)

		  	# Return the data from the API call
		  	return(results)

		  })

#' @title LegiScan Caller Methods - amendment
#' @description Generic method for the LegiScan getAmendment API call
#' @family LegiScan API Caller Methods
#' @importFrom RCurl getURL
#' @export amendment
#' @rdname amendment-methods
#' @aliases amendment,LegiScan,numeric-method
setMethod(f = "amendment",
		  signature("LegiScan", "numeric"),
		  definition = function(legiscan, id) {

		  	# Validate class of the ID parameter
		  	if (!is.numeric(id)) {

		  		# Error out for non-numeric IDs
		  		stop("Error: The amendment_id supplied to the id argument was non-numeric")

		  	} else {

		  		# create amendment_id parameter and ID string
		  		amendment_id <- paste0("&id=", id)

		  	}

		  	# Build the requesting URL
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan),
		  						 "&op=getAmendment", amendment_id)

		  	# Make API call
		  	theAmendment <- RCurl::getURL(requestURL)
		  	theAmendment <- xjformat(theAmendment)

		  	# Put calling URL and results into single list object
		  	results <- list(getAmendment = theAmendment, url = requestURL)

		  	# Return the retrieved data to the user
		  	return(results)

		  })

#' @title LegiScan Caller Methods - supplement
#' @description Generic method for the LegiScan getSupplement API call
#' @family LegiScan API Caller Methods
#' @importFrom RCurl getURL
#' @export supplement
#' @rdname supplement-methods
#' @aliases supplement,LegiScan,numeric-method
setMethod(f = "supplement",
		  signature("LegiScan", "numeric"),
		  definition = function(legiscan, id) {

		  	# Validate class of the ID parameter
		  	if (!is.numeric(id)) {

		  		# Error out for non-numeric IDs
		  		stop("Error: The supplement_id supplied to the id argument was non-numeric")

		  	} else {

		  		# create amendment_id parameter and ID string
		  		supplement_id <- paste0("&id=", id)

		  	}

		  	# Build URL to request the data from the API
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan),
		  						 "&op=getSupplement", supplement_id)

		  	# Make the call to the API
		  	theSupplement <- RCurl::getURL(requestURL)
		  	theSupplement <- xjformat(theSupplement)

		  	# Put calling URL and results into single list object
		  	results <- list(getSupplement = theSupplement, url = requestURL)

		  	# Return the data to the user
		  	return(results)

		  })

#' @title LegiScan Caller Methods - rollCall
#' @description Generic method for the LegiScan getRollcall API call
#' @family LegiScan API Caller Methods
#' @importFrom RCurl getURL
#' @export rollCall
#' @rdname rollCall-methods
#' @aliases rollCall,LegiScan,numeric-method
setMethod(f = "rollCall",
		  signature("LegiScan", "numeric"),
		  definition = function(legiscan, id) {

		  	# Validate class of the ID parameter
		  	if (!is.numeric(id)) {

		  		# Error out for non-numeric IDs
		  		stop("Error: The roll_call_id supplied to the id argument was non-numeric")

		  	} else {

		  		# create roll_call_id parameter and ID string
		  		roll_call_id <- paste0("&id=", id)

		  	}

			# Build the URL for the API request
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan),
		  						 "&op=getRollcall", roll_call_id)

			# Makle the request to the API
			roll_call <- RCurl::getURL(requestURL)
			roll_call <- xjformat(roll_call)

			# Put calling URL and results into single list object
			results <- list(getRollCall = roll_call, url = requestURL)

			# Return the results to the user
		  	return(results)

		  })

#' @title LegiScan Caller Methods - sponsor
#' @description Generic method for the LegiScan sponsor API call
#' @family LegiScan API Caller Methods
#' @importFrom RCurl getURL
#' @export sponsor
#' @rdname sponsor-methods
#' @aliases sponsor,LegiScan,numeric-method
setMethod(f = "sponsor",
		  signature("LegiScan", "numeric"),
		  definition = function(legiscan, id) {

		  	# Check the id parameter
		  	if (!is.numeric(id)) {

		  		# Error out for invalid parameter value
		  		stop("Error: The people_id supplied to the id argument was non-numeric")

		  	} else {

		  		# create sponsors_id parameter and ID string
		  		people_id <- paste0("&id=", id)
		  	}

		  	# Construct the URL for the API call
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan),
		  						 "&op=getSponsor", people_id)

		  	# Use RCurl to make the API call
		  	people <- RCurl::getURL(requestURL); people <- xjformat(people)

		  	# Put calling URL and results into single list object
		  	results <- list(getSponsor = people, url = requestURL)

		  	# Return the data
		  	return(results)

		  })

#' @title LegiScan Caller Methods - legisearch
#' @description Generic method for the LegiScan search API call
#' @family LegiScan API Caller Methods
#' @details
#' The accepted values the the year parameter are described in more detail here.
#' \itemize{
#' 		\item{"year = 1"}{All Years of Data Available}
#' 		\item{"year = 2"}{Current Year of Data Only (default)}
#' 		\item{"year = 3"}{Recent Years of Data Available}
#' 		\item{"year = 4"}{Prior Years of Data Available}
#' 		\item{"year >= 1900"}{Exact Year Only}
#' }
#' @importFrom RCurl getURL
#' @export legisearch
#' @rdname legisearch-methods
#' @aliases legisearch,LegiScan,character,numeric,missing,missing,missing-method
setMethod(f = "legisearch",
		  signature("LegiScan", "character", "numeric",
		  		  "missing", "missing", "missing"),
		  definition = function(legiscan, state, bill, query, year, page) {

		  	# Validate class of the bill parameter
		  	if (!is.numeric(bill)) {

		  		# Error out for non-numeric IDs
		  		stop("Error: The billnumber supplied to the bill argument was non-numeric")

		  	} else {

		  		# create billnumber parameter and ID string
		  		billnumber <- paste0("&bill=", bill)

		  	}

	  		# Validate the value of the State variable
	  		state <- checkState(state)

		  	# Construct the URL for the API call
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan),
		  						 "&op=search", state, billnumber)

		  	# Submit the request to the API
		  	queryResults <- RCurl::getURL(requestURL)
		  	queryResults <- xjformat(queryResults)

		  	# Put calling URL and results into single list object
		  	results <- list(search = queryResults, url = requestURL)

		  	# Return the request from the request
		  	return(results)

		  })

#' @title LegiScan Caller Methods - legisearch
#' @description Generic method for the LegiScan search API call
#' @family LegiScan API Caller Methods
#' @details
#' The accepted values the the year parameter are described in more detail here.
#' \itemize{
#' 		\item{"year = 1"}{All Years of Data Available}
#' 		\item{"year = 2"}{Current Year of Data Only (default)}
#' 		\item{"year = 3"}{Recent Years of Data Available}
#' 		\item{"year = 4"}{Prior Years of Data Available}
#' 		\item{"year >= 1900"}{Exact Year Only}
#' }
#' @importFrom RCurl getURL
#' @export legisearch
#' @rdname legisearch-methods
#' @aliases legisearch,LegiScan,character,missing,character,missing,missing-method
setMethod(f = "legisearch",
		  signature("LegiScan", "character", "missing",
		  		  "character", "missing", "missing"),
		  definition = function(legiscan, state, bill, query, year, page) {

		  	# Validate the value of the State variable
		  	state <- checkState(state)

		  	# Encode the query string as an URL string
		  	query <- URLencode(query)

		  	# Construct the URL with the query
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan),
		  						 "&op=search", state, query)

		  	# Submit the query to the API
		  	queryResults <- RCurl::getURL(requestURL)
		  	queryResults <- xjformat(queryResults)

		  	# Put calling URL and results into single list object
		  	results <- list(search = queryResults, url = requestURL)

		  	# Return the request from the request
		  	return(results)

		  })

#' @title LegiScan Caller Methods - legisearch
#' @description Generic method for the LegiScan search API call
#' @family LegiScan API Caller Methods
#' @details
#' The accepted values the the year parameter are described in more detail here.
#' \itemize{
#' 		\item{"year = 1"}{All Years of Data Available}
#' 		\item{"year = 2"}{Current Year of Data Only (default)}
#' 		\item{"year = 3"}{Recent Years of Data Available}
#' 		\item{"year = 4"}{Prior Years of Data Available}
#' 		\item{"year >= 1900"}{Exact Year Only}
#' }
#' @importFrom RCurl getURL
#' @export legisearch
#' @rdname legisearch-methods
#' @aliases legisearch,LegiScan,character,missing,character,numeric,missing-method
setMethod(f = "legisearch",
		  signature("LegiScan", "character", "missing",
		  		  "character", "numeric", "missing"),
		  definition = function(legiscan, state, bill, query, year, page) {

		  	# Validate the value of the State variable
		  	state <- checkState(state)

		  	# Encode the query string as URL
		  	query <- URLencode(query)

		  	# Validate the year values
		  	year <- checkYear(year)

		  	# Construct the URL including the year parameter
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan),
		  						 "&op=search", state, query, year)

		  	# Send the API Call
		  	queryResults <- RCurl::getURL(requestURL)
		  	queryResults <- xjformat(queryResults)

		  	# Put calling URL and results into single list object
		  	results <- list(search = queryResults, url = requestURL)

		  	# Return the request from the request
		  	return(results)

		  })


#' @title LegiScan Caller Methods - legisearch
#' @description Generic method for the LegiScan search API call
#' @family LegiScan API Caller Methods
#' @details
#' The accepted values the the year parameter are described in more detail here.
#' \itemize{
#' 		\item{"year = 1"}{All Years of Data Available}
#' 		\item{"year = 2"}{Current Year of Data Only (default)}
#' 		\item{"year = 3"}{Recent Years of Data Available}
#' 		\item{"year = 4"}{Prior Years of Data Available}
#' 		\item{"year >= 1900"}{Exact Year Only}
#' }
#' @importFrom RCurl getURL
#' @export legisearch
#' @rdname legisearch-methods
#' @aliases legisearch,LegiScan,character,missing,character,missing,numeric-method
setMethod(f = "legisearch",
		  signature("LegiScan", "character", "missing",
		  		  "character", "missing", "numeric"),
		  definition = function(legiscan, state, bill, query, year, page) {

		  	# Validate the value of the State variable
		  	state <- checkState(state)

		  	# Encode the query as URL string
		  	query <- URLencode(query)
		  	queryResults <- xjformat(queryResults)

		  	# Validate the page return values
		  	page <- checkYear(page)

			# Build the URL for the API call
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan),
		  						 "&op=search", state, query, page)

			# Make the call to the API
			queryResults <- RCurl::getURL(requestURL)

			# Put calling URL and results into single list object
			results <- list(search = queryResults, url = requestURL)

			# Return the request from the request
			return(results)

		  })


#' @title LegiScan Caller Methods - legisearch
#' @description Generic method for the LegiScan search API call
#' @family LegiScan API Caller Methods
#' @details
#' The accepted values the the year parameter are described in more detail here.
#' \itemize{
#' 		\item{"year = 1"}{All Years of Data Available}
#' 		\item{"year = 2"}{Current Year of Data Only (default)}
#' 		\item{"year = 3"}{Recent Years of Data Available}
#' 		\item{"year = 4"}{Prior Years of Data Available}
#' 		\item{"year >= 1900"}{Exact Year Only}
#' }
#' @importFrom RCurl getURL
#' @export legisearch
#' @rdname legisearch-methods
#' @aliases legisearch,LegiScan,character,missing,character,numeric,numeric-method
setMethod(f = "legisearch",

		  # Signature for case where all parameters except the bill number are
		  # non-null
		  signature("LegiScan", "character", "missing", "character", "numeric", "numeric"),

		  # Define the function
		  definition = function(legiscan, state, bill, query, year, page) {

	  		# Validate the value of the State variable
	  		state <- checkState(state)

		  	# Encode the query as an URL string
		  	query <- URLencode(query)
	  		queryResults <- xjformat(queryResults)

		  	# Validate the year argument
		  	year <- checkYear(year)

		  	# Validate the page return values
		  	page <- checkYear(page)

		  	# Create the API request URL from the components
		  	requestURL <- paste0(getUrl(legiscan), getAPI(legiscan),
		  						 "&op=search", state, query, year, page)

		  	# Make the call to the API
		  	queryResults <- RCurl::getURL(requestURL)
	  		queryResults <- xjformat(queryResults)

	  		# Put calling URL and results into single list object
	  		results <- list(search = queryResults, url = requestURL)

	  		# Return the request from the request
	  		return(results)

		  })
