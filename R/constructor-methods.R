#' @title LegiScan Constructor Methods - setUrl
#' @description Constructor method used for setting the urlBase slot
#' of a LegiScan class object
#' @return Returns the LegiScan class object with prespecified url string
#' @examples \dontrun{
#' # Create object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Set the URL slot of the LegiScan object
#' setUrl(myLegiScan)
#' }
#' @note The URL string set by this method is "http://api.legiscan.com/?key="
#' @family LegiScan Constructor Methods
#' @export setUrl
#' @aliases setUrl,LegiScan-method
#' @rdname setUrl-methods
setMethod(f = "setUrl",
		  signature("LegiScan"),
		  definition = function(legiscan) {
		  	legiscan@urlBase = "http://api.legiscan.com/?key="
		  	return(legiscan)
		  })

#' @title LegiScan Constructor Methods - setFormat
#' @description Constructor method for setting the format slot of a
#' LegiScan class object
#' @return Returns the LegiScan class object with user specified format slot
#' @examples \dontrun{
#' # Create an object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Sets format slot to JSON
#' setFormat(myLegiScan, TRUE)
#'
#' # Sets format slot to XML
#' setFormat(myLegiScan, FALSE)
#' }
#' @family LegiScan Constructor Methods
#' @export setFormat
#' @aliases setFormat,LegiScan,logical-method
#' @rdname setFormat-methods
setMethod(f = "setFormat",

		  # Class specification for the arguments
		  signature("LegiScan", "logical"),

		  # Function definition for the method
		  definition = function(legiscan, json) {

		  	# json is set as TRUE
		  	if (json == TRUE) legiscan@format = "JSON"
		  	else legiscan@format = "XML"

		  	# Return the object
		  	return(legiscan)

		  }) # End of Method definition

#' @title LegiScan Constructor Methods - setAPI
#' @description Constructor method for modifying/setting the api slot
#' of a LegiScan class object
#' @return Returns the LegiScan class object with user specified api slot
#' @examples \dontrun{
#' # Create object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Set the API key to the value of the .myApiKey file on the desktop
#' setAPI(myLegiScan, file = '~/Desktop/.myApiKey')
#'
#' # Set the API key to a value specified in the method call
#' setAPI(myLegiScan, key = 'abcd1234')
#'
#' # Set the API key based on the default file search location
#' setAPI(myLegiScan)
#' }
#' @note The default file search location is in the user's home directory in
#' a file called .legiscanAPI.
#' @family LegiScan Constructor Methods
#' @export setAPI
#' @aliases setAPI,LegiScan,missing,missing-method
#' @rdname setAPI-methods
setMethod(f = "setAPI",
		  signature("LegiScan", "missing", "missing"),
		  definition = function(legiscan, file = NULL, key = NULL) {
		  	legiscan@api = scan("~/.legiscanAPI", what = "character")
		  })

#' @family LegiScan Constructor Methods
#' @export setAPI
#' @aliases setAPI,LegiScan,missing,character-method
#' @rdname setAPI-methods
setMethod(f = "setAPI",
		  signature("LegiScan", "missing", "character"),
		  definition = function(legiscan, file = NULL, key) {
		  	if (length(key) == 1) {
		  		legiscan@api = key
		  	} else {
		  		stop("Error: Length of key vector != 1")
		  	}
		  })

#' @family LegiScan Constructor Methods
#' @export setAPI
#' @aliases setAPI,LegiScan,character,missing-method
#' @rdname setAPI-methods
setMethod(f = "setAPI",
		  signature("LegiScan", "character", "missing"),
		  definition = function(legiscan, file, key = NULL) {
		  	if (length(file) == 1) {
		  		legiscan@api = scan(file, what = "character")
		  	} else {
		  		stop("Error: Length of file vector != 1")
		  	}
		  })



