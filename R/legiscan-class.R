#' LegiScan API Class object
#'
#' @description Defines a LegiScan class object which is passed to methods
#' to submit/retrieve data from calls to the LegiScan API and for parsing
#' data retrieve from the LegiScan API
#'
#' @rdname LegiScan-class
#' @aliases LegiScan
#' @docType class
#' @slot urlBase The base for the request URL (predefined in the class prototype)
#' @slot api The API key for the end user
#' @slot format Specifies if you have requested your API pulls in XML or JSON
#' @note
#'
#' The default behavior for the class constructor is to read the API key
#' from ~/.legiscanAPI.  Accessor methods are available to get/set the values
#' of the LegiScan object.
#'
#' @note
#'
#' The file format returned by the LegiScan API is set up when logging
#' into your LegiScan account.  The value provided by the class object is used
#' to select the appropriate parsing methods based on the object returned
#' from the API call
#'
#' @import methods
#' @export LegiScan
#' @exportClass LegiScan
LegiScan <- setClass(Class = "LegiScan",
					 representation(urlBase = "character", api = "character", format = "character"))


validLegiScan <- function(object) {
	# Test if the root URL is correct
	# Make sure the api slot is character and is more than 5 characters long
	# Make sure the format slot is one of the accepted formats
	# Object passed all validation criteria

	if (object@urlBase != "https://api.legiscan.com/?key=") FALSE
	else if (class(object@api) != "character" || nchar(object@api) < 5) FALSE
	else if (!(object@format %in% c("JSON", "XML"))) FALSE
	else TRUE
}


# sets validLegiScan function as the function to validate LegiScan class objects
setValidity("LegiScan", validLegiScan)

#' @title LegiScan constructor function
#' @rdname LegiScan-class
#' @param ... Should be passed arguments named by the slot names from getSlots("LegiScan")
#' @examples \donttest{
#' # Create new object with class LegiScan
#' x <- legiscanR()
#'
#' # Check class of the object x
#' class(x)
#' }
#' @export legiscanR
#' @return Returns the LegiScan object if the validation tests are passed
legiscanR <- function(...) {

	# Initialize new object of class LegiScan
	legiScanObject <- new("LegiScan", ...)

	# Validate object/return if valid object
	validObject(legiScanObject); return(legiScanObject)

}

#' LegiScan default construction method
#' @param .Object a LegiScan object to create
#' @param urlBase Root URL for LegiScan API calls
#' @param api If specified, will search for matching file otherwise will treat as character string
#' @param format XML or JSON API requests (set on the LegiScan website)
#' @rdname LegiScan-class
#' @aliases initialize,LegiScan-method
setMethod("initialize", "LegiScan",
		  function(.Object, urlBase, api, format) {

		  	# Set default values if nothing specified
		  	if (missing(urlBase) || is.null(urlBase) || is.na(urlBase)) {
		  		cat(paste("Missing, NULL, or NA urlBase argument;",
							"setting @urlBase = 'https://api.legiscan.com/?key='",
							sep = "\n"))
		  		.Object@urlBase <- "https://api.legiscan.com/?key="
		  	} else {
		  		.Object@urlBase <- urlBase
		  	}

		  	# Default to option(legiscanR = "API key") followed by checking default file
		  	# location, then testing whether the character string is a file, then entering
		  	# the character string as a valid value
		  	if (missing(api) || (is.null(api) && !is.null(getOption("legiscanR")))) {
		  		cat(paste("Missing, NULL, or NA api argument;",
					  "setting value from legiscanR option", sep = "\n"))
		  		.Object@api <- getOption("legiscanR")
		  	} else if (missing(api) || (is.null(api) && is.null(getOption("legiscanR")) &&
		  									file.exists("~/.legiscanAPI"))) {
		  		cat(paste("Missing, NULL, or NA api argument;",
						  "setting value from ~/.legiscanR", sep = "\n"))
		  		.Object@api <- scan("~/.legiscanAPI", what = "character")
		  	} else {
		  		if (file.exists(api)) {
		  			cat("LegiScan@api set to the file specified in the api parameter")
		  			.Object@api <- scan(api, what = "character")
		  		} else {
		  			cat("LegiScan@api set to the string specified in the api parameter")
		  			.Object@api <- api
		  		}
		  	}

		  	# Default format value if nothing supplied
		  	if (missing(format) || is.null(format) || is.na(format)) {
		  		.Object@format <- "JSON"
		  		cat(paste("Missing, NULL, or NA format argument;",
						  "set to 'JSON' as default", sep = "\n"))
		  	} else {
		  		.Object@format <- format
		  	}

		  	# Return the object
		  	return(.Object)

		  }
) # End of default object initialization
