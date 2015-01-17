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
#' @import methods RCurl RJSONIO lubridate plyr dplyr
#' @export LegiScan
#' @exportClass LegiScan
LegiScan <- setClass(Class = "LegiScan",

		 # Slot representations
		 # representation = representation(urlBase = "character",
		 # 							api = "character", format = "character"),

		 # Slot definitions for the class
		 slots = list(urlBase = "character",
		 			 api = "character", format = "character"),

		 # Default values for the slots
		 prototype = list( urlBase = "http://api.legiscan.com/?key=",
		 				  api = scan("~/.legiscanAPI", what = "character"),
		 				  format = "JSON"),

		 # Ensure the only values for the format slot are XML and JSON
		 validity = function(object) {
		 	if (!(object@format %in% c("XML", "JSON"))) {
		 		return("Warning: invalid file format specified when creating LegiScan object")
		 	}
		 }

) # End LegiScan class definition


#' @title LegiScan constructor function
#' @rdname LegiScan-class
#' @param .Object Object of class LegiScan
#' @param urlBase Character string identifying root URL for API request
#' @param api LegiScan API key
#' @param format Character string indicating if requested data are in JSON or XML format
#' @param ... Additional arguments (reserved for future use)
#' @examples \donttest{
#' # Create new object with class LegiScan
#' x <- legiscanR()
#'
#' # Check class of the object x
#' class(x)
#' }
#' @docType class
#' @export legiscanR
legiscanR <- function(urlBase = "http://api.legiscan.com/?key=",
					  api = "ABCDEFGHIJKLMNOP",
					  format = "JSON", ...) {

	LegiScan(urlBase = urlBase, api = api, format = format)

}

#' LegiScan default construction method
#' @rdname LegiScan-class
#' @aliases initialize,LegiScan,missing,missing,missing,...-method
setMethod("initialize", "LegiScan",
		  function(.Object, urlBase = "missing",
		  		 api = "missing", format = "missing", ...) {

		  	# Set default values
		  	.Object@urlBase <- "http://api.legiscan.com/?key="

		  	# Check for presence of .legiscanAPI file in home directory
		  	if ((".legiscanAPI" %in% list.files(all.files = TRUE, path = "~/"))) {

		  		# Set the slot value based on the value in the file
		  		.Object@api <- scan("~/.legiscanAPI", what = "character")

		  	} else {

		  		# Issue error message if file is missing
		  		stop("Error: .legiscanAPI not found in ~/")

		  	}

		  	# Default to JSON
		  	.Object@format <- "JSON"

		  	# Validate the LegiScan object
		  	validObject(.Object)

		  	# Return the object
		  	return(.Object)

		  }
) # End of default object initialization

#' LegiScan construction method for user specified format
#' @rdname LegiScan-class
#' @aliases initialize,LegiScan,character,character,character,...-method
setMethod("initialize", "LegiScan",
		  function(.Object, urlBase = "character",
		  		 api = "character", format = "character", ...) {

		  	# Set default values
		  	.Object@urlBase <- "http://api.legiscan.com/?key="

		  	# Check for presence of .legiscanAPI file in home directory
		  	if ((".legiscanAPI" %in% list.files(all.files = TRUE, path = "~/"))) {

		  		# Set the slot value based on the value in the file
		  		.Object@api <- scan("~/.legiscanAPI", what = "character")

		  	} else {

		  		# Issue error message if file is missing
		  		stop("Error: .legiscanAPI not found in ~/")

		  	}

		  	# Check format value
		  	if (!(toupper(format) %in% c("XML", "JSON"))) {

		  		# Display warning message
		  		warning("Error: format value can only be XML or JSON; Setting default format")

		  		# Set format to JSON
		  		.Object@format <- "JSON"

		  	} else {

		  		# Set format to user specified value
		  		.Object@format <- format

		  	}

		  	# Validate the LegiScan object
		  	validObject(.Object)

		  	# Return the object
		  	return(.Object)

		  }

) # End of default object initialization





#' LegiScan construction method for user specified format
#' @rdname LegiScan-class
#' @aliases initialize,LegiScan,missing,character,character,...-method
setMethod("initialize", "LegiScan",
		  function(.Object, urlBase = "missing",
		  		 api = "character", format = "character", ...) {

		  	# Set default values
		  	.Object@urlBase <- "http://api.legiscan.com/?key="

		  	# Check for presence of .legiscanAPI file in home directory
		  	if ((".legiscanAPI" %in% list.files(all.files = TRUE, path = "~/"))) {

		  		# Set the slot value based on the value in the file
		  		.Object@api <- scan("~/.legiscanAPI", what = "character")

		  	} else {

		  		# Issue error message if file is missing
		  		stop("Error: .legiscanAPI not found in ~/")

		  	}

		  	# Check format value
		  	if (!(toupper(format) %in% c("XML", "JSON"))) {

		  		# Display warning message
		  		warning("Error: format value can only be XML or JSON; Setting default format")

		  		# Set format to JSON
		  		.Object@format <- "JSON"

		  	} else {

		  		# Set format to user specified value
		  		.Object@format <- format

		  	}

		  	# Validate the LegiScan object
		  	validObject(.Object)

		  	# Return the object
		  	return(.Object)

		  }

) # End of default object initialization

#' LegiScan construction method with user specified api key value/path
#' @rdname LegiScan-class
#' @note
#' The api value should either be the file path to the .legiscanAPI file
#' or be the api key itself
#' @aliases initialize,LegiScan,missing,character,missing,...-method
setMethod("initialize", "LegiScan",
		  function(.Object, urlBase = "missing",
		  		 api = "character", format = "missing", ...) {

		  	# Set default values
		  	.Object@urlBase <- "http://api.legiscan.com/?key="

		  	# Check for presence of .legiscanAPI file in home directory
		  	if ((".legiscanAPI" %in% list.files(all.files = TRUE, path = api))) {

		  		# Check/append ending backslash in filepath
		  		if (grepl(".*/$", api) == FALSE) api <- paste0(api, "/")

		  		# Set the slot value based on the value in the file
		  		.Object@api <- scan(paste0(api, ".legiscanAPI"), what = "character")

		  	} else {

		  		# Set the api slot to what is assumed to be the api key
		  		.Object@api <- api

		  	}

		  	# Set format to JSON
		  	f.Object@ormat <- "JSON"

		  	# Validate the LegiScan object
		  	validObject(.Object)

		  	# Return the object
		  	return(.Object)

		  }

) # End of default object initialization

#' LegiScan construction method with user specified api and format values
#' @rdname LegiScan-class
#' @note
#' The api value should either be the file path to the .legiscanAPI file
#' or be the api key itself
#' @aliases initialize,LegiScan,missing,missing,character,...-method
setMethod("initialize", "LegiScan",
		  function(.Object, urlBase = "missing",
		  		 api = "missing", format = "character", ...) {

		  	# Set default values
		  	.Object@urlBase <- "http://api.legiscan.com/?key="

		  	# Check for presence of .legiscanAPI file in home directory
		  	if ((".legiscanAPI" %in% list.files(all.files = TRUE, path = "~/"))) {

		  		# Set the slot value based on the value in the file
		  		.Object@api <- scan("~/.legiscanAPI", what = "character")

		  	} else {

		  		# Issue error message if file is missing
		  		stop("Error: .legiscanAPI not found in ~/")

		  	}

		  	# Check format value
		  	if (!(toupper(format) %in% c("XML", "JSON"))) {

		  		# Display warning message
		  		warning("Error: format value can only be XML or JSON; Setting default format")

		  		# Set format to JSON
		  		.Object@format <- "JSON"

		  	} else {

		  		# Set format to user specified value
		  		.Object@format <- format

		  	}

		  	# Validate the LegiScan object
		  	validObject(.Object)

		  	# Return the object
		  	return(.Object)

		  }

) # End of default object initialization

