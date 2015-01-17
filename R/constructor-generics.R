#' @title LegiScan Constructor Methods - setUrl
#' @description Generic constructor method for setting the urlBase slot of a
#' LegiScan class object
#' @param legiscan An object of class LegiScan
#' @rdname setUrl-methods
#' @family LegiScan Constructor Generics
#' @docType methods
setGeneric("setUrl",
		   def = function(legiscan) {
		   	standardGeneric("setUrl")
		   })

#' @title LegiScan Constructor Methods - setFormat
#' @description Generic constructor method for setting the format slot of a
#' LegiScan class object
#' @param legiscan An object of class LegiScan
#' @param json A logical indicating if the format slot should be JSON or XML
#' @rdname setFormat-methods
#' @family LegiScan Constructor Generics
#' @docType methods
setGeneric("setFormat",
		   def = function(legiscan, json) {
		   	standardGeneric("setFormat")
		   })

#' @title LegiScan Accessor Methods - setAPI
#' @description Generic method for setting the api slot of the LegiScan object
#' @param legiscan An object of class LegiScan
#' @param file A fully qualified file path/name where the LegiScan API key is located
#' @param key Argument for specifying the API key in the method call
#' @rdname setAPI-methods
#' @family LegiScan Constructor Generics
#' @docType methods
setGeneric("setAPI",
		   def = function(legiscan, file, key) {
		   	standardGeneric("setAPI")
		   })
