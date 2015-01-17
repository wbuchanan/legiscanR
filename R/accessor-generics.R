#' @title LegiScan Accessor Methods - getUrl
#' @description Generic accessor method for retrieving the base of the request URL
#' @param legiscan An object of class LegiScan
#' @rdname getUrl-methods
#' @family LegiScan Accessor Generics
#' @docType methods
setGeneric("getUrl",
		   def = function(legiscan) {
		   	standardGeneric("getUrl")
		   })

#' @title LegiScan Accessor Methods - getFormat
#' @description Generic accessor method retrieving the format slot of a
#' LegiScan class object
#' @param legiscan An object of class LegiScan
#' @rdname getFormat-methods
#' @family LegiScan Accessor Generics
#' @docType methods
setGeneric("getFormat",
		   def = function(legiscan) {
		   	standardGeneric("getFormat")
		   })

#' @title LegiScan Accessor Methods - getAPI
#' @description Generic accessor method for retrieving the value from the api slot
#' @param legiscan An object of class LegiScan
#' @rdname getAPI-methods
#' @family LegiScan Accessor Generics
#' @docType methods
setGeneric("getAPI",
		   def = function(legiscan) {
		   	standardGeneric("getAPI")
		   })
