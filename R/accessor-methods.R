#' @title LegiScan Accessor Methods - getUrl
#' @description Accessor method for retrieving the urlBase slot of
#' a LegiScan class object
#' @return Returns the value stored in urlBase slot of the LegiScan object
#' @examples \dontrun{
#' # Create object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Retrieve the base URL from the LegiScan object
#' getUrl(myLegiScan)
#' }
#' @family LegiScan Accessor Methods
#' @export getUrl
#' @rdname getUrl-methods
#' @aliases getUrl,LegiScan-methods
setMethod(f = "getUrl",
		  signature("LegiScan"),
		  definition = function(legiscan) {
		  	return(legiscan@urlBase)
		  })

#' @title LegiScan Accessor Methods - getFormat
#' @description Accessor method for the format slot of the LegiScan class object
#' @return Returns the value stored in format slot of the LegiScan object
#' @examples \dontrun{
#' # Create object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Retrieve the
#' getFormat(myLegiScan)
#' }
#' @family LegiScan Accessor Methods
#' @export getFormat
#' @rdname getFormat-methods
#' @aliases getFormat,LegiScan-methods
setMethod(f = "getFormat",
		  signature("LegiScan"),
		  definition = function(legiscan) {
		  	return(legiscan@format)
		  })

#' @title LegiScan Accessor Methods - getAPI
#' @description Accessor method for the api slot of the LegiScan class object
#' @return Returns the users' api key stored in the LegiScan object
#' @examples \dontrun{
#' # Create an object of class LegiScan
#' myLegiScan <- legiscanR()
#'
#' # Retrieve the value of the stored API key
#' getAPI(myLegiScan)
#' }
#' @rdname getAPI-methods
#' @family LegiScan Accessor Methods
#' @export getAPI
#' @aliases getAPI,LegiScan-methods
setMethod(f = "getAPI",
		  signature("LegiScan"),
		  definition = function(legiscan) {
		  	return(legiscan@api)
		  })
