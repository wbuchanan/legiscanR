#' @title LegiScan Caller Methods - stateList
#' @description Generic method for the LegiScan getStateList API call
#' @param legiscan An object of class LegiScan
#' @docType methods
#' @rdname stateList-methods
#' @family LegiScan API Caller Generics
#' @export stateList
setGeneric("stateList",
		   def = function(legiscan) {
		   	standardGeneric("stateList")
		   })

#' @title LegiScan Caller Methods - sessionList
#' @description Generic method for the LegiScan getSessionList API call
#' @param legiscan An object of class LegiScan
#' @param state A two character US State abbreviation (validated against the
#' state.abb dataset in R)
#' @rdname sessionList-methods
#' @family LegiScan API Caller Generics
#' @docType methods
#' @export sessionList
setGeneric("sessionList",
		   def = function(legiscan, state) {
		   	standardGeneric("sessionList")
		   })

#' @title LegiScan Caller Methods - masterList
#' @description Generic method for the LegiScan getMasterList API call
#' @param legiscan An object of class LegiScan
#' @param state A two character US State abbreviation (validated against the
#' state.abb dataset in R)
#' @param id A LegiScan session_id value
#' @rdname masterList-methods
#' @family LegiScan API Caller Generics
#' @docType methods
#' @export masterList
setGeneric("masterList",
		   def = function(legiscan, state, id) {
		   	standardGeneric("masterList")
		   })

#' @title LegiScan Caller Methods - bill
#' @description Generic method for the LegiScan getBill API call
#' @param legiscan An object of class LegiScan
#' @param id The LegiScan bill_id or LegiScan billnumber when called with a
#' valid state abbreviation
#' @param state A two character US State abbreviation (validated against the
#' state.abb dataset in R)
#' @param billnumber A LegiScan billnumber (can be retrieved in call to masterList)
#' @rdname bill-methods
#' @family LegiScan API Caller Generics
#' @docType methods
#' @export bill
setGeneric("bill",
		   def = function(legiscan, id, state, billnumber) {
		   	standardGeneric("bill")
		   })

#' @title LegiScan Caller Methods - billText
#' @description Generic method for the LegiScan getBillText API call
#' @param legiscan An object of class LegiScan
#' @param id A LegiScan doc_id value
#' @rdname billText-methods
#' @family LegiScan API Caller Generics
#' @docType methods
#' @export billText
setGeneric("billText",
		   def = function(legiscan, id) {
		   	standardGeneric("billText")
		   })

#' @title LegiScan Caller Methods - amendment
#' @description Generic method for the LegiScan getAmendment API call
#' @param legiscan An object of class LegiScan
#' @param id A LegiScan amendment_id value
#' @rdname amendment-methods
#' @family LegiScan API Caller Generics
#' @docType methods
#' @export amendment
setGeneric("amendment",
		   def = function(legiscan, id) {
		   	standardGeneric("amendment")
		   })

#' @title LegiScan Caller Methods - supplement
#' @description Generic method for the LegiScan getSupplement API call
#' @param legiscan An object of class LegiScan
#' @param id A LegiScan numeric supplement id
#' @rdname supplement-methods
#' @family LegiScan API Caller Generics
#' @docType methods
#' @export supplement
setGeneric("supplement",
		   def = function(legiscan, id) {
		   	standardGeneric("supplement")
		   })

#' @title LegiScan Caller Methods - rollCall
#' @description Generic method for the LegiScan getRollCall API call
#' @param legiscan An object of class LegiScan
#' @param id A LegiScan Roll_Call_ID numeric value
#' @rdname rollCall-methods
#' @family LegiScan API Caller Generics
#' @docType methods
#' @export rollCall
setGeneric("rollCall",
		   def = function(legiscan, id) {
		   	standardGeneric("rollCall")
		   })

#' @title LegiScan Caller Methods - sponsor
#' @description Generic method for the LegiScan getSponsor API call
#' @param legiscan An object of class LegiScan
#' @param id A LegiScan people_id value
#' @rdname sponsor-methods
#' @family LegiScan API Caller Generics
#' @docType methods
#' @export sponsor
setGeneric("sponsor",
		   def = function(legiscan, id) {
		   	standardGeneric("sponsor")
		   })

#' @title LegiScan Caller Methods - legisearch
#' @description Generic method for the LegiScan search API call
#' @param legiscan An object of class LegiScan
#' @param state A two character US State abbreviation (validated against the
#' state.abb dataset in R)
#' @param bill A LegiScan billnumber for an exact bill number or to limit query
#' @param query A character vector containing the search terms
#' @param year An indicator for either classes of years or for a specific year
#' from 1900 through till the present
#' @param page Indicates the page number of the results requested
#' @rdname legisearch-methods
#' @family LegiScan API Caller Generics
#' @details
#' The accepted values the the year parameter are described in more detail here.
#' \itemize{
#' 		\item{"year = 1"}{All Years of Data Available}
#' 		\item{"year = 2"}{Current Year of Data Only (default)}
#' 		\item{"year = 3"}{Recent Years of Data Available}
#' 		\item{"year = 4"}{Prior Years of Data Available}
#' 		\item{"year >= 1900"}{Exact Year Only}
#' }
#' @docType methods
#' @export legisearch
setGeneric("legisearch",
		   def = function(legiscan, state, bill, query, year, page) {
		   	standardGeneric("legisearch")
		   })


