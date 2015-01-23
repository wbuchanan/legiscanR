#' @title LegiScan Parser Methods - parseStates
#' @description Generic method for parsing data retrieved from stateList API call
#' @param rawStateList An object with the returned data from the stateList method
#' @param dataframe A logical defining the returned object data type (set to false for a list)
#' @docType methods
#' @rdname parseSessions-methods
#' @family LegiScan API Parser Generics
#' @export parseSessions
setGeneric("parseStates",
		   def = function(rawStateList, dataframe = TRUE) {
		   	standardGeneric("parseStates")
		   })

#' @title LegiScan Parser Methods - parseSessions
#' @description Generic method for parsing data retrieved from sessionList API call
#' @param rawSessionList An object with the returned data from the sessionList method
#' @param dataframe A logical defining the returned object data type (set to false for a list)
#' @docType methods
#' @rdname parseSessions-methods
#' @family LegiScan API Parser Generics
#' @export parseSessions
setGeneric("parseSessions",
		   def = function(rawSessionList, dataframe = TRUE) {
		   	standardGeneric("parseSessions")
		   })

#' @title LegiScan Parser Methods - parseMasterList
#' @description Generic method for parsing data retrieved from getMasterList API call
#' @param rawMasterList An object with the returned data from the masterList method
#' @param dataframe A logical defining the returned object data type (set to
#' false for tbl_df object)
#' @param archive A logical specifying if an archived file will be created. File will
#' be saved as MasterListArchive followed by a timestamp in the current directory.
#' @docType methods
#' @rdname parseMasterList-methods
#' @family LegiScan API Parser Generics
#' @export parseMasterList
setGeneric("parseMasterList",
		   def = function(rawMasterList, dataframe = FALSE, archive = TRUE) {
		   	standardGeneric("parseMasterList")
		   })

#' @title LegiScan Parser Methods - parseBill
#' @description Generic method for parsing data retrieved from getBill API call
#' @param rawBill An object with the returned data from the masterList method
#' @param dataframe If TRUE method returns multiple data objects to the
#' global environment; if FALSE the method returns a list of data frames
#' @param fullText Accepts either "", "state_link", or "url" as arguments; the ""
#' argument is used to skip retrieving the full text of the bill; the "state_link"
#' argument is used to retrieve the full text of the bill from the State's URL
#' provided in the API call response; the "url" argument will attempt to retrieve the
#' full text of the bill from LegiScan.
#' @docType methods
#' @rdname parseBill-methods
#' @family LegiScan API Parser Generics
#' @export parseBill
setGeneric("parseBill",
		   def = function(rawBill, dataframe = FALSE, fullText = "") {
		   	standardGeneric("parseBill")
		   })

#' @title LegiScan Parser Methods - parseRollCall
#' @description Generic method for parsing data retrieved from getRollCall API call
#' @param theRollCall An object with the returned data from the rollCall method
#' @param dataframe If TRUE method returns multiple data objects to the
#' global environment; if FALSE the method returns a list of data frames
#' @docType methods
#' @rdname parseRollCall-methods
#' @family LegiScan API Parser Generics
#' @export parseRollCall
setGeneric("parseRollCall",
		   def = function(theRollCall, dataframe = FALSE) {
		   	standardGeneric("parseRollCall")
		   })

#' @title LegiScan Parser Methods - parseSponsors
#' @description Generic method for parsing data retrieved from getSponsors API call
#' @param theSponsor An object with the returned data from the sponsor method
#' @param dataframe If TRUE method returns a data.frame object;
#' if FALSE the method returns a tbl_df object
#' @docType methods
#' @rdname parseSponsors-methods
#' @family LegiScan API Parser Generics
#' @export parseSponsor
setGeneric("parseSponsor",
		   def = function(theSponsor, dataframe = FALSE) {
		   	standardGeneric("parseSponsor")
		   })

#' @title LegiScan Parser Methods - parseQuery
#' @description Generic method for parsing data retrieved from search API call
#' @param query An object with the returned data from the search method
#' @docType methods
#' @rdname parseQuery-methods
#' @family LegiScan API Parser Generics
#' @export parseQuery
setGeneric("parseQuery",
		   def = function(query) {
		   	standardGeneric("parseQuery")
		   })

#' @title LegiScan Parser Methods - parseBillText
#' @description Generic method for parsing data retrieved from getBillText API call
#' @param theBillText An object with the returned data from the billText method
#' @docType methods
#' @rdname parseBillText-methods
#' @family LegiScan API Parser Generics
#' @export parseBillText
#' @return A list object containing metadata and the MIME data retrieved by the API call
setGeneric("parseBillText",
		   def = function(theBillText) {
		   	standardGeneric("parseBillText")
		   })

#' @title LegiScan Parser Methods - parseSupplement
#' @description Generic method for parsing data retrieved from getSupplement API call
#' @param theSupplement An object with the returned data from the supplement method
#' @docType methods
#' @rdname parseSupplement-methods
#' @family LegiScan API Parser Generics
#' @export parseSupplement
#' @return A list object containing metadata and the MIME data retrieved by the API call
setGeneric("parseSupplement",
		   def = function(theSupplement) {
		   	standardGeneric("parseSupplement")
		   })

#' @title LegiScan Parser Methods - parseAmendment
#' @description Generic method for parsing data retrieved from getAmendment API call
#' @param theAmendment An object with the returned data from the amendment method
#' @docType methods
#' @rdname parseAmendment-methods
#' @family LegiScan API Parser Generics
#' @export parseAmendment
#' @return A list object containing metadata and the MIME data retrieved by the API call
setGeneric("parseAmendment",
		   def = function(theAmendment) {
		   	standardGeneric("parseAmendment")
		   })
