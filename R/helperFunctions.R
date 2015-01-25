# Wrapper functions

#' @title Wrappers - Parsing/Cleaning State List
#' @description
#' This is a wrapper function for the parseStates method
#' @param StateList Object returned from the stateList method for the LegiScan class
#' @param data A logical indicating whether the function should return
#' a data frame object (TRUE) or return a list (FALSE)
#' @return Returns the texts object with the full text of the bill(s) joined to
#' the data frame object
#' @family LegiScan Wrapper Functions
#' @name cleanStates
#' @export cleanStates
cleanStates <- function(StateList, data = FALSE) {

	# Verify that the the StateList came from the states method
	if (length(StateList) == 2 && class(StateList) == "list") {

		# Select the first element from the list only
		states <- StateList[[1]]

		# Pass the object to the method
		clStates <- parseStates(rawStateList = states, dataframe = data)

		# Return the object
		return(clStates)

	} else {
		stop("StateList did not have length == 2 or was not a list object")
	}
}


#' @title Wrappers - Parsing/Cleaning Sessions List
#' @description
#' This is a wrapper function for the parseSessions method
#' @param SessionList Object returned from the sessions method for the LegiScan class
#' @param data A logical indicating whether the function should return
#' a data frame object (TRUE) or return a list (FALSE)
#' @return Returns the texts object with the full text of the bill(s) joined to
#' the data frame object
#' @family LegiScan Wrapper Functions
#' @name cleanSessions
#' @export cleanSessions
cleanSessions <- function(SessionList, data = FALSE) {

	# Verify that the the bill object came from the sessions method
	if (length(SessionList) == 2 && class(SessionList) == "list") {

		# Select the first element from the list only
		sessions <- SessionList[[1]]

		# Pass the object to the method
		clSession <- parseSessions(rawSessionList = sessions, dataframe = data)

		# Return the object
		return(clSession)

	} else {
		stop("SessionList did not have length == 2 or was not a list object")
	}
}



#' @title Wrappers - Parsing/Cleaning Master Lists
#' @description
#' This is a wrapper function for the parseMasterList method
#' @param MasterList Object returned from the masterList method for the LegiScan class
#' @param data A logical indicating whether the function should return
#' a data frame object (TRUE) or a tbl_df object (FALSE)
#' @param makeArchive A logical indicating whether or not to save an archive file
#' @param xmloptions Is used to specify additional options to the xmlParse
#' used to define the behavior of the underlying XML parsing engine.  By default,
#' xmloptions = c(RECOVER, NOCDATA) is specified
#' @return Returns the master list of all legislation post processing/cleaning
#' @family LegiScan Wrapper Functions
#' @name cleanMasterList
#' @export cleanMasterList
cleanMasterList <- function(MasterList, data = FALSE, makeArchive = TRUE,
										xmloptions = c(RECOVER, NOCDATA)) {

	# Verify that the the MasterList object came from the masterList method
	if (length(MasterList) == 2 && class(MasterList) == "list") {

		# Select the first element from the list only
		theMasterList <- MasterList[[1]]

		if (class(theMasterList) == "character") xmloptions <- NULL

		# Pass the object to the method
		clMasterList <- parseMasterList(rawMasterList = theMasterList, dataframe = data,
									archive = makeArchive, option = xmloptions)

		# Return the object
		return(clMasterList)

	} else {
		stop("MasterList did not have length == 2 or was not a list object")
	}
}



#' @title Wrappers - Parsing/Cleaning Bill Data
#' @description
#' This is a wrapper function for the parseBill method
#' @param BillObject An object returned from the bill method for the LegiScan class
#' @param data A logical indicating whether the function should return
#' separate data frame objects (TRUE) or return a list of data frames (FALSE)
#' @param text A character parameter passed to the parseBill method to identify
#' if the function should attempt to retrieve the full text of the bill
#' @return Returns the texts object with the full text of the bill(s) joined to
#' the data frame object
#' @family LegiScan Wrapper Functions
#' @name cleanBill
#' @export cleanBill
cleanBill <- function(BillObject, data = FALSE, text = "") {
	# Verify that the the bill object came from the getBill method
	if (length(BillObject) == 2 && class(BillObject) == "list") {

		# Retrieve the first element of the billobject list
		billob <- BillObject[[1]]

		# Pass arguments along to the parseBill method
		if (data == FALSE) {
			bills <- parseBill(rawBill = billob, dataframe = data, fullText = text)
			return(bills)
		} else {
			# Call the parseBill method and then pass the returned data frame
			# objects back to the global environment
			parseBill(rawBill = billob, dataframe = data, fullText = text)

			# Return individual data frame objects
	  		billmeta <<- billMeta; sponsors <<- sponsors; texts <<- texts;
	  		progress <<- progress; committees <<- committees; votes <<- votes
		}
	} else {
		stop("BillObject did not have length == 2 or was not a list object")
	}
}


#' @title Wrappers - Parsing/Cleaning API response to the getBillText call
#' @description
#' This is a wrapper function for the parseBillText method
#' @param BillText Object returned from the billText method for the LegiScan class
#' @return Returns a list containing meta data and the binary MIME object containing
#' the full text of the bill
#' @note It is recommended to retrieve the full text of the bill by specifying a
#' value (e.g., 'url' or 'state_link') to the cleanBill function if you are only
#' interested in the text itself.
#' @family LegiScan Wrapper Functions
#' @name cleanBillText
#' @export cleanBillText
cleanBillText <- function(BillText) {

	# Verify that the the BillText object came from the billText method
	if (length(BillText) == 2 && class(BillText) == "list") {

		# Select the first element from the list only
		rawBillText <- BillText[[1]]

		# Pass the object to the method
		clBillText <- parseBillText(theBillText = rawBillText)

		# Return the object
		return(clBillText)

	} else {
		stop("BillText did not have length == 2 or was not a list object")
	}
}

#' @title Wrappers - Parsing/Cleaning API response to the getAmendment call
#' @description
#' This is a wrapper function for the parseAmendment method
#' @param anAmendment Object returned from the amendment method for the LegiScan class
#' @return Returns a list containing meta data and the binary MIME object containing
#' the full text of the amendment
#' @family LegiScan Wrapper Functions
#' @name cleanAmendment
#' @export cleanAmendment
cleanAmendment <- function(anAmendment) {

	# Verify that the the anAmendment object came from the amendment method
	if (length(anAmendment) == 2 && class(anAmendment) == "list") {

		# Select the first element from the list only
		rawAmendment <- anAmendment[[1]]

		# Pass the object to the method
		clAmendment <- parseAmendment(theAmendment = rawAmendment)

		# Return the object
		return(clAmendment)

	} else {
		stop("anAmendment did not have length == 2 or was not a list object")
	}

}

#' @title Wrappers - Parsing/Cleaning API response to the getSupplement call
#' @description
#' This is a wrapper function for the parseSupplement method
#' @param aSupplement Object returned from the supplement method for the LegiScan class
#' @return Returns a list containing meta data and the binary MIME object containing
#' the full text of the supplement
#' @family LegiScan Wrapper Functions
#' @name cleanSupplement
#' @export cleanSupplement
cleanSupplement <- function(aSupplement) {

	# Verify that the the aSupplement object came from the supplement method
	if (length(aSupplement) == 2 && class(aSupplement) == "list") {

		# Select the first element from the list only
		rawSupplement <- aSupplement[[1]]

		# Pass the object to the method
		clSupplement <- parseSupplement(theSupplement = rawSupplement)

		# Return the object
		return(clSupplement)

	} else {
		stop("aSupplement did not have length == 2 or was not a list object")
	}
}

#' @title Wrappers - Parsing/Cleaning API response to the getRollCall call
#' @description
#' This is a wrapper function for the parseRollCall method
#' @param aRollCall Object returned from the rollCall method for the LegiScan class
#' @param data A logical indicating whether the data should be returned to the global
#' environment as individual objects (TRUE); or as a list of data frame objects (FALSE)
#' @return Returns voting meta data and individual legislator voting records
#' @family LegiScan Wrapper Functions
#' @name cleanRollCall
#' @export cleanRollCall
cleanRollCall <- function(aRollCall, data = FALSE) {

	# Verify that the the aRollCall object came from the rollCall method
	if (length(aRollCall) == 2 && class(aRollCall) == "list") {

		# Select the first element from the list only
		roll_call <- aRollCall[[1]]

		# Pass the object to the method
		clRollCall <- parseRollCall(theRollCall = roll_call, dataframe = data)

		# Return the object
		return(clRollCall)

	} else {
		stop("aRollCall did not have length == 2 or was not a list object")
	}
}

#' @title Wrappers - Parsing/Cleaning API response to the getSponsor call
#' @description
#' This is a wrapper function for the parseSponsor method
#' @param aSponsor Object returned from the sponsor method for the LegiScan class
#' @param data A logical indicating whether to return a data.frame object (true)
#' or a tbl_df object (FALSE)
#' @return Returns ID data for an individual bill sponsor
#' @family LegiScan Wrapper Functions
#' @name cleanSponsor
#' @export cleanSponsor
cleanSponsor <- function(aSponsor, data = FALSE) {

	# Verify that the the aSponsor object came from the sponsor method
	if (length(aSponsor) == 2 && class(aSponsor) == "list") {

		# Select the first element from the list only
		sponsors <- aSponsor[[1]]

		# Pass the object to the method
		clSponsor <- parseSponsor(theSponsor = sponsors, dataframe = data)

		# Return the object
		return(clSponsor)

	} else {
		stop("aSponsor did not have length == 2 or was not a list object")
	}
}

