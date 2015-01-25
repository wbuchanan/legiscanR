#' @title legiscanR functions and the LegiScan Class object/methods
#' @description
#' An R interface to the LegiScan API (\url{http://legiscan.com/}) for
#' accessing US legislative data.  This package provides the LegiScan class
#' and methods for sending requests to the API for regularly updated
#' changes to legislation/votes, as well as functions and methods to
#' parse the data from the API calls and from master download files.
#'
#' @docType package
#' @name legiscanR-package
NULL

#' @title Lookup tables for legiscanR programs
#' @description
#' This dataset contains a single list object 'lookuptables' containing 10 nested
#' data.frame objects.  Each of the data.frame objects contains the key - value pairs
#' used by LegiScan to encode categorical data numerically.  There is a brief summary
#' of these data below:
#'
#' \tabular{cccc}{
#' 		data.frame  	\tab 1st Variable 	\tab 2nd Variable	\tab	3rd Variable 	\cr
#' 		billTextMime	\tab bill_text_mime_id (int)	\tab mime_type (chr)	\tab   \cr
#' 		billTextType	\tab bill_text_type_id (int)	\tab bill_text_name (chr)	\tab  \cr
#' 		billType	\tab bill_type_id (int)	\tab bill_type_name (chr) 	\tab bill_type_abbr (chr) \cr
#' 		body	\tab body_id (int)	\tab state_id (int) 	\tab role_id (int) \cr
#' 		body (continued)	\tab body_name (chr) 	\tab body_abbr (chr) 	\tab body_short (chr) \cr
#' 		body (continued)	\tab body_role_abbr (chr)	\tab body_role_name (chr)	\tab    \cr
#' 		progress	\tab progress_event (int) 	\tab progress_desc (chr) 	\tab   \cr
#' 		reason 	\tab reason_id (int) 	\tab reason_desc (chr) 	\tab    \cr
#' 		role	\tab role_id (int) 	\tab role_name (chr) 	\tab role_abbr (chr) \cr
#' 		sastType	\tab sast_id (int) 	\tab sast_description (chr) 	\tab     \cr
#' 		state 	\tab state_id (int) 	\tab state_abbr (chr) 	\tab state_name (chr) \cr
#' 		supplement 	\tab supplement_type_id (int) 	\tab supplement_type_desc (chr) 	\tab   \cr
#' }
#'
#' @format A list of 10 data.frame objects
#' @source Sean Bolt (LegiScan Employee) \url{http://legiscan.com/misc/LegiScan_API_User_Manual.pdf}
#' @name lookuptables
#' @docType data
#' @usage data(lookuptables)
#' @keywords datasets
NULL
