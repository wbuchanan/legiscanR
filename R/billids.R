billids <- function(billobject) {

	idList <- list(session_id = billobject[["session"]][[1]],
				   bill_id = billobject[["bill_id"]],
				   change_hash = billobject[["change_hash"]],
				   state = billobject[["state"]],
				   state_id = billobject[["state_id"]],
				   bill_number = billobject[["bill_number"]])

	idVars <- llply(idList, .fun = function(rmNulls) {
		if (length(rmNulls) == 0) rmNulls <- NA
		else rmNulls <- as.character(rmNulls)
	}) %>% as.data.frame(stringsAsFactors = FALSE)


	return(idVars)

}
