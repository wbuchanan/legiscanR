##' @title Legiscan Bill Text Data
##' @description Parses and arranges JSON output from
##' Legiscan master data downloads bills subdirectory
##' @param fileobject A JSON file object from the bills subdirectory
##' @return Creates a list object containing a data frame
##' for the progress of the bill
##' @export
##' @examples
#' require(legiscanR)
#' directoryTree <- fileStructure("data/msHistoricalJSON/")
#' files <- fileLists(directoryTree)
#' legalText <- legiBillTxt(files[["bills"]][[10]][[12]])

# Function to parse/clean JSON output from LegiScan API calls
legiBillTxt <- function(fileobject) {

	# Parse the JSON object
	billobject <- billdata(eval(fileobject))

	# Named list of ID elements
	IDs <- billids(billobject)

	# Add the ID columns to the data frames and fill the required
	# number of records to rectangularize the data frame
	billTxt <- as.data.frame(cbind(IDs[rep(seq_len(nrow(IDs)),
											nrow(billobject[["texts"]])), ],
									billobject[["texts"]]), stringsAsFactors = FALSE)

	# Generate a list of all of the state link elements
	linkLists <- as.list(billTxt$state_link)

	# Name the elements of the link list
	names(linkLists) <- c(billobject[["bill_number"]], billobject[["bill_number"]])

	# Retrieve, parse, and clean the text of the bills
	cleanText <- lapply(linkLists, FUN = function(links) {
		tryCatch(paste(xpathApply(htmlParse(links),
								  "//p", xmlValue), collapse = "\n"),
				 error = function(e) {
				 	list(c("drop me"),
				 		 c("Error loading the bill text"))
				 })
	})

	# Create data table with the text data
	fullText <- ldply(cleanText, rbind)

	# Convert text back to character vector
	fullText[, 2] <- toString(fullText[, 2])

	# Assign a name to the cleaned full text data table
	names(fullText) <- c("drop", "full_bill_text")

	# Remove the ID column generated by dplyr
	fullText <- fullText[, 2]

	# Add the full text of the bill to the other bill text data
	billText <- as.data.frame(cbind(billTxt, full_bill_text = fullText),
							  stringsAsFactors = FALSE)

	# Convert the text of the legislation back to string data
	billText$full_bill_text <- toString(billText$full_bill_text)

	# Return the parsed/cleaned object
	return(billText)

# End of function call
}