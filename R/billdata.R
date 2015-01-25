#' @importFrom RJSONIO fromJSON
#' @importFrom XML xmlRoot xmlParse
# Function to parse bill data used by LegiScan bill parsers
billdata <- function(fileobject, JSON = TRUE) {

	# Parse the JSON object
	if (JSON == TRUE) legiObject <- RJSONIO::fromJSON(fileobject)
	else legiObject <- XML::xmlRoot(XML::xmlParse(fileobject))

	# Strip the outer layer elements
	legiObject <- legiObject[["bill"]]

}
