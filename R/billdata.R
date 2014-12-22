# Function to parse bill data used by LegiScan bill parsers
billdata <- function(fileobject, JSON = TRUE) {

	# Parse the JSON object
	if (JSON == TRUE) legiObject <- fromJSON(fileobject)
	else legiObject <- xmlRoot(xmlParse(fileobject))

	# Strip the outer layer elements
	legiObject <- legiObject[["bill"]]

}
