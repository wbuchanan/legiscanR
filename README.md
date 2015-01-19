# legiscanR

legiscanR is an R package that provides an interface to the [LegiScan](http://www.legiscan.com) API for US State and Congressional Legislative data.  The package is currently being developed, but includes functions to parse and clean data retrieved from LegiScan master download files and functions to automate calls to the LegiScan API interactively.

## Installation Notes

The legiscanR package will automatically look for your API key when constructing new LegiScan objects in the following order:

* If no value is supplied to the legiscanR() function for the API slot
** getOption("legiscanR") 
** scan("~/.legiscanAPI", what = "character")
* If a value is supplied to the legiscanR() function for the API slot
** Checks to see if the value is the name of a file with the API key
** If not a valid file, treats the value as the API key

The legiscanR option can be set permanently in your R profile, or can be set interactively using:

option(legiscanR = "ABCD1234EFGHIJK78910")


