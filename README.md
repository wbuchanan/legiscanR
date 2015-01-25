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

### Additional info

While trying to work out issues with the installation process, you may need to use:

install.packages(c('XML', 'RJSONIO', 'httr', 'dplyr', 'plyr', 'magrittr', 'lubridate', 'RCurl'), dep = TRUE, repo = "http://cran.rstudio.com")
devtools::install_github('wbuchanan/legiscanR', dependencies = FALSE) 

I have been running [Revolution R Open](http://mran.revolutionanalytics.com), which specifies a different CRAN repository based on a snapshot taken on a single date.  Installing the packages from an active CRAN repository first and setting the install_github function to not install the dependencies may be the easiest way around this for now.
