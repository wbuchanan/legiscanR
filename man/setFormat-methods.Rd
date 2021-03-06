% Generated by roxygen2 (4.1.0): do not edit by hand
% Please edit documentation in R/constructor-generics.R, R/constructor-methods.R
\docType{methods}
\name{setFormat}
\alias{setFormat}
\alias{setFormat,LegiScan,logical-method}
\title{LegiScan Constructor Methods - setFormat}
\usage{
setFormat(legiscan, json)

\S4method{setFormat}{LegiScan,logical}(legiscan, json)
}
\arguments{
\item{legiscan}{An object of class LegiScan}

\item{json}{A logical indicating if the format slot should be JSON or XML}
}
\value{
Returns the LegiScan class object with user specified format slot
}
\description{
Generic constructor method for setting the format slot of a
LegiScan class object

Constructor method for setting the format slot of a
LegiScan class object
}
\examples{
\dontrun{
# Create an object of class LegiScan
myLegiScan <- legiscanR()

# Sets format slot to JSON
setFormat(myLegiScan, TRUE)

# Sets format slot to XML
setFormat(myLegiScan, FALSE)
}
}
\seealso{
Other LegiScan Constructor Generics: \code{\link{setAPI}},
  \code{\link{setAPI,LegiScan,character,missing-method}},
  \code{\link{setAPI,LegiScan,missing,character-method}},
  \code{\link{setAPI,LegiScan,missing,missing-method}};
  \code{\link{setUrl}},
  \code{\link{setUrl,LegiScan-method}}

Other LegiScan Constructor Methods: \code{\link{setAPI}},
  \code{\link{setAPI,LegiScan,character,missing-method}},
  \code{\link{setAPI,LegiScan,missing,character-method}},
  \code{\link{setAPI,LegiScan,missing,missing-method}};
  \code{\link{setUrl}},
  \code{\link{setUrl,LegiScan-method}}
}

