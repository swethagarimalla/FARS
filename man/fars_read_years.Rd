% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_read_years}
\alias{fars_read_years}
\title{Retrieves Month & Year from the Accident Year Files}
\usage{
fars_read_years(years)
}
\arguments{
\item{years}{A list of integer numbers, each of which denote a year.}
}
\value{
Returns a list of dataframes/tibbles of months (under the column 'MONTH') and 
years (under the column 'year'). Each of these tibbles is associated with each accident 
file that is associated with each of the \code{year} elements of the list of \code{years}.
If a \code{year} within the list does not have an associated file in the current working
directory named in the format 'accident_<year>.csv.bz2', then a warning is thrown and returns
NULL.
}
\description{
This is a simple function that takes a numerical list of integers that 
denotes \code{years}, accesses the accident file for each \code{year} of the list of
\code{years} and return a list of dataframes of \code{MONTH} and \code{year}, one
for each accident file, each associated with a year. The accident files for each year 
within list must be located within the current working directory and must be named in 
the format 'accident_<year>.csv.bz2'
}
\examples{
\dontrun{
fars_read_years(2013)
fars_read_years(list(2013, 2014))
fars_read_years(2013:2015)

#A not-found warning is thrown and null is returned for the following
fars_read_years(list(2013, 2014, 2015, 2016))
fars_read_years(2017)
}



}
