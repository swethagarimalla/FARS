% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fars_functions.R
\name{fars_summarize_years}
\alias{fars_summarize_years}
\title{Count the number of accidents within each month for each year}
\usage{
fars_summarize_years(years)
}
\arguments{
\item{years}{A list/vector of integer numbers, each of which denote a year.}
}
\value{
Returns a pivot data frame containing two columns (one for accident count, and 
one for each year in the \code{years} list/vector), where each month is a row. As per use
of the fars_read_years function, a warning will be returned if an element of \code{years}
does not have an associated file.
}
\description{
This is a simple function that takes a list of numerical integers,
each element of which denotes a year, and produces a data frame with the number of accidents
for each month within each year.
}
\examples{
\dontrun{
fars_summarize_years(2013)
fars_summarize_years(2013:2014)
fars_summarize_years(list(2013, 2014, 2015))
}

}
