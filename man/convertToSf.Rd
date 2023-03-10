% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/drone_createLitchiMissionGrid.R
\name{convertToSf}
\alias{convertToSf}
\title{Convert a data frame to an sf object with WGS 84 coordinate reference system (CRS)}
\usage{
convertToSf(data)
}
\arguments{
\item{data}{A data frame containing latitude and longitude columns.}
}
\value{
An sf object with WGS 84 CRS.
}
\description{
Convert a data frame to an sf object with WGS 84 coordinate reference system (CRS)
}
\examples{
convertToSf(data.frame(latitude = 45.5236, longitude = -122.675))

}
\keyword{internal}
