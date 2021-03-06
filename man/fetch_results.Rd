% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{fetch_results}
\alias{fetch_results}
\title{Fetch Results Table}
\usage{
fetch_results(filter = NULL)
}
\arguments{
\item{filter}{Named vector of fields and values to filter query on. SIHF
  requires filter. Details on supported filtering options are unknown. Known
  options include \itemize{ \item{Season (e.g. "2017"); this filter appears
  to be required} \item{Date (e.g. "06.04.2017" or "01.09.2016-05.03.2018")}
  \item{League (e.g. "1" for National League )} \item{Phase (e.g. "2443", key
  not fully understood)} }.

  As filter is required, passing \code{filter = NULL} (default) returns
  current season (new season starts being returned from July).}
}
\value{
httr response object
}
\description{
Retrieve a summary table from SIHF API. This functionality is particularly
useful to compile a list of game IDs that can be user in
\code{get_gameDetail}.
}
\details{
The date filter of the SIHF API functions in a non-strict way: if no games
matching all other filters took place on the specified date (or in the
interval), the API will return the last games before that date / interval
matching the filters. That is, if we query the API for a date where no games
took place, it will still return games.
}
\examples{
results <- fetch_results(c(Season = "2017", Date = "09.10.2016"))
}
