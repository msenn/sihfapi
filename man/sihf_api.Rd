% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{sihf_api}
\alias{sihf_api}
\title{Generi Call to SIHF API}
\usage{
sihf_api(path, query)
}
\arguments{
\item{path}{Character string for path to desired API endpoint}

\item{query}{Search query; see examples and \code{?httr::modify_url}}
}
\value{
httr response object
}
\description{
Generi Call to SIHF API
}
\examples{
# Query game list
\dontrun{
resp <- sihf_api("/statistic/api/cms/table", query = list(
  alias       = "results",
  searchQuery = "1,8,10,11//1,8,9,20,47,48,50,90,81",
  filterQuery = "2017/1/01.09.2016-12.09.2017",
  filterBy    = "Season,League,Date"
  ))
}
}
