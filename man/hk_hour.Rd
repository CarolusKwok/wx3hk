% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hk_time.R
\name{hk_hour}
\alias{hk_hour}
\title{Call China/ Hong Kong/ Macau/ Taiwan Standard Time, hour only}
\usage{
hk_hour()
}
\value{
POSIXct, in HKT (closest hour only)
}
\description{
Hong Kong Standard Time (HKT) will be called by this function. However, it will only call the latest hour value.

\emph{e.g.} if time now is "2024/01/01 01:02:03 HKT", it will only call "2024/01/01 01:00:00 HKT"
}
\examples{
hk_hour()
}
