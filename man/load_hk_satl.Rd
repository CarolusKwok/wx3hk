% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_hk_satl.R
\name{load_hk_satl}
\alias{load_hk_satl}
\title{HK weather - Downloading satellite images}
\usage{
load_hk_satl(
  time = seq.POSIXt(from = Sys.time(), by = "-10 min", length.out = 432),
  magn = c(2L, 4L, 8L),
  type = c("tc", "ir", "dc"),
  list_fail = T,
  dir = getwd(),
  attempt = 5L,
  worker = 1L
)
}
\arguments{
\item{time}{(POSIXt) An array of date-time to be downloaded.}

\item{magn}{(numeric) Satellite image magnification. Accepts 2/ 4/ 8.}

\item{type}{Type of satellite image. Accepts the following (any combinations).
\itemize{
\item "tc" for True color
\item "ir" for Infrared
\item "dc" for Deep convection
}}

\item{list_fail}{(logical) List failed-to-download items.
\itemize{
\item TRUE for listing all failed-to-download items. (DEFAULT)
\item FALSE for not listing all failed-to-download items.
}}

\item{dir}{(character) Directory of downloaded data. A folder (\code{HK_Data}) will be created in the directory provided to store the aforementioned data.}

\item{attempt}{(integer) Attempts to be made per file to download.}

\item{worker}{(integer) Number of sessions to be open to download a list of files. Files will be downloaded simultaneously.}

\item{lan}{(character) Language to download. Accepts the following
\itemize{
\item "en" for English
\item "tc" for Traditional Chinese
\item "sc" for Simplified Chinese
}}
}
\value{
Images from https://www.hko.gov.hk/en/index.html
}
\description{
Download data from https://www.hko.gov.hk/en/index.html
}
\examples{
load_hk_satl()
}
