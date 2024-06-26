% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_hk_ltng.R
\name{load_hk_ltng}
\alias{load_hk_ltng}
\title{HK weather - Downloading lighting & RADAR images}
\usage{
load_hk_ltng(
  time = seq.POSIXt(from = Sys.time(), by = "-6 min", length.out = 961),
  type = c("cc", "cg"),
  range = c(64, 256),
  list_fail = T,
  dir = getwd(),
  attempt = 5L,
  worker = 1L
)
}
\arguments{
\item{time}{(POSIXt) An array of date-time to be downloaded.}

\item{type}{(character) Type of lighting data to be loaded. Accepts the following
\itemize{
\item "cc" for cloud-to-cloud lighting
\item "cg" for cloud-to-ground lighting
}}

\item{range}{(numeric) Radar range in km. Accepts 64 and 256.}

\item{list_fail}{(logical) List failed-to-download items.
\itemize{
\item TRUE for listing all failed-to-download items. (DEFAULT)
\item FALSE for not listing all failed-to-download items.
}}

\item{dir}{(character) Directory of downloaded data. A folder (\code{HK_Data}) will be created in the directory provided to store the aforementioned data.}

\item{attempt}{(integer) Attempts to be made per file to download.}

\item{worker}{(integer) Number of sessions to be open to download a list of files. Files will be downloaded simultaneously.}
}
\value{
Images from https://www.hko.gov.hk/en/index.html
}
\description{
Download lighting and RADAR images from https://www.hko.gov.hk/en/index.html
}
\examples{
load_hk_ltng()
}
