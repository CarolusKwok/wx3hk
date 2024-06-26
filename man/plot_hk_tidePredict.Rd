% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_hk_tidePredict.R
\name{plot_hk_tidePredict}
\alias{plot_hk_tidePredict}
\title{HK Weather - Plot predicted tide}
\usage{
plot_hk_tidePredict(
  station,
  period = c("2024/01/01", "2024/01/15"),
  criteria = NA,
  plot = TRUE,
  night = TRUE
)
}
\arguments{
\item{station}{(character) Array of station name abbreviations to be downloaded. Check \code{wx3hk::dict_hk_tide()} for more information}

\item{period}{(character) A 2-character array of dates accepted within \code{base::as.Date}.}

\item{criteria}{(list-numeric) A list of 3-length long numeric or \strong{special value} \code{NA}. Descriptions of NAs and arrays are as following:
\itemize{
\item \code{NA}: no preference in tidal height
\item 1st number: the lower-bound of tidal height accepted
\item 2nd number: the upper-bound of tidal height accepted
\item 3rd number: the slope of the tidal height change. \code{+1} for an increasing tidal height over time, \code{-1} for a decreasing tidal height over time, \code{0} for no preference
}}

\item{plot}{(logical) Should the function plot out the data
\itemize{
\item TRUE: return the data as \code{ggplot}
\item FALSE: return the data as \code{tibble}
}}

\item{night}{(logical) Should the function plot out night-time as translucent black strips? Only available when \code{plot == TRUE}.
\itemize{
\item TRUE: Plot night time as translucent black strips
\item FALSE: Not plot night time as translucent black strips
}}
}
\value{
One of the following

\code{ggplot}, with Y-axis as predicted tide, and X-axis as time.
\itemize{
\item Red line: Out-of-criteria tidal heights
\item Green line: In-criteria tidal heights with favorable tidal trends
\item Blue line: In-criteria tidal heights but unfavorable tidal trends
}

\code{tibble}, with following columns
\itemize{
\item time: Date-time value, in Hong Kong time zone
\item tide: Predicted tidal height, in mCD
\item station: Station code. Check \code{wx3hk::dict_hk_tide()} for more information
\item slope: Slope of the upcoming tidal height trend, folliwng \code{criteria}
\item criteria: Values of "OUT", "BEST", "IN", following the meaning of "Red", "Green", and "Blue" lines in \code{ggplot}.
}
}
\description{
Supplied by \code{wx3hk::load_hk_tidePredict} and smoothed by \code{wx3::calc_smooth_lm}, the data will be plotted out as a \code{ggplot} object or returned as a \code{tibble}, with advanced functions.
}
\examples{
wx3hk::plot_hk_tidePredict(station = "CLK", criteria = list(c(1,1.3,-1)))
}
