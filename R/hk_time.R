#' @title
#' Call China/ Hong Kong/ Macau/ Taiwan Standard Time
#'
#' @description
#' Hong Kong Standard Time (HKT) will be called by this function
#'
#' @return POSIXct, in HKT
#' @export
#'
#' @examples hk_time()
hk_time = function(){
  return(lubridate::with_tz(time = Sys.time(), tzone = "Asia/Hong_Kong"))
}

#' @title
#' Call China/ Hong Kong/ Macau/ Taiwan Standard Time, hour only
#'
#' @description
#' Hong Kong Standard Time (HKT) will be called by this function. However, it will only call the latest hour value.
#'
#' _e.g._ if time now is "2024/01/01 01:02:03 HKT", it will only call "2024/01/01 01:00:00 HKT"
#'
#' @return POSIXct, in HKT (closest hour only)
#' @export
#'
#' @examples hk_hour()
hk_hour = function(){
  time = wx3hk::hk_time()
  return(ISOdatetime(year = lubridate::year(time),
                     month = lubridate::month(time),
                     day = lubridate::day(time),
                     hour = lubridate::hour(time),
                     min = 00,
                     sec = 00,
                     tz = "Asia/Hong_Kong"))
}
