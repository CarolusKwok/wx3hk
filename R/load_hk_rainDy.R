#' @title
#' HK weather - Downloading daily rainfall distribution image
#'
#' @description
#' Download data from https://www.hko.gov.hk/en/index.html
#'
#' @param lan (character) Language to download. Accepts the following
#' * "en" for English
#' * "tc" for Traditional Chinese
#' * "sc" for Simplified Chinese
#' @param list_fail (logical) List failed-to-download items.
#' * TRUE for listing all failed-to-download items. (DEFAULT)
#' * FALSE for not listing all failed-to-download items.
#' @param dir (character) Directory of downloaded data. A folder (`HK_Data`) will be created in the directory provided to store the aforementioned data.
#' @param attempt (integer) Attempts to be made per file to download.
#' @param worker (integer) Number of sessions to be open to download a list of files. Files will be downloaded simultaneously.
#'
#' @return Images from https://www.hko.gov.hk/en/index.html
#' @export
#'
#' @examples load_hk_rainDY()
load_hk_rainDY = function(lan = "en",
                          list_fail = TRUE,
                          dir = getwd(),
                          attempt = 5L,
                          worker = 1L){
  #Check
  wx3hk:::sys_ckf_HKLoad(time = ISOdatetime(2023, 01, 01, 00, 00, 00, tz = "HongKong"), list_fail = list_fail, attempt = attempt, worker = worker)
  wx3hk:::sys_ckf_HKLoadLan(lan = lan)

  #Additional variables
  nlan = ifelse(lan == "en", "e",
                ifelse(lan == "tc", "c",
                       ifelse(lan == "sc", "c", NA)))

  #Find the latest leap year
  for(i in lubridate::year(Sys.time()):0){
    if(lubridate::leap_year(i)){
      leap = i
      break
    }
  }

  #Generate time and format it to be HKT
  time = seq.POSIXt(from = Sys.time() - lubridate::days(2), by = "-1 day", length.out = 363) %>%
    lubridate::with_tz(tzone = "HongKong") %>%
    append(ISOdatetime(leap, 02, 29, 08, 00, 00, tz = "HongKong"))
  #Format
  URL = tibble::tibble(time = time) %>%
    dplyr::mutate(year = lubridate::year(time),
                  month = lubridate::month(time),
                  day = lubridate::day(time),
                  Date = paste0(sprintf("%04d", lubridate::year(time)),
                                sprintf("%02d", lubridate::month(time)),
                                sprintf("%02d", lubridate::day(time))),
                  Info = Date,
                  URL = paste0("https://www.hko.gov.hk/wxinfo/rainfall/cokrig_barnes/rfmap24hrs",
                               substr(Date, start = 5, stop = 8), "0000", nlan, ".png"),
                  DIR = paste0(getwd(),
                               "/", "HK_Data",
                               "/", "RAIN",
                               "/", "RAIN_dy", lan,
                               "/", substr(Date, 1, 4),
                               "/", substr(Date, 1, 6),
                               "/", "HK_RAIN_dy", lan, "_", Date, ".png")) %>%
    dplyr::select(Info, URL, DIR) %>%
    dplyr::distinct()
  #Start to download
  wx3::load_file(data = URL,
                 worker = worker,
                 attempt = attempt,
                 threshold = 250000,
                 list_fail = list_fail,
                 title = "Daily Rainfall Image (HKO)")
}
