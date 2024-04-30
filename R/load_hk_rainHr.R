#' @title
#' HK weather - Downloading daily rainfall distribution images
#'
#' @description
#' Download data from https://www.hko.gov.hk/en/index.html
#'
#' @param time (POSIXt) An array of date-time to be downloaded.
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
#' @examples load_hk_rainHr()
load_hk_rainHr = function(time = seq.POSIXt(from = wx3hk::hk_time(),
                                            by = "-15 min",
                                            length.out = 672),
                          lan = "en",
                          list_fail = TRUE,
                          dir = getwd(),
                          attempt = 5L,
                          worker = 1L){
  #Check
  wx3hk:::sys_ckf_HKLoad(time = time, list_fail = list_fail, attempt = attempt, worker = worker)
  wx3hk:::sys_ckf_HKLoadLan(lan = lan)

  #Additional variables
  dit = 15
  nlan = ifelse(lan == "en", "e",
                ifelse(lan == "tc", "c",
                       ifelse(lan == "sc", "c", NA)))
  #Force time to be HKT
  time = lubridate::with_tz(time, tzone = "HongKong")
  #Format
  URL = tibble::tibble(time = time) %>%
    dplyr::mutate(year = lubridate::year(time),
                  month = lubridate::month(time),
                  day = lubridate::day(time),
                  hour = lubridate::hour(time),
                  min = lubridate::minute(time),
                  com = min %% dit,
                  Ltime = ISOdatetime(year, month, day, hour, min - com, 00, tz = "HongKong")) %>%
    dplyr::select(time, Ltime) %>%
    dplyr::mutate(LDate = paste0(sprintf("%04d", lubridate::year(Ltime)),
                                 sprintf("%02d", lubridate::month(Ltime)),
                                 sprintf("%02d", lubridate::day(Ltime))),
                  LHour = paste0(sprintf("%02d", lubridate::hour(Ltime)),
                                 sprintf("%02d", lubridate::minute(Ltime))),
                  Info = paste0(LDate, "-", LHour),
                  URL = paste0("https://www.hko.gov.hk/wxinfo/rainfall/cokrig_barnes/rfmap",
                               LDate, LHour, nlan, ".png"),
                  DIR = paste0(dir,
                               "/", "HK_Data",
                               "/", "RAIN",
                               "/", "RAIN_hr", lan,
                               "/", substr(LDate, 1, 4),
                               "/", substr(LDate, 1, 6),
                               "/", LDate,
                               "/", "HK_RAIN_hr", lan, "_", LDate, "_", LHour, ".png")) %>%
    dplyr::select(Info, URL, DIR) %>%
    dplyr::distinct()
  #Start to download
  wx3::load_file(data = URL,
                 worker = worker,
                 attempt = attempt,
                 threshold = 250000,
                 list_fail = list_fail,
                 title = paste0("Hourly Rain distribution_", lan, " (HKO)"))
}
