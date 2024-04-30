#' @title
#' HK weather - Downloading relative humidity information
#'
#' @description
#' Download data from https://data.gov.hk/en/help/api-spec
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
#' @return CSV from https://data.gov.hk/en/help/api-spec
#' @export
#'
#' @examples load_hk_rhum()
load_hk_rhum = function(time = seq.POSIXt(from = wx3hk::hk_time(),
                                          by = "-10 min",
                                          length.out = 1008),
                        lan = "en",
                        list_fail = TRUE,
                        dir = getwd(),
                        attempt = 5L,
                        worker = 1L){
  #Check
  wx3hk:::sys_ckf_HKLoad(time = time, list_fail = list_fail, attempt = attempt, worker = worker)
  wx3hk:::sys_ckf_HKLoadLan(lan = lan)

  #Additional variables
  nlan = ifelse(lan == "en", "",
         ifelse(lan == "tc", "_uc",
         ifelse(lan == "sc", "_sc", NA)))
  dit = 10
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
                  URL = paste0("https://api.data.gov.hk/v1/historical-archive/get-file?url=https%3A%2F%2Fdata.weather.gov.hk%2FweatherAPI%2Fhko_data%2Fregional-weather%2Flatest_1min_humidity",
                               nlan, ".csv&time=", LDate, "-", LHour),

                  Ptime = Ltime - lubridate::minutes(10),
                  PDate = paste0(sprintf("%04d", lubridate::year(Ptime)),
                                 sprintf("%02d", lubridate::month(Ptime)),
                                 sprintf("%02d", lubridate::day(Ptime))),
                  PHour = paste0(sprintf("%02d", lubridate::hour(Ptime)),
                                 sprintf("%02d", lubridate::minute(Ptime))),

                  DIR = paste0(dir,
                               "/", "HK_Data",
                               "/", "RHUM",
                               "/", "RHUM", lan,
                               "/", substr(PDate, 1, 4),
                               "/", substr(PDate, 1, 6),
                               "/", PDate,
                               "/", "HK_RHUM", lan, "_", PDate, "_", PHour, ".csv")) %>%
    dplyr::select(Info, URL, DIR) %>%
    dplyr::distinct()
  #Start to download
  wx3::load_file(data = URL,
                 worker = worker,
                 attempt = attempt,
                 threshold = 710,
                 list_fail = list_fail,
                 title = "Relative Humidity (HKO)")
}
