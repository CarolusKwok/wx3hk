#' @title
#' HK weather - Downloading tidal height data
#'
#' @description
#' Downloaded from https://data.gov.hk/en/help/api-spec
#'
#' @param time (POSIXt) An array of date-time to be downloaded.
#' @param lan (character) Language to download. Accepts the following
#' * "en" for English
#' * "tc" for Traditional Chinese
#' * "sc" for Simplified Chinese
#' @param type (character) Source of data to be downloaded. Accepts the following (any combination)
#' * "hko" for Hong Kong Observatory
#' * "md" for Marine Department
#' @param list_fail (logical) List failed-to-download items.
#' * TRUE for listing all failed-to-download items. (DEFAULT)
#' * FALSE for not listing all failed-to-download items.
#' @param dir (character) Directory of downloaded data. A folder (`HK_Data`) will be created in the directory provided to store the aforementioned data.
#' @param attempt (integer) Attempts to be made per file to download.
#' @param worker (integer) Number of sessions to be open to download a list of files. Files will be downloaded simultaneously.
#'
#' @return CSVs from https://data.gov.hk/en/help/api-spec
#' @export
#'
#' @examples load_hk_tide()
load_hk_tide = function(time = seq.POSIXt(from = wx3hk::hk_time(),
                                          by = "-5 min",
                                          length.out = 2016),
                        lan = "en",
                        type = c("hko", "md"),
                        list_fail = TRUE,
                        dir = getwd(),
                        attempt = 5L,
                        worker = 1L){
  #Check
  wx3hk:::sys_ckf_HKLoad(time = time, list_fail = list_fail, attempt = attempt, worker = worker)
  wx3hk:::sys_ckf_HKLoadLan(lan)
  if(sum(!(type %in% c("hko", "md")))){
    call::abort(message = c("x" = "Incorrect {.arg type}",
                                "i" = "{.arg type} only accepts the following value",
                                "i" = "hko",
                                "i" = "md"))
  }

  #Additional variables
  nlan = ifelse(lan == "en", "en",
         ifelse(lan == "tc", "tc",
         ifelse(lan == "sc", "sc", NA)))

  #Force time to be HKT
  time = lubridate::with_tz(time, tzone = "HongKong")
  #URL

  URL = tidyr::crossing(time = time, type = type) %>%
    dplyr::mutate(dit = ifelse(type == "hko", 5, 10),
                  year = lubridate::year(time),
                  month = lubridate::month(time),
                  day = lubridate::day(time),
                  hour = lubridate::hour(time),
                  min = lubridate::minute(time),
                  com = min %% dit,
                  Ltime = ISOdatetime(year, month, day, hour, min - com, 00, tz = "HongKong"),
                  LDate = paste0(sprintf("%04d", lubridate::year(Ltime)),
                                 sprintf("%02d", lubridate::month(Ltime)),
                                 sprintf("%02d", lubridate::day(Ltime))),
                  LHour = paste0(sprintf("%02d", lubridate::hour(Ltime)),
                                 sprintf("%02d", lubridate::minute(Ltime))),
                  Info = paste0(LDate, "-", LHour),
                  URL = ifelse(type == "hko",
                               paste0("https://api.data.gov.hk/v1/historical-archive/get-file?url=https%3A%2F%2Fdata.weather.gov.hk%2FweatherAPI%2Fhko_data%2Ftide%2FALL_",
                                      nlan, ".csv&time=", LDate, "-", LHour),
                               paste0("https://api.data.gov.hk/v1/historical-archive/get-file?url=https%3A%2F%2Ftide1.hydro.gov.hk%2Fhotide%2FOpenData%2FAll_",
                                      nlan, ".csv&time=", LDate, "-", LHour)),
                  Ptime = Ltime - lubridate::minutes(5),
                  PDate = paste0(sprintf("%04d", lubridate::year(Ptime)),
                                 sprintf("%02d", lubridate::month(Ptime)),
                                 sprintf("%02d", lubridate::day(Ptime))),
                  PHour = paste0(sprintf("%02d", lubridate::hour(Ptime)),
                                 sprintf("%02d", lubridate::minute(Ptime))),
                  DIR = paste0(dir,
                               "/", "HK_Data",
                               "/", "TIDE",
                               "/", "TIDE(", type, ")", lan,
                               "/", substr(PDate, 1, 4),
                               "/", substr(PDate, 1, 6),
                               "/", PDate,
                               "/", "HK_TIDE(", type, ")", lan, "_", PDate, "_", PHour, ".csv")) %>%
    dplyr::arrange(Ltime) %>%
    dplyr::select(Info, URL, DIR) %>%
    dplyr::distinct()

  title = paste0("Tidal Height_(",
                 stringr::str_flatten(type, collapse = ","), ")")
  wx3::load_file(data = URL,
                 worker = worker,
                 attempt = attempt,
                 threshold = 170,
                 list_fail = list_fail,
                 title = title)
}
