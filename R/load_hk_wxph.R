#' @title
#' HK weather - Downloading weather photo
#'
#' @description
#' Download weather photo from https://www.hko.gov.hk/en/index.html
#'
#' @param time (POSIXt) An array of date-time to be downloaded.
#' @param station (character) An array of stations selected for task or _special value_ `all` to download all stations. Read `wx3hk::dict_hk_wxph()` for more information
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
#' @examples load_hk_wxph()
load_hk_wxph = function(time = seq.POSIXt(from = Sys.time(),
                                          by = "-5 min",
                                          length.out = 432),
                        station = "all",
                        list_fail = TRUE,
                        dir = getwd(),
                        attempt = 5L,
                        worker = 1L){
  #Check
  wx3hk:::sys_ckf_HKLoad(time = time, list_fail = list_fail, attempt = attempt, worker = worker)

  if("all" %in% station | "ALL" %in% station){station = wx3hk:::dict_hk_wxph()$code}
  station = unique(toupper(station))
  #Force time to be HKT
  time = lubridate::with_tz(time, tzone = "HongKong")

  #Additional variables
  dit = 5

  URL = tidyr::crossing(time = time, station = station) %>%
    dplyr::mutate(year = lubridate::year(time),
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

                  Info = paste0(LDate, "-", LHour, "-", station),
                  URL = paste0("http://www.weather.gov.hk/wxinfo/aws/hko_mica/",
                               tolower(station),
                               "/img", toupper(station), "_", substr(LDate, 3, 8), "_", LHour, ".jpg"),
                  DIR = paste0(dir,
                               "/", "HK_Data",
                               "/", "WXPH",
                               "/", "WXPH(", station, ")",
                               "/", substr(LDate, 1, 4),
                               "/", substr(LDate, 1, 6),
                               "/", LDate,
                               "/", "HK_WXPH(", station, ")_", LDate, "_", LHour, ".jpg")) %>%
    dplyr::select(Info, URL, DIR) %>%
    dplyr::distinct()

  wx3::load_file(data = URL,
                 worker = worker,
                 attempt = attempt,
                 threshold = 26000,
                 list_fail = list_fail,
                 title = paste0("Weather Photo_", stringr::str_flatten(station, ","), " (HKO)"))
}
