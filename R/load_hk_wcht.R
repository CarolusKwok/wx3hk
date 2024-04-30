#' @title
#' HK weather - Downloading weather chart
#'
#' @description
#' Downloaded from https://www.hko.gov.hk/en/index.html
#'
#' @param time (POSIXt) An array of date-time to be downloaded.
#' @param list_fail (logical) List failed-to-download items.
#' * TRUE for listing all failed-to-download items. (DEFAULT)
#' * FALSE for not listing all failed-to-download items.
#' @param dir (character) Directory of downloaded data. A folder (`HK_Data`) will be created in the directory provided to store the aforementioned data.
#' @param attempt (integer) Attempts to be made per file to download.
#' @param worker (integer) Number of sessions to be open to download a list of files. Files will be downloaded simultaneously.
#'
#' @return Images of Hong Kong weather chart, from https://www.hko.gov.hk/en/index.html
#' @export
#'
#' @examples load_hk_wcht()
load_hk_wcht = function(time = seq.POSIXt(from = Sys.time(), by  = "-1 hour", length.out = 168),
                        list_fail = TRUE, dir = getwd(), attempt = 5L, worker = 1L){
  #Check
  wx3hk:::sys_ckf_HKLoad(time = time, list_fail = list_fail, attempt = attempt, worker = worker)

  #Additional variables
  dit = 6
  #Force time to be HKT
  time = lubridate::with_tz(time, tzone = "HongKong")
  #Format
  URL = tibble::tibble(time = time) %>%
    dplyr::mutate(Ztime = lubridate::with_tz(time, tzone = "UTC"),
                  Zyear = lubridate::year(Ztime),
                  Zmonth = lubridate::month(Ztime),
                  Zday = lubridate::day(Ztime),
                  Zhour = lubridate::hour(Ztime),
                  com = Zhour %% dit,
                  ZLtime = ISOdatetime(Zyear, Zmonth, Zday, Zhour - com, 00, 00, tz = "UTC"),
                  Ltime = lubridate::with_tz(ZLtime, tzone = "HongKong"),
                  LDate = paste0(sprintf("%04d", lubridate::year(Ltime)),
                                 sprintf("%02d", lubridate::month(Ltime)),
                                 sprintf("%02d", lubridate::day(Ltime))),
                  LHour = paste0(sprintf("%02d", lubridate::hour(Ltime)),
                                 sprintf("%02d", lubridate::minute(Ltime))),
                  Info = paste0(LDate, "-", LHour),
                  URL = paste0("https://www.hko.gov.hk/wxinfo/currwx/wxchart/",
                               LDate, substr(LHour, 1, 2), ".gif"),
                  DIR = paste0(dir,
                               "/", "HK_Data",
                               "/", "WCHT",
                               "/", substr(LDate, 1, 4),
                               "/", substr(LDate, 1, 6),
                               "/", "HK_WCHT", "_", LDate, "_", LHour, ".gif")) %>%
    dplyr::distinct() %>%
    dplyr::select(Info, URL, DIR)

  wx3::load_file(data = URL,
                 worker = worker,
                 attempt = attempt,
                 threshold = 1000000,
                 list_fail = list_fail,
                 title = "Weather Chart (HKO)")
}
