#' @title
#' MO weather - Download tidal images
#'
#' @description
#' Data downloaded from https://www.smg.gov.mo/zh
#'
#' @param time (POSIXt) An array of date-time to be downloaded.
#' @param dir (character) Directory of downloaded data. A folder (`HK_Data`) will be created in the directory provided to store the aforementioned data.
#' @param attempt (integer) Attempts to be made per file to download.
#' @param subattempt (integer) Attempts to be made per attempt to download.
#' @param worker (integer) Number of sessions to be open to download a list of files. Files will be downloaded simultaneously.
#' @param list_fail (logical) List failed-to-download items.
#' * TRUE for listing all failed-to-download items. (DEFAULT)
#' * FALSE for not listing all failed-to-download items.
#'
#' @return Images from https://www.smg.gov.mo/zh
#' @export
#'
#' @examples load_mo_tide()
load_mo_tide = function(time = seq.POSIXt(from = wx3hk::hk_hour(),
                                          tz = "Asia/Macau",
                                          by = "-1 hour",
                                          length.out = 91),
                        dir = getwd(),
                        attempt = 900L,
                        subattempt = 5L,
                        worker = 1L,
                        list_fail = TRUE){
  #Check
  wx3hk:::sys_ckf_HKLoad(time = time, list_fail = list_fail, attempt = subattempt, worker = worker)

  #Additional variables
  dit = 15

  #Force time to be HKT
  time = lubridate::with_tz(time, tzone = "Asia/Macau")

  #Format
  URL = tidyr::expand_grid(Time = time,
                           seq = 1:attempt) %>%
    dplyr::left_join(y = tibble::tibble(Time = time,
                                        Set = 1:length(time)),
                     by = "Time") %>%
    dplyr::mutate(Year = lubridate::year(Time),
                  Month= lubridate::month(Time),
                  Day = lubridate::day(Time),
                  Hour = lubridate::hour(Time),
                  Min = lubridate::minute(Time),
                  com = Min %% dit,
                  Ltime = ISOdatetime(Year, Month, Day, Hour, (Min - com), 00, tz = "Asia/Macau"),

                  LDate = paste0(sprintf("%04d", lubridate::year(Ltime)),
                                 sprintf("%02d", lubridate::month(Ltime)),
                                 sprintf("%02d", lubridate::day(Ltime))),
                  LHour = paste0(sprintf("%02d", lubridate::hour(Ltime)),
                                 sprintf("%02d", lubridate::minute(Ltime))),
                  Ztime = as.numeric(difftime(lubridate::with_tz(Ltime,tzone = "UTC"),
                                              ISOdatetime(1970, 1, 1, 0, 0, 0, tz = "UTC"),
                                              units = "sec")),
                  URL = paste0("https://cms.smg.gov.mo/uploads/backup/WL/WL_",
                               (Ztime+seq),
                               ".png"),
                  Info = paste0(LDate, "-", LHour),
                  DIR = paste0(dir,
                               "/", "MO_Data",
                               "/", "TIDE",
                               "/", substr(LDate, 1, 4),
                               "/", substr(LDate, 1, 6),
                               "/", LDate,
                               "/", "MO_TIDE_", LDate, "_", LHour, ".png")) %>%
    dplyr::select(Set, URL, DIR, Info)

  #Start
  wx3::load_fileset(data = URL,
                    worker = worker,
                    attempt = subattempt,
                    threshold = 12000,
                    list_fail = list_fail,
                    title = "MO Tidal Height")
}
