#' @title
#' HK weather - Downloading satellite images
#'
#' @description
#' Download data from https://www.hko.gov.hk/en/index.html
#'
#' @param time (POSIXt) An array of date-time to be downloaded.
#' @param magn (numeric) Satellite image magnification. Accepts 2/ 4/ 8.
#' @param type Type of satellite image. Accepts the following (any combinations).
#' * "tc" for True color
#' * "ir" for Infrared
#' * "dc" for Deep convection
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
#' @examples load_hk_satl()
load_hk_satl = function(time = seq.POSIXt(from = Sys.time(), by = "-10 min", length.out = 432),
                        magn = c(2L, 4L, 8L),
                        type = c("tc", "ir", "dc"),
                        list_fail = T, dir = getwd(), attempt = 5L, worker = 1L){
  #Check
  wx3hk:::sys_ckf_HKLoad(time = time, list_fail = list_fail, attempt = attempt, worker = worker)

  if(sum(!(magn %in% c(2L, 4L, 8L)))){
    call::abort(message = c("x" = "Incorrect {.arg type}",
                                "i" = "{.arg Type} only accepts the following value",
                                "i" = "2L",
                                "i" = "4L",
                                "i" = "8L"))
  }
  if(sum(!(type %in% c("tc", "ir", "dc")))){
    call::abort(message = c("x" = "Incorrect {.arg type}",
                                "i" = "{.arg Type} only accepts the following value",
                                "i" = "tc",
                                "i" = "ir",
                                "i" = "dc"))
  }

  #Force time to be HKT
  time = lubridate::with_tz(time, tzone = "HongKong")

  #Additional variables
  dit = 10

  URL = tidyr::crossing(time = time, magn = magn, type = type) %>%
    dplyr::mutate(year = lubridate::year(time),
                  month = lubridate::month(time),
                  day = lubridate::day(time),
                  hour = lubridate::hour(time),
                  min = lubridate::minute(time),
                  com = min %% dit,
                  Ltime = ISOdatetime(year, month, day, hour, min - com, 00, tz = "HongKong"),
                  Ltime_UTC = lubridate::with_tz(Ltime, tzone = "UTC"),
                  LDate = paste0(sprintf("%04d", lubridate::year(Ltime)),
                                 sprintf("%02d", lubridate::month(Ltime)),
                                 sprintf("%02d", lubridate::day(Ltime))),
                  LHour = paste0(sprintf("%02d", lubridate::hour(Ltime)),
                                 sprintf("%02d", lubridate::minute(Ltime))),
                  LDate_UTC = paste0(sprintf("%04d", lubridate::year(Ltime_UTC)),
                                     sprintf("%02d", lubridate::month(Ltime_UTC)),
                                     sprintf("%02d", lubridate::day(Ltime_UTC))),
                  LHour_UTC = paste0(sprintf("%02d", lubridate::hour(Ltime_UTC)),
                                     sprintf("%02d", lubridate::minute(Ltime_UTC)),
                                     "00"),
                  Info = paste0(LDate, "-", LHour, "-", type),
                  URL = paste0("https://www.hko.gov.hk/wxinfo/intersat/satellite/image/images/h8L_",
                               type,"_x",magn,"M_",LDate_UTC, LHour_UTC,".png"),
                  DIR = paste0(dir,
                               "/", "HK_Data",
                               "/", "SATL",
                               "/", "SATL", sprintf("%02d", magn), type,
                               "/", substr(LDate, 1, 4),
                               "/", substr(LDate, 1, 6),
                               "/", LDate,
                               "/", "HK_SATL", sprintf("%02d", magn), type, "_", LDate, "_", LHour, ".png")) %>%
    dplyr::select(Info, URL, DIR)
  #Start to download
  wx3::load_file(data = URL,
                 worker = worker,
                 attempt = attempt,
                 threshold = 160000,
                 list_fail = list_fail,
                 title = paste0("Satellite Image_",
                                stringr::str_flatten(sprintf("%02d", magn)), " ",
                                stringr::str_flatten(type), " (HKO)"))
}
