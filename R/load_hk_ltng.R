#' @title
#' HK weather - Downloading lighting & RADAR images
#'
#' @description
#' Download lighting and RADAR images from https://www.hko.gov.hk/en/index.html
#'
#' @param time (POSIXt) An array of date-time to be downloaded.
#' @param type (character) Type of lighting data to be loaded. Accepts the following
#' * "cc" for cloud-to-cloud lighting
#' * "cg" for cloud-to-ground lighting
#' @param range (numeric) Radar range in km. Accepts 64 and 256.
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
#' @examples load_hk_ltng()
load_hk_ltng = function(time = seq.POSIXt(from = Sys.time(), by = "-6 min", length.out = 961),
                        type = c("cc", "cg"),
                        range = c(64, 256),
                        list_fail = T,
                        dir = getwd(),
                        attempt = 5L,
                        worker = 1L) {
  #Check
  wx3hk:::sys_ckf_HKLoad(time = time,
                         list_fail = list_fail,
                         attempt = attempt,
                         worker = worker)
  if(sum(!(type %in% c("cc", "cg")))){
    call::abort(message = c("x" = "Incorrect {.arg type}",
                                "i" = "{.arg Type} only accepts the following value",
                                "i" = "cc",
                                "i" = "cg"))
  }
  if(sum(!(range %in% c(64L, 256L)))){
    call::abort(message = c("x" = "Incorrect {.arg range}",
                                "i" = "{.arg Type} only accepts the following value",
                                "i" = "64",
                                "i" = "256"))
  }

  #Find all combinations of type and range
  URL = tidyr::crossing(type = type,
                        range = range,
                        time = time) %>%
    dplyr::mutate(
      dit = ifelse(range == 64, 6, 12),
      push = ifelse(range == 64, 0, 6),
      time = lubridate::with_tz(time, tzone = "HongKong"),
      year = lubridate::year(time),
      month = lubridate::month(time),
      day = lubridate::day(time),
      hour = lubridate::hour(time),
      min = lubridate::minute(time),
      com = min %% dit,
      Ltime = ISOdatetime(year, month, day, hour, min - com + push, 00, tz = "HongKong"),
      LDate = paste0(
        sprintf("%04d", lubridate::year(Ltime)),
        sprintf("%02d", lubridate::month(Ltime)),
        sprintf("%02d", lubridate::day(Ltime))
      ),
      LHour = paste0(
        sprintf("%02d", lubridate::hour(Ltime)),
        sprintf("%02d", lubridate::minute(Ltime))
      ),
      Info = paste0(LDate, "-", LHour),
      URL = paste0("https://www.hko.gov.hk/wxinfo/llis/llisradar/images/lli_", range, toupper(type), "_", LDate, LHour, ".png"),
      DIR = paste0(dir,
        "/", "HK_Data", "/", "LTNG", "/", "LTNG", sprintf("%03d", range), type,
        "/", substr(LDate, 1, 4),
        "/", substr(LDate, 1, 6),
        "/", LDate,
        "/", "HK_LTNG",
        sprintf("%03d", range), type, "_", LDate, "_", LHour, ".png"
      )
    ) %>%
    dplyr::arrange(Ltime) %>%
    dplyr::select(Info, URL, DIR) %>%
    dplyr::distinct()
  #Start to download
  wx3::load_file(
    data = URL,
    attempt = attempt,
    title = paste0(
      "Lighting & Radar_",
      stringr::str_flatten(sprintf("%03d", range)),
      " ",
      stringr::str_flatten(type),
      "(HKO)"
    ),
    worker = worker,
    threshold = 220000,
    list_fail = list_fail
  )
}
