#' @title
#' CN weather - Downloading RADAR data
#'
#' @description
#' RADAR images of the entire China will be provided. Images are downloaded from http://www.weather.com.cn/.
#'
#' @param time (POSIXt) An array of date-time to be downloaded.
#' @param zoom (character) Radar photo zoom range, the following values are accepted
#' * *all*: All of the following zoom ranges will be downloaded
#' * CHN: Entire China
#' * NCN: Northern China
#' * NEC: North-Eastern China
#' * ECN: Eastern China
#' * CCN: Central China
#' * SCN: Southern China
#' * SWC: South-Western China
#' * NWC: North-Western China
#' @param list_fail (logical) List failed-to-download items.
#' * TRUE for listing all failed-to-download items. (DEFAULT)
#' * FALSE for not listing all failed-to-download items.
#' @param dir (character) Directory of downloaded data. A folder (HK_Data) will be created in the directory provided to store the aforementioned data.
#' @param attempt (integer) Attempts to be made per file to download.
#' @param worker (integer) Number of sessions to be open to download a list of files. Files will be downloaded simultaneously.
#'
#' @return Images from http://www.weather.com.cn/
#' @export
#'
#' @examples load_cn_radr()
load_cn_radr = function(time = seq.POSIXt(from = wx3hk::hk_hour(),
                                          tz = "HongKong",
                                          by = "-1 hour",
                                          length.out = 168),
                        zoom = "*all*",
                        list_fail = TRUE,
                        dir = getwd(),
                        attempt = 2L,
                        worker = 1L){
  #Check
  wx3hk:::sys_ckf_HKLoad(time = time, list_fail = list_fail, attempt = attempt, worker = worker)

  stations = c("CHN", "NCN", "NEC", "ECN", "CCN", "SCN", "SWC", "NWC")
  if(zoom == "*all*"){
    zoom = stations
  } else {
    check = !(zoom %in% stations)
    if(sum(check)){
      call::abort(message = c("x" = "Incorrect {.arg zoom}",
                              "i" = "{.arg zoom} only accepts the following value:",
                              "i" = stringr::str_flatten(string = stations, collapse = "; "),
                              "i" = "Your input:",
                              "i" = stringr::str_flatten(string = zoom, collapse = "; ")))
    }
  }

  #Start
  URL = tidyr::expand_grid(data.frame(zoom = stringr::str_to_lower(string = zoom)),
                           dplyr::mutate(.data = data.frame(time = lubridate::with_tz(time, tzone = "UTC")),
                                         time_scan = time -
                                           lubridate::minutes(lubridate::minute(time) %% 6) -
                                           lubridate::seconds(lubridate::second(time)),
                                         time_print = lubridate::with_tz(time_scan, tzone = "Asia/Hong_Kong"))) %>%
    dplyr::mutate(Set = dplyr::row_number()) %>%
    tidyr::expand_grid(data.frame(dit = -100:2160)) %>%
    dplyr::select(time_scan, time_print, zoom, dit, Set) %>%
    dplyr::mutate(time_update = time_scan + lubridate::seconds(dit),
                  time_print = stringr::str_c(sprintf("%04d", lubridate::year(time_print)),
                                              sprintf("%02d", lubridate::month(time_print)),
                                              sprintf("%02d", lubridate::day(time_print)),
                                              "_",
                                              sprintf("%02d", lubridate::hour(time_print)),
                                              sprintf("%02d", lubridate::minute(time_print))),
                  URL = stringr::str_c("https://pi.weather.com.cn/i/product/pic/l/z_rada_c_babj_",
                                       sprintf("%04d", lubridate::year(time_update)),
                                       sprintf("%02d", lubridate::month(time_update)),
                                       sprintf("%02d", lubridate::day(time_update)),
                                       sprintf("%02d", lubridate::hour(time_update)),
                                       sprintf("%02d", lubridate::minute(time_update)),
                                       sprintf("%02d", lubridate::second(time_update)),
                                       "_p_dor_a", zoom, "_cref_",
                                       sprintf("%04d", lubridate::year(time_scan)),
                                       sprintf("%02d", lubridate::month(time_scan)),
                                       sprintf("%02d", lubridate::day(time_scan)),
                                       "_",
                                       sprintf("%02d", lubridate::hour(time_scan)),
                                       sprintf("%02d", lubridate::minute(time_scan)),
                                       "00", ".png"),
                  Info = stringr::str_c(time_print, "_", stringr::str_to_upper(zoom)),
                  DIR = paste0(dir, "/", "CN_Data", "/",
                               "RADR", "/",
                               substr(Info, 1, 4), "/",
                               substr(Info, 1, 6), "/",
                               substr(Info, 1, 8), "/",
                               "CN_RADR_", stringr::str_to_upper(zoom), "_", time_print, ".png")) %>%
    dplyr::select(Set, URL, Info, DIR)
  #Start to download
  wx3::load_fileset(data = URL,
                    attempt = attempt,
                    worker = worker,
                    threshold = 560000,
                    list_fail = list_fail,
                    title = "China RADAR Data (www.weather.com.cn)")
}
