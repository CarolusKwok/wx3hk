#' @title
#' HK weather - Downloading tidal height data (snapshots)
#'
#' @description
#' Downloaded from https://www.hko.gov.hk/en/tide/marine/realtide.htm
#'
#' Snapshots of tidal height data provides more stations than normal API method. However, the tidal data must be snapshot once per day and lacks quality-control.
#'
#' @param station (character) Array of station name abbreviations to be downloaded. Check `wx3hk::dict_hk_tide()` for more information
#' @param list_fail (logical) List failed-to-download items.
#' * TRUE for listing all failed-to-download items. (DEFAULT)
#' * FALSE for not listing all failed-to-download items.
#' @param dir (character) Directory of downloaded data. A folder (`HK_Data`) will be created in the directory provided to store the aforementioned data.
#' @param attempt (integer) Attempts to be made per file to download.
#' @param worker (integer) Number of sessions to be open to download a list of files. Files will be downloaded simultaneously.
#'
#' @return CSV files from https://www.hko.gov.hk/en/tide/marine/realtide.htm. Each CSV file corresponding to a snapshot of one of the stations
#' @export
#'
#' @examples load_hk_tideSnap()
load_hk_tideSnap = function(station = dplyr::filter(wx3hk::dict_hk_tide(), available)$name,
                            list_fail = TRUE,
                            dir = getwd(),
                            attempt = 5L,
                            worker = 1L){
  #Checking inputs
  dict = wx3hk::dict_hk_tide()
  if(rlang::is_missing(station)){
    call::abort_NoArg(station)
  }
  check_station = !(station %in% dict$name)
  if(sum(check_station)){
    call::abort(message = c("x" = "Incorrect {.arg station}",
                                "i" = "{.arg station} only accepts the following value",
                                "i" = stringr::str_flatten(string = dict$name, collapse = ", ")))
  }

  wx3hk:::sys_ckf_HKLoad(time = ISOdatetime(2024, 04, 22, 00, 00, 00),
                         list_fail = list_fail,
                         attempt = attempt,
                         worker = worker)

  #Start ###
  time = lubridate::with_tz(time = Sys.time(), tzone = "HongKong")
  year = sprintf("%04d", lubridate::year(time))
  month = sprintf("%02d", lubridate::month(time))
  day = sprintf("%02d", lubridate::day(time))
  hour = sprintf("%02d", lubridate::hour(time))
  min = sprintf("%02d", lubridate::minute(time))

  ##Download the data ####
  data = dplyr::mutate(.data = data.frame(Info = station),
                       URL = paste0("https://www.hko.gov.hk/tide/marine/data/", Info, ".cht.txt"),
                       DIR = paste0(tempdir(),
                                    "/wx3hk",
                                    "/HK_TIDE(", station, ")_", year, month, day, "_", hour, min, ".txt"),
                       CSV = paste0(dir,
                                    "/HK_Data",
                                    "/TIDE",
                                    "/TIDE(snapshot)",
                                    "/", year,
                                    "/", year, month,
                                    "/HK_TIDE(", station, ")_", year, month, day, "_", hour, min, ".csv")) %>%
    dplyr::mutate(file_exist = file.exists(DIR) | file.exists(CSV)) %>%
    dplyr::filter(!file_exist)

  wx3::load_file(data = data,
                 worker = worker,
                 attempt = attempt,
                 threshold = 62000,
                 list_fail = list_fail,
                 title = "test")

  #Modify the data downloaded ####
  data = dplyr::mutate(.data = data,
                       file_exist = file.exists(DIR)) %>%
    dplyr::filter(file_exist)
  lapply(X = seq_len(nrow(data)),
         FUN = function(X, data){
           DIR = data$DIR[[X]]
           CSV = data$CSV[[X]]

           dir.create(path = dirname(CSV), recursive = TRUE, showWarnings = FALSE)

           read.csv(file = data$DIR[[X]],
                    header = FALSE, na.strings = "----") %>%
             dplyr::mutate(station = data$Info[[X]]) %>%
             dplyr::rename(time = V1,
                           tide = V2,
                           predicted.tide = V3) %>%
             readr::write_csv(file = data$CSV[[X]], na = "")
         },
         data = data)

  invisible(file.remove(data$DIR))
}
