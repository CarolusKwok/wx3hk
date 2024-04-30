#' @title
#' HK weather - Download predicted tidal height data
#'
#' @description
#' Downloaded from https://www.hko.gov.hk/en/tide/ttext.htm
#'
#' @param station (character) Array of station name abbreviations to be downloaded. Check `wx3hk::dict_hk_tide()` for more information
#' @param year (Integer) Array of years to download. Accepts value larger than 2015L
#'
#' @return `tibble` with columns `time`, `station`, `tide`
#' @export
#'
#' @examples load_hk_tidePredict(station = "CLK", c(2016L, 2021L))
load_hk_tidePredict = function(station,
                               year = 2015L:2025L){
  #Check
  dict = wx3hk:::dict_hk_tide()
  if(!sum(station %in% dict$name)){
    call::abort(message = c("x" = "Incorrect {.arg station}",
                                "i" = "{.arg station} only accepts the following value",
                                "i" = stringr::str_flatten(string = dict$name, collapse = ", ")))
  }
  if(!is.integer(year)){call::abort_WrongClass(x = year, class = "integer")}
  if(sum(year < 2015L)){
    call::abort(message = c("x" = "Incorrect {.arg year}",
                                "i" = "{.arg year} only accepts value",
                                "i" = stringr::str_flatten(string = dict$name, collapse = ", ")))
  }

  #Work
  URL = tidyr::expand_grid(data.frame(station = station),
                           data.frame(year = year),
                           data.frame(mode = c("HR", "HL")))
  data = lapply(X = seq_len(nrow(URL)),
                FUN = function(X, URL){
                  station = URL$station[[X]]
                  mode = URL$mode[[X]]
                  year = URL$year[[X]]

                  if(mode == "HR" & year < 2020){
                    return(wx3hk:::load_hk_tidePredict_2015HR(station = station, year = year))
                  }
                  if(mode == "HR" & year >= 2020){
                    return(wx3hk:::load_hk_tidePredict_2020HR(station = station, year = year))
                  }
                  if(mode == "HL" & year < 2020){
                    return(wx3hk:::load_hk_tidePredict_2015HL(station = station, year = year))
                  }
                  if(mode == "HL" & year >= 2020){
                    return(wx3hk:::load_hk_tidePredict_2020HL(station = station, year = year))
                  }
                }, URL = URL)

  return(dplyr::mutate(.data = dplyr::arrange(.data = do.call(rbind, data),
                                              station, time),
                       time = lubridate::force_tz(time = time, tzone = "HongKong")))
}
