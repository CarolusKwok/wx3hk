#' @title
#' Dictionary of HK tidal stations
#'
#' @description
#' Provides the dictionary of HK tidal stations, with the following columns
#' * name = Code name of the tidal stations
#' * full_name = Name of the tidal stations, in English
#' * lon = Longitude of the tidal stations
#' * lat = Latitude of the tidal stations
#' * available = Availability of the tidal stations, `FALSE` represents the tidal station is not operational, `TRUE` represents the tidal station is operational and will provide real-time tidal data to HKO
#'
#' @return `tibble`, with columns `name`, `full_name`, `lon`, `lat`, `available`
#' @export
#'
#' @examples dict_hk_tide()
dict_hk_tide = function() {
  return(data.frame(
    name = c("CLK","CCH","CMW","KLW","KCT",
             "LOP","MWC","QUB","SPW","TMW",
             "TAO","TPK","TBT","WAG"),
    full_name = c("Chek Lap Kok(E)", "Cheung Chau", "Chi Ma Wan", "Ko Lau Wan", "Kwai Chung",
                  "Lok On Pai", "Ma Wan", "Quarry Bay", "Shek Pik", "Tai Miu Wan",
                  "Tai O", "Tai Po Kau", "Tsim Bei Tsui", "Waglan Island"),
    lon = c(
      113.9452778,
      114.0230556,
      113.9999115,
      114.3608333,
      114.1227778,
      114.0016415,
      114.0713889,
      114.2133333,
      113.8944444,
      114.2886111,
      113.8655556,
      114.1838889,
      114.0141667,
      114.3027778
    ),
    Lat = c(
      22.3205556,
      22.2141667,
      22.2395810,
      22.4586111,
      22.3236111,
      22.3614868,
      22.3638889,
      22.2911111,
      22.2202778,
      22.2697222,
      22.2550000,
      22.4425000,
      22.4872222,
      22.1830556
    ),
    available = c( TRUE,TRUE,FALSE,TRUE,TRUE,
                  FALSE,TRUE, TRUE,TRUE,TRUE,
                   TRUE,TRUE, TRUE,TRUE)
  ))
}

