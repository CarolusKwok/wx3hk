#' @title
#' HK Weather - Plot predicted tide
#'
#' @description
#' Supplied by `wx3hk::load_hk_tidePredict` and smoothed by `wx3::calc_smooth_lm`, the data will be plotted out as a `ggplot` object or returned as a `tibble`, with advanced functions.
#'
#' @param station (character) Array of station name abbreviations to be downloaded. Check `wx3hk::dict_hk_tide()` for more information
#' @param period (character) A 2-character array of dates accepted within `base::as.Date`.
#' @param criteria (list-numeric) A list of 3-length long numeric or __special value__ `NA`. Descriptions of NAs and arrays are as following:
#' * `NA`: no preference in tidal height
#' * 1st number: the lower-bound of tidal height accepted
#' * 2nd number: the upper-bound of tidal height accepted
#' * 3rd number: the slope of the tidal height change. `+1` for an increasing tidal height over time, `-1` for a decreasing tidal height over time, `0` for no preference
#' @param plot (logical) Should the function plot out the data
#' * TRUE: return the data as `ggplot`
#' * FALSE: return the data as `tibble`
#' @param night (logical) Should the function plot out night-time as translucent black strips? Only available when `plot == TRUE`.
#' * TRUE: Plot night time as translucent black strips
#' * FALSE: Not plot night time as translucent black strips
#'
#' @return One of the following
#'
#' `ggplot`, with Y-axis as predicted tide, and X-axis as time.
#' * Red line: Out-of-criteria tidal heights
#' * Green line: In-criteria tidal heights with favorable tidal trends
#' * Blue line: In-criteria tidal heights but unfavorable tidal trends
#'
#' `tibble`, with following columns
#' * time: Date-time value, in Hong Kong time zone
#' * tide: Predicted tidal height, in mCD
#' * station: Station code. Check `wx3hk::dict_hk_tide()` for more information
#' * slope: Slope of the upcoming tidal height trend, folliwng `criteria`
#' * criteria: Values of "OUT", "BEST", "IN", following the meaning of "Red", "Green", and "Blue" lines in `ggplot`.
#'
#' @export
#' @examples wx3hk::plot_hk_tidePredict(station = "CLK", criteria = list(c(1,1.3,-1)))
plot_hk_tidePredict = function(station,
                               period = c("2024/01/01", "2024/01/15"),
                               criteria = NA,
                               plot = TRUE,
                               night = TRUE){




  period = as.Date(period, tz = "HongKong")
  start = min(period)
  end = max(period)
  period = c(ISOdatetime(year = lubridate::year(start),
                         month = lubridate::month(start),
                         day = lubridate::day(start),
                         00, 00, 00, tz = "HongKong"),
             ISOdatetime(year = lubridate::year(end),
                         month = lubridate::month(end),
                         day = lubridate::day(end),
                         24, 00, 00, tz = "HongKong"))

  data = lapply(X = station,
                FUN = function(station, period, criteria){
                  start = period[[1]] - lubridate::days(x = 1)
                  end   = period[[2]] + lubridate::days(x = 1)

                  model_data = wx3hk::load_hk_tidePredict(station = station,
                                                          year = lubridate::year(start):lubridate::year(end)) %>%
                    dplyr::filter(start <= time & time <= end) %>%
                    dplyr::rename(orgTime = time,
                                  orgTide = tide) %>%
                    dplyr::mutate(leadTime = dplyr::lead(orgTime),
                                  leadTide = dplyr::lead(orgTide),
                                  grp = as.factor(dplyr::row_number())) %>%
                    tidyr::drop_na() %>%
                    dplyr::distinct() %>%
                    dplyr::reframe(time = c(orgTime, leadTime),
                                   tide = c(orgTide, leadTide),
                                   station = c(station, station),
                                   grp = c(grp, grp)) %>%
                    dplyr::arrange(grp, time)

                  model = lm(formula = tide ~ time * grp, data = model_data, singular.ok = TRUE)

                  data = dplyr::left_join(x = tibble::tibble(time = seq.POSIXt(from = start, to = end, by = "1 min")),
                                          y = dplyr::select(model_data, -station), by = "time") %>%
                    tidyr::fill(grp, .direction = "downup") %>%
                    dplyr::mutate(newTide = predict.lm(object = model, newdata = .),
                                  tide = ifelse(is.na(tide), newTide, tide),
                                  newTide = NULL, grp = NULL,
                                  station = station) %>%
                    dplyr::distinct() %>%
                    dplyr::mutate(leadTide = dplyr::lead(tide),
                                  slope = sign(leadTide - tide)) %>%
                    tidyr::fill(slope, .direction = "downup") %>%
                    dplyr::select(-leadTide)

                  if(sum(is.na(criteria)) == length(criteria)){
                    data = dplyr::mutate(.data = data, criteria = "IN")
                  } else {
                    data = dplyr::mutate(.data = data, criteria = "OUT")
                    for(i in criteria){
                      crit_min = min(i[1:2])
                      crit_max = max(i[1:2])
                      crit_slope = ifelse(sign(i[[3]]) == 0, c(+1, -1), sign(i[[3]]))
                      data = dplyr::mutate(.data = data,
                                           criteria = ifelse(criteria == "OUT" & slope %in% crit_slope & crit_min <= tide & tide <= crit_max, "BEST",
                                                             ifelse(criteria == "OUT" & !(slope %in% crit_slope) & crit_min <= tide & tide <= crit_max, "IN", criteria)))
                    }
                  }
                  return(data)},
                period = period, criteria = criteria)

  if(!plot){
    data = lapply(X = data, FUN = function(data, start, end){
      return(dplyr::filter(.data = data, start <= time & time <= end))
    },
    start = period[[1]], end = period[[2]])

    return(data)
  }

  if(night){
    start = period[[1]]
    end = period[[2]]
    night = tibble::tibble(x = seq.POSIXt(from = start, to = end, by = "24 hours")) %>%
      dplyr::mutate(x1 = x - lubridate::hours(6),
                    x2 = x + lubridate::hours(6),
                    x1 = as.POSIXct(ifelse(x1 <= start, start, x1), origin = "1970/01/01"),
                    x2 = as.POSIXct(ifelse(x2 >= end, end, x2), origin = "1970/01/01"),
                    x = NULL,
                    grp = dplyr::row_number()) %>%
      tidyr::pivot_longer(cols = c("x1", "x2"), values_to = "x", names_to = NULL) %>%
      dplyr::bind_rows(., .) %>%
      dplyr::arrange(grp, x) %>%
      dplyr::mutate(y = rep(c(-10, 10, 10, -10), nrow(.)/4))
  }


  plot = lapply(X = data,
                FUN = function(X, data, period, night){
                  station = unique(X$station)
                  start = period[[1]]
                  end = period[[2]]
                  period = seq.POSIXt(from = start, to = end, by = "12 hours")

                  min = min(X$tide[start <= X$time & X$time <= end])
                  max = max(X$tide[start <= X$time & X$time <= end])
                  range = (max - min)*0.05

                  plot = ggplot2::ggplot(data = X, mapping = ggplot2::aes(x = time, y = tide, color = criteria, group = 1))+
                    ggplot2::labs(y = "Predicted Tide (mCD)", x = "Time (Marked at 12:00 nn HKT)",
                                  title = paste0("Predicted Tide at ", station))+
                    ggplot2::scale_color_manual(breaks = c("BEST", "IN", "OUT"),
                                                values = c("#37A660", "#0000FF", "#FF0000"))+
                    ggplot2::scale_y_continuous(breaks = seq(-10, 10, 0.5),
                                                minor_breaks = seq(-10, 10, 0.1))+
                    ggplot2::scale_x_datetime(breaks = period,
                                              minor_breaks = seq.POSIXt(from = start, to = end, by = "2 hours"),
                                              labels = ifelse(lubridate::hour(period) == 0,
                                                              "",
                                                              format(period, format = "%Y/%m/%d\n%a")))+
                    ggplot2::coord_cartesian(xlim = c(start, end),
                                             ylim = c(min - range, max + range),
                                             expand = FALSE)+
                    ggplot2::theme_bw()+
                    ggplot2::theme(legend.position = "none", axis.ticks = ggplot2::element_line(linewidth = 1),
                                   axis.title = ggplot2::element_text(size = 12),
                                   axis.text = ggplot2::element_text(size = 10),
                                   panel.grid.major = ggplot2::element_line(linewidth = 1,
                                                                            colour = "#CCCCCC"),
                                   panel.grid.minor = ggplot2::element_line(linewidth = 1,
                                                                            linetype = "31",
                                                                            colour = "#DDDDDD"))+
                    ggplot2::geom_line(linewidth = 1)

                  if(is.data.frame(night)){
                    plot = plot +
                      ggplot2::geom_polygon(data = night,
                                            mapping = ggplot2::aes(x = x, y = y, group = grp),
                                            alpha = 0.25, inherit.aes = FALSE)
                  }
                  return(plot)
                }, period = period, night = night)

  return(plot)
}
