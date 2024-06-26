#' Download high/ low predicted tidal height data, using the year 2015 method
#'
#' @param station Station to download. Check the `dict_hk_tide` for more information
#' @param year Year to download
#'
#' @keywords internal
#'
#' @examples N/A
load_hk_tidePredict_2015HL = function(station,
                                      year){
  URL = paste0("https://www.hko.gov.hk/tide/e", station, "text", year, ".html")
  data1 = readLines(URL) %>%
    tibble::as_tibble() %>%
    dplyr::slice((match(x = "    Date      Time Height(m)  Time Height(m)  Time Height(m)  Time Height(m)",
                        table = value)+1):
                   (match(x = "<DIV id=wcag_logo_area>", table = value)-7)) %>%
    dplyr::mutate(value = stringr::str_remove_all(string = value, pattern = "_"),
                  value = stringr::str_remove_all(string = value, pattern = "Date|Time|Height|\\(m\\)"),
                  value = trimws(value)) %>%
    dplyr::bind_rows(tibble::tibble(value = "MM DD t1 h1 t2 h2 t3 h3 t4 h4"),.) %>%
    .$value %>%
    I() %>%
    readr::read_table() %>%
    suppressWarnings() %>%
    dplyr::mutate(t1 = ISOdatetime(year, MM, DD, substr(t1, 1, 2), substr(t1, 3, 4), 00, tz = "HongKong"),
                  t2 = ISOdatetime(year, MM, DD, substr(t2, 1, 2), substr(t2, 3, 4), 00, tz = "HongKong"),
                  t3 = ISOdatetime(year, MM, DD, substr(t3, 1, 2), substr(t3, 3, 4), 00, tz = "HongKong"),
                  t4 = ISOdatetime(year, MM, DD, substr(t4, 1, 2), substr(t4, 3, 4), 00, tz = "HongKong"))
  data2 = dplyr::bind_rows(dplyr::select(data1, time = t1, tide = h1),
                           dplyr::select(data1, time = t2, tide = h2),
                           dplyr::select(data1, time = t3, tide = h3),
                           dplyr::select(data1, time = t4, tide = h4)) %>%
    dplyr::mutate(station = station) %>%
    dplyr::relocate(time, station, tide) %>%
    dplyr::arrange(time) %>%
    tidyr::drop_na()
  return(data2)
}

#' Download hourly predicted tidal height data, using the year 2015 method
#'
#' @param station Station to download. Check the `dict_hk_tide` for more information
#' @param year Year to download
#'
#' @keywords internal
#'
#' @examples N/A
load_hk_tidePredict_2015HR = function(station,
                                      year){
  URL = paste0("https://www.hko.gov.hk/tide/", station, "textPH", year, ".htm")
  data = readLines(URL) %>%
    tibble::as_tibble() %>%
    dplyr::slice((match(x = '<TABLE cellSpacing=5 cellPadding=5 width="100%" align=center border=0>', table = value)+6):
                   (match(x = '<DIV id=wcag_logo_area>', table = value)-7)) %>%
    dplyr::mutate(value = stringr::str_remove_all(value, '_|Date|Hour'),
                  value = stringr::str_remove_all(value, 'MM  DD     01     02     03     04     05     06     07     08     09     10     11     12     13     14     15     16     17     18     19     20     21     22     23     24 '),
                  value = trimws(value)) %>%
    dplyr::bind_rows(tibble::tibble(value = "MM DD 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24"), .) %>%
    .$value %>%
    I() %>%
    readr::read_table() %>%
    suppressWarnings() %>%
    tidyr::pivot_longer(cols = -c("MM", "DD"), names_to = "HH", values_to = "tide") %>%
    dplyr::mutate(time = ISOdatetime(year, MM, DD, HH, 00, 00, tz = "HongKong"),
                  station = station) %>%
    dplyr::select(time, station, tide) %>%
    dplyr::relocate(time, station, tide) %>%
    dplyr::arrange(time) %>%
    tidyr::drop_na()

  return(data)
}

#' Download high/ low predicted tidal height data, using the year 2020 method
#'
#' @param station Station to download. Check the `dict_hk_tide` for more information
#' @param year Year to download
#'
#' @keywords internal
#'
#' @examples N/A
load_hk_tidePredict_2020HL = function(station, year){
  URL = paste0("https://www.hko.gov.hk/tide/e", station, "text", year, ".html")

  data1 = readLines(URL)%>%
    tibble::as_tibble() %>%
    dplyr::slice((match(x = "<TR><TH COLSPAN='2'>Date</TH><TH>Time</TH><TH>Height(m)</TH><TH>Time</TH><TH>Height(m)</TH><TH>Time</TH><TH>Height(m)</TH><TH>Time</TH><TH>Height(m)</TH></TR>",
                        table = value)+2):
                   (match(x = "<DIV id=wcag_logo_area>",
                          table = value)-6)) %>%
    dplyr::mutate(value = stringr::str_replace_all(value, pattern = "<TR>|<TD>|</TD>|</TR>|&nbsp;", replacement = " "),
                  value = stringr::str_remove_all(value, pattern = "<TBODY>|</TBODY>|<TABLE>|</TABLE>|<TH>|</TH>"),
                  value = stringr::str_remove_all(value, pattern = "<br><TABLE align=center border=0 width=580>"),
                  value = stringr::str_remove_all(value, pattern = "<TH COLSPAN='2'>|Date|Time|Height"),
                  value = stringr::str_remove_all(value, pattern = '\\(m\\)'),
                  value = trimws(value)) %>%
    dplyr::bind_rows(tibble::tibble(value = c("MM DD t1 h1 t2 h2 t3 h3 t4 h4")),.) %>%
    .$value %>%
    I() %>%
    readr::read_table() %>%
    suppressWarnings() %>%
    dplyr::mutate(t1 = ISOdatetime(year, MM, DD, substr(t1, 1, 2), substr(t1, 3, 4), 00, tz = "HongKong"),
                  t2 = ISOdatetime(year, MM, DD, substr(t2, 1, 2), substr(t2, 3, 4), 00, tz = "HongKong"),
                  t3 = ISOdatetime(year, MM, DD, substr(t3, 1, 2), substr(t3, 3, 4), 00, tz = "HongKong"),
                  t4 = ISOdatetime(year, MM, DD, substr(t4, 1, 2), substr(t4, 3, 4), 00, tz = "HongKong"))
  data2 = dplyr::bind_rows(dplyr::select(data1, time = t1, tide = h1),
                           dplyr::select(data1, time = t2, tide = h2),
                           dplyr::select(data1, time = t3, tide = h3),
                           dplyr::select(data1, time = t4, tide = h4)) %>%
    dplyr::mutate(station = station) %>%
    tidyr::drop_na() %>%
    dplyr::relocate(time, station, tide) %>%
    dplyr::arrange(time) %>%
    tidyr::drop_na()
  return(data2)
}
#' Download hourly predicted tidal height data, using the year 2020 method
#'
#' @param station Station to download. Check the `hk_dict_tide` for more information
#' @param year Year to download
#'
#' @keywords internal
#'
#' @examples N/A
load_hk_tidePredict_2020HR = function(station,
                                      year){
  URL = paste0("https://www.hko.gov.hk/tide/", station, "textPH", year, ".htm")
  data = readLines(URL) %>%
    tibble::as_tibble() %>%
    dplyr::slice((match("<TR><TH COLSPAN='2'>Date</TH><TH>&nbsp;</TH><TH COLSPAN='24'>Hour</TH></TR>", .$value)+4):
                   (match('    <a href="http://www.w3.org/WAI/WCAG2AA-Conformance" target="_blank">', .$value)-7)) %>%
    dplyr::mutate(value = stringr::str_replace_all(string = value, pattern = '<TR>|<TD>|</TD>|</TR>', replacement = ' '),
                  value = stringr::str_remove_all(string = value, pattern = '</TBODY>|</TABLE>|<TBODY>|</TABLE>'),
                  value = stringr::str_remove_all(string = value, pattern = "<TH COLSPAN='2'>Date</TH><TH>&nbsp;</TH><TH COLSPAN='24'>Hour</TH>"),
                  value = stringr::str_remove_all(string = value, pattern = '<br><TABLE align=center border=0 width=1100>|<br><TABLE align=center border=0 width=1100>'),
                  value = stringr::str_remove_all(string = value, pattern = '<TH>MM</TH><TH>DD</TH><TH>&nbsp;</TH><TH>01</TH><TH>02</TH><TH>03</TH><TH>04</TH><TH>05</TH><TH>06</TH><TH>07</TH><TH>08</TH><TH>09</TH><TH>10</TH><TH>11</TH><TH>12</TH><TH>13</TH><TH>14</TH><TH>15</TH><TH>16</TH><TH>17</TH><TH>18</TH><TH>19</TH><TH>20</TH><TH>21</TH><TH>22</TH><TH>23</TH><TH>24</TH>'),
                  value = trimws(value)) %>%
    tidyr::drop_na()%>%
    dplyr::bind_rows(tibble::tibble(value = "MM DD 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24"), .) %>%
    .$value %>%
    I() %>%
    readr::read_table() %>%
    suppressWarnings() %>%
    tidyr::pivot_longer(cols = -c("MM", "DD"),
                        names_to = "HH",
                        values_to = "tide") %>%
    dplyr::mutate(time = ISOdatetime(year, MM, DD, HH, 00, 00, tz = ""),
                  station = station) %>%
    dplyr::select(time, station, tide) %>%
    dplyr::relocate(time, station, tide) %>%
    tidyr::drop_na()
  return(data)
}
