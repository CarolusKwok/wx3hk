#' @title Download (all) available HK data
#'
#' @description
#' This function downloads all of the available HK data present in this package `wx3hk`. To understand what data is available, please check `?wx3hk::load_hk`
#'
#' @param worker (integer) Number of sessions to be open to download a list of files. Files will be downloaded simultaneously.
#' @param list_fail (logical) List failed-to-download items.
#' * TRUE for listing all failed-to-download items. (DEFAULT)
#' * FALSE for not listing all failed-to-download items.
#'
#' @return Data downloaded from most functions under `?wx3hk::load_hk`
#' @export
#'
#' @examples wx3hk::load_hk_all()
load_hk_all = function(worker = 1L, list_fail = TRUE){
  wx3hk::load_hk_gtmp(list_fail = list_fail, worker = worker)
  wx3hk::load_hk_ltng(list_fail = list_fail, worker = worker)
  wx3hk::load_hk_mslp(list_fail = list_fail, worker = worker)
  wx3hk::load_hk_rain(list_fail = list_fail, worker = worker)
  wx3hk::load_hk_rainDy(list_fail = list_fail, worker = worker)
  wx3hk::load_hk_rainHr(list_fail = list_fail, worker = worker)
  wx3hk::load_hk_rhum(list_fail = list_fail, worker = worker)
  wx3hk::load_hk_sart(list_fail = list_fail, worker = worker)
  wx3hk::load_hk_satl(list_fail = list_fail, worker = worker)
  wx3hk::load_hk_temp(list_fail = list_fail, worker = worker)
  wx3hk::load_hk_tide(list_fail = list_fail, worker = worker)
  wx3hk::load_hk_tideSnap(list_fail = list_fail, worker = worker)
  wx3hk::load_hk_wind(list_fail = list_fail, worker = worker)
}
