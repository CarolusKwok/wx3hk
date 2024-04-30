#' @title
#' HK/MOLoad standard check
#'
#' @description
#' Check if items within the HKLoad or MOLoad are valid
#'
#' @param time time within the function
#' @param list_fail list_fail within the function
#' @param attempt attempt within the function
#' @param worker worker within the function
#'
#' @return NA
#' @keywords internal
#'
#' @examples sys_ckf_HKLoad(time, list_fail, attempt, worker)
sys_ckf_HKLoad = function(time,
                          list_fail,
                          attempt,
                          worker){
  if(rlang::is_missing(time)){call::abort_NoArg(time)}
  if(rlang::is_missing(list_fail)){call::abort_NoArg(list_fail)}
  if(rlang::is_missing(attempt)){call::abort_NoArg(attempt)}
  if(rlang::is_missing(worker)){call::abort_NoArg(worker)}

  if(!lubridate::is.POSIXct(time)){call::abort_WrongClass(x = time, class = "POSIXct")}
  if(!is.logical(list_fail)){call::abort_WrongClass(x = list_fail, class = "logical")}
  if(!is.integer(attempt)){call::abort_WrongClass(x = attempt, class = "integer")}
  if(!is.integer(worker)){call::abort_WrongClass(x = worker, class = "integer")}
}

#' @title
#' HK/MOLoad language standard check
#'
#' @description
#' Check if language is supported
#'
#' @param lan lan within the function
#'
#' @return NA
#' @keywords internal
#'
#' @examples sys_ckf_HKLoadLan(lan)
sys_ckf_HKLoadLan = function(lan){
  if(rlang::is_missing(lan)){call::abort_NoArg(lan)}
  if(length(lan) != 1){call::abort_WrongLength(x = lan, length = 1L)}
  if(!(lan %in% c("tc", "sc", "en"))){
    call::abort(message = c("x" = "{.arg lan} should be of the following value",
                                  "*" = "tc",
                                  "*" = "sc",
                                  "*" = "en",
                                  "i" = "Please enter one of the above"))
    }
}
