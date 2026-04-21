#' Get raw policy scores (2014–2021)
#'
#' Loads and returns the raw policy score table, optionally filtered by year and
#' reduced to selected columns.
#'
#' @param year Integer vector of survey years to keep. Default \code{NULL} keeps all years (2014--2021).
#' @param cols Character vector of column names to return. Default \code{NULL} returns all columns.
#'   Note: \code{case_id} and \code{year} are always included to support joins.
#'
#' @return A data frame containing raw policy scores.
#' @export
get_policy_raw <- function(year = NULL, cols = NULL) {
  data("policy_raw", package = "surveyMDV", envir = environment())

  df <- policy_raw

  if (!is.null(year)) {
    year <- as.integer(year)
    df <- df[df$year %in% year, , drop = FALSE]
  }

  if (!is.null(cols)) {
    cols <- unique(as.character(cols))
    keep <- unique(c("case_id", "year", cols))
    missing_cols <- setdiff(keep, names(df))
    if (length(missing_cols) > 0) {
      stop("Unknown column(s) in `cols`: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    df <- df[, keep, drop = FALSE]
  }

  if (nrow(df) == 0) {
    stop("No rows returned. Check `year` (available: 2014–2021) and your filters.", call. = FALSE)
  }

  df
}

.get_policy_pred <- function(dataset_name, year = NULL, cols = NULL) {
  utils::data(list = dataset_name, package = "surveyMDV", envir = environment())
  df <- get(dataset_name, envir = environment())

  if (!is.null(year)) {
    year <- as.integer(year)
    df <- df[df$year %in% year, , drop = FALSE]
  }

  if (!is.null(cols)) {
    cols <- unique(as.character(cols))
    keep <- unique(c("case_id", "year", cols))
    missing_cols <- setdiff(keep, names(df))
    if (length(missing_cols) > 0) {
      stop("Unknown column(s) in `cols`: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    df <- df[, keep, drop = FALSE]
  }

  if (nrow(df) == 0) {
    stop("No rows returned. Check `year` (available: 2014–2021) and your filters.", call. = FALSE)
  }

  df
}

#' Get RF-predicted policy scores (2014–2021)
#' @param year Integer vector of survey years to keep.
#' @param cols Character vector of column names to return.
#' @return A data frame containing RF-predicted policy scores.
#' @export
get_policy_rf <- function(year = NULL, cols = NULL) {
  .get_policy_pred("policy_rf", year = year, cols = cols)
}

#' Get XGB-predicted policy scores (2014–2021)
#' @param year Integer vector of survey years to keep.
#' @param cols Character vector of column names to return.
#' @return A data frame containing XGB-predicted policy scores.
#' @export
get_policy_xgb <- function(year = NULL, cols = NULL) {
  .get_policy_pred("policy_xgb", year = year, cols = cols)
}

#' Get LM-predicted policy scores (2014–2021)
#' @param year Integer vector of survey years to keep.
#' @param cols Character vector of column names to return.
#' @return A data frame containing LM-predicted policy scores.
#' @export
get_policy_lm <- function(year = NULL, cols = NULL) {
  .get_policy_pred("policy_lm", year = year, cols = cols)
}

#' Get SVR-predicted policy scores (2014–2021)
#' @param year Integer vector of survey years to keep.
#' @param cols Character vector of column names to return.
#' @return A data frame containing SVR-predicted policy scores.
#' @export
get_policy_svr <- function(year = NULL, cols = NULL) {
  .get_policy_pred("policy_svr", year = year, cols = cols)
}

#' Get respondent demographics (2014–2021)
#'
#' Loads and returns the demographics table, optionally filtered by year and
#' reduced to selected columns.
#'
#' @param year Integer vector of survey years to keep. Default \code{NULL} keeps all years (2014--2021).
#' @param cols Character vector of column names to return. Default \code{NULL} returns all columns.
#'   Note: \code{case_id} and \code{year} are always included to support joins.
#'
#' @return A data frame containing respondent demographics and weights.
#' @export
get_demographics <- function(year = NULL, cols = NULL) {
  data("demographics", package = "surveyMDV", envir = environment())

  df <- demographics

  if (!is.null(year)) {
    year <- as.integer(year)
    df <- df[df$year %in% year, , drop = FALSE]
  }

  if (!is.null(cols)) {
    cols <- unique(as.character(cols))
    keep <- unique(c("case_id", "year", cols))
    missing_cols <- setdiff(keep, names(df))
    if (length(missing_cols) > 0) {
      stop("Unknown column(s) in `cols`: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    df <- df[, keep, drop = FALSE]
  }

  if (nrow(df) == 0) {
    stop("No rows returned. Check `year` (available: 2014–2021) and your filters.", call. = FALSE)
  }

  df
}
