#' Random-forest predicted policy scores by year (2014--2021)
#'
#' Predicted policy preference scores from the RF model.
#'
#' @format A data frame with \code{case_id}, \code{year}, and policy columns
#' suffixed with \code{_rf}.
"policy_rf"

#' XGB predicted policy scores by year (2014--2021)
#'
#' Predicted policy preference scores from the XGB model.
#'
#' @format A data frame with \code{case_id}, \code{year}, and policy columns
#' suffixed with \code{_xgb}.
"policy_xgb"

#' Linear-model predicted policy scores by year (2014--2021)
#'
#' Predicted policy preference scores from the linear model.
#'
#' @format A data frame with \code{case_id}, \code{year}, and policy columns
#' suffixed with \code{_lm}.
"policy_lm"

#' SVR predicted policy scores by year (2014--2021)
#'
#' Predicted policy preference scores from the SVR model.
#'
#' @format A data frame with \code{case_id}, \code{year}, and policy columns
#' suffixed with \code{_svr}.
"policy_svr"

#' Respondent demographics and weights by year (2014--2021)
#'
#' A dataset of respondent-level demographics and survey weights covering
#' survey years 2014 through 2021. Each row corresponds to one respondent
#' identified by \code{case_id} in one survey \code{year}.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{case_id}{Unique respondent identifier used for merging across datasets.}
#'   \item{year}{Integer survey year (2014--2021).}
#'   \item{partisan}{Partisanship coding used in analyses.}
#'   \item{race}{Race/ethnicity coding used in analyses.}
#'   \item{gender}{Gender coding used in analyses.}
#'   \item{age}{Respondent age.}
#'   \item{educ}{Education coding used in analyses.}
#'   \item{rural_urban}{Rural/urban classification used in analyses.}
#'   \item{weight_cumulative}{Survey sample weight used.}
#' }
"demographics"
