#' Raw policy preference scores by year (2014--2021)
#'
#' A dataset of survey-based raw policy preference scores covering survey years
#' 2014 through 2021. Each row corresponds to one respondent identified by
#' \code{case_id} in one survey \code{year}. The policy scores are normalized
#' to the \code{[0, 1]} interval.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{case_id}{Unique respondent identifier used for merging across datasets.}
#'   \item{year}{Integer survey year (2014--2021).}
#'   \item{immig}{Raw immigration policy preference score, normalized to \code{[0, 1]}.}
#'   \item{enviro}{Raw environment policy preference score, normalized to \code{[0, 1]}.}
#'   \item{abortion}{Raw abortion policy preference score, normalized to \code{[0, 1]}.}
#'   \item{guns}{Raw gun policy preference score, normalized to \code{[0, 1]}.}
#'   \item{healthcare}{Raw healthcare policy preference score, normalized to \code{[0, 1]}.}
#'   \item{military}{Raw military policy preference score, normalized to \code{[0, 1]}.}
#'   \item{spending}{Raw government spending policy preference score, normalized to \code{[0, 1]}.}
#'   \item{trade}{Raw trade policy preference score, normalized to \code{[0, 1]}.}
#' }
#'
#' @details
#' Join keys are \code{case_id} and \code{year}. The raw scores are intended
#' for descriptive raw-score visualizations; model-predicted scores are stored
#' separately in \code{\link{policy_rf}}, \code{\link{policy_xgb}},
#' \code{\link{policy_lm}}, and \code{\link{policy_svr}}.
"policy_raw"

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
