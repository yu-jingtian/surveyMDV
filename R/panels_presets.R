# R/panels_presets.R
# Panel preset generators used by the high-level plotting functions.

#' Preset panels: Population plus three party groups
#'
#' Returns the standard four-panel set: Population, Republican, Independent,
#' and Democrat. Most users can call \code{plot_policy_partisan_grid()} or
#' \code{plot_policy_heatrow_year()} directly instead of using this helper.
#'
#' @return A list of panel specifications, each with elements \code{label} and \code{filter}.
#' @export
panels_partisan <- function() {
  list(
    list(label = "Population", filter = function(df) df),
    list(label = "Republican", filter = function(df) df[df$partisan == "Rep.", , drop = FALSE]),
    list(label = "Independent", filter = function(df) df[df$partisan == "Ind.", , drop = FALSE]),
    list(label = "Democrat",   filter = function(df) df[df$partisan == "Dem.", , drop = FALSE])
  )
}

.party_label <- function(party) {
  if (party == "Rep.") return("Rep.")
  if (party == "Dem.") return("Dem.")
  if (party == "Ind.") return("Ind.")
  party
}

.group_to_party_code <- function(group) {
  key <- tolower(trimws(as.character(group)))

  if (key %in% c("republican", "republicans", "rep", "rep.")) return("Rep.")
  if (key %in% c("democrat", "democrats", "dem", "dem.")) return("Dem.")
  if (key %in% c("independent", "independents", "ind", "ind.")) return("Ind.")
  if (key %in% c("population", "overall", "all")) return(NULL)

  stop(
    "Unknown group: ", group,
    ". Supported values are 'population', 'republican', 'independent', and 'democrat'.",
    call. = FALSE
  )
}

.gender_is_female <- function(x) x %in% c("Female", "Woman", "Women")
.gender_is_male   <- function(x) x %in% c("Male", "Men", "Man")

.panels_population_decompose <- function() {
  list(
    list(label = "Population", filter = function(df) df),
    list(label = "Big Metro", filter = function(df) df[df$rural_urban %in% c(1), , drop = FALSE]),
    list(label = "Other Co.", filter = function(df) df[df$rural_urban %in% c(2,3,4,5,6,7,8,9), , drop = FALSE]),
    list(label = "Non-college", filter = function(df) df[df$educ %in% c(1,2,3), , drop = FALSE]),
    list(label = "College", filter = function(df) df[df$educ %in% c(4,5,6), , drop = FALSE]),
    list(label = "Female", filter = function(df) df[.gender_is_female(df$gender), , drop = FALSE]),
    list(label = "Male", filter = function(df) df[.gender_is_male(df$gender), , drop = FALSE])
  )
}

#' Preset panels: population-level demographic decomposition
#'
#' Returns panels for Population, metro status, college status, and gender.
#' Most users can call \code{plot_policy_subgroup_grid(group = "population")} directly.
#'
#' @return A list of panel specifications.
#' @export
panels_population_decompose <- function() {
  .panels_population_decompose()
}

.panels_party_decompose <- function(party) {
  lab <- .party_label(party)

  list(
    list(
      label = paste0(lab),
      filter = function(df) df[df$partisan == party, , drop = FALSE]
    ),
    list(
      label = paste0(lab, " Big Metro"),
      filter = function(df) df[df$partisan == party & df$rural_urban %in% c(1), , drop = FALSE]
    ),
    list(
      label = paste0(lab, " Other Co."),
      filter = function(df) df[df$partisan == party & df$rural_urban %in% c(2,3,4,5,6,7,8,9), , drop = FALSE]
    ),
    list(
      label = paste0(lab, " Non-college"),
      filter = function(df) df[df$partisan == party & df$educ %in% c(1,2,3), , drop = FALSE]
    ),
    list(
      label = paste0(lab, " College"),
      filter = function(df) df[df$partisan == party & df$educ %in% c(4,5,6), , drop = FALSE]
    ),
    list(
      label = paste0(lab, " Female"),
      filter = function(df) df[df$partisan == party & .gender_is_female(df$gender), , drop = FALSE]
    ),
    list(
      label = paste0(lab, " Male"),
      filter = function(df) df[df$partisan == party & .gender_is_male(df$gender), , drop = FALSE]
    )
  )
}

.panels_for_group_decompose <- function(group) {
  party <- .group_to_party_code(group)
  if (is.null(party)) return(.panels_population_decompose())
  .panels_party_decompose(party)
}

#' Preset panels: Republican decomposition
#'
#' @return A list of panel specifications.
#' @export
panels_rep_decompose <- function() {
  .panels_party_decompose("Rep.")
}

#' Preset panels: Democrat decomposition
#'
#' @return A list of panel specifications.
#' @export
panels_dem_decompose <- function() {
  .panels_party_decompose("Dem.")
}

#' Preset panels: Independent decomposition
#'
#' @return A list of panel specifications.
#' @export
panels_ind_decompose <- function() {
  .panels_party_decompose("Ind.")
}
