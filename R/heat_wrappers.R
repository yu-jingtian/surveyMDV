# R/heat_wrappers.R
# Exported user-facing plotting wrappers built on the core.
# IMPORTANT UPDATE: drop year columns where either policy is missing for that year,
# and warn the user which years were dropped.

#' Multiyear (or single-year) heatmap grid for a policy pair
#'
#' Creates a grid of heatmaps comparing \code{years} (columns) across subgroup panels (rows),
#' for a pair of policy dimensions. Works for either raw scores (\code{type = "raw"}) or
#' RF-predicted scores (\code{type = "rf"}).
#'
#' If a year has no data for either requested policy (i.e., the relevant column is all \code{NA}
#' for that year), the entire year column is removed from the plot and a warning is issued.
#'
#' @param years Integer vector of years (2014--2021).
#' @param policy_x,policy_y Policy variable names. For \code{type="raw"}, use raw names
#'   (e.g., \code{"abortion"}). For \code{type="rf"}, you may pass base names (e.g., \code{"abortion"})
#'   or suffixed names (e.g., \code{"abortion_rf"}); base names will be converted to \code{*_rf}.
#' @param type One of \code{"rf"} or \code{"raw"}.
#' @param panels A panel specification list, e.g. \code{panels_partisan()},
#'   \code{panels_rep_decompose()}, \code{panels_dem_decompose()}, \code{panels_ind_decompose()}.
#' @param scale One of \code{"across_years"} (default) or \code{"within_year"}.
#' @param scale_ref Scaling reference: \code{"population"} (default) or \code{"panel"}.
#' @param scale_ref_panel Integer panel index used when \code{scale_ref="panel"} (default 1).
#' @param breaks Numeric breaks used for RF binning (ignored for raw). Default is \code{seq(0,1,0.05)}.
#'
#' @return A \code{GGally} \code{ggmatrix} object.
#' @export
plot_policy_heatgrid <- function(years,
                                 policy_x,
                                 policy_y,
                                 type = c("rf", "raw"),
                                 panels = panels_partisan(),
                                 scale = c("across_years", "within_year"),
                                 scale_ref = c("population", "panel"),
                                 scale_ref_panel = 1L,
                                 breaks = seq(0, 1, by = 0.05)) {
  type  <- match.arg(type,  c("rf", "raw"))
  scale <- match.arg(scale, c("across_years", "within_year"))
  scale_ref <- match.arg(scale_ref, c("population", "panel"))

  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop("Package 'GGally' is required. Install it with install.packages('GGally').", call. = FALSE)
  }

  years <- as.integer(years)
  if (any(is.na(years))) stop("`years` must be coercible to integer.", call. = FALSE)

  # Resolve policy column names
  policy_vec <- .resolve_policy_names(type, policy_x, policy_y)

  # Load/merge per-year data (initial)
  df_year_list <- lapply(years, function(y) .prepare_year_df(y, type = type))

  # Determine which years are valid (policy exists in that year reference data)
  # We drop a year if either policy column is ALL NA for that year (population reference).
  drop_reason <- character(0)
  keep <- rep(TRUE, length(years))

  for (i in seq_along(years)) {
    df_y <- df_year_list[[i]]

    # if columns are missing entirely, treat as missing
    missing_cols <- setdiff(policy_vec, names(df_y))
    if (length(missing_cols) > 0) {
      keep[i] <- FALSE
      drop_reason <- c(drop_reason,
                       paste0(years[i], ": missing column(s) ", paste(missing_cols, collapse = ", ")))
      next
    }

    x <- df_y[[policy_vec[1]]]
    y <- df_y[[policy_vec[2]]]

    miss_x <- all(is.na(x))
    miss_y <- all(is.na(y))

    if (miss_x || miss_y) {
      keep[i] <- FALSE
      which_missing <- c()
      if (miss_x) which_missing <- c(which_missing, policy_vec[1])
      if (miss_y) which_missing <- c(which_missing, policy_vec[2])
      drop_reason <- c(drop_reason,
                       paste0(years[i], ": all NA for ", paste(which_missing, collapse = ", ")))
    }
  }

  if (!all(keep)) {
    warning(
      "Dropping year(s) with missing policy data:\n- ",
      paste(drop_reason, collapse = "\n- "),
      call. = FALSE
    )
  }

  years <- years[keep]
  df_year_list <- df_year_list[keep]

  if (length(years) == 0) {
    stop("All requested years were dropped because at least one policy is missing (all NA).", call. = FALSE)
  }

  # Compute scaling specs from the chosen reference and remaining years only
  scale_specs <- .compute_scale_specs(
    df_year_list     = df_year_list,
    panels           = panels,
    type             = type,
    policy_vec       = policy_vec,
    scale            = scale,
    scale_ref        = scale_ref,
    scale_ref_panel  = scale_ref_panel,
    breaks           = breaks
  )

  # Draw all panels (row-major ordering for ggmatrix)
  n_panels <- length(panels)
  n_years  <- length(years)
  p_list <- vector("list", n_panels * n_years)
  k <- 1

  for (r in seq_len(n_panels)) {
    for (c in seq_len(n_years)) {
      df_rc <- .apply_panel_filter(df_year_list[[c]], panels[[r]])
      p_list[[k]] <- .draw_panel(
        df = df_rc,
        type = type,
        policy_vec = policy_vec,
        scale_spec = scale_specs[[r]][[c]],
        breaks = breaks
      )
      k <- k + 1
    }
  }

  GGally::ggmatrix(
    plots = p_list,
    nrow  = n_panels,
    ncol  = n_years,
    yAxisLabels = vapply(panels, function(p) p$label, character(1)),
    xAxisLabels = years,
    showStrips  = NULL
  ) +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = 16, face = "bold"),
      strip.text.y = ggplot2::element_text(size = 8,  face = "bold")
    )
}

#' Single-year RF heatmap with top/right marginals
#'
#' Draws one RF heatmap for a single year and a single population subgroup, with
#' marginal histograms (binned bar charts) for the x and y scores placed on the top
#' and right side of the heatmap.
#'
#' This function is intentionally RF-only. It is meant as a presentation-style figure
#' to complement the multi-year heatmap grid.
#'
#' @param year A single integer year.
#' @param policy_x,policy_y Policy variable names. You may pass base names
#'   (e.g. \code{"guns_pred"}) or suffixed RF names (e.g. \code{"guns_pred_rf"});
#'   base names will be converted to \code{*_rf}.
#' @param panel Optional single panel specification list with elements \code{label}
#'   and \code{filter}. Use \code{NULL} (default) for the whole population.
#' @param breaks Numeric breaks used for RF binning. Default is \code{seq(0, 1, by = 0.05)}.
#' @param title Optional custom title. Default uses year and panel label.
#'
#' @return A combined \code{ggplot}/patchwork object.
#' @export
plot_policy_heat_single_rf <- function(year,
                                       policy_x,
                                       policy_y,
                                       panel = NULL,
                                       breaks = seq(0, 1, by = 0.05),
                                       title = NULL) {
  year <- as.integer(year)
  if (length(year) != 1 || is.na(year)) {
    stop("`year` must be a single integer.", call. = FALSE)
  }

  policy_vec <- .resolve_policy_names("rf", policy_x, policy_y)
  df <- .prepare_year_df(year, type = "rf")

  if (!is.null(panel)) {
    if (!is.list(panel) || is.null(panel$filter)) {
      stop("`panel` must be NULL or a single panel specification with elements `label` and `filter`.", call. = FALSE)
    }
    df <- .apply_panel_filter(df, panel)
    panel_label <- if (!is.null(panel$label)) panel$label else "Custom panel"
  } else {
    panel_label <- "Population"
  }

  .check_required_cols(df, policy_vec, context = "single RF plot")
  if (all(is.na(df[[policy_vec[1]]])) || all(is.na(df[[policy_vec[2]]]))) {
    stop("At least one requested RF policy column is entirely NA for this year/panel.", call. = FALSE)
  }

  if (is.null(title)) {
    title <- paste0(year, " - ", panel_label)
  }
  subtitle <- paste0(policy_vec[1], " vs ", policy_vec[2])

  .draw_single_rf_heatmap_with_marginals(
    df = df,
    policy_vec = policy_vec,
    breaks = breaks,
    title = title,
    subtitle = subtitle
  )
}
