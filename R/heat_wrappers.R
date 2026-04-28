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

  policy_vec <- .resolve_policy_names(type, policy_x, policy_y)
  df_year_list <- lapply(years, function(y) .prepare_year_df(y, type = type))

  drop_reason <- character(0)
  keep <- rep(TRUE, length(years))

  for (i in seq_along(years)) {
    df_y <- df_year_list[[i]]
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

#' Single-year RF heatmap with marginals
#'
#' Draw a single-year RF heatmap for one policy pair, with a left y-marginal
#' and a bottom x-marginal.
#'
#' @param year Integer year.
#' @param policy_x,policy_y RF policy names. You may pass base names
#'   like "guns" and "enviro", or suffixed names like "guns_rf".
#' @param subgroup1 Optional first subgroup label, e.g. "Rep.", "college", "metro".
#' @param subgroup2 Optional second subgroup label for an interaction, e.g. "Rep." and "college".
#' @param panel Optional panel specification for backward compatibility. When supplied,
#'   it overrides `subgroup1` and `subgroup2`.
#' @param breaks RF score bin edges. Default seq(0,1,0.05).
#' @param title Optional custom title.
#'
#' @return A grid grob.
#' @export
plot_policy_heat_single <- function(year,
                                       policy_x,
                                       policy_y,
                                       model = c("rf", "xgb", "lm", "svr"),
                                       subgroup1 = NULL,
                                       subgroup2 = NULL,
                                       panel = NULL,
                                       breaks = seq(0, 1, by = 0.05),
                                       title = NULL) {
  year <- as.integer(year)
  if (length(year) != 1 || is.na(year)) {
    stop("`year` must be a single integer.", call. = FALSE)
  }

  model <- match.arg(model)
  policy_vec <- .resolve_policy_names("pred", policy_x, policy_y, model = model)
  df <- .prepare_year_df(year, type = "pred", model = model)

  if (!is.null(panel)) {
    if (!is.list(panel)) {
      stop("`panel` must be NULL or a panel specification list.", call. = FALSE)
    }
    df <- .apply_panel_filter(df, panel)
    panel_label <- .panel_label(panel)
  } else {
    subgroup_spec <- .resolve_subgroup_filters(subgroup1, subgroup2)
    df <- subgroup_spec$filter(df)
    panel_label <- subgroup_spec$label
  }

  .check_required_cols(df, policy_vec, context = "single RF plot")
  if (nrow(df) == 0) {
    stop("No observations remain after applying the requested subgroup filter(s).", call. = FALSE)
  }
  if (all(is.na(df[[policy_vec[1]]])) || all(is.na(df[[policy_vec[2]]]))) {
    stop("At least one requested RF policy column is entirely NA for this year/panel.", call. = FALSE)
  }

  if (is.null(title)) {
    title <- paste0(year, ", ", panel_label, ", Model: ", toupper(model))
  }

  .draw_single_rf_heatmap_with_marginals(
    df = df,
    policy_vec = policy_vec,
    breaks = breaks,
    title = title,
    subtitle = NULL
  )
}


# Backward-compatible alias
plot_policy_heat_single_rf <- plot_policy_heat_single
