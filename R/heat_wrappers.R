# R/heat_wrappers.R
# Exported user-facing plotting wrappers built on the core.
# IMPORTANT UPDATE: drop year columns where either policy is missing for that year,
# and warn the user which years were dropped.


# ---- shared outer axes for heatmap grids ----

.heatgrid_x_axis_grob <- function(policy_name,
                                  title_size = 16,
                                  tick_size = 9,
                                  line_col = "grey30",
                                  tick_pad = 0.06) {
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required.", call. = FALSE)
  }

  tick_labels <- .policy_tick_labels(policy_name)
  axis_title  <- .pretty_policy_label(policy_name)

  # Put the end tick labels slightly inside the grob. Otherwise the
  # left/right labels can be clipped when the full grid is saved.
  tick_pad <- max(0, min(as.numeric(tick_pad), 0.20))
  tick_x <- c(tick_pad, 0.5, 1 - tick_pad)

  grid::grobTree(
    grid::linesGrob(
      x = grid::unit(c(tick_pad, 1 - tick_pad), "npc"),
      y = grid::unit(c(0.74, 0.74), "npc"),
      gp = grid::gpar(col = line_col, lwd = 0.8)
    ),
    grid::segmentsGrob(
      x0 = grid::unit(tick_x, "npc"),
      x1 = grid::unit(tick_x, "npc"),
      y0 = grid::unit(0.74, "npc"),
      y1 = grid::unit(0.64, "npc"),
      gp = grid::gpar(col = line_col, lwd = 0.8)
    ),
    grid::textGrob(
      label = tick_labels,
      x = grid::unit(tick_x, "npc"),
      y = grid::unit(0.45, "npc"),
      just = "centre",
      gp = grid::gpar(fontsize = tick_size, col = "black")
    ),
    grid::textGrob(
      label = axis_title,
      x = grid::unit(0.5, "npc"),
      y = grid::unit(0.12, "npc"),
      just = "centre",
      gp = grid::gpar(fontsize = title_size, col = "black")
    )
  )
}

.heatgrid_y_axis_grob <- function(policy_name,
                                  title_size = 16,
                                  tick_size = 9,
                                  line_col = "grey30",
                                  tick_pad = 0.06) {
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required.", call. = FALSE)
  }

  tick_labels <- .policy_tick_labels(policy_name)
  axis_title  <- .pretty_policy_label(policy_name)

  # Put the end tick labels slightly inside the grob. Otherwise the
  # top/bottom labels can be clipped when the full grid is saved.
  tick_pad <- max(0, min(as.numeric(tick_pad), 0.20))
  tick_y <- c(tick_pad, 0.5, 1 - tick_pad)

  grid::grobTree(
    grid::linesGrob(
      x = grid::unit(c(0.74, 0.74), "npc"),
      y = grid::unit(c(tick_pad, 1 - tick_pad), "npc"),
      gp = grid::gpar(col = line_col, lwd = 0.8)
    ),
    grid::segmentsGrob(
      x0 = grid::unit(0.74, "npc"),
      x1 = grid::unit(0.64, "npc"),
      y0 = grid::unit(tick_y, "npc"),
      y1 = grid::unit(tick_y, "npc"),
      gp = grid::gpar(col = line_col, lwd = 0.8)
    ),
    grid::textGrob(
      label = tick_labels,
      x = grid::unit(0.45, "npc"),
      y = grid::unit(tick_y, "npc"),
      rot = 90,
      just = "centre",
      gp = grid::gpar(fontsize = tick_size, col = "black")
    ),
    grid::textGrob(
      label = axis_title,
      x = grid::unit(0.13, "npc"),
      y = grid::unit(0.5, "npc"),
      rot = 90,
      just = "centre",
      gp = grid::gpar(fontsize = title_size, col = "black")
    )
  )
}

.wrap_heatgrid_with_policy_axes <- function(grid_plot,
                                            policy_vec,
                                            axis_title_size = 16,
                                            axis_tick_size = 9,
                                            bottom_axis_height = 0.85,
                                            left_axis_width = 1.15,
                                            axis_tick_pad = 0.06) {
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required. Install it with install.packages('gridExtra').", call. = FALSE)
  }
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required.", call. = FALSE)
  }

  x_axis <- .heatgrid_x_axis_grob(
    policy_name = policy_vec[1],
    title_size = axis_title_size,
    tick_size = axis_tick_size,
    tick_pad = axis_tick_pad
  )
  y_axis <- .heatgrid_y_axis_grob(
    policy_name = policy_vec[2],
    title_size = axis_title_size,
    tick_size = axis_tick_size,
    tick_pad = axis_tick_pad
  )

  # GGally::ggmatrix is list-like, so gridExtra::arrangeGrob()
  # cannot use it directly as one grob. Capture the printed ggmatrix
  # first, then arrange the captured grob with the shared axes.
  grid_plot_grob <- grid::grid.grabExpr(print(grid_plot))

  gridExtra::arrangeGrob(
    grobs = list(y_axis, grid_plot_grob, grid::nullGrob(), x_axis),
    layout_matrix = matrix(c(1, 2,
                             3, 4), nrow = 2, byrow = TRUE),
    widths = grid::unit.c(grid::unit(left_axis_width, "in"), grid::unit(1, "null")),
    heights = grid::unit.c(grid::unit(1, "null"), grid::unit(bottom_axis_height, "in"))
  )
}

#' Recommended output size for a heatmap grid
#'
#' Computes a suggested \code{ggsave()} width and height from the number of
#' years that are actually kept after dropping years with missing policy data.
#'
#' @param plot A plot returned by \code{plot_policy_heatgrid()}.
#' @param cell_width Width, in inches, allocated to each retained year column.
#' @param cell_height Height, in inches, allocated to each panel row.
#' @param min_width,min_height Lower bounds for the suggested figure size.
#'
#' @return A named numeric vector with \code{width} and \code{height}.
#' @export
heatgrid_recommended_size <- function(plot,
                                      cell_width = 1.9,
                                      cell_height = 1.45,
                                      min_width = 5.5,
                                      min_height = 5.0) {
  layout <- attr(plot, "heatgrid_layout")

  if (is.null(layout)) {
    warning(
      "No `heatgrid_layout` attribute found. Returning a generic 8 x 6 inch size.",
      call. = FALSE
    )
    return(c(width = 8, height = 6))
  }

  # Older objects used n_years/n_panels. Newer objects also store
  # n_cols/n_rows so this helper works for both full grids and one-row grids.
  n_cols <- if (!is.null(layout$n_cols)) layout$n_cols else layout$n_years
  n_rows <- if (!is.null(layout$n_rows)) layout$n_rows else layout$n_panels

  width <- 1.15 + 0.40 + cell_width * n_cols + 0.30
  height <- 0.85 + 0.35 + cell_height * n_rows + 0.30

  c(
    width = max(min_width, width),
    height = max(min_height, height)
  )
}

#' Save a heatmap grid using its recommended output size
#'
#' This is a thin wrapper around \code{ggplot2::ggsave()} that uses the number
#' of retained years, rather than the number of requested years, to choose the
#' output dimensions.
#'
#' @param filename Output file name.
#' @param plot A plot returned by \code{plot_policy_heatgrid()}.
#' @param width,height Optional output dimensions. If omitted, dimensions are
#'   computed by \code{heatgrid_recommended_size(plot)}.
#' @param units Output units passed to \code{ggsave()}.
#' @param dpi Output resolution passed to \code{ggsave()}.
#' @param ... Additional arguments passed to \code{ggplot2::ggsave()}.
#'
#' @return Invisibly returns the size used.
#' @export
save_policy_heatgrid <- function(filename,
                                 plot,
                                 width = NULL,
                                 height = NULL,
                                 units = "in",
                                 dpi = 300,
                                 ...) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }

  size <- heatgrid_recommended_size(plot)
  if (is.null(width)) width <- unname(size["width"])
  if (is.null(height)) height <- unname(size["height"])

  ggplot2::ggsave(
    filename = filename,
    plot = plot,
    width = width,
    height = height,
    units = units,
    dpi = dpi,
    ...
  )

  invisible(c(width = width, height = height))
}


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
#' @param show_policy_axes Logical; if \code{TRUE}, add one shared outer x-axis and y-axis
#'   with policy-specific tick labels and policy titles.
#' @param policy_axis_title_size Font size for shared policy-axis titles.
#' @param policy_axis_tick_size Font size for shared policy-axis tick labels.
#' @param policy_axis_tick_pad Padding used to move edge tick labels inward and
#'   avoid clipping in the saved figure.
#'
#' @return A grob containing the heatmap grid and shared policy axes when
#'   \code{show_policy_axes = TRUE}; otherwise a \code{GGally} \code{ggmatrix} object.
#' @export
plot_policy_heatgrid <- function(years,
                                 policy_x,
                                 policy_y,
                                 type = c("rf", "raw"),
                                 panels = panels_partisan(),
                                 scale = c("across_years", "within_year"),
                                 scale_ref = c("population", "panel"),
                                 scale_ref_panel = 1L,
                                 breaks = seq(0, 1, by = 0.05),
                                 show_policy_axes = TRUE,
                                 policy_axis_title_size = 16,
                                 policy_axis_tick_size = 9,
                                 policy_axis_tick_pad = 0.06) {
  type  <- match.arg(type,  c("rf", "raw"))
  scale <- match.arg(scale, c("across_years", "within_year"))
  scale_ref <- match.arg(scale_ref, c("population", "panel"))

  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop("Package 'GGally' is required. Install it with install.packages('GGally').", call. = FALSE)
  }

  years <- as.integer(years)
  if (any(is.na(years))) stop("`years` must be coercible to integer.", call. = FALSE)
  years_requested <- years

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
  heatgrid_layout <- list(
    plot_type = "multi_year_grid",
    years_requested = as.integer(years_requested),
    years_kept = as.integer(years),
    drop_reason = drop_reason,
    n_years = n_years,
    n_panels = n_panels,
    n_cols = n_years,
    n_rows = n_panels
  )

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

  grid_plot <- GGally::ggmatrix(
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

  if (isTRUE(show_policy_axes)) {
    out <- .wrap_heatgrid_with_policy_axes(
      grid_plot = grid_plot,
      policy_vec = policy_vec,
      axis_title_size = policy_axis_title_size,
      axis_tick_size = policy_axis_tick_size,
      axis_tick_pad = policy_axis_tick_pad
    )
    attr(out, "heatgrid_layout") <- heatgrid_layout
    return(out)
  }

  attr(grid_plot, "heatgrid_layout") <- heatgrid_layout
  grid_plot
}


#' Single-year heatmap row for a policy pair
#'
#' Creates a one-row heatmap grid for a single year. By default, the row shows
#' Population, Republican, Independent, and Democrat panels. This is equivalent
#' to taking one year-column from \code{plot_policy_heatgrid()} and laying the
#' panels out horizontally.
#'
#' The plot has no main title and no year strip. Panel labels are shown as
#' column strips above each subplot. Shared outer policy-axis titles and
#' policy-specific tick labels follow the same rule as \code{plot_policy_heatgrid()}.
#'
#' @param year Single integer year.
#' @param policy_x,policy_y Policy variable names. For \code{type="raw"}, use raw names
#'   (e.g., \code{"abortion"}). For \code{type="rf"}, you may pass base names
#'   (e.g., \code{"abortion"}) or suffixed names (e.g., \code{"abortion_rf"}).
#' @param type One of \code{"rf"} or \code{"raw"}.
#' @param panels A panel specification list. Default is \code{panels_partisan()}.
#' @param scale_ref Scaling reference: \code{"population"} (default) or \code{"panel"}.
#' @param scale_ref_panel Integer panel index used when \code{scale_ref="panel"}.
#' @param breaks Numeric breaks used for RF binning (ignored for raw). Default is \code{seq(0,1,0.05)}.
#' @param show_policy_axes Logical; if \code{TRUE}, add one shared outer x-axis and y-axis
#'   with policy-specific tick labels and policy titles.
#' @param policy_axis_title_size Font size for shared policy-axis titles.
#' @param policy_axis_tick_size Font size for shared policy-axis tick labels.
#' @param policy_axis_tick_pad Padding used to move edge tick labels inward and
#'   avoid clipping in the saved figure.
#'
#' @return A grob containing the heatmap row and shared policy axes when
#'   \code{show_policy_axes = TRUE}; otherwise a \code{GGally} \code{ggmatrix} object.
#' @export
plot_policy_heatrow_year <- function(year,
                                     policy_x,
                                     policy_y,
                                     type = c("rf", "raw"),
                                     panels = panels_partisan(),
                                     scale_ref = c("population", "panel"),
                                     scale_ref_panel = 1L,
                                     breaks = seq(0, 1, by = 0.05),
                                     show_policy_axes = TRUE,
                                     policy_axis_title_size = 16,
                                     policy_axis_tick_size = 9,
                                     policy_axis_tick_pad = 0.06) {
  type <- match.arg(type, c("rf", "raw"))
  scale_ref <- match.arg(scale_ref, c("population", "panel"))

  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop("Package 'GGally' is required. Install it with install.packages('GGally').", call. = FALSE)
  }

  year <- as.integer(year)
  if (length(year) != 1 || is.na(year)) {
    stop("`year` must be a single integer.", call. = FALSE)
  }

  policy_vec <- .resolve_policy_names(type, policy_x, policy_y)
  df_year <- .prepare_year_df(year, type = type)

  missing_cols <- setdiff(policy_vec, names(df_year))
  if (length(missing_cols) > 0) {
    stop(
      "Missing column(s) for requested policies in year ", year, ": ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  miss_x <- all(is.na(df_year[[policy_vec[1]]]))
  miss_y <- all(is.na(df_year[[policy_vec[2]]]))
  if (miss_x || miss_y) {
    which_missing <- c()
    if (miss_x) which_missing <- c(which_missing, policy_vec[1])
    if (miss_y) which_missing <- c(which_missing, policy_vec[2])
    stop(
      "At least one requested policy column is entirely NA for year ", year, ": ",
      paste(which_missing, collapse = ", "),
      call. = FALSE
    )
  }

  scale_specs <- .compute_scale_specs(
    df_year_list     = list(df_year),
    panels           = panels,
    type             = type,
    policy_vec       = policy_vec,
    scale            = "within_year",
    scale_ref        = scale_ref,
    scale_ref_panel  = scale_ref_panel,
    breaks           = breaks
  )

  n_panels <- length(panels)
  p_list <- vector("list", n_panels)

  for (j in seq_len(n_panels)) {
    df_j <- .apply_panel_filter(df_year, panels[[j]])
    p_list[[j]] <- .draw_panel(
      df = df_j,
      type = type,
      policy_vec = policy_vec,
      scale_spec = scale_specs[[j]][[1]],
      breaks = breaks
    )
  }

  panel_labels <- vapply(panels, function(p) p$label, character(1))

  grid_plot <- GGally::ggmatrix(
    plots = p_list,
    nrow  = 1,
    ncol  = n_panels,
    xAxisLabels = panel_labels,
    yAxisLabels = NULL,
    showStrips  = NULL
  ) +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = 14, face = "bold"),
      strip.text.y = ggplot2::element_blank()
    )

  heatgrid_layout <- list(
    plot_type = "single_year_row",
    year = year,
    years_requested = as.integer(year),
    years_kept = as.integer(year),
    drop_reason = character(0),
    n_years = 1L,
    n_panels = n_panels,
    n_cols = n_panels,
    n_rows = 1L
  )

  if (isTRUE(show_policy_axes)) {
    out <- .wrap_heatgrid_with_policy_axes(
      grid_plot = grid_plot,
      policy_vec = policy_vec,
      axis_title_size = policy_axis_title_size,
      axis_tick_size = policy_axis_tick_size,
      axis_tick_pad = policy_axis_tick_pad
    )
    attr(out, "heatgrid_layout") <- heatgrid_layout
    return(out)
  }

  attr(grid_plot, "heatgrid_layout") <- heatgrid_layout
  grid_plot
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
