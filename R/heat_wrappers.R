# R/heat_wrappers.R
# User-facing plotting functions for the four supported figure styles.

# ---- shared outer axes for heatmap grids ----

.heatgrid_x_axis_grob <- function(policy_name,
                                  title_size = .DEFAULT_AXIS_TITLE_SIZE,
                                  tick_size = .DEFAULT_AXIS_TICK_SIZE,
                                  line_col = "grey30",
                                  tick_pad = .DEFAULT_AXIS_TICK_PAD) {
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required.", call. = FALSE)
  }

  tick_labels <- .policy_tick_labels(policy_name)
  axis_title  <- .pretty_policy_label(policy_name)

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
                                  title_size = .DEFAULT_AXIS_TITLE_SIZE,
                                  tick_size = .DEFAULT_AXIS_TICK_SIZE,
                                  line_col = "grey30",
                                  tick_pad = .DEFAULT_AXIS_TICK_PAD) {
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required.", call. = FALSE)
  }

  tick_labels <- .policy_tick_labels(policy_name)
  axis_title  <- .pretty_policy_label(policy_name)

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
                                            axis_title_size = .DEFAULT_AXIS_TITLE_SIZE,
                                            axis_tick_size = .DEFAULT_AXIS_TICK_SIZE,
                                            bottom_axis_height = 0.85,
                                            left_axis_width = 1.15,
                                            axis_tick_pad = .DEFAULT_AXIS_TICK_PAD) {
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

  grid_plot_grob <- grid::grid.grabExpr(print(grid_plot))

  gridExtra::arrangeGrob(
    grobs = list(y_axis, grid_plot_grob, grid::nullGrob(), x_axis),
    layout_matrix = matrix(c(1, 2,
                             3, 4), nrow = 2, byrow = TRUE),
    widths = grid::unit.c(grid::unit(left_axis_width, "in"), grid::unit(1, "null")),
    heights = grid::unit.c(grid::unit(1, "null"), grid::unit(bottom_axis_height, "in"))
  )
}

# ---- internal grid engine ----

.plot_policy_heatgrid_engine <- function(years,
                                         policy_x,
                                         policy_y,
                                         model = c("rf", "xgb", "lm", "svr"),
                                         panels = panels_partisan(),
                                         scale = c("within_year", "across_years"),
                                         scale_ref = c("population", "panel"),
                                         scale_ref_panel = 1L,
                                         plot_type = "multi_year_grid") {
  model <- match.arg(model)
  scale <- match.arg(scale)
  scale_ref <- match.arg(scale_ref)

  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop("Package 'GGally' is required. Install it with install.packages('GGally').", call. = FALSE)
  }

  years <- as.integer(years)
  if (any(is.na(years))) stop("`years` must be coercible to integer.", call. = FALSE)
  years_requested <- years

  policy_vec <- .resolve_policy_names(model = model, policy_x = policy_x, policy_y = policy_y)
  df_year_list <- lapply(years, function(y) .prepare_year_df(y, model = model))

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
    policy_vec       = policy_vec,
    scale            = scale,
    scale_ref        = scale_ref,
    scale_ref_panel  = scale_ref_panel,
    breaks           = .DEFAULT_HEAT_BREAKS
  )

  n_panels <- length(panels)
  n_years  <- length(years)
  heatgrid_layout <- list(
    plot_type = plot_type,
    model = model,
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
        policy_vec = policy_vec,
        scale_spec = scale_specs[[r]][[c]],
        breaks = .DEFAULT_HEAT_BREAKS
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

  out <- .wrap_heatgrid_with_policy_axes(
    grid_plot = grid_plot,
    policy_vec = policy_vec
  )
  attr(out, "heatgrid_layout") <- heatgrid_layout
  out
}

# ---- save helpers ----

#' Recommended output size for a heatmap grid
#'
#' Computes a suggested \code{ggsave()} width and height from the number of
#' columns and rows in a plotted heatmap grid.
#'
#' @param plot A plot returned by a heatmap-grid function.
#' @param cell_width Width, in inches, allocated to each plot column.
#' @param cell_height Height, in inches, allocated to each plot row.
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
#' This is a thin wrapper around \code{ggplot2::ggsave()}.
#'
#' @param filename Output file name.
#' @param plot A plot returned by a heatmap-grid function.
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

# ---- user-facing plot styles ----

#' Single heatmap with marginal distributions
#'
#' Draws one model-predicted policy heatmap for a single year and group, with
#' marginal histograms and density curves.
#'
#' @param year Single integer year.
#' @param policy_x,policy_y Base policy names, such as \code{"immig"},
#'   \code{"trade"}, \code{"enviro"}, or \code{"guns"}. Suffixed names such as
#'   \code{"immig_rf"} are also accepted and will be converted to the selected model.
#' @param model Prediction model used for the plotted scores. One of
#'   \code{"rf"}, \code{"xgb"}, \code{"lm"}, or \code{"svr"}.
#' @param group Main group to plot. One of \code{"population"},
#'   \code{"republican"}, \code{"independent"}, or \code{"democrat"}.
#' @param subgroup Optional subgroup within \code{group}. Supported values include
#'   \code{"college"}, \code{"non-college"}, \code{"metro"}, \code{"non-metro"},
#'   \code{"female"}, and \code{"male"}.
#' @param title Optional custom title. If \code{NULL}, a default title is used.
#'
#' @return A grid grob.
#' @export
plot_policy_heat_single <- function(year,
                                    policy_x,
                                    policy_y,
                                    model = c("rf", "xgb", "lm", "svr"),
                                    group = "population",
                                    subgroup = NULL,
                                    title = NULL) {
  year <- as.integer(year)
  if (length(year) != 1 || is.na(year)) {
    stop("`year` must be a single integer.", call. = FALSE)
  }

  model <- match.arg(model)
  policy_vec <- .resolve_policy_names(model = model, policy_x = policy_x, policy_y = policy_y)
  df <- .prepare_year_df(year, model = model)

  group_spec <- .resolve_group_subgroup_filter(group = group, subgroup = subgroup)
  df <- group_spec$filter(df)
  panel_label <- group_spec$label

  .check_required_cols(df, policy_vec, context = "single heatmap")
  if (nrow(df) == 0) {
    stop("No observations remain after applying the requested group/subgroup filter.", call. = FALSE)
  }
  if (all(is.na(df[[policy_vec[1]]])) || all(is.na(df[[policy_vec[2]]]))) {
    stop("At least one requested policy column is entirely NA for this year/group.", call. = FALSE)
  }

  if (is.null(title)) {
    title <- paste0(year, ", ", panel_label, ", Model: ", toupper(model))
  }

  .draw_single_pred_heatmap_with_marginals(
    df = df,
    policy_vec = policy_vec,
    breaks = .DEFAULT_HEAT_BREAKS,
    title = title
  )
}

#' Heatmap row for one year
#'
#' Draws the one-row partisan decomposition for a single year. The columns are
#' Population, Republican, Independent, and Democrat.
#'
#' @param year Single integer year.
#' @param policy_x,policy_y Base policy names.
#' @param model Prediction model used for the plotted scores.
#'
#' @return A grob containing the heatmap row and shared policy axes.
#' @export
plot_policy_heatrow_year <- function(year,
                                     policy_x,
                                     policy_y,
                                     model = c("rf", "xgb", "lm", "svr")) {
  year <- as.integer(year)
  if (length(year) != 1 || is.na(year)) {
    stop("`year` must be a single integer.", call. = FALSE)
  }

  model <- match.arg(model)
  panels <- panels_partisan()

  if (!requireNamespace("GGally", quietly = TRUE)) {
    stop("Package 'GGally' is required. Install it with install.packages('GGally').", call. = FALSE)
  }

  policy_vec <- .resolve_policy_names(model = model, policy_x = policy_x, policy_y = policy_y)
  df_year <- .prepare_year_df(year, model = model)

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
    policy_vec       = policy_vec,
    scale            = "within_year",
    scale_ref        = "population",
    scale_ref_panel  = 1L,
    breaks           = .DEFAULT_HEAT_BREAKS
  )

  n_panels <- length(panels)
  p_list <- vector("list", n_panels)

  for (j in seq_len(n_panels)) {
    df_j <- .apply_panel_filter(df_year, panels[[j]])
    p_list[[j]] <- .draw_panel(
      df = df_j,
      policy_vec = policy_vec,
      scale_spec = scale_specs[[j]][[1]],
      breaks = .DEFAULT_HEAT_BREAKS
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
    model = model,
    year = year,
    years_requested = as.integer(year),
    years_kept = as.integer(year),
    drop_reason = character(0),
    n_years = 1L,
    n_panels = n_panels,
    n_cols = n_panels,
    n_rows = 1L
  )

  out <- .wrap_heatgrid_with_policy_axes(
    grid_plot = grid_plot,
    policy_vec = policy_vec
  )
  attr(out, "heatgrid_layout") <- heatgrid_layout
  out
}

#' Partisan decomposition grid
#'
#' Draws a multi-year grid with years as columns and the standard partisan
#' decomposition as rows: Population, Republican, Independent, and Democrat.
#'
#' @param years Integer vector of years.
#' @param policy_x,policy_y Base policy names.
#' @param model Prediction model used for the plotted scores.
#'
#' @return A grob containing the heatmap grid and shared policy axes.
#' @export
plot_policy_partisan_grid <- function(years,
                                      policy_x,
                                      policy_y,
                                      model = c("rf", "xgb", "lm", "svr")) {
  .plot_policy_heatgrid_engine(
    years = years,
    policy_x = policy_x,
    policy_y = policy_y,
    model = model,
    panels = panels_partisan(),
    scale = "within_year",
    scale_ref = "population",
    scale_ref_panel = 1L,
    plot_type = "partisan_grid"
  )
}

#' Subgroup-analysis grid
#'
#' Draws a multi-year subgroup analysis grid for the population or for one
#' partisan group. Years are columns; rows are the selected group followed by
#' metro, education, and gender subgroup panels.
#'
#' @param years Integer vector of years.
#' @param policy_x,policy_y Base policy names.
#' @param model Prediction model used for the plotted scores.
#' @param group Main group to decompose. One of \code{"population"},
#'   \code{"republican"}, \code{"independent"}, or \code{"democrat"}.
#'
#' @return A grob containing the heatmap grid and shared policy axes.
#' @export
plot_policy_subgroup_grid <- function(years,
                                      policy_x,
                                      policy_y,
                                      model = c("rf", "xgb", "lm", "svr"),
                                      group = c("population", "republican", "independent", "democrat")) {
  model <- match.arg(model)
  group <- match.arg(group)

  .plot_policy_heatgrid_engine(
    years = years,
    policy_x = policy_x,
    policy_y = policy_y,
    model = model,
    panels = .panels_for_group_decompose(group),
    scale = "within_year",
    scale_ref = "panel",
    scale_ref_panel = 1L,
    plot_type = "subgroup_grid"
  )
}

#' Advanced heatmap grid
#'
#' Draws a multi-year heatmap grid with a user-supplied panel specification.
#' Most users should prefer \code{plot_policy_partisan_grid()} or
#' \code{plot_policy_subgroup_grid()}.
#'
#' @param years Integer vector of years.
#' @param policy_x,policy_y Base policy names.
#' @param model Prediction model used for the plotted scores.
#' @param panels Panel specification list, such as \code{panels_partisan()}.
#'
#' @return A grob containing the heatmap grid and shared policy axes.
#' @export
plot_policy_heatgrid <- function(years,
                                 policy_x,
                                 policy_y,
                                 model = c("rf", "xgb", "lm", "svr"),
                                 panels = panels_partisan()) {
  .plot_policy_heatgrid_engine(
    years = years,
    policy_x = policy_x,
    policy_y = policy_y,
    model = model,
    panels = panels,
    scale = "within_year",
    scale_ref = "population",
    scale_ref_panel = 1L,
    plot_type = "advanced_grid"
  )
}

# Backward-compatible alias for old code that used the RF-specific name.
plot_policy_heat_single_rf <- plot_policy_heat_single
