# R/heat_core.R
# Internal (non-exported) helpers for data prep, scaling, and drawing single panels.

# ---- utilities ----

.is_null_or_empty <- function(x) is.null(x) || length(x) == 0

.resolve_policy_names <- function(type, policy_x, policy_y) {
  type <- match.arg(type, c("raw", "rf"))
  if (type == "raw") return(c(policy_x, policy_y))

  normalize_one <- function(x) {
    x <- as.character(x)
    x <- sub("_pred_rf$", "", x)
    x <- sub("_pred$", "", x)
    x <- sub("_rf$", "", x)
    paste0(x, "_rf")
  }

  c(normalize_one(policy_x), normalize_one(policy_y))
}

.check_required_cols <- function(df, cols, context = "") {
  missing <- setdiff(cols, names(df))
  if (length(missing) > 0) {
    stop("Missing column(s) in ", context, ": ", paste(missing, collapse = ", "), call. = FALSE)
  }
}

.usable_xy <- function(df, policy_vec) {
  x <- df[[policy_vec[1]]]
  y <- df[[policy_vec[2]]]
  !is.na(x) & !is.na(y)
}

.blank_panel <- function() {
  ggplot2::ggplot() +
    ggplot2::theme(
      legend.position = "none",
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::coord_equal()
}


.pretty_policy_label <- function(x) {
  key <- as.character(x)
  key <- sub("_pred_rf$", "", key)
  key <- sub("_pred$", "", key)
  key <- sub("_rf$", "", key)

  lab_map <- c(
    guns = "Gun control",
    enviro = "Environment",
    abortion = "Abortion",
    immig = "Immigration",
    gay = "Gay rights",
    health = "Healthcare",
    taxes = "Taxes",
    educ = "Education",
    race = "Race relations",
    welfare = "Welfare"
  )

  if (!is.na(lab_map[key])) return(unname(lab_map[key]))

  parts <- strsplit(gsub("_+", " ", key), " ")[[1]]
  parts <- parts[nzchar(parts)]
  parts <- paste0(toupper(substring(parts, 1, 1)), substring(parts, 2))
  paste(parts, collapse = " ")
}

# ---- data prep ----

.prepare_year_df <- function(year, type) {
  year <- as.integer(year)
  type <- match.arg(type, c("raw", "rf"))

  if (type == "raw") {
    pol <- get_policy_raw(year = year, cols = NULL)
  } else {
    pol <- get_policy_rf(year = year, cols = NULL)
  }
  demo <- get_demographics(year = year, cols = NULL)

  # base join (keeps package lightweight; no need for dplyr here)
  merge(pol, demo, by = c("case_id", "year"), all = FALSE)
}

.apply_panel_filter <- function(df, panel) {
  if (is.null(panel$filter)) return(df)
  panel$filter(df)
}

# ---- scaling ----
# scale specs always computed from the REFERENCE dataset only (population or a chosen panel),
# then re-used for all subgroup panels.

.compute_scale_spec_rf <- function(df, policy_vec, breaks = seq(0, 1, by = 0.05)) {
  .check_required_cols(df, policy_vec, context = "RF scaling")

  x <- df[[policy_vec[1]]]
  y <- df[[policy_vec[2]]]

  # structural missing for the reference
  if (all(is.na(x)) || all(is.na(y))) {
    return(list(max_freq = NA_integer_, total = 0L, empty = TRUE))
  }

  ok <- !is.na(x) & !is.na(y)
  if (!any(ok)) {
    return(list(max_freq = NA_integer_, total = 0L, empty = TRUE))
  }

  x_cut <- cut(x[ok], breaks = breaks, include.lowest = TRUE, right = TRUE)
  y_cut <- cut(y[ok], breaks = breaks, include.lowest = TRUE, right = TRUE)

  ft <- as.data.frame(table(x_cut, y_cut))

  list(
    max_freq = max(ft$Freq, na.rm = TRUE),
    total    = sum(ft$Freq, na.rm = TRUE),
    empty    = FALSE
  )
}

.compute_scale_spec_raw <- function(df, policy_vec) {
  .check_required_cols(df, policy_vec, context = "Raw scaling")

  x <- df[[policy_vec[1]]]
  y <- df[[policy_vec[2]]]

  if (all(is.na(x)) || all(is.na(y))) {
    return(list(max_freq = NA_integer_, total = 0L, empty = TRUE))
  }

  ok <- !is.na(x) & !is.na(y)
  if (!any(ok)) {
    return(list(max_freq = NA_integer_, total = 0L, empty = TRUE))
  }

  fx <- factor(x[ok])
  fy <- factor(y[ok])

  ft <- as.data.frame(table(fx, fy))

  list(
    max_freq = max(ft$Freq, na.rm = TRUE),
    total    = sum(ft$Freq, na.rm = TRUE),
    empty    = FALSE
  )
}

.compute_scale_specs <- function(df_year_list, panels, type,
                                 policy_vec,
                                 scale = c("across_years", "within_year"),
                                 scale_ref = c("population", "panel"),
                                 scale_ref_panel = 1L,
                                 breaks = seq(0, 1, by = 0.05)) {
  type  <- match.arg(type,  c("raw", "rf"))
  scale <- match.arg(scale, c("across_years", "within_year"))
  scale_ref <- match.arg(scale_ref, c("population", "panel"))

  n_panels <- length(panels)
  n_years  <- length(df_year_list)

  specs <- vector("list", n_panels)
  for (r in seq_len(n_panels)) specs[[r]] <- vector("list", n_years)

  compute_one <- function(df) {
    if (type == "rf") .compute_scale_spec_rf(df, policy_vec, breaks = breaks)
    else             .compute_scale_spec_raw(df, policy_vec)
  }

  get_ref_df <- function(df_year) {
    if (scale_ref == "population") return(df_year)
    idx <- as.integer(scale_ref_panel)
    if (is.na(idx) || idx < 1 || idx > length(panels)) {
      stop("Invalid scale_ref_panel index.", call. = FALSE)
    }
    .apply_panel_filter(df_year, panels[[idx]])
  }

  if (scale == "within_year") {
    # One reference-derived scale per YEAR, reused for all subgroup panels in that year
    for (c in seq_len(n_years)) {
      ref_df   <- get_ref_df(df_year_list[[c]])
      ref_spec <- compute_one(ref_df)
      for (r in seq_len(n_panels)) specs[[r]][[c]] <- ref_spec
    }
    return(specs)
  }

  # across_years (FIXED):
  # Compute the reference scale per year, then take the max across years.
  # Do NOT pool rows across years (that inflates counts and washes out colors).
  ref_specs_by_year <- vector("list", n_years)
  for (c in seq_len(n_years)) {
    ref_df <- get_ref_df(df_year_list[[c]])
    .check_required_cols(ref_df, policy_vec, context = "scale reference (across_years)")
    ref_specs_by_year[[c]] <- compute_one(ref_df)
  }

  # Keep only non-empty years
  nonempty <- vapply(ref_specs_by_year, function(s) !is.null(s$empty) && !isTRUE(s$empty), logical(1))
  if (!any(nonempty)) {
    global_spec <- list(max_freq = NA_integer_, total = 0L, empty = TRUE)
  } else {
    max_freqs <- vapply(ref_specs_by_year[nonempty], function(s) s$max_freq, numeric(1))
    totals    <- vapply(ref_specs_by_year[nonempty], function(s) s$total,    numeric(1))

    global_spec <- list(
      max_freq = as.integer(max(max_freqs, na.rm = TRUE)),
      # For RF tiles, total isn't used; for raw points, this keeps sizing stable.
      # Using max(total) is a safe cross-year reference (avoids pooled inflation).
      total    = as.integer(max(totals, na.rm = TRUE)),
      empty    = FALSE
    )
  }

  # Reuse the same global spec for every panel-row and year-column
  for (c in seq_len(n_years)) {
    for (r in seq_len(n_panels)) specs[[r]][[c]] <- global_spec
  }

  specs
}


# ---- panel drawing ----

.draw_panel_rf <- function(df, policy_vec, scale_spec,
                           breaks = seq(0, 1, by = 0.05),
                           low = "#c6dbef", mid = "#F4B811", high = "#CC2929") {
  .check_required_cols(df, policy_vec, context = "RF panel drawing")

  # subgroup may be empty even if year is valid
  ok <- .usable_xy(df, policy_vec)
  if (!any(ok)) return(.blank_panel())

  if (!is.null(scale_spec$empty) && isTRUE(scale_spec$empty)) return(.blank_panel())
  if (is.null(scale_spec$max_freq) || is.na(scale_spec$max_freq) || scale_spec$max_freq <= 0) return(.blank_panel())

  x_cut <- cut(df[[policy_vec[1]]][ok], breaks = breaks, include.lowest = TRUE, right = TRUE)
  y_cut <- cut(df[[policy_vec[2]]][ok], breaks = breaks, include.lowest = TRUE, right = TRUE)

  ft <- as.data.frame(table(x_cut, y_cut))
  names(ft) <- c("col1", "col2", "Freq")
  ft$col1 <- as.character(ft$col1)
  ft$col2 <- as.character(ft$col2)

  ggplot2::ggplot(ft, ggplot2::aes(x = col1, y = col2, fill = Freq)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_gradient2(
      midpoint = 0.5 * scale_spec$max_freq,
      limits   = c(0, scale_spec$max_freq),
      low = low, mid = mid, high = high
    ) +
    ggplot2::theme(
      legend.position = "none",
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::coord_equal()
}

.draw_panel_raw <- function(df, policy_vec, scale_spec,
                            low = "#c6dbef", mid = "#F4B811", high = "#CC2929",
                            total_size = 12) {
  .check_required_cols(df, policy_vec, context = "Raw panel drawing")

  ok <- .usable_xy(df, policy_vec)
  if (!any(ok)) return(.blank_panel())

  if (!is.null(scale_spec$empty) && isTRUE(scale_spec$empty)) return(.blank_panel())
  if (is.null(scale_spec$total) || scale_spec$total <= 0) return(.blank_panel())
  if (is.null(scale_spec$max_freq) || is.na(scale_spec$max_freq) || scale_spec$max_freq <= 0) return(.blank_panel())

  fx <- factor(df[[policy_vec[1]]][ok])
  fy <- factor(df[[policy_vec[2]]][ok])

  ft <- as.data.frame(table(fx, fy))
  names(ft) <- c("col1", "col2", "Freq")
  ft$prop <- ft$Freq / scale_spec$total
  ft$prop_scaled <- sqrt(ft$prop) * total_size

  ggplot2::ggplot(ft, ggplot2::aes(x = col1, y = col2, color = Freq, size = prop_scaled)) +
    ggplot2::geom_point() +
    ggplot2::scale_color_gradient2(
      midpoint = 0.5 * scale_spec$max_freq,
      low = low, mid = mid, high = high
    ) +
    ggplot2::scale_size_continuous(range = c(0, max(ft$prop_scaled, na.rm = TRUE))) +
    ggplot2::theme(
      legend.position = "none",
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "lightgrey"),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    ) +
    ggplot2::coord_equal()
}

.draw_panel <- function(df, type, policy_vec, scale_spec, breaks = seq(0, 1, by = 0.05)) {
  type <- match.arg(type, c("raw", "rf"))
  if (type == "rf") .draw_panel_rf(df, policy_vec, scale_spec, breaks = breaks)
  else              .draw_panel_raw(df, policy_vec, scale_spec)
}

# ---- single RF heatmap + marginals ----

.format_break_labels <- function(breaks) {
  mids <- (breaks[-1] + breaks[-length(breaks)]) / 2
  format(round(mids, 2), nsmall = 2, trim = TRUE)
}

.single_rf_counts <- function(df, policy_vec, breaks = seq(0, 1, by = 0.05)) {
  .check_required_cols(df, policy_vec, context = "single RF heatmap")

  ok <- .usable_xy(df, policy_vec)
  if (!any(ok)) return(NULL)

  x <- df[[policy_vec[1]]][ok]
  y <- df[[policy_vec[2]]][ok]

  x_cut <- cut(x, breaks = breaks, include.lowest = TRUE, right = TRUE)
  y_cut <- cut(y, breaks = breaks, include.lowest = TRUE, right = TRUE)

  x_levels <- levels(x_cut)
  y_levels <- levels(y_cut)
  x_mids <- (breaks[-1] + breaks[-length(breaks)]) / 2
  y_mids <- (breaks[-1] + breaks[-length(breaks)]) / 2
  bin_w <- diff(breaks)[1]

  heat <- as.data.frame(table(x_cut, y_cut), stringsAsFactors = FALSE)
  names(heat) <- c("x_bin", "y_bin", "Freq")
  heat$x_bin <- factor(heat$x_bin, levels = x_levels, ordered = TRUE)
  heat$y_bin <- factor(heat$y_bin, levels = y_levels, ordered = TRUE)
  heat$x_mid <- x_mids[match(heat$x_bin, x_levels)]
  heat$y_mid <- y_mids[match(heat$y_bin, y_levels)]

  list(
    heat = heat,
    x = x,
    y = y,
    n = sum(ok),
    bin_w = bin_w
  )
}

.draw_single_rf_heatmap_with_marginals <- function(df,
                                                   policy_vec,
                                                   breaks = seq(0, 1, by = 0.05),
                                                   title = NULL,
                                                   subtitle = NULL,
                                                   low = "#c6dbef",
                                                   mid = "#F4B811",
                                                   high = "#CC2929") {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    stop("Package 'patchwork' is required. Install it with install.packages('patchwork').", call. = FALSE)
  }

  counts <- .single_rf_counts(df, policy_vec, breaks = breaks)
  if (is.null(counts)) {
    stop("No non-missing observations available for the requested year/panel/policies.", call. = FALSE)
  }

  heat_df <- counts$heat
  max_freq <- max(heat_df$Freq, na.rm = TRUE)
  x_lab <- .pretty_policy_label(policy_vec[1])
  y_lab <- .pretty_policy_label(policy_vec[2])
  rng <- range(breaks)
  bin_w <- counts$bin_w

  p_heat <- ggplot2::ggplot(heat_df, ggplot2::aes(x = x_mid, y = y_mid, fill = Freq)) +
    ggplot2::geom_tile(width = bin_w, height = bin_w, color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_gradient2(
      midpoint = 0.5 * max_freq,
      limits   = c(0, max_freq),
      low = low, mid = mid, high = high,
      name = "Count"
    ) +
    ggplot2::scale_x_continuous(limits = rng, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = rng, expand = c(0, 0)) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.margin = ggplot2::margin(5.5, 5.5, 0, 0)
    ) +
    ggplot2::labs(
      title = title,
      subtitle = subtitle
    ) +
    ggplot2::coord_equal()

  p_left <- ggplot2::ggplot(df, ggplot2::aes(x = y)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ..density..),
      breaks = breaks,
      fill = "grey70",
      color = "grey35",
      linewidth = 0.3
    ) +
    ggplot2::geom_density(linewidth = 0.6, na.rm = TRUE) +
    ggplot2::coord_flip() +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    ggplot2::labs(x = NULL, y = y_lab) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 5.5, r = 5.5, b = 0, l = 5.5)
    )

  p_bottom <- ggplot2::ggplot(df, ggplot2::aes(x = x)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ..density..),
      breaks = breaks,
      fill = "grey70",
      color = "grey35",
      linewidth = 0.3
    ) +
    ggplot2::geom_density(linewidth = 0.6, na.rm = TRUE) +
    ggplot2::scale_x_continuous(
      limits = c(0, 1),
      expand = c(0, 0)
    ) +
    ggplot2::scale_y_reverse() +
    ggplot2::labs(x = x_lab, y = NULL) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(t = 0, r = 5.5, b = 5.5, l = 5.5)
    )

    p_comb <- patchwork::wrap_plots(
    patchwork::plot_spacer(), p_heat,
    p_left, p_bottom,
    ncol = 2,
    widths = c(1.15, 4),
    heights = c(4, 1.15),
    guides = "collect"
  )

  p_comb <- p_comb + patchwork::plot_annotation(
    theme = ggplot2::theme(
      legend.position = "right"
    )
  )

  p_comb
}
