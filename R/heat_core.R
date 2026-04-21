# R/heat_core.R
# Internal (non-exported) helpers for data prep, scaling, and drawing panels.

# ---- utilities ----

.is_null_or_empty <- function(x) is.null(x) || length(x) == 0

.resolve_policy_names <- function(type, policy_x, policy_y, model = NULL) {
  type <- match.arg(type, c("raw", "rf", "xgb", "lm", "svr", "pred"))
  if (type == "raw") return(c(policy_x, policy_y))

  if (type == "pred") {
    if (is.null(model)) model <- "rf"
    model <- match.arg(model, c("rf", "xgb", "lm", "svr"))
  } else {
    model <- type
  }

  normalize_one <- function(x) {
    x <- as.character(x)
    x <- sub("_pred_(rf|xgb|lm|svr)$", "", x)
    x <- sub("_pred$", "", x)
    x <- sub("_(rf|xgb|lm|svr)$", "", x)
    paste0(x, "_", model)
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
  x0 <- gsub("_pred_(rf|xgb|lm|svr)$|_pred$|_(rf|xgb|lm|svr)$", "", x)

  lab_map <- c(
    guns = "Gun control",
    enviro = "Environment",
    environment = "Environment",
    abortion = "Abortion",
    immig = "Immigration",
    immigration = "Immigration",
    healthcare = "Healthcare",
    military = "Military",
    spending = "Spending",
    trade = "Trade"
  )

  if (x0 %in% names(lab_map)) return(unname(lab_map[[x0]]))

  x0 <- gsub("_", " ", x0)
  paste0(toupper(substr(x0, 1, 1)), substr(x0, 2, nchar(x0)))
}

# ---- data prep ----

.prepare_year_df <- function(year, type, model = NULL) {
  year <- as.integer(year)
  type <- match.arg(type, c("raw", "rf", "xgb", "lm", "svr", "pred"))

  if (type == "pred") {
    if (is.null(model)) model <- "rf"
    type <- match.arg(model, c("rf", "xgb", "lm", "svr"))
  }

  if (type == "raw") {
    pol <- get_policy_raw(year = year, cols = NULL)
  } else if (type == "rf") {
    pol <- get_policy_rf(year = year, cols = NULL)
  } else if (type == "xgb") {
    pol <- get_policy_xgb(year = year, cols = NULL)
  } else if (type == "lm") {
    pol <- get_policy_lm(year = year, cols = NULL)
  } else {
    pol <- get_policy_svr(year = year, cols = NULL)
  }
  demo <- get_demographics(year = year, cols = NULL)

  merge(pol, demo, by = c("case_id", "year"), all = FALSE)
}

.apply_panel_filter <- function(df, panel) {
  if (is.null(panel$filter)) return(df)
  panel$filter(df)
}


.panel_label <- function(panel) {
  if (is.null(panel)) return("Population")

  one_label <- function(p) {
    if (is.list(p) && !is.null(p$label)) return(as.character(p$label))
    "Custom panel"
  }

  if (!is.null(panel$filter)) return(one_label(panel))

  if (is.list(panel)) {
    labs <- vapply(panel, one_label, character(1))
    labs <- labs[nzchar(labs)]
    if (length(labs) == 0) return("Custom panel")
    return(paste(labs, collapse = " "))
  }

  "Custom panel"
}

.resolve_single_subgroup_filter <- function(subgroup) {
  if (is.null(subgroup)) {
    return(list(label = "Population", filter = function(df) df))
  }

  key <- trimws(as.character(subgroup))
  key_low <- tolower(key)

  if (key_low %in% c("population", "all", "overall")) {
    return(list(label = "Population", filter = function(df) df))
  }
  if (key_low %in% c("rep", "rep.", "republican", "republicans")) {
    return(list(label = "Rep.", filter = function(df) df[df$partisan == "Rep.", , drop = FALSE]))
  }
  if (key_low %in% c("dem", "dem.", "democrat", "democrats")) {
    return(list(label = "Dem.", filter = function(df) df[df$partisan == "Dem.", , drop = FALSE]))
  }
  if (key_low %in% c("ind", "ind.", "independent", "independents")) {
    return(list(label = "Ind.", filter = function(df) df[df$partisan == "Ind.", , drop = FALSE]))
  }
  if (key_low %in% c("college", "college+", "college educated")) {
    return(list(label = "College", filter = function(df) df[df$educ %in% c(4, 5, 6), , drop = FALSE]))
  }
  if (key_low %in% c("non-college", "noncollege", "no college")) {
    return(list(label = "Non-college", filter = function(df) df[df$educ %in% c(1, 2, 3), , drop = FALSE]))
  }
  if (key_low %in% c("metro", "big metro", "urban")) {
    return(list(label = "Big Metro", filter = function(df) df[df$rural_urban %in% c(1), , drop = FALSE]))
  }
  if (key_low %in% c("non-metro", "nonmetro", "other co.", "other county", "rural")) {
    return(list(label = "Other Co.", filter = function(df) df[df$rural_urban %in% c(2,3,4,5,6,7,8,9), , drop = FALSE]))
  }
  if (key_low %in% c("female", "woman", "women")) {
    return(list(label = "Female", filter = function(df) df[.gender_is_female(df$gender), , drop = FALSE]))
  }
  if (key_low %in% c("male", "man", "men")) {
    return(list(label = "Male", filter = function(df) df[.gender_is_male(df$gender), , drop = FALSE]))
  }

  stop(
    "Unknown subgroup: ", subgroup,
    ". Supported values include 'Rep.', 'Dem.', 'Ind.', 'college', 'non-college', 'metro', 'non-metro', 'female', 'male'.",
    call. = FALSE
  )
}

.resolve_subgroup_filters <- function(subgroup1 = NULL, subgroup2 = NULL) {
  s1 <- .resolve_single_subgroup_filter(subgroup1)

  if (is.null(subgroup2)) {
    return(list(label = s1$label, filter = s1$filter))
  }

  s2 <- .resolve_single_subgroup_filter(subgroup2)

  list(
    label = paste(s1$label, s2$label),
    filter = function(df) {
      out <- s1$filter(df)
      s2$filter(out)
    }
  )
}

# ---- scaling ----

.compute_scale_spec_rf <- function(df, policy_vec, breaks = seq(0, 1, by = 0.05)) {
  .check_required_cols(df, policy_vec, context = "RF scaling")

  x <- df[[policy_vec[1]]]
  y <- df[[policy_vec[2]]]

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
    else               .compute_scale_spec_raw(df, policy_vec)
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
    for (c in seq_len(n_years)) {
      ref_df   <- get_ref_df(df_year_list[[c]])
      ref_spec <- compute_one(ref_df)
      for (r in seq_len(n_panels)) specs[[r]][[c]] <- ref_spec
    }
    return(specs)
  }

  ref_specs_by_year <- vector("list", n_years)
  for (c in seq_len(n_years)) {
    ref_df <- get_ref_df(df_year_list[[c]])
    .check_required_cols(ref_df, policy_vec, context = "scale reference (across_years)")
    ref_specs_by_year[[c]] <- compute_one(ref_df)
  }

  nonempty <- vapply(ref_specs_by_year, function(s) !is.null(s$empty) && !isTRUE(s$empty), logical(1))
  if (!any(nonempty)) {
    global_spec <- list(max_freq = NA_integer_, total = 0L, empty = TRUE)
  } else {
    max_freqs <- vapply(ref_specs_by_year[nonempty], function(s) s$max_freq, numeric(1))
    totals    <- vapply(ref_specs_by_year[nonempty], function(s) s$total,    numeric(1))

    global_spec <- list(
      max_freq = as.integer(max(max_freqs, na.rm = TRUE)),
      total    = as.integer(max(totals, na.rm = TRUE)),
      empty    = FALSE
    )
  }

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
  else                .draw_panel_raw(df, policy_vec, scale_spec)
}


# ---- single RF heatmap + marginals ----

.single_pred_counts <- function(df, policy_vec, breaks = seq(0, 1, by = 0.05)) {
  .check_required_cols(df, policy_vec, context = "single predicted heatmap")

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
  heat$Prop <- heat$Freq / sum(ok)

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
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required. Install it with install.packages('gridExtra').", call. = FALSE)
  }
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required.", call. = FALSE)
  }

  counts <- .single_pred_counts(df, policy_vec, breaks = breaks)
  if (is.null(counts)) {
    stop("No non-missing observations available for the requested year/panel/policies.", call. = FALSE)
  }

  heat_df <- counts$heat
  max_prop <- max(heat_df$Prop, na.rm = TRUE)
  if (!is.finite(max_prop) || max_prop <= 0) max_prop <- 1

  x_lab <- .pretty_policy_label(policy_vec[1])
  y_lab <- .pretty_policy_label(policy_vec[2])
  rng <- c(0, 1)
  bin_w <- counts$bin_w
  tick_breaks <- c(0, 0.25, 0.5, 0.75, 1)

  plot_dist_hist <- function(z, lab = NULL, rng = c(0, 1), side = c("bottom", "left")) {
    side <- match.arg(side, c("bottom", "left"))
    dfz <- data.frame(z = z)

    p <- ggplot2::ggplot(dfz, ggplot2::aes(x = z)) +
      ggplot2::geom_histogram(
        ggplot2::aes(y = after_stat(density)),
        breaks = breaks,
        fill = "ivory4",
        color = "grey35",
        linewidth = 0.3
      ) +
      ggplot2::geom_density(
        color = "black",
        linewidth = 0.8,
        adjust = 1
      ) +
      ggplot2::scale_x_continuous(
        limits = rng,
        breaks = tick_breaks,
        expand = c(0, 0)
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(
        legend.position = "none",
        panel.border = ggplot2::element_blank(),
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.line = ggplot2::element_line(color = "grey30", linewidth = 0.3),
        panel.background = ggplot2::element_blank(),
        axis.text = ggplot2::element_text(size = 9, color = "black"),
        axis.ticks = ggplot2::element_line(color = "grey30", linewidth = 0.3)
      )

    if (side == "bottom") {
      p <- p +
        ggplot2::scale_y_reverse(expand = c(0, 0)) +
        ggplot2::labs(x = lab, y = "Density") +
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(size = 16),
          axis.title.y = ggplot2::element_text(size = 12),
          plot.margin = ggplot2::margin(t = 0, r = 5.5, b = 5.5, l = 0)
        )
    } else {
      p <- p +
        ggplot2::coord_flip() +
        ggplot2::scale_y_reverse(expand = c(0, 0)) +
        ggplot2::labs(x = NULL, y = "Density") +
        ggplot2::theme(
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_text(size = 12),
          plot.margin = ggplot2::margin(t = 5.5, r = 0, b = 0, l = 0)
        )
    }

    p
  }

  p_heat_base <- ggplot2::ggplot(heat_df, ggplot2::aes(x = x_mid, y = y_mid, fill = Prop)) +
    ggplot2::geom_tile(width = bin_w, height = bin_w, color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_gradient2(
      midpoint = 0.5 * max_prop,
      limits = c(0, max_prop),
      breaks = c(0, 0.5 * max_prop, max_prop),
      low = low, mid = mid, high = high,
      name = "Proportion",
      labels = function(x) format(round(x, 3), nsmall = 3)
    ) +
    ggplot2::scale_x_continuous(limits = rng, breaks = tick_breaks, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = rng, breaks = tick_breaks, expand = c(0, 0)) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.title = ggplot2::element_blank(),
      legend.position = "right",
      legend.title = ggplot2::element_text(size = 11),
      legend.text = ggplot2::element_text(size = 9),
      plot.margin = ggplot2::margin(5.5, 5.5, 0, 0)
    )

  p_left <- plot_dist_hist(counts$y, lab = y_lab, rng = rng, side = "left") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(size = 9, color = "black"),
      axis.ticks.y = ggplot2::element_line(color = "grey30", linewidth = 0.3),
      axis.line.y = ggplot2::element_line(color = "grey30", linewidth = 0.3),
      plot.margin = ggplot2::margin(5.5, 0, 0, 0)
    )

  p_bottom <- plot_dist_hist(counts$x, lab = x_lab, rng = rng, side = "bottom") +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(size = 9, color = "black"),
      axis.ticks.x = ggplot2::element_line(color = "grey30", linewidth = 0.3),
      axis.line.x = ggplot2::element_line(color = "grey30", linewidth = 0.3),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      plot.margin = ggplot2::margin(0, 0, 5.5, 0)
    )

  heat_grob_full <- ggplot2::ggplotGrob(p_heat_base)
  guide_idx <- which(sapply(heat_grob_full$grobs, function(x) x$name) == "guide-box")
  if (length(guide_idx) > 0) {
    legend_grob <- heat_grob_full$grobs[[guide_idx[1]]]
  } else {
    legend_grob <- grid::nullGrob()
  }

  p_heat <- p_heat_base + ggplot2::theme(legend.position = "none", plot.margin = ggplot2::margin(5.5, 0, 0, 0))

  y_lab_grob <- grid::textGrob(
    y_lab,
    rot = 90,
    gp = grid::gpar(fontsize = 16)
  )

  top_grob <- gridExtra::arrangeGrob(
    grobs = list(y_lab_grob, p_left, p_heat, legend_grob),
    ncol = 4,
    widths = c(0.75, 1.2, 5.8, 1.4)
  )

  bottom_grob <- gridExtra::arrangeGrob(
    grobs = list(grid::nullGrob(), grid::nullGrob(), p_bottom, grid::nullGrob()),
    ncol = 4,
    widths = c(0.75, 1.2, 5.8, 1.4)
  )

  g <- gridExtra::arrangeGrob(
    grobs = list(top_grob, bottom_grob),
    ncol = 1,
    heights = c(5.2, 1.4)
  )

  if (!is.null(title) && nzchar(title)) {
    g <- gridExtra::arrangeGrob(
      g,
      top = grid::textGrob(
        label = title,
        x = 0.02, hjust = 0,
        gp = grid::gpar(fontsize = 20, fontface = "bold")
      )
    )
  }

  g
}
