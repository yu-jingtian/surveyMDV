# R/heat_core.R
# Internal helpers for model-predicted policy heatmaps.

# ---- constants ----

.DEFAULT_HEAT_BREAKS <- seq(0, 1, by = 0.05)
.DEFAULT_AXIS_TITLE_SIZE <- 16
.DEFAULT_AXIS_TICK_SIZE <- 9
.DEFAULT_AXIS_TICK_PAD <- 0.06
.DEFAULT_LOW_COLOR <- "#c6dbef"
.DEFAULT_MID_COLOR <- "#F4B811"
.DEFAULT_HIGH_COLOR <- "#CC2929"

# ---- utilities ----

.is_null_or_empty <- function(x) is.null(x) || length(x) == 0

.resolve_policy_names <- function(model = c("rf", "xgb", "lm", "svr"), policy_x, policy_y) {
  model <- match.arg(model)

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
    enviro = "Environmental protection",
    environment = "Environmental protection",
    abortion = "Abortion access",
    immig = "Immigration restriction",
    immigration = "Immigration restriction",
    healthcare = "Public health care",
    military = "Military intervention",
    spending = "Government spending priorities",
    trade = "Trade protectionism"
  )

  if (x0 %in% names(lab_map)) return(unname(lab_map[[x0]]))

  x0 <- gsub("_", " ", x0)
  paste0(toupper(substr(x0, 1, 1)), substr(x0, 2, nchar(x0)))
}

.policy_tick_labels <- function(x) {
  x0 <- gsub("_pred_(rf|xgb|lm|svr)$|_pred$|_(rf|xgb|lm|svr)$", "", x)

  tick_map <- list(
    abortion = c("Access", "Mixed", "Restriction"),
    enviro = c("Weaker", "Mixed", "Stronger"),
    environment = c("Weaker", "Mixed", "Stronger"),
    guns = c("Permissive", "Mixed", "Stricter"),
    immig = c("Legalization", "Mixed", "Restriction"),
    immigration = c("Legalization", "Mixed", "Restriction"),
    healthcare = c("Market-based", "Mixed", "Government-led"),
    military = c("Restrained", "Mixed", "Hawkish"),
    spending = c("Expand services", "Mixed", "Cut services"),
    trade = c("Free trade", "Mixed", "Protectionist")
  )

  if (x0 %in% names(tick_map)) return(unname(tick_map[[x0]]))
  c("Low", "Mixed", "High")
}

# ---- data prep ----

.prepare_year_df <- function(year, model = c("rf", "xgb", "lm", "svr")) {
  year <- as.integer(year)
  model <- match.arg(model)

  pol <- get_policy_predicted(model = model, year = year, cols = NULL)
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

.resolve_group_filter <- function(group = "population") {
  if (is.null(group)) group <- "population"
  key <- tolower(trimws(as.character(group)))

  if (key %in% c("population", "all", "overall")) {
    return(list(label = "Population", filter = function(df) df))
  }
  if (key %in% c("rep", "rep.", "republican", "republicans")) {
    return(list(label = "Rep.", filter = function(df) df[df$partisan == "Rep.", , drop = FALSE]))
  }
  if (key %in% c("dem", "dem.", "democrat", "democrats")) {
    return(list(label = "Dem.", filter = function(df) df[df$partisan == "Dem.", , drop = FALSE]))
  }
  if (key %in% c("ind", "ind.", "independent", "independents")) {
    return(list(label = "Ind.", filter = function(df) df[df$partisan == "Ind.", , drop = FALSE]))
  }

  stop(
    "Unknown group: ", group,
    ". Supported values are 'population', 'republican', 'independent', and 'democrat'.",
    call. = FALSE
  )
}

.resolve_subgroup_filter <- function(subgroup = NULL) {
  if (is.null(subgroup)) {
    return(list(label = NULL, filter = function(df) df))
  }

  key <- trimws(as.character(subgroup))
  key_low <- tolower(key)

  if (key_low %in% c("population", "all", "overall")) {
    return(list(label = NULL, filter = function(df) df))
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
  if (key_low %in% c("non-metro", "nonmetro", "other co.", "other co", "other county", "rural")) {
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
    ". Supported values include 'college', 'non-college', 'metro', 'non-metro', 'female', and 'male'.",
    call. = FALSE
  )
}

.resolve_group_subgroup_filter <- function(group = "population", subgroup = NULL) {
  group_spec <- .resolve_group_filter(group)
  subgroup_spec <- .resolve_subgroup_filter(subgroup)

  label <- group_spec$label
  if (!is.null(subgroup_spec$label)) {
    if (identical(label, "Population")) {
      label <- subgroup_spec$label
    } else {
      label <- paste(label, subgroup_spec$label)
    }
  }

  list(
    label = label,
    filter = function(df) {
      out <- group_spec$filter(df)
      subgroup_spec$filter(out)
    }
  )
}

# ---- scaling ----

.compute_scale_spec_pred <- function(df, policy_vec, breaks = .DEFAULT_HEAT_BREAKS) {
  .check_required_cols(df, policy_vec, context = "predicted-score scaling")

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

.compute_scale_specs <- function(df_year_list, panels,
                                 policy_vec,
                                 scale = c("across_years", "within_year"),
                                 scale_ref = c("population", "panel"),
                                 scale_ref_panel = 1L,
                                 breaks = .DEFAULT_HEAT_BREAKS) {
  scale <- match.arg(scale, c("across_years", "within_year"))
  scale_ref <- match.arg(scale_ref, c("population", "panel"))

  n_panels <- length(panels)
  n_years  <- length(df_year_list)

  specs <- vector("list", n_panels)
  for (r in seq_len(n_panels)) specs[[r]] <- vector("list", n_years)

  get_ref_df <- function(df_year) {
    if (scale_ref == "population") return(df_year)
    idx <- as.integer(scale_ref_panel)
    if (is.na(idx) || idx < 1 || idx > length(panels)) {
      stop("Invalid scale reference panel index.", call. = FALSE)
    }
    .apply_panel_filter(df_year, panels[[idx]])
  }

  if (scale == "within_year") {
    for (c in seq_len(n_years)) {
      ref_df   <- get_ref_df(df_year_list[[c]])
      ref_spec <- .compute_scale_spec_pred(ref_df, policy_vec, breaks = breaks)
      for (r in seq_len(n_panels)) specs[[r]][[c]] <- ref_spec
    }
    return(specs)
  }

  ref_specs_by_year <- vector("list", n_years)
  for (c in seq_len(n_years)) {
    ref_df <- get_ref_df(df_year_list[[c]])
    .check_required_cols(ref_df, policy_vec, context = "scale reference (across years)")
    ref_specs_by_year[[c]] <- .compute_scale_spec_pred(ref_df, policy_vec, breaks = breaks)
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

.draw_panel_pred <- function(df, policy_vec, scale_spec,
                             breaks = .DEFAULT_HEAT_BREAKS,
                             low = .DEFAULT_LOW_COLOR,
                             mid = .DEFAULT_MID_COLOR,
                             high = .DEFAULT_HIGH_COLOR) {
  .check_required_cols(df, policy_vec, context = "predicted-score panel drawing")

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

.draw_panel <- function(df, policy_vec, scale_spec, breaks = .DEFAULT_HEAT_BREAKS) {
  .draw_panel_pred(df, policy_vec, scale_spec, breaks = breaks)
}

# ---- single heatmap + marginals ----

.single_pred_counts <- function(df, policy_vec, breaks = .DEFAULT_HEAT_BREAKS) {
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

.draw_single_pred_heatmap_with_marginals <- function(df,
                                                     policy_vec,
                                                     breaks = .DEFAULT_HEAT_BREAKS,
                                                     title = NULL,
                                                     low = .DEFAULT_LOW_COLOR,
                                                     mid = .DEFAULT_MID_COLOR,
                                                     high = .DEFAULT_HIGH_COLOR) {
  if (!requireNamespace("gridExtra", quietly = TRUE)) {
    stop("Package 'gridExtra' is required. Install it with install.packages('gridExtra').", call. = FALSE)
  }
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package 'grid' is required.", call. = FALSE)
  }

  counts <- .single_pred_counts(df, policy_vec, breaks = breaks)
  if (is.null(counts)) {
    stop("No non-missing observations available for the requested year/group/policies.", call. = FALSE)
  }

  heat_df <- counts$heat
  max_prop <- max(heat_df$Prop, na.rm = TRUE)
  if (!is.finite(max_prop) || max_prop <= 0) max_prop <- 1

  x_lab <- .pretty_policy_label(policy_vec[1])
  y_lab <- .pretty_policy_label(policy_vec[2])
  rng <- c(0, 1)
  bin_w <- counts$bin_w
  tick_breaks <- c(0, 0.5, 1)
  x_tick_labels <- .policy_tick_labels(policy_vec[1])
  y_tick_labels <- .policy_tick_labels(policy_vec[2])

  plot_dist_hist <- function(z, lab = NULL, rng = c(0, 1), side = c("bottom", "left")) {
    side <- match.arg(side, c("bottom", "left"))
    dfz <- data.frame(z = z)

    p <- ggplot2::ggplot(dfz, ggplot2::aes(x = z)) +
      ggplot2::geom_histogram(
        ggplot2::aes(y = ggplot2::after_stat(density)),
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
        labels = if (side == "bottom") x_tick_labels else y_tick_labels,
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
    ggplot2::scale_x_continuous(limits = rng, breaks = tick_breaks, labels = x_tick_labels, expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = rng, breaks = tick_breaks, labels = y_tick_labels, expand = c(0, 0)) +
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
      axis.text.y = ggplot2::element_text(size = 9, color = "black", angle = 90, vjust = 0.5, hjust = 0.5),
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
        x = 0.5, hjust = 0.5,
        gp = grid::gpar(fontsize = 20, fontface = "bold")
      )
    )
  }

  g
}
