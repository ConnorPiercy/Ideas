method_info <- list(
  loss = list(
    row_suffix      = "_loss",
    metric_suffix   = "_loss_percentage",
    total_suffix    = "_loss_total",
    axis_label      = "Loss (%)",
    total_label     = "Total loss",
    line_subtitle   = "Loss as % of raw total",
    total_subtitle  = "Total loss (raw - conf)",
    smooth_subtitle = "Smoothed loss (% of raw total), columns = raw total"
  ),
  mae = list(
    row_suffix      = "_dist",
    metric_suffix   = "_dist_mae",
    total_suffix    = "_dist_total",
    axis_label      = "MAE (original units)",
    total_label     = "Total absolute error",
    line_subtitle   = "Mean absolute error per row",
    total_subtitle  = "Total absolute error |raw - conf|",
    smooth_subtitle = "Smoothed MAE, columns = raw total"
  )
)

# One builder per plot type; every type is built for every metric column. Add a type by adding an entry.
plot_builders <- list(
  
  line = function(result, variable, x_axis, group_label, info) {
    y_max <- max(result[[variable]], na.rm = TRUE)
    if (!is.finite(y_max) || y_max <= 0) y_max <- 1
    
    ggplot2::ggplot(
      result,
      ggplot2::aes(
        x = .data[[x_axis]],
        y = .data[[variable]],
        colour = subgroup,
        group = subgroup
      )
    ) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::labs(
        title = paste(variable, "by", x_axis),
        subtitle = paste0(info$line_subtitle, " | grouped by ", group_label),
        x = x_axis,
        y = info$axis_label,
        colour = "Subgroup"
      ) +
      ggplot2::coord_cartesian(ylim = c(NA, y_max * 1.15)) +
      ggplot2::theme_minimal()
  },
  
  bar = function(result, variable, x_axis, group_label, info) {
    y_max <- max(result[[variable]], na.rm = TRUE)
    if (!is.finite(y_max) || y_max <= 0) y_max <- 1
    
    ggplot2::ggplot(
      result,
      ggplot2::aes(
        x = factor(.data[[x_axis]]),
        y = .data[[variable]],
        fill = subgroup
      )
    ) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::labs(
        title = paste(variable, "by", x_axis),
        subtitle = paste0(info$line_subtitle, " | grouped by ", group_label),
        x = x_axis,
        y = info$axis_label,
        fill = "Subgroup"
      ) +
      ggplot2::coord_cartesian(ylim = c(0, y_max * 1.15)) +
      ggplot2::theme_minimal()
  },
  
  totals = function(result, variable, x_axis, group_label, info) {
    
    base      <- sub(paste0(info$metric_suffix, "$"), "", variable)
    total_col <- paste0(base, info$total_suffix)
    
    ggplot2::ggplot(
      result,
      ggplot2::aes(
        x = factor(.data[[x_axis]]),
        y = .data[[total_col]],
        fill = subgroup
      )
    ) +
      ggplot2::geom_col(position = "dodge") +
      ggplot2::labs(
        title = paste(total_col, "by", x_axis),
        subtitle = paste0(info$total_subtitle, " | grouped by ", group_label),
        x = x_axis,
        y = info$total_label,
        fill = "Subgroup"
      ) +
      ggplot2::theme_minimal()
  },
  
  smooth_columns = function(result, variable, x_axis, group_label, info) {
    
    base    <- sub(paste0(info$metric_suffix, "$"), "", variable)
    raw_col <- paste0(base, "_raw_total")
    
    y_max <- max(result[[variable]], na.rm = TRUE)
    if (!is.finite(y_max) || y_max <= 0) y_max <- 1
    
    raw_max <- max(result[[raw_col]], na.rm = TRUE)
    if (!is.finite(raw_max) || raw_max <= 0) raw_max <- 1
    
    # columns sit below zero; k converts raw units to plot units
    band <- 0.35 * y_max
    k    <- band / raw_max
    
    # loess needs a numeric x; otherwise join the points
    smooth_layer <- if (is.numeric(result[[x_axis]])) {
      ggplot2::geom_smooth(
        ggplot2::aes(y = .data[[variable]]),
        method = "loess", formula = y ~ x,
        method.args = list(degree = 1),
        span = 1, se = FALSE, linewidth = 1
      )
    } else {
      ggplot2::geom_line(
        ggplot2::aes(y = .data[[variable]]),
        linewidth = 1
      )
    }
    
    ggplot2::ggplot(
      result,
      ggplot2::aes(
        x = .data[[x_axis]],
        colour = subgroup,
        fill = subgroup,
        group = subgroup
      )
    ) +
      ggplot2::geom_col(
        ggplot2::aes(y = -.data[[raw_col]] * k),
        position = ggplot2::position_dodge(),
        alpha = 0.5,
        colour = NA
      ) +
      ggplot2::geom_hline(yintercept = 0, colour = "grey40") +
      smooth_layer +
      ggplot2::geom_point(ggplot2::aes(y = .data[[variable]]), size = 2) +
      ggplot2::scale_y_continuous(
        breaks = pretty(c(0, y_max)),
        sec.axis = ggplot2::sec_axis(
          ~ -. / k,
          name = "Raw total (columns)",
          breaks = pretty(c(0, raw_max))
        )
      ) +
      ggplot2::coord_cartesian(ylim = c(-band * 1.05, y_max * 1.1)) +
      ggplot2::labs(
        title = paste(variable, "by", x_axis),
        subtitle = paste0(info$smooth_subtitle, " | grouped by ", group_label),
        x = x_axis,
        y = info$axis_label,
        colour = "Subgroup",
        fill = "Subgroup"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank())
  }
)

# Aggregates by x_axis within each grouping and builds every plot.
# method = "loss": total loss / raw total * 100.  method = "mae": mean |raw - conf| per row.
aggregate_distances <- function(data,
                                x_axis,
                                groups,
                                method = c("loss", "mae")) {
  
  method <- match.arg(method)
  info   <- method_info[[method]]
  
  # groups: a character vector, or a list of them (one grouping each)
  if (is.character(groups)) groups <- list(groups)
  
  if (!is.list(groups) ||
      length(groups) == 0 ||
      !all(vapply(groups, is.character, logical(1)))) {
    stop("groups must be a character vector or a list of character vectors.")
  }
  
  group_labels <- vapply(groups, paste, character(1), collapse = " x ")
  if (!is.null(names(groups))) {
    has_name <- nzchar(names(groups))
    group_labels[has_name] <- names(groups)[has_name]
  }
  names(groups) <- group_labels
  
  distance_cols <- names(data)[grepl("_dist$", names(data))]
  
  if (length(distance_cols) == 0) {
    stop("No columns ending in '_dist' found.")
  }
  
  required_cols <- unique(c(x_axis, unlist(groups)))
  missing_cols  <- setdiff(required_cols, names(data))
  
  if (length(missing_cols) > 0) {
    hint <- if (any(grepl(",", missing_cols))) {
      "\nA name contains a comma - did you write c(\"a, b\") instead of c(\"a\", \"b\")?"
    } else {
      ""
    }
    
    stop(
      "Missing columns: ",
      paste0("'", missing_cols, "'", collapse = ", "),
      hint,
      call. = FALSE
    )
  }
  
  raw_cols <- sub("_dist$", "", distance_cols)
  
  if (!all(raw_cols %in% names(data))) {
    stop("Could not find matching raw columns for all distance columns.")
  }
  
  row_cols <- paste0(raw_cols, info$row_suffix)
  
  if (!all(row_cols %in% names(data))) {
    stop(
      "Could not find '", info$row_suffix, "' columns needed for method = '",
      method, "'. Re-run class_distance() / distance() to create them.",
      call. = FALSE
    )
  }
  
  metric_cols <- paste0(raw_cols, info$metric_suffix)
  
  row_fns <- if (method == "mae") {
    list(total = ~ sum(.x, na.rm = TRUE),
         mae   = ~ mean(.x, na.rm = TRUE))
  } else {
    list(total = ~ sum(.x, na.rm = TRUE))
  }
  
  results <- lapply(names(groups), function(group_label) {
    
    group_vars <- groups[[group_label]]
    
    result <- data |>
      dplyr::group_by(
        dplyr::across(
          dplyr::all_of(unique(c(group_vars, x_axis)))
        )
      ) |>
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(row_cols),
          row_fns,
          .names = "{.col}_{.fn}"
        ),
        dplyr::across(
          dplyr::all_of(raw_cols),
          ~ sum(.x, na.rm = TRUE),
          .names = "{.col}_raw_total"
        ),
        n = dplyr::n(),
        .groups = "drop"
      )
    
    # loss % = total loss / raw total * 100
    if (method == "loss") {
      for (base in raw_cols) {
        raw_total  <- result[[paste0(base, "_raw_total")]]
        loss_total <- result[[paste0(base, "_loss_total")]]
        
        result[[paste0(base, info$metric_suffix)]] <-
          ifelse(raw_total == 0, NA_real_, loss_total / raw_total * 100)
      }
    }
    
    result <- result |>
      dplyr::mutate(
        subgroup = interaction(
          dplyr::across(dplyr::all_of(group_vars)),
          drop = TRUE,
          sep = " / "
        )
      )
    
    # every plot type for every metric column
    plot_grid <- expand.grid(
      variable = metric_cols,
      type     = names(plot_builders),
      stringsAsFactors = FALSE
    )
    
    plots <- Map(
      function(variable, type) {
        plot_builders[[type]](result, variable, x_axis, group_label, info)
      },
      plot_grid$variable,
      plot_grid$type
    )
    
    names(plots) <- paste(plot_grid$variable, plot_grid$type, sep = "_")
    
    list(
      aggregated_data = result,
      plots = plots
    )
  })
  
  names(results) <- names(groups)
  
  results
}