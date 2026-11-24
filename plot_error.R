plot_group_lines <- function(data,
                             x,
                             y,
                             group,
                             title        = NULL,
                             x_label      = NULL,
                             y_label      = NULL,
                             legend_title = NULL) {
  
  # ---- 1. Find the raw outcome column (e.g. "name_abserror" -> "name") ----
  baseline <- sub("_(abserror|loss)$", "", y)
  if (identical(baseline, y)) {
    stop("`y` must end in \"_abserror\" or \"_loss\" so the matching raw ",
         "column can be found (got \"", y, "\").", call. = FALSE)
  }
  
  # ---- 2. Check inputs ----
  if (length(group) != 1) {
    stop("`group` must be a single column name, e.g. group = \"label1\".",
         call. = FALSE)
  }
  needed       <- c(x, y, baseline, group)
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found in data: ",
         paste(missing_cols, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[x]])) {
    stop("`x` must be a numeric column.", call. = FALSE)
  }
  
  # ---- 3. Calculate (sum(y) - sum(name)) * 100 per category per x value ----
  df <- data.frame(
    .x     = data[[x]],
    .group = as.character(data[[group]]),
    .y     = data[[y]],
    .base  = data[[baseline]],
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df$.group), , drop = FALSE]
  
  plot_data <- df %>%
    dplyr::group_by(.group, .x) %>%
    dplyr::summarise(
      value   = (sum(.y, na.rm = TRUE) - sum(.base, na.rm = TRUE)) * 100,
      .groups = "drop"
    ) %>%
    dplyr::arrange(.group, .x)
  
  names(plot_data)[names(plot_data) == ".x"]     <- x
  names(plot_data)[names(plot_data) == ".group"] <- group
  
  # ---- 4. Plot: one line (with points) per category ----
  if (is.null(x_label))      x_label      <- x
  if (is.null(y_label))      y_label      <- paste0("(sum(", y, ") - sum(", baseline, ")) * 100")
  if (is.null(legend_title)) legend_title <- group
  
  n_groups <- length(unique(plot_data[[group]]))
  
  p <- ggplot2::ggplot(
    plot_data,
    ggplot2::aes(
      x      = .data[[x]],
      y      = value,
      colour = .data[[group]],
      group  = .data[[group]]
    )
  ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::labs(title = title, x = x_label, y = y_label, colour = legend_title) +
    ggplot2::theme_minimal() +
    # With 40-50 categories, spread the legend over several columns
    ggplot2::guides(colour = ggplot2::guide_legend(ncol = ceiling(n_groups / 20)))
  
  # Show every x value as a tick when there are only a few (e.g. years)
  xs <- sort(unique(plot_data[[x]]))
  if (length(xs) <= 15) {
    p <- p + ggplot2::scale_x_continuous(breaks = xs)
  }
  
  p
}
