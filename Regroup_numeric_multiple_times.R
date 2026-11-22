# Interactive questions asked by regroup_sweep().
prompt_column <- function(choices, labels = choices) {
  pick <- utils::menu(labels, title = "Which grouping column do you want to re-aggregate?")
  if (pick == 0) stop("No column chosen", call. = FALSE)
  choices[pick]
}
prompt_max_width <- function() {
  ans <- readline(paste0("Widest bracket to try (a whole number, 2 or more; ",
                         "3 tries widths 1, 2 and 3): "))
  suppressWarnings(as.numeric(ans))
}

# Loss % (or MAE) per outcome column, optionally per value of `by`.
summarise_loss <- function(rd, measures, method, by = NULL) {
  
  grp <- if (is.null(by)) factor(rep("all", nrow(rd))) else rd[[by]]
  
  out <- lapply(measures, function(m) {
    row_col <- paste0(m, if (method == "loss") "_loss" else "_dist")
    
    err <- tapply(rd[[row_col]], grp, sum, na.rm = TRUE)
    raw <- tapply(rd[[m]],       grp, sum, na.rm = TRUE)
    n   <- tapply(rd[[row_col]], grp, function(v) sum(!is.na(v)))
    
    value <- if (method == "loss") {
      ifelse(raw == 0, NA_real_, err / raw * 100)
    } else {
      err / n
    }
    
    data.frame(
      outcome     = m,
      bracket     = names(err),
      error_total = as.vector(err),
      raw_total   = as.vector(raw),
      n_rows      = as.vector(n),
      value       = as.vector(value),
      stringsAsFactors = FALSE
    )
  })
  
  do.call(rbind, out)
}
#######################

regroup_sweep <- function(data,
                          conf_rules,
                          column = NULL,
                          max_width = NULL,
                          method = c("loss", "mae"),
                          start = NULL,
                          indicators = character(),
                          conf_prefix = "conf_",
                          seed = NULL,
                          entity_col = NULL,
                          entity_merge = c("max", "sum")) {
  
  method       <- match.arg(method)
  entity_merge <- match.arg(entity_merge)
  
  if (missing(conf_rules) || is.null(conf_rules)) {
    stop("conf_rules is required: the rules are re-applied to every re-bracketed version",
         call. = FALSE)
  }
  
  classed    <- class_columns(data, indicators = indicators, conf_prefix = conf_prefix)
  pairs      <- attr(classed, "pairs")
  measures   <- pairs$raw
  candidates <- setdiff(names(classed),
                        c(pairs$raw, pairs$conf_name, indicators, entity_col))
  
  if (length(measures) == 0)   stop("No raw/conf column pairs found in data", call. = FALSE)
  if (length(candidates) == 0) stop("No grouping columns found in data", call. = FALSE)
  
  if (is.null(column)) {
    if (!interactive()) {
      stop("column is required when R is not interactive. Choose one of: ",
           paste(candidates, collapse = ", "), call. = FALSE)
    }
    labels <- paste0(candidates, "  (",
                     ifelse(vapply(classed[candidates], is.numeric, logical(1)),
                            "numeric", "category"), ")")
    column <- prompt_column(candidates, labels)
  }
  
  if (length(column) != 1 || !column %in% candidates) {
    stop("'", paste(column, collapse = ", "), "' is not a grouping column. Choose one of: ",
         paste(candidates, collapse = ", "), call. = FALSE)
  }
  if (!is.numeric(data[[column]])) {
    stop("'", column, "' is not numeric. regroup_sweep() brackets numeric columns ",
         "(year, age, ...). For a category, use regroup_data() with keep_levels().",
         call. = FALSE)
  }
  
  if (is.null(max_width)) {
    if (!interactive()) {
      stop("max_width is required when R is not interactive, e.g. max_width = 3",
           call. = FALSE)
    }
    max_width <- prompt_max_width()
  }
  if (!is.numeric(max_width) || length(max_width) != 1 || is.na(max_width) ||
      max_width < 2 || max_width != round(max_width)) {
    stop("max_width must be a single whole number of 2 or more", call. = FALSE)
  }
  
  widths <- seq_len(max_width)
  message("regroup_sweep(): '", column, "' (", min(data[[column]], na.rm = TRUE), " to ",
          max(data[[column]], na.rm = TRUE), "), bracket widths ", min(widths), " to ",
          max(widths), ", method = ", method)
  
  datasets <- list()
  summaries_outcome <- list()
  summaries_time    <- list()
  
  # one regrouped, re-confidentialised dataset per width
  for (w in widths) {
    
    merge_spec <- stats::setNames(list(by_width(w, start = start)), column)
    
    regrouped <- suppressMessages(
      regroup_data(
        data,
        merge        = merge_spec,
        conf_rules   = conf_rules,
        conf_method  = "recompute",
        conf_prefix  = conf_prefix,
        seed         = seed,
        entity_col   = entity_col,
        entity_merge = entity_merge
      )
    )
    
    message("  width ", w, ": ", nlevels(regrouped[[column]]), " brackets, ",
            nrow(regrouped), " rows")
    
    datasets[[paste0("width_", w)]] <- regrouped
    
    rd <- class_distance(regrouped, indicators = indicators, conf_prefix = conf_prefix)
    
    agg_label <- if (w == 1) "1 (original)" else as.character(w)
    
    o <- summarise_loss(rd, measures, method)
    o$bracket <- NULL
    summaries_outcome[[agg_label]] <- cbind(aggregate = agg_label, o, stringsAsFactors = FALSE)
    
    summaries_time[[agg_label]] <- cbind(
      aggregate = agg_label,
      summarise_loss(rd, measures, method, by = column),
      stringsAsFactors = FALSE
    )
  }
  
  agg_levels <- names(summaries_outcome)
  
  by_outcome <- do.call(rbind, summaries_outcome)
  by_outcome$aggregate <- factor(by_outcome$aggregate, levels = agg_levels)
  by_outcome$outcome   <- factor(by_outcome$outcome, levels = measures)
  rownames(by_outcome) <- NULL
  
  by_time <- do.call(rbind, summaries_time)
  by_time$aggregate <- factor(by_time$aggregate, levels = agg_levels)
  by_time$outcome   <- factor(by_time$outcome, levels = measures)
  # unique x position per width: a label such as "2024" can occur in several
  by_time$key <- paste(by_time$aggregate, by_time$bracket, sep = "|")
  by_time$key <- factor(by_time$key, levels = unique(by_time$key))
  rownames(by_time) <- NULL
  
  y_label <- if (method == "loss") "Loss (% of raw total)" else "MAE (original units)"
  what    <- if (method == "loss") "Loss as % of raw total" else "Mean absolute error per row"
  legend  <- paste0("Width of '", column, "' bracket")
  
  p_outcome <- ggplot2::ggplot(
    by_outcome,
    ggplot2::aes(x = outcome, y = value, fill = aggregate)
  ) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.9)) +
    ggplot2::labs(
      title    = "Information lost by outcome column",
      subtitle = paste0(what, " | each bar = one bracket width of '", column, "'"),
      x        = "Outcome column",
      y        = y_label,
      fill     = legend
    ) +
    ggplot2::theme_minimal()
  
  if (method == "mae") {
    p_outcome <- p_outcome + ggplot2::facet_wrap(~ outcome, scales = "free")
  }
  
  x_labels <- stats::setNames(by_time$bracket, by_time$key)
  
  p_time <- ggplot2::ggplot(
    by_time,
    ggplot2::aes(x = key, y = value, colour = aggregate, group = aggregate)
  ) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::facet_wrap(~ outcome, ncol = 1,
                        scales = if (method == "mae") "free_y" else "fixed") +
    ggplot2::scale_x_discrete(labels = x_labels) +
    ggplot2::labs(
      title    = paste0("Information lost across '", column, "' brackets"),
      subtitle = paste0(what, " | one line per bracket width, left to right: width ",
                        paste(widths, collapse = ", then ")),
      x        = column,
      y        = y_label,
      colour   = legend
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
  
  list(
    column     = column,
    method     = method,
    widths     = widths,
    data       = datasets,
    by_outcome = by_outcome,
    by_time    = by_time,
    plots      = list(by_outcome = p_outcome, by_time = p_time)
  )
}