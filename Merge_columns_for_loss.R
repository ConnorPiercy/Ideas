# Brackets for a whole-number column, e.g. by_width(2) gives 2020-2021, 2022-2023.
by_width <- function(width, start = NULL) {
  if (!is.numeric(width) || length(width) != 1 || width < 1 || width != round(width)) {
    stop("width must be a single whole number >= 1")
  }
  structure(list(width = width, start = start), class = "merge_width")
}

# Keeps the named levels and merges the rest (forcats::fct_other).
keep_levels <- function(keep, other = "Other") {
  if (!is.character(keep) || length(keep) == 0) {
    stop("keep must be a character vector of the levels to leave unmerged")
  }
  structure(list(keep = keep, other = other), class = "merge_keep")
}

# Applies one by_width() / keep_levels() spec to a column.
apply_merge <- function(x, spec, var) {
  
  if (inherits(spec, "merge_width")) {
    
    if (!is.numeric(x) || any(x != round(x), na.rm = TRUE)) {
      stop("'", var, "' must hold whole numbers to be bracketed with by_width()",
           call. = FALSE)
    }
    
    start <- if (is.null(spec$start)) min(x, na.rm = TRUE) else spec$start
    if (start > min(x, na.rm = TRUE)) {
      stop("start (", start, ") is above the smallest value of '", var, "' (",
           min(x, na.rm = TRUE), ")", call. = FALSE)
    }
    
    bracket <- (x - start) %/% spec$width
    lo <- start + bracket * spec$width
    hi <- pmin(lo + spec$width - 1, max(x, na.rm = TRUE))
    
    fmt   <- function(v) format(v, scientific = FALSE, trim = TRUE)
    label <- ifelse(lo == hi, fmt(lo), paste0(fmt(lo), "-", fmt(hi)))
    label[is.na(x)] <- NA
    
    levs <- unique(label[order(lo)])
    return(factor(label, levels = levs[!is.na(levs)]))
  }
  
  if (inherits(spec, "merge_keep")) {
    
    x <- factor(x)
    unknown <- setdiff(spec$keep, levels(x))
    if (length(unknown) > 0) {
      stop("Level(s) not found in '", var, "': ",
           paste(unknown, collapse = ", "),
           "\nAvailable levels: ", paste(levels(x), collapse = ", "),
           call. = FALSE)
    }
    return(forcats::fct_other(x, keep = spec$keep, other_level = spec$other))
  }
  
  stop("Merge for '", var, "' must be made with by_width() or keep_levels()",
       call. = FALSE)
}

############################################################################

regroup_data <- function(data,
                         merge,
                         conf_rules = NULL,
                         conf_method = c("recompute", "sum_existing"),
                         conf_prefix = "conf_",
                         seed = NULL,
                         entity_col = NULL,
                         entity_merge = c("max", "sum")) {
  
  conf_method  <- match.arg(conf_method)
  entity_merge <- match.arg(entity_merge)
  
  # raw/conf pairs; every other column defines a cell
  pairs     <- attr(class_columns(data, conf_prefix = conf_prefix), "pairs")
  measures  <- pairs$raw
  conf_cols <- pairs$conf_name
  
  if (length(measures) == 0) stop("No raw/conf column pairs found in data")
  
  if (!is.null(entity_col)) {
    if (length(entity_col) != 1 || !entity_col %in% names(data)) {
      stop("entity_col '", paste(entity_col, collapse = ", "), "' is not a column of data",
           call. = FALSE)
    }
    if (entity_col %in% c(measures, conf_cols)) {
      stop("entity_col cannot be one of the raw/conf measure columns", call. = FALSE)
    }
    if (!is.numeric(data[[entity_col]])) {
      stop("entity_col '", entity_col, "' must be numeric", call. = FALSE)
    }
  }
  
  key_cols <- setdiff(names(data), c(measures, conf_cols, entity_col))
  
  if (!is.list(merge) ||
      (length(merge) > 0 && (is.null(names(merge)) || any(!nzchar(names(merge)))))) {
    stop("merge must be a named list, e.g. list(year = by_width(2))")
  }
  bad_vars <- setdiff(names(merge), key_cols)
  if (length(bad_vars) > 0) {
    stop("Cannot merge ", paste0("'", bad_vars, "'", collapse = ", "),
         ": not a grouping column. Grouping columns are: ",
         paste(key_cols, collapse = ", "), call. = FALSE)
  }
  
  if (conf_method == "recompute") {
    if (is.null(conf_rules)) stop("conf_rules is required when conf_method = \"recompute\"")
    if (is.function(conf_rules)) {
      conf_rules <- stats::setNames(rep(list(conf_rules), length(measures)), measures)
    }
    missing_rules <- setdiff(measures, names(conf_rules))
    if (length(missing_rules) > 0) {
      stop("No conf rule supplied for: ", paste(missing_rules, collapse = ", "),
           call. = FALSE)
    }
  }
  
  # merge the chosen columns
  for (var in names(merge)) {
    data[[var]] <- apply_merge(data[[var]], merge[[var]], var)
  }
  
  # sum the raw measures within the new cells (old conf columns are dropped)
  sum_cols <- if (conf_method == "sum_existing") c(measures, conf_cols) else measures
  
  entity_fn <- if (entity_merge == "max") max else sum
  
  merged <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(key_cols))) |>
    dplyr::summarise(
      dplyr::across(dplyr::all_of(sum_cols), ~ sum(.x, na.rm = TRUE)),
      dplyr::across(dplyr::all_of(if (is.null(entity_col)) character() else entity_col),
                    ~ entity_fn(.x, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    as.data.frame()
  
  # lets a rule check that entity_col was given
  attr(merged, "entity_col") <- entity_col
  
  # re-apply the confidentiality rules
  if (conf_method == "recompute") {
    if (!is.null(seed)) set.seed(seed)
    
    for (m in measures) {
      rule <- conf_rules[[m]]
      conf <- if ("data" %in% names(formals(rule))) {
        rule(merged[[m]], data = merged)
      } else {
        rule(merged[[m]])
      }
      
      if (length(conf) != nrow(merged) || !(is.numeric(conf) || all(is.na(conf)))) {
        stop("The conf rule for '", m, "' must return a numeric vector the same ",
             "length as its input", call. = FALSE)
      }
      merged[[paste0(conf_prefix, m)]] <- conf
    }
  }
  
  # same layout as the input
  merged <- merged[, setdiff(names(data), entity_col), drop = FALSE]
  
  message("regroup_data(): ", nrow(data), " rows -> ", nrow(merged), " rows (",
          if (length(merge)) paste(names(merge), collapse = ", ") else "no merges",
          "; conf: ", conf_method, ")")
  
  merged
}