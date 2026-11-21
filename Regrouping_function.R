source("Ideas_work.R")

##############################################################################
# regroup_data()
#
# Coarsen the groupings of a raw/conf dataset, then re-apply the
# confidentiality rules to the coarser table.
#
# Why: summing cells that were ALREADY confidentialised carries the noise and
# suppression of the fine-grained cells into the merged ones. Merging the RAW
# data first and applying the rules to the bigger cells afterwards usually
# keeps more information (fewer cells fall under a suppression threshold).
#
# Steps
#   1. find the raw/conf pairs (same rule as class_columns())
#   2. coarsen the chosen grouping variables
#         numeric  -> brackets of a chosen width   by_width(2)
#         category -> keep some levels, merge the rest   keep_levels("European")
#   3. drop the old conf columns
#   4. sum the raw measures within the new, coarser groups
#   5. re-apply the confidentiality rules to the summed raw columns
#   6. return a data frame with the SAME layout as the input, so it can go
#      straight into class_distance() / class_columns() -> aggregate_distances()
#
# Needs: dplyr, forcats, and class_columns() from the main script.
##############################################################################


# ---- merge helpers: describe HOW one variable is coarsened ----------------

# Brackets of `width` for a whole-number variable (year, age, ...).
#   by_width(2)                  2020-2021, 2022-2023, 2024
#   by_width(5, start = 0)       0-4, 5-9, 10-14, ...

######## FUNCTION 1
by_width <- function(width, start = NULL) {
  if (!is.numeric(width) || length(width) != 1 || width < 1 || width != round(width)) {
    stop("width must be a single whole number >= 1")
  }
  structure(list(width = width, start = start), class = "merge_width")
}

# Keep the named levels as they are and merge every other level into one.
# Wraps forcats::fct_other(keep = ...).
#   keep_levels("European")                      European / Other
#   keep_levels(c("Maori", "Pacific"), "Rest")   Maori / Pacific / Rest


######## FUNCTION 2

keep_levels <- function(keep, other = "Other") {
  if (!is.character(keep) || length(keep) == 0) {
    stop("keep must be a character vector of the levels to leave unmerged")
  }
  structure(list(keep = keep, other = other), class = "merge_keep")
}


# ---- apply one merge spec to one column -----------------------------------
######## FUNCTION 3

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
    hi <- pmin(lo + spec$width - 1, max(x, na.rm = TRUE))  # last bracket may be short
    
    fmt   <- function(v) format(v, scientific = FALSE, trim = TRUE)
    label <- ifelse(lo == hi, fmt(lo), paste0(fmt(lo), "-", fmt(hi)))
    label[is.na(x)] <- NA
    
    # keep brackets in numeric order, not alphabetical
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


# ---- main function ---------------------------------------------------------
#
# data        raw + conf_ dataset, in the layout class_columns() expects
# merge       named list: variable -> by_width(...) or keep_levels(...)
#             (list() = no merging, i.e. just re-apply the rules)
# conf_rules  how to confidentialise a summed raw column.
#               - one function(x), used for every measure, or
#               - a named list: list(ppl = function(x) ..., cost = function(x) ...)
#             A rule takes the summed raw column and returns a vector of the same
#             length. It can be written as function(x) or function(x, data):
#             with `data` it also receives the merged table (the raw columns and
#             the entity count), so one column's rule can depend on another's
#             (e.g. cost suppressed when the people count is small).
#             Suppressed cells: return 0 if they should count as loss
#             (NA cells are ignored by distance()).
# conf_method "recompute"    (default) re-apply conf_rules to the merged raw data
#             "sum_existing" sum the conf columns that were already in `data`.
#                            This is the "confidentialise first, then merge"
#                            comparison: run both and compare in aggregate_distances().
# seed        set.seed() before applying the rules (for random rounding)
# entity_col  optional column holding the number of distinct entities (businesses,
#             schools, ...) behind each row. It is NOT treated as a grouping column
#             and is not in the output; it is merged with entity_merge and handed
#             to the rules in `data`. Distinct entities cannot be added up from
#             cell counts, because the same entity can sit in several merged cells:
#               "max" (default) the largest merged cell's count - a lower bound, so
#                     it errs towards suppressing
#               "sum" exact only if no entity appears in more than one merged cell,
#                     otherwise it overstates and can let a cell through that should
#                     be suppressed
#             For exact numbers, recompute distinct entities from the unit records.

######## FUNCTION 4

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
  
  # 1. Which columns are raw/conf pairs? (same check as the rest of the pipeline)
  pairs     <- attr(class_columns(data, conf_prefix = conf_prefix), "pairs")
  measures  <- pairs$raw
  conf_cols <- pairs$conf_name
  
  if (length(measures) == 0) stop("No raw/conf column pairs found in data")
  
  # The entity count (if any) is neither a grouping column nor a measure
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
  
  # everything else (groupings and indicators) are the keys that define a cell
  key_cols <- setdiff(names(data), c(measures, conf_cols, entity_col))
  
  # 2. Check the merge request
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
  
  # Check the rules (only needed when they will be re-applied)
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
  
  # 3. Coarsen the chosen grouping variables
  for (var in names(merge)) {
    data[[var]] <- apply_merge(data[[var]], merge[[var]], var)
  }
  
  # 4. Sum the raw measures inside the new cells.
  #    The old conf columns are dropped here (kept only for "sum_existing").
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
  
  # Tell the rules which column is the entity count, so a rule that needs one
  # fails loudly if entity_col was not given (otherwise a stray numeric column
  # would quietly be treated as a grouping column).
  attr(merged, "entity_col") <- entity_col
  
  # 5. Re-apply the confidentiality rules to the summed raw columns
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
  
  # 6. Same columns, same order as the input (the entity column is not part of it)
  merged <- merged[, setdiff(names(data), entity_col), drop = FALSE]
  
  message("regroup_data(): ", nrow(data), " rows -> ", nrow(merged), " rows (",
          if (length(merge)) paste(names(merge), collapse = ", ") else "no merges",
          "; conf: ", conf_method, ")")
  
  merged
}


##############################################################################
# EXAMPLE  (run the main script first so test_data, class_distance() and
#           aggregate_distances() exist)
#
# The two rules below are PLACEHOLDERS - swap in your real confidentiality rules.
##############################################################################

placeholder_rules <- list(
  ppl  = function(x) ifelse(x < 60, 0, round(x / 5) * 5),  # suppress < 60 (as 0), else round to 5
  cost = function(x) round(x, -2)                           # round to nearest 100
)
  
merge_plan <- list(
  age      = by_width(3)             # 2020-2021, 2022-2023, 2024
)

###############################################################################

# MAIN FUNCTION IN USE
regrouped <- regroup_data(test_data, merge = merge_plan, conf_rules = placeholder_rules)
  
# RE RUN THE DISTANCE
regrouped_distance <- class_distance(regrouped)
# RE RUN THE AGGREGATE DISTANCES GRAPHS

results_regrouped_mae <- aggregate_distances(
  regrouped_distance,
  x_axis = "age",
  groups = list("Visit_hospital",
              c("Visit_hospital", "region")),
  method = "mae"
)
  
names(results_regrouped_mae[["Visit_hospital"]]$plots)

results_mae[["Visit_hospital x region"]]$plots[[7]]
results_regrouped_mae[["Visit_hospital x region"]]$plots[[7]]

