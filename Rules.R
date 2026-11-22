random_round <- function(x, base = 3) {
  if (any(x != round(x), na.rm = TRUE)) {
    stop("random_round() is for whole-number counts", call. = FALSE)
  }
  r  <- x %% base
  lo <- x - r
  lo + base * (stats::runif(length(x)) < r / base)
}

# Graduated random rounding bases for count magnitudes.
grr_base <- function(x) {
  ifelse(x <= 18, 3,
         ifelse(x == 19, 2,
                ifelse(x < 100, 5,
                       ifelse(x < 1000, 10, 100))))
}

# Entity counts as passed by regroup_data(entity_col = ).
get_entities <- function(data, entity_col) {
  if (is.null(data) || !identical(attr(data, "entity_col"), entity_col)) {
    stop("This rule needs the entity count column '", entity_col, "'. ",
         "Pass entity_col = \"", entity_col, "\" to regroup_data().", call. = FALSE)
  }
  data[[entity_col]]
}

# Rule for a count: suppress below min_count (or min_entities), then RR3 or GRR.
statsnz_count <- function(min_count = 6,
                          rounding = c("rr3", "grr"),
                          min_entities = NULL,
                          entity_col = "n_entities",
                          suppress_value = 0) {
  rounding <- match.arg(rounding)
  
  function(x, data) {
    suppress <- x < min_count
    if (!is.null(min_entities)) {
      suppress <- suppress | get_entities(data, entity_col) < min_entities
    }
    
    rounded <- if (rounding == "rr3") random_round(x, 3) else random_round(x, grr_base(x))
    rounded[suppress %in% TRUE] <- suppress_value
    rounded
  }
}

# Rule for a value total (e.g. cost): suppress if the count behind it is below min_count.
statsnz_value <- function(count_col,
                          min_count = 20,
                          min_entities = NULL,
                          entity_col = "n_entities",
                          suppress_value = 0) {
  
  function(x, data) {
    if (is.null(data) || !count_col %in% names(data)) {
      stop("statsnz_value() needs the count column '", count_col, "' in the table",
           call. = FALSE)
    }
    suppress <- data[[count_col]] < min_count
    if (!is.null(min_entities)) {
      suppress <- suppress | get_entities(data, entity_col) < min_entities
    }
    
    x[suppress %in% TRUE] <- suppress_value
    x
  }
}
