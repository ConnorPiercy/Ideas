class_columns <- function(data, indicators = character(),
                          conf_prefix = "conf_") {
  cols <- names(data)
  if (anyDuplicated(cols)){
    stop("Dataset contains duplicate column names. Please check and rename columns")
  }
  conf_cols <- cols[startsWith(cols, conf_prefix)]
  original_raws <- substring(
    conf_cols,
    nchar(conf_prefix) + 1
  )
  missing_pairs <- setdiff(original_raws, cols)
  if (length(missing_pairs) > 0) {
    stop(
      "Missing original columns for ",
      paste(missing_pairs, collapse = ", ")
    )
  }
  
  pairs <- data.frame(
    raw = original_raws,
    conf_name = conf_cols,
    stringsAsFactors = FALSE
  )
  
  paired_cols <- c(pairs$raw, pairs$conf_name)
  
  unknown_indicators <- setdiff(indicators, cols)
  
  if (length(unknown_indicators) > 0) {
    stop(
      "Unknown indictor columns: ",
      paste(unknown_indicators, collapse = ", ")
    )
  }
  overlapping <- intersect(indicators, paired_cols)
  if (length(overlapping) > 0) {
    stop(
      "Raw and Conf paired Columns cannot also be indicators: ",
      paste(overlapping, collapse = ", ")
    )
  }
  
  groupings <- setdiff(
    cols,
    c(paired_cols, indicators)
  )
  
  ordered_pairs  <- as.vector(
    t(as.matrix(pairs[c("raw", "conf_name")]))
  )
  
  column_order <- c(
    groupings,
    indicators,
    ordered_pairs
  )
  
  data <- data[, column_order, drop = FALSE]
  
  attr(data, "pairs") <- pairs
  
  return(data)
}
###############################################################################
# Adds <raw>_dist = |raw - conf| (for "mae") and <raw>_loss = max(raw - conf, 0) (for "loss") for every pair.
distance <- function(data) {
  
  pair_table <- attr(data, "pairs")
  
  if (is.null(pair_table)) {
    stop("Data must be processed with class_columns() first.")
  }
  
  for (i in seq_len(nrow(pair_table))) {
    
    raw_col <- pair_table$raw[i]
    conf_col <- pair_table$conf_name[i]
    
    row_diff <- data[[raw_col]] - data[[conf_col]]
    
    data[[paste0(raw_col, "_dist")]] <- abs(row_diff)
    data[[paste0(raw_col, "_loss")]] <- pmax(row_diff, 0)
  }
  return(data)
}
###############################################################################
# class_columns() then distance().
class_distance <- function(data, indicators = character(),
                           conf_prefix = "conf_") {
  
  classified_data <- class_columns(
    data = data,
    indicators = indicators,
    conf_prefix = conf_prefix
  )
  
  distance_data <- distance(
    data = classified_data
  )
  
  return(distance_data)
}