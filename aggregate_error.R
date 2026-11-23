aggregate_error <- function(data,
                            x_axis,
                            groups = NULL,
                            conf_prefix = "conf_") {
  
  rd       <- class_distance(data, conf_prefix = conf_prefix)
  pairs    <- attr(rd, "pairs")
  
  #Stop if pair isnt in data
  outcomes <- pairs$raw
  if (length(outcomes) == 0) stop("No raw/conf column pairs found in data", call. = FALSE)
  
  #Stop if x_axis is given as "year" instead of year
  if (length(x_axis) != 1 || !is.character(x_axis)) {
    stop("x_axis must be a single column name in quotes, e.g. x_axis = \"year\"", call. = FALSE)
  }
  
  #Stop if groups are written as track instead of "track"
  if (!is.null(groups) && !is.character(groups)) {
    stop("groups must be column names in quotes, e.g. groups = c(\"track\", \"region\")", call. = FALSE)
  }
  
  #Stop if more then 3 columns
  if (length(groups) > 3) stop("groups can have at most 3 columns", call. = FALSE)
  
  #Stop if x_axis and groupings are same
  chosen <- c(x_axis, groups)
  if (anyDuplicated(chosen)) stop("x_axis and groups must be different columns", call. = FALSE)
  
  #Stop if chosen x_axis is missing from data
  missing_cols <- setdiff(chosen, names(data))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste0("'", missing_cols, "'", collapse = ", "), call. = FALSE)
  }
  
  #Stop if chosen x_axis is same as raw or conf pair
  outcome_cols <- intersect(chosen, c(pairs$raw, pairs$conf_name))
  if (length(outcome_cols) > 0) {
    stop(paste0("'", outcome_cols, "'", collapse = ", "),
         " is an outcome column, so it cannot be the x axis or a group", call. = FALSE)
  }
  #Identify the two types of error calculated
  measures   <- c("loss", "abserror")
  #Make new columns for each outcome for each of the aggregate groups you want to make
  #want to aggregate both types of error for every outcome, without manually specifying every column.
  error_cols <- paste0(rep(outcomes, each = 2), "_", measures)
  
  
  result <- rd |>
    dplyr::group_by(dplyr::across(dplyr::all_of(chosen))) |>
    #Groups by the chosen groupings (x_axis and groups)

    dplyr::summarise(
      #takes every column identified in error_cols and sums its values within each group.
      #, .names Adds _total to the original column name.
      dplyr::across(dplyr::all_of(error_cols), ~ sum(.x), .names = "{.col}_total"),
      #does essentially the same thing as the previous section, except it operates on your original raw outcome columns
      #Instead of the error columns
      dplyr::across(dplyr::all_of(outcomes),   ~ sum(.x), .names = "{.col}_raw_total"),
      #n() counts how many original rows contributed to each group
      n_rows = dplyr::n(),
      #removes the grouping structure after summarisation.
      .groups = "drop"
    )
  
  #Two loops
  #First more m in outcomes, cycles through each outcome like ppl, cost
  for (m in outcomes) {
    #Then retreves the raw total for that outcome
    raw_total <- result[[paste0(m, "_raw_total")]]
    #Then goes through both types of error
    for (measure in measures) {
      #Retrieves the error column it wants 
      error_total <- result[[paste0(m, "_", measure, "_total")]]
      #Then calculates the percentage (error_total/raw_total)*100
      result[[paste0(m, "_", measure, "_pct")]] <- ifelse(raw_total == 0, NA_real_, error_total / raw_total * 100)
    }
  }
#Start of Plotting
#Checks if x_axis is numeric or categorical as ggplot treats 2010 vs "2010" differently
  if (!is.numeric(result[[x_axis]])) result[[x_axis]] <- factor(result[[x_axis]])
  
#creates one variable that identifies each unique grouping combination.
#e.g. ethincity x region, you would European and north, this then gives a variable named European/North
#Does for each combination
  result$subgroup <- if (length(groups) == 0) {
    factor("All")
  } else {
    interaction(result[groups], drop = TRUE, sep = " / ")
  }
#Creates the labels for each variable group. e.g. European x North
  group_label <- if (length(groups) == 0) "all rows combined" else paste(groups, collapse = " x ")
  
#If your x-axis is numeric, it extracts all unique values and sorts them.
  x_breaks    <- if (is.numeric(result[[x_axis]])) sort(unique(result[[x_axis]])) else NULL
  
#Creates every possible plot for outcome x measures e.g. abserror and population, etc
  plot_grid <- expand.grid(measure = measures, outcome = outcomes, stringsAsFactors = FALSE)
#takes the two columns from plot_grid and passes their values into the function.
  plots <- Map(function(outcome, measure) {
#This constructs the name of the percentage column that the current plot needs.
#This creates a readable label for the plot.
    pct_col <- paste0(outcome, "_", measure, "_pct")
    label   <- if (measure == "loss") "Loss" else "Absolute error"
#Creates the plot
    p <- ggplot2::ggplot(
      result,
      #using .data[[]] lets ggplot cycle through the columns in each.
      ggplot2::aes(x = .data[[x_axis]], y = .data[[pct_col]], colour = subgroup, group = subgroup)
    ) +
    #Adds the lines and points needed
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::expand_limits(y = 0) +
      #Adds the labels per grouping
      ggplot2::labs(
        title    = paste0(outcome, ": ", tolower(label), " by ", x_axis),
        subtitle = paste0(label, " as % of the raw total | grouped by ", group_label),
        x        = x_axis,
        y        = paste0(label, " (% of raw total)"),
        colour   = group_label
      ) +
      ggplot2::theme_minimal()
#checks if x_breaks from earlier exists
#It ensures all x_axis values appear as axis breaks.
#If there are more than 20 values, it lets ggplot choose the breaks automatically.
    if (!is.null(x_breaks) && length(x_breaks) <= 20) {
      p <- p + ggplot2::scale_x_continuous(breaks = x_breaks)
    }
#Returns the plot p 
    p
  }, 

  plot_grid$outcome, plot_grid$measure)
#names plots in call e.g. plots$population_loss, #plots$population_abserror
#plots$income_loss, #plots$income_abserror
    names(plots) <- paste(plot_grid$outcome, plot_grid$measure, sep = "_")

#Lastly returns 2 things:
#data - Your aggregated dataset, containing: Raw totals, Loss totals, 
#Absolute error totals, Loss percentages, Absolute error percentages, Grouping variables.

#plots - A list of ggplot objects, with one plot for each outcome and measure combination.
  list(data = as.data.frame(result), plots = plots)
}


#Access results using the following:
#Get the raw data results used in graphs
#results$data

#View population loss percentage plots 
# results$plots$outcome_loss

#View population absolute error percentage plots
# results$plots$outcome_abserror