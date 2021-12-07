
#'@title Plot frequency graph
#'
#'@description Produce bar chart (plotly) for single factor frequency data. 
#'
#'@param table Frequency table (data frame). 2 columns - cateogry names and frequencies. 
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param bar_colour Colour name. Defaults to blue (see get_gradient())
#'@param n sample size
#'@param font_size minimum font size for the plot (numeric).
#'@param orientation plot orientation ("h" = horizontal, "v" = verical). Vertical by default
#'@param break_q_names_col applies break_q_names to the column. Not applied by default
#'@param max_lines maximum number of lines. Int, defaults to 2/ See carsurvey::break_q_names()
#'@param ... additional plotly_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_freqs <- function(table, xlab, ylab, bar_colour, n, font_size = 12, orientation = "v", break_q_names_col = NULL, max_lines = 2, ...) {
  
  # Set default bar colour
  if (missing(bar_colour)) {
    c <- unlist(get_gradient(1))
    bar_colour <- grDevices::rgb(c[1], c[2], c[3], max = 255)
  } else if (!is.character(bar_colour) | length(bar_colour) != 1) {
    stop("Unexpected input - bar_colour should be a single colour name.")
  }
  
  # Validate table
  if (!is.data.frame(table)) {
    stop("Unexpected input - table is not a data.frame.")
  } else if (ncol(table) != 2) {
    stop("Unexpected input - table does not contain two columns.")
  } else if (!is.numeric(table[[2]])) {
    stop("Unexpected input - table column 2 is not numeric.")
  }
  
  # Validate labels
  if (!is.character(xlab) | !is.character(ylab) | length(xlab) > 1 | length(ylab) > 1) {
    stop("Unexpected input - labels should be single character strings.")
  }
  
  # Validate n
  if ((!is.numeric(n) & !is.character(n)) | length(n) > 1) {
    stop("Unexpected input - n is not a single number or string")
  }
  
  # Validate font size
  if (!is.numeric(font_size)) {
    stop("Unexpected input - font_size is not numeric.")
  }
  
  # Validate orientation
  if (!(orientation %in% c("h", "v"))) {
    stop("Unexpected input - orientation should be set to 'h' or 'v'")
  }
  
  # Apply break_q_names to a column
  if(!is.null(break_q_names_col)) {
    # Coerce to character type
    table[[break_q_names_col]] <- as.character(table[[break_q_names_col]])
    
    table[[break_q_names_col]] <- break_q_names(table[[break_q_names_col]], max_lines = max_lines)
    
    table[[break_q_names_col]] <- factor(table[[break_q_names_col]], levels = table[[break_q_names_col]])
  }
  
  x <- list(
    title = xlab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  y <- list(
    title = ylab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  if (orientation == "v") {
    table <- dplyr::arrange(table, table[,1])
    table[,1] <- factor(table[,1], levels = table[,1])
    x_vals <- table[[1]]
    y_vals <- table[[2]]
    x_axis <- x
    y_axis <- y
  } else if (orientation == "h") {
    table <- dplyr::arrange(table, dplyr::desc(table[,1]))
    table[,1] <- factor(table[,1], levels = table[,1])
    x_vals <- table[[2]]
    y_vals <- table[[1]]
    x_axis <- y
    y_axis <- x
  }
  
  ylab <- y_axis$title
  y_axis$title <- "" # Y axis title is created as a caption instead
  
  fig <- plotly::plot_ly(
    x = x_vals,
    y = y_vals,
    marker = list(color = bar_colour),
    type = "bar",
    orientation = orientation,
    ...
  )
  
  fig <- plotly::config(fig, displayModeBar = F)
  fig <- plotly::layout(fig,
                        xaxis = x_axis, 
                        yaxis = y_axis,
                        margin = list(b = 100),
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                        annotations = list(x = 1, y = 0, text = paste0("Sample size = ", n), 
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size))
  )
  
  fig <- plotly::layout(fig, annotations = create_y_lab(ylab, font_size))
  
  return(fig)
  
}

#'@title Plot stacked bar graph
#'
#'@description Produce stacked bar chart (plotly). 
#'
#'@param table Frequency table for stacked bar chart (data frame). 3+ columns - sub-question names in column 1 with answer options in subsequent columns.. 
#'@param colour_scale type of colour scale ("gradient", "scale" or "2gradients"). See get_gradient(), get_2colour_scale() and get_2colour_gradients(). 
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param n sample size
#'@param font_size minimum font size for the plot (numeric).
#'@param neutral_mid whether the midpoint of the colour scale should be neutral ("2gradients" scale only). TRUE by default
#'@param ... additional plotly_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_stacked <- function(table, xlab, ylab, n, colour_scale = "2gradients", font_size = 12, neutral_mid = TRUE, ...) {
  
  # Validate table
  if (!is.data.frame(table)) {
    stop("Unexpected input - table is not a data.frame.")
  } else if (ncol(table) < 3) {
    stop("Unexpected input - table should have at least three columns")
  }
  
  # Validate labels
  if (!is.character(xlab) | !is.character(ylab) | length(xlab) > 1 | length(ylab) > 1) {
    stop("Unexpected input - labels should be single character strings.")
  }
  
  # Validate n
  if ((!is.numeric(n) & !is.character(n)) | length(n) > 1) {
    stop("Unexpected input - n is not a single number or string")
  }
  
  # Validate font size
  if (!is.numeric(font_size)) {
    stop("Unexpected input - font_size is not numeric.")
  }
  
  # Validate colour_scale
  if (length(colour_scale) > 1 | !colour_scale %in% c("gradient", "scale", "2gradients", "3scale")) {
    stop("Unexpected input - colour_scale should be set to 'gradient', 'scale', '2gradients' or '3scale'.")
  }
  
  x <- list(
    title = xlab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  y <- list(
    title = "",
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  #reorder table
  table <- dplyr::arrange(table, dplyr::desc(table[,1]))
  table[,1] <- factor(table[,1], levels = table[,1])
  
  # Reshape data
  suppressMessages(
    longdata <- reshape2::melt(table)
  )

  
  # Get bar colours
  ncolours <- ncol(table) - 1
  if (colour_scale == "gradient") {
    colours <- get_gradient(ncolours)
  } else if (colour_scale == "scale") {
    colours <- get_2colour_scale(ncolours)
  } else if (colour_scale == "2gradients") {
    mid <- ceiling(ncolours/2)
    colours <- get_2colour_gradients(ncol(table)-1, mid = mid, neutral_mid = neutral_mid)
  } else if (colour_scale == "3scale") {
    colours <- get_3colour_scale(ncolours)
  }
  
  colours <- lapply(colours, function(x) grDevices::rgb(x[1], x[2], x[3], max = 255))
  colours <- lapply(colours, function(x) rep(x, nrow(table)))
  colours <- unlist(colours)
  
  hovertext <- paste0(longdata[[2]], ": ", longdata[[3]], " <extra></extra>")
  
  fig <- plotly::plot_ly(y = longdata[[1]], 
                         x=longdata[[3]], 
                         type="bar", 
                         color = longdata[[2]], 
                         orientation = "h", 
                         hovertemplate = hovertext,
                         marker = list(color = colours),
                         ...)
  
  fig <- plotly::config(fig, displayModeBar = F)
  
  fig <- plotly::layout(fig,  
                        barmode = "stack", 
                        clickmode = "none",
                        legend = list(orientation = "h",   # show entries horizontally
                                      xanchor = "center",  # use center of legend as anchor
                                      yanchor = "bottom",
                                      x = 0.5,
                                      y = 1,
                                      traceorder = "normal",
                                      font = list(size = font_size)),
                        margin = list(b = 100),
                        xaxis = x, 
                        yaxis = y,
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                        annotations = list(x = 1, y = 0, text = paste0("Sample size = ", n), 
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size)
                        )
  )
  
  fig <- plotly::layout(fig, annotations = create_y_lab(ylab, font_size))
  
  return(fig)
  
}

#'@title Plot likert graph
#'
#'@description Produce likert stacked bar chart (plotly). At least 2 questions per plot.
#'
#'@param table Frequency table for likert quesitons (data frame). 4+ columns - question names in column 1 with answer options in subsequent columns. Frequencies should proportions, between 0 and 1. 
#'@param mid the mid-point of the scale. should be higher than 2 and lower than the number of answers.
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param n sample size
#'@param font_size minimum font size for the plot (numeric).
#'@param neutral_mid whether the middle of the scale should be a neutral category (logical). TRUE by default
#'@param break_q_names_col applies break_q_names to the column. Not applied by default
#'@param ... additional plot_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_likert <- function(table, mid, xlab, ylab, n, font_size = 12, neutral_mid = TRUE, break_q_names_col = NULL, ...) {
  
  # Validate table
  if (!is.data.frame(table)) {
    stop("Unexpected input - table is not a data.frame.")
  } else if (ncol(table) < 4 | nrow(table) < 2) {
    stop("Unexpected input - table should have at least four columns and two rows.")
  }
  
  # Validate labels
  if (!is.character(xlab) | !is.character(ylab) | length(xlab) > 1 | length(ylab) > 1) {
    stop("Unexpected input - labels should be single character strings.")
  }
  
  # Validate n
  if ((!is.numeric(n) & !is.character(n)) | length(n) > 1) {
    stop("Unexpected input - n is not a single number or string")
  }
  
  # Validate font size
  if (!is.numeric(font_size)) {
    stop("Unexpected input - font_size is not numeric.")
  }
  
  # Validate mid
  if (!is.numeric(mid)) {
    stop("Unexpected input - mid is not numeric.")  
  } else if (mid < 2) {
    stop("Unexpected inout - mid is smaller than 2.")
  } else if (neutral_mid & mid > ncol(table)-2) {
    stop("Unexpected input - mid >= the number of answers.")
  } else if (neutral_mid & mid > ncol(table)-1) {
    stop("Unexpected input - mid >= the number of answers.")
  }
  
  # Validate neutral mid
  if (!is.logical(neutral_mid)) {
    stop("Unexpected input - mid is not logical (TRUE/FALSE)")
  }
  
  # Apply break_q_names to a column
  if(!is.null(break_q_names_col)) {
    table[[break_q_names_col]] <- break_q_names(table[[break_q_names_col]])
    table[[break_q_names_col]] <- factor(table[[break_q_names_col]], levels = table[[break_q_names_col]])
    
  }
  
  x <- list(
    title = xlab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2),
    range = list(-1, 1), 
    tickformat = ".0%", title = "Percent"
  )
  
  y <- list(
    title = "",
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  # Reorder table 
  table <- dplyr::arrange(table, dplyr::desc(table[,1]))
  table[,1] <- factor(table[,1], levels = table[,1])
  
  # Reshape data
  suppressMessages(
    longdata <- reshape2::melt(table)
  )
  
  # Calculate bases for bars
  bases <- apply(table[2:ncol(table)], 1, cumsum)
  
  # Bases are needed as a vector for plotly
  bases <- as.vector(apply(bases, 1, function(x) x))
  
  # Remove the values corresponding to the last response option so the base for the final respnse option
  # is the cumulative sum up to and including the previous response option. Add bases of 0 for the first 
  # response option.
  bases <- utils::head(bases, -nrow(table)) 
  bases <- c(rep(0, nrow(table)), bases)
  
  # Subtract negative bases from the cumulative sums. This will shift the stacked bars back into the negative
  # part of the chart. 
  if (neutral_mid) {
    negative_bases <- rowSums(table[c(2:mid)]) + table[mid + 1]/2  
  } else {
    negative_bases <- rowSums(table[c(2:mid)])
  }
  
  negative_bases <- unname(unlist(negative_bases))
  bases <- bases - negative_bases
  
  # Get bar colours
  if (neutral_mid) {
    colours <- get_2colour_gradients(ncol(table)-1, mid = mid, neutral_mid = neutral_mid)
  } else {
    colours <- get_2colour_gradients(ncol(table)-1, mid = mid-1, neutral_mid = neutral_mid)
  }
  
  colours <- lapply(colours, function(x) grDevices::rgb(x[1], x[2], x[3], max = 255))
  colours <- lapply(colours, function(x) rep(x, nrow(table)))
  colours <- unlist(colours)
  
  hovertext <- paste0(longdata[[2]], ": ", round(abs(longdata[[3]]) * 100, 1), "%", " <extra></extra>")

  fig <- plotly::plot_ly(y = longdata[[1]], 
                         x=longdata[[3]], 
                         type="bar", 
                         color = longdata[[2]], 
                         orientation = "h", 
                         base = bases,
                         hovertemplate = hovertext,
                         marker = list(color = colours),
                         ...)
  
  fig <- plotly::config(fig, displayModeBar = F)
  
  fig <- plotly::layout(fig,  
                        barmode = "stack", 
                        clickmode = "none",
                        margin = list(b = 100),
                        annotations = list(x = 1, y = 0, text = paste0("Sample size = ", n), 
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size)),
                        xaxis = x, 
                        yaxis = y, 
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)))
  
  fig <- plotly::layout(fig, annotations = create_y_lab(ylab, font_size))
  
  fig <- plotly::layout(fig, legend = list(xanchor = "left",
                                           yanchor = "bottom",
                                           orientation = "h",
                                           y = 1,
                                           traceorder = "normal",
                                           font = list(size = font_size))
                        )
  
  # Disable interactive legend
  
  id <- paste0("plot", stringi::stri_rand_strings(1, 10))
  javascript <- paste0(id, ".on('plotly_legenddoubleclick', function(d, i) {return false});",
                       id, ".on('plotly_legendclick', function(d, i) {return false});")
  
  fig$elementId <- id
  fig <- htmlwidgets::prependContent(fig, htmlwidgets::onStaticRenderComplete(javascript), data=list(''))
  
  return(fig)
  
}

#'@title Plot grouped frequency graph
#'
#'@description Produce bar chart (plotly) for frequency data with grouping variable. 
#'
#'@param table Frequency table (data frame). 3 columns - cateogry names, groups and frequencies. 
#'@param xlab X axis title
#'@param ylab Y axis title
#'@param n sample size
#'@param font_size minimum font size for the plot (numeric).
#'@param orientation plot orientation ("h" = horizontal, "v" = verical). Vertical by default.
#'@param ... additional plotly_ly arguments
#'
#'@return bar chart
#'
#'@export

plot_grouped <- function(table, xlab, ylab, n, font_size = 12, orientation = "v", ...) {
  
  # Set default bar colours
  n_groups <- length(unique(table[[2]]))
  c <- get_2colour_scale(n_groups)
  colours <- unlist(lapply(c, function(x) grDevices::rgb(x[1], x[2], x[3], maxColorValue = 255))) 
  colours <- unlist(colours)
  colours <- rep(colours, c(unlist(table(table[[2]]))))
  
  # Validate table
  if (!is.data.frame(table)) {
    stop("Unexpected input - table is not a data.frame.")
  } else if (ncol(table) != 3) {
    stop("Unexpected input - table does not contain 3 columns.")
  } else if (!is.numeric(table[[3]])) {
    stop("Unexpected input - table column 3 is not numeric.")
  }
  
  # Validate labels
  if (!is.character(xlab) | !is.character(ylab) | length(xlab) > 1 | length(ylab) > 1) {
    stop("Unexpected input - labels should be single character strings.")
  }
  
  # Validate n
  if ((!is.numeric(n) & !is.character(n)) | length(n) > 1) {
    stop("Unexpected input - n is not a single number or string")
  }
  
  # Validate font size
  if (!is.numeric(font_size)) {
    stop("Unexpected input - font_size is not numeric.")
  }
  
  # Validate orientation
  if (!(orientation %in% c("h", "v"))) {
    stop("Unexpected input - orientation should be set to 'h' or 'v'")
  }
  
  x <- list(
    title = xlab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  y <- list(
    title = ylab,
    tickfont = list(size = font_size),
    titlefont = list(size = font_size * 1.2)
  )
  
  if (orientation == "v") {
    table <- dplyr::arrange(table, table[,1])
    table[,1] <- factor(table[,1], levels = table[,1])
    x_vals <- table[[1]]
    y_vals <- table[[3]]
    x_axis <- x
    y_axis <- y
  } else if (orientation == "h") {
    table <- dplyr::arrange(table, dplyr::desc(table[,1]))
    table[,1] <- factor(table[,1], levels = table[,1])
    x_vals <- table[[3]]
    y_vals <- table[[1]]
    x_axis <- y
    y_axis <- x
  }
  
  ylab <- y_axis$title
  y_axis$title <- ""
  
  fig <- plotly::plot_ly(
    x = x_vals,
    y = y_vals,
    color = table[[2]],
    marker = list(color = colours),
    type = "bar",
    ...
  )
    
  fig <- plotly::config(fig, displayModeBar = F)
  fig <- plotly::layout(fig,  
                        xaxis = x_axis, 
                        yaxis = y_axis, 
                        margin = list(b = 100),
                        legend = list(traceorder = "reversed"),
                        hoverlabel = list(bgcolor = "white", font = list(size = font_size)),
                        annotations = list(x = 1, y = 0, text = paste0("Sample size = ", n), 
                                           showarrow = F, xanchor='right', yanchor='auto', xshift=0, yshift=-100,
                                           xref='paper', yref='paper', font=list(size = font_size))
  )
  
  fig <- plotly::layout(fig, annotations = create_y_lab(ylab, font_size))
  
  return(fig)
  
}


#'@title Create custom Y axis label
#'
#'@description Create a custom y axis label (plotly annotation). This label is placed just above the y axis
#' and is horizontal, to replace the vertically flipped label produced by default. 
#'
#'@param ylab Y axis label
#'@param font_size font size used in the chart. This function will return a slightly larger font.
#'
#'@return list of parameters for plotly annotation
#'
#'@export

create_y_lab <- function(ylab, font_size) {
  annotation <- list(text = ylab, # Custom Y axis label 
                     y = 1,
                     x = "min",
                     showarrow = FALSE, 
                     yshift = 30,
                     xref = "paper",
                     yref = "paper",
                     font = list(size = font_size * 1.2)
  )

  return(annotation)

}