
#' @importFrom rlang call2
build_nesting <- function(x, group_df, i_group, label, stat, type) {
  if (i_group > ncol(group_df)) {
    if (type == "Points") {
      nodes <- NULL
    } else {
      nodes <- list(build_nesting(x, group_df, i_group, label, stat, "Points"))
    }
  } else {
    group_levels <- levels(as.factor(group_df[[i_group]]))

    nodes <- lapply(group_levels, function(gl) {
      mask <- group_df[[i_group]] == gl

      node_x <- subset(x, mask)
      node_group_df <- subset(group_df, mask)
      n <- build_nesting(node_x, node_group_df, i_group + 1, gl, stat, "Label")
      n
    })
  }

  list(
    type = type,
    x = x,
    stat = eval(call2(.fn = stat, x)),
    label = label,
    nodes = nodes
  )
}

build_drawing_element_list <- function(nesting, group_df, y_gap) {
  elm_list <- list()

  # We'll need a queue for a level order tree traversal
  # We'll track the heights of each grouping too
  level_counts <- c()
  queue <- list()
  push_queue <- function(obj, parent, level) {
    # the root (not drawn) is level=1
    parent_level_order <- ifelse(is.null(parent), 1, parent$level_order)
    # level_heights is an accumulator that keeps increasing (by 1) as
    # subsequent items within a level are drawn
    level_counts[level] <<- sum(level_counts[level], 1, na.rm = TRUE)
    level_order <- level_counts[level]
    level_heights <- c(1, level_counts)
    ancestor_level_heights <- ifelse(level <= 2,
                                     0,
                                     sum(level_heights[2:(level - 1)]))
    y_obj <-
      ancestor_level_heights +
      y_gap * max(0, level - 2) +
      (parent_level_order - 1)
    queue <<- c(queue, list(list(
      obj = obj,
      parent = parent,
      level = level,
      level_order = level_order,
      y = -y_obj
    )))
  }
  pop_queue <- function() {
    obj <- queue[[1]]
    queue <<- queue[-1]
    obj
  }

  # We'll traverse the "nesting" tree using level-order traversal
  push_queue(nesting, NULL, 1)
  while (length(queue) > 0) {
    cur_item <- pop_queue()
    for (n in cur_item$obj$nodes) {
      push_queue(n, cur_item, cur_item$level + 1)
    }

    if (cur_item$obj$type != "Root") {
      elm_list <- c(elm_list, list(cur_item))
    }
  }

  minor_breaks <- sapply(seq_along(level_counts) + 1, function(level) {
    level_heights <- c(1, level_counts)
    ancestor_level_heights <- ifelse(level <= 2,
                                     0,
                                     sum(level_heights[2:(level - 1)]))
    y_obj <-
      ancestor_level_heights +
      y_gap * max(0, level - 2) -
      y_gap / 2 - 0.5
  })
  labels <- c(names(group_df), "")
  breaks <- minor_breaks[-1] - diff(minor_breaks) / 2

  axis_info <- list(
    breaks = rev(-breaks),
    labels = rev(labels),
    minor_breaks = rev(-minor_breaks)
  )

  list(
    elm_list = (elm_list),
    axis_info = (axis_info)
  )
}

#' @importFrom ggplot2 xlab ylab scale_y_continuous expansion theme
#' @importFrom ggplot2 element_blank element_line geom_hline
set_axes <- function(g, xlabel, axis_info, divider_color) {
  breaks <- axis_info$breaks
  labels <- axis_info$labels
  minor_breaks <- axis_info$minor_breaks
  g <- g +
    xlab(xlabel) +
    scale_y_continuous(
      breaks = breaks,
      labels = labels,
      minor_breaks = minor_breaks,
      limits = c(minor_breaks[1], minor_breaks[length(minor_breaks)]),
      expand = expansion()
    ) +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_line(size = 0.1, color = "black")) +
    ylab("")

  if (!is.null(divider_color)) {
    lapply(minor_breaks, function(b) {
      g <<- g + geom_hline(yintercept = b, color = divider_color)
    })
  }

  g
}

#' @importFrom purrr map_dfr
#' @importFrom rlang exec
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
draw_connectors <- function(g, elm_list, connector_args) {
  points <- map_dfr(
    elm_list,
    function(cur_item) {
      if (!is.null(cur_item$parent) & !is.na(cur_item$parent$y) &
          cur_item$parent$obj$type == "Label") {
        data.frame(
          x = cur_item$parent$obj$stat,
          y = cur_item$y
        )
      } else {
        NULL
      }
    }
  )

  if (nrow(points) == 0) {
    g
  } else {
    g +
      exec(
        ggplot2::geom_point,
        data = points,
        mapping = aes(x = .data$x, y = .data$y),
        !!!connector_args
      )
  }
}

#' @importFrom purrr map_dfr
#' @importFrom rlang exec
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
draw_vert_lines_to_labels <- function(g, elm_list, vline_args) {
  line_segments <- map_dfr(
    elm_list,
    function(cur_item) {
      if (!is.null(cur_item$parent) & !is.na(cur_item$parent$y) &
          cur_item$parent$obj$type == "Label") {
        data.frame(
          x = cur_item$parent$obj$stat,
          xend = cur_item$parent$obj$stat,
          y = cur_item$y,
          yend = cur_item$parent$y
        )
      } else {
        NULL
      }
    }
  )

  if (nrow(line_segments) == 0) {
    g
  } else {
    g +
      exec(
        ggplot2::geom_segment,
        data = line_segments,
        mapping = aes(x = .data$x, xend = .data$xend,
                      y = .data$y, yend = .data$yend),
        !!!vline_args
      )
  }
}

#' @importFrom purrr map_dfr
#' @importFrom rlang exec
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
draw_horiz_lines_to_labels <- function(g, elm_list, hline_args) {
  line_segments <- map_dfr(
    elm_list,
    function(cur_item) {
      if (!is.null(cur_item$parent) & !is.na(cur_item$parent$y) &
          cur_item$parent$obj$type == "Label") {
        data.frame(
          x = cur_item$obj$stat,
          xend = cur_item$parent$obj$stat,
          y = cur_item$y,
          yend = cur_item$y
        )
      } else {
        NULL
      }
    }
  )

  if (nrow(line_segments) == 0) {
    g
  } else {
    g +
      exec(
        ggplot2::geom_segment,
        data = line_segments,
        mapping = aes(x = .data$x, xend = .data$xend,
                      y = .data$y, yend = .data$yend),
        !!!hline_args
      )
  }
}

#' @importFrom purrr map_dfr
#' @importFrom rlang exec
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
draw_points <- function(g, elm_list, point_args, dline_args) {
  points <- map_dfr(
    elm_list,
    function(cur_item) {
      if (cur_item$obj$type == "Points") {
        data.frame(
          x = cur_item$obj$x,
          y = cur_item$y
        )
      } else {
        return(NULL)
      }
    }
  )

  line_segments <- map_dfr(
    elm_list,
    function(cur_item) {
      if (cur_item$obj$type == "Points") {
        data.frame(
          x = min(cur_item$obj$x),
          xend = max(cur_item$obj$x),
          y = cur_item$y,
          yend = cur_item$y
        )
      } else {
        return(NULL)
      }
    }
  )

  g +
    exec(
      ggplot2::geom_point,
      data = points,
      mapping = aes(x = .data$x, y = .data$y),
      !!!point_args
    ) +
    exec(
      ggplot2::geom_segment,
      data = line_segments,
      mapping = aes(
        x = .data$x,
        xend = .data$xend,
        y = .data$y,
        yend = .data$yend
      ),
      !!!dline_args
    )
}

#' @importFrom purrr map_dfr
#' @importFrom rlang exec
#' @importFrom ggplot2 aes
#' @importFrom rlang .data
draw_labels <- function(g, elm_list, label_args) {
  labels <- map_dfr(
    elm_list,
    function(cur_item) {
      if (cur_item$obj$type == "Label") {
        data.frame(
          x = cur_item$obj$stat,
          y = cur_item$y,
          label = cur_item$obj$label
        )
      } else {
        NULL
      }
    }
  )

  if (nrow(labels) == 0) {
    g
  } else {
    g +
      exec(
        ggplot2::geom_label,
        data = labels,
        mapping = aes(x = .data$x, y = .data$y, label = .data$label),
        !!!label_args
      )
  }
}

#' Create a plot of nested sources of variation
#'
#' @description
#' Creates a plot showing the breakdown of variation within a sample. This
#' function uses [ggplot2] internally.
#'
#' @param dat a `data.frame` or similar object
#' @param x the variable within `dat` to plot. Most often this would be a
#'          strength or modulus variable.
#' @param groups a vector of variables to group the data by
#' @param stat a function for computing the central location for each group.
#'             This is normally "mean" but could be "median" or another
#'             function.
#' @param y_gap the vertical gap between grouping variables
#' @param divider_color the color of the lines between grouping variables.
#'                      Or `NULL` to omit these lines.
#' @param point_args arguments to pass to [ggplot2::geom_point] when plotting
#'                   individual data points.
#' @param dline_args arguments to pass to [ggplot2::geom_segment] when plotting
#'                   the horizontal lines between data points.
#' @param vline_args arguments to pass to [ggplot2::geom_segment] when plotting
#'                   vertical lines
#' @param hline_args arguments to pass to [ggplot2::geom_segment] when plotting
#'                   horizontal lines connecting levels in groups
#' @param label_args arguments to pass to [ggplot2::geom_label] when plotting
#'                   labels
#' @param connector_args arguments to pass to [ggplot2::geom_point] when
#'                       plotting the connection between the vertical lines
#'                       and the horizontal lines connecting levels in groups
#'
#' @examples
#' library(dplyr)
#' carbon.fabric.2 %>%
#'   filter(test == "WT" & condition == "RTD") %>%
#'   nested_data_plot(strength,
#'                    groups = c(batch, panel))
#'
#' @importFrom rlang ensym
#' @importFrom ggplot2 ggplot
#' @importFrom dplyr ungroup
#' @export
nested_data_plot <- function(dat, x, groups = c(),
                             stat = "mean", y_gap = 1,
                             divider_color = "grey50",
                             point_args = list(),
                             dline_args = list(),
                             vline_args = list(),
                             hline_args = list(),
                             label_args = list(),
                             connector_args = list()) {
  dat <- ungroup(dat)
  group_df <- select(dat, !!enquo(groups))
  xlabel <- ensym(x)
  x <- select(dat, !!enquo(x))[[1]]
  nesting <- build_nesting(x, group_df, 1, "Root", stat, "Root")
  el <- build_drawing_element_list(nesting, group_df, y_gap)
  elm_list <- el$elm_list
  axis_info <- el$axis_info
  g <- ggplot()
  g <- set_axes(g, xlabel, axis_info, divider_color)
  g <- draw_vert_lines_to_labels(g, elm_list, vline_args)
  g <- draw_horiz_lines_to_labels(g, elm_list, hline_args)
  g <- draw_connectors(g, elm_list, connector_args)
  g <- draw_points(g, elm_list, point_args, dline_args)
  g <- draw_labels(g, elm_list, label_args)
  g
}
