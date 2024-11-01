
#' Separate multiple failure modes into multiple rows
#'
#' @param data a `data.frame`
#' @param failure_mode the column in `data` containing the failure modes
#' @param sep a regular expression with the character(s) separating individual
#'            failure modes. Default `"[/, ]+"`.
#'
#' @description
#' For a `data.frame` containing a column with (some) multiple failure modes,
#' this function expands the `data.frame` by repeating each row with multiple
#' failure modes so that each row contains only a single failure mode.
#'
#' @details
#' When multiple failure modes are reported, they are commonly reported in
#' the format "LGM/GIT" or "LGM,GIT". This function will separate these multiple
#' failure modes into multiple rows.
#'
#' This can be useful when counting the number of coupons exhibited each
#' failure mode.
#'
#' @examples
#' library(dplyr)
#' data.frame(strength = c(101, 102), fm = c("LGM/GIT", "LGM")) %>%
#'   separate_failure_modes(fm)
#' ##
#' ## # A tibble: 3 × 2
#' ##   strength fm
#' ##      <dbl> <chr>
#' ## 1      101 LGM
#' ## 2      101 GIT
#' ## 3      102 LGM
#'
#' @importFrom tidyr separate_rows
#'
#' @export
separate_failure_modes <- function(data, failure_mode, sep = "[/, ]+") {
  separate_rows(data, {{ failure_mode }}, sep = sep)
}



#' @importFrom ggplot2 GeomPoint
#nolint start
GeomPointFailureMode <- ggproto(
  "GeomPointFailureMode",
  GeomPoint,
#nolint end
  required_aes = c("x", "y", "colour|shape"),
  extra_params = c("sep"),
  setup_data = function(data, params) {
    if ("colour" %in% names(data)) {
      new_data <- separate_failure_modes(data, "colour", sep = params$sep)
      if ("shape" %in% names(data)) {
        # both colour and shape specified
        if (!identical(data$colour, data$shape)) {
          stop(paste0("If both `shape` and `colour` are specified, ",
                      "geom_jitter_failure_mode requires that both are equal"))
        }
        new_data$shape <- new_data$colour
      }
    } else if ("shape" %in% names(data)) {
      new_data <- separate_failure_modes(data, "shape", sep = params$sep)
    } else {
      # neither were specified
      new_data <- data
    }
    new_data
  }
)

#' Jittered points showing (possibly multiple) failure modes
#'
#' @description
#' The `geom_jitter_failure_mode` is very similar to
#' [ggplot2::geom_jitter()] except that a failure mode variable specified
#' as the color and/or shape aesthetic is parsed to separate multiple
#' failure modes and plot them separately. For example, if an observation
#' has the failure mode "LAT/LAB", two points will be plotted, one with the
#' failure mode "LAT" and the second with the failure mode "LAB".
#'
#'
#' @param mapping Set of aesthetic mapping created by `aes()`.
#'                See [ggplot2::geom_jitter()] for additional details.
#' @param data The data to be displayed by this layer.
#'             See [ggplot2::geom_jitter()] for additional details.
#' @param stat The statistical transformation to use on the data for this layer.
#'             See [ggplot2::geom_jitter()] for additional details.
#' @param position A position adjustment to use on the data for this layer.
#'                 See [ggplot2::geom_jitter()] for additional details.
#' @param ... Other arguments passed on to `layer()`'s `params` argument.
#'            See [ggplot2::geom_jitter()] for additional details.
#' @param width The amount of horizontal jitter.
#'              See [ggplot2::geom_jitter()] for additional details.
#' @param height The amount of vertical jitter.
#'               See [ggplot2::geom_jitter()] for additional details.
#' @param na.rm If FALSE, the default, missing values are removed with warning.
#'              See [ggplot2::geom_jitter()] for additional details.
#' @param show.legend `NA`, the default, indicates that nay aesthetics are
#'                    mapped.
#'                    See [ggplot2::geom_jitter()] for additional details.
#' @param inherit.aes if FALSE, overrides the default aesthetics.
#'                    See [ggplot2::geom_jitter()] for additional details.
#' @param sep A regular expression indicating the character(s) separating
#'            multiple failure modes. By default `"[/, ]+"`
#'
#' @details
#' The variable specified for the aesthetics `shape` and `color` are passed
#' to the function [separate_failure_modes()] to parse the failure modes and
#' separate multiple failure modes separated by character(s) specified in
#' the regular expression given in the parameter `sep`. By default, multiple
#' failure modes are expected to be separated by spaces, commas or forward
#' slashes, but this can be overridden.
#'
#' If both `shape` and `color` aesthetics are specified, both must be identical.
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#' carbon.fabric.2 %>%
#'   filter(test == "WT") %>%
#'   ggplot(aes(x = condition, y = strength)) +
#'   geom_boxplot() +
#'   geom_jitter_failure_mode(aes(color = failure_mode, shape = failure_mode))
#'
#' @seealso [separate_failure_modes()]
#' @seealso [ggplot2::geom_jitter()]
#'
#' @importFrom ggplot2 position_jitter
#' @importFrom ggplot2 layer
#'
#' @export
geom_jitter_failure_mode <- function(mapping = NULL, data = NULL,
                                     stat = "identity", position = "jitter",
                                     ...,
                                     width = NULL,
                                     height = NULL,
                                     #nolint start
                                     na.rm = FALSE,
                                     show.legend = NA,
                                     inherit.aes = TRUE,
                                     #nolint end
                                     sep = "[/, ]+") {
  if (!missing(width) || !missing(height)) {
    if (!missing(position)) {
      stop(paste0(
        "Both `position` and `width`/`height` were supplied. ",
        "Choose a single approach to alter the position."
      ))
    }

    position <- position_jitter(width = width, height = height)
  }

  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPointFailureMode,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list2(
      na.rm = na.rm,
      sep = sep,
      ...
    )
  )
}
