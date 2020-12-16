
#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Stat
StatNormalSurvFunc <- ggproto(  # nolint
  "stat_normal_surv_func",
  Stat,
  compute_group = function(data, scales, n) {
    x <- seq(from = min(data$x), to = max(data$x), length.out = n)
    y <- pnorm(x, mean(data$x), sd(data$x), lower.tail = FALSE)
    data.frame(x = x, y = y)
  },
  required_aes = c("x")
)

#' Normal Survival Function
#'
#' @description
#' The Normal survival function provides a visualization of a
#' distribution. A normal curve is fit based on the mean and standard
#' deviation of the data, and the survival function of this normal
#' curve is plotted. The survival function is simply one minus the
#' CDF.
#'
#' @param mapping Set of aesthetic mappings created by `aes()`.
#' @param data The data to be displayed in this layer. This has the
#'             same usage as a `ggplot2` `stat` function.
#' @param geom The geometric object to use to display the data.
#' @param position Position argument
#' @param ... Other arguments to pass on to `layer`.
#' @param n If `NULL`, do not interpolated. Otherwise, the
#'          number of points to interpolate.
#' @param pad If `TRUE`, pad the ESF with additional points
#'            `(-Inf, 0)` and `(0, Inf)`.
#' @param show.legend Should this layer be included in the legends?
#' @param inherit.aes If `FALSE`, overrides the default aesthetic,
#'                    rather than combining with them.
#'
#' @importFrom ggplot2 layer
#'
#' @export
stat_normal_surv_func <- function(mapping = NULL, data = NULL,
                                  geom = "smooth", position = "identity",
                                  show.legend = NA, inherit.aes = TRUE,  # nolint
                                  n = 100, pad = FALSE, ...) {
  layer(
    stat = StatNormalSurvFunc, data = data, geom = geom, position = position,
    mapping = mapping, show.legend = show.legend,  # nolint
    inherit.aes = inherit.aes, params = list(n = n, ...)
  )
}

#' @importFrom ggplot2 ggproto
#' @importFrom ggplot2 Stat
StatESF <- ggproto(  # nolint
  "stat_esf",
  Stat,
  compute_group = function(data, scales, n, pad) {
    if (is.null(n)) {
      x <- unique(data$x)
    } else {
      x <- seq(from = min(data$x), to = max(data$x), length.out = n)
    }
    if (pad) {
      x <- c(-Inf, x, Inf)
    }
    y <- 1 - ecdf(data$x)(x)
    data.frame(x = x, y = y)
  },
  required_aes = c("x")
)

#' Empirical Survival Function
#'
#' @description
#' The empirical survival function (ESF) provides a visualization of a
#' distribution. This is closely related to the empirical cumulative
#' distribution function (ECDF). The empirical survival function is
#' simply ESF = 1 - ECDF.
#'
#' @param mapping Set of aesthetic mappings created by `aes()`.
#' @param data The data to be displayed in this layer. This has the
#'             same usage as a `ggplot2` `stat` function.
#' @param geom The geometric object to use to display the data.
#' @param position Position argument
#' @param ... Other arguments to pass on to `layer`.
#' @param n If `NULL`, do not interpolated. Otherwise, the
#'          number of points to interpolate.
#' @param pad If `TRUE`, pad the ESF with additional points
#'            `(-Inf, 0)` and `(0, Inf)`.
#' @param show.legend Should this layer be included in the legends?
#' @param inherit.aes If `FALSE`, overrides the default aesthetic,
#'                    rather than combining with them.
#'
#' @importFrom ggplot2 layer
#'
#' @export
stat_esf <- function(mapping = NULL, data = NULL, geom = "point",
                     position = "identity", show.legend = NA,  # nolint
                     inherit.aes = TRUE, n = NULL,  # nolint
                     pad = FALSE, ...) {
  layer(
    stat = StatESF, data = data, geom = geom, position = position,
    mapping = mapping, show.legend = show.legend,  # nolint
    inherit.aes = inherit.aes, params = list(n = n, pad = pad, ...)  # nolint
  )
}
