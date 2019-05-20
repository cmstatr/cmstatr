

#' Normalizes strength values to ply thickness
#'
#' @description
#' This function takes a strength value (or a vector of strength values) and a
#' value (or vector of the same length as strength) of the measured thicknesses
#' and returns the normalized strength.
#'
#' @param strength the strength to be normalized. Either a vector or a numeric
#' @param measured_thk the measured thickness of the samples. Must be the same
#' length as strength
#' @param nom_thk the nominal thickness. Must be a single numeric value.
#'
#' @return
#' A value (or vector) with the normalized strength values
#'
#' @details
#' It is often necessary to normalize strength values so that variation in
#' specimen thickness does not unnecessarily increase variation in strength.
#' See CMH-17, or other references, for information about the cases where
#' normalization is appropriate.
#'
#' Either cured ply thickness or laminate thickness may be used for
#' \code{measured_thk} and \code{nom_thk}, as long as CPT or laminate
#' thickness is used for both.
#'
#' The formula applied is:
#' \deqn{normalized value = test value \\frac{t_{measured}}{t_{nominal}}}{normalized value = test value * t_measured / t_nominal}
#'
#' If you need to normalize based on fiber volume fraction (or another method),
#' you will first need to calculate the nominal cured ply thickness (or laminate
#' thickness). Those calculations are outside the scope of this documentation.
#'
#' @export
normalize_ply_thickness <- function(strength, measured_thk, nom_thk) {
  if (length(strength) != length(measured_thk)) {
    stop("strength and measured_thk must be the same length")
  }
  if (length(nom_thk) != 1) {
    stop("nom_thk must be a single numeric value (not a vector)")
  }

  return(strength * measured_thk / nom_thk)
}
