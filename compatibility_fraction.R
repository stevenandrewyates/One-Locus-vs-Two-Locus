#' Fraction of compatible crosses in a two-locus SI system
#'
#' Calculates the expected fraction of compatible crosses given
#' the number of S and Z alleles in a two-locus self-incompatibility system.
#'
#' The fraction is computed as:
#'
#'   (total crosses − incompatible crosses) / total crosses
#'
#' where total crosses excludes selfings.
#'
#' @param S Integer. Number of alleles at the S locus.
#' @param Z Integer. Number of alleles at the Z locus.
#'
#' @return Numeric value between 0 and 1 representing the fraction
#' of compatible crosses.
#'
#' @examples
#' compatibility_fraction(3,3)
compatibility_fraction <- function(S, Z) {

  if (!is.numeric(S) || S < 2) {
    stop("S must be a numeric value ≥ 2.")
  }

  if (!is.numeric(Z) || Z < 1) {
    stop("Z must be a numeric value ≥ 1.")
  }

  G <- genotypes_2locus(S, Z)

  total_crosses <- G * (G - 1)

  incompatible <- incompatible_crosses(S, Z)

  return((total_crosses - incompatible) / total_crosses)
}
