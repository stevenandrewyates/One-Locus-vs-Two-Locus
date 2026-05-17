#' Number of possible crosses in a two-locus SI system
#'
#' Calculates the total number of possible genotype crosses in a
#' two-locus self-incompatibility system, excluding selfings.
#'
#' If `G` is the number of genotypes, the number of ordered crosses is:
#'
#'   G(G − 1)
#'
#' @param S Integer. Number of alleles at the S locus.
#' @param Z Integer. Number of alleles at the Z locus.
#'
#' @return Integer representing the number of possible crosses.
#'
#' @examples
#' number_of_crosses(4,4)
number_of_crosses <- function(S, Z) {

  if (!is.numeric(S) || S < 2) {
    stop("S must be a numeric value ≥ 2.")
  }

  if (!is.numeric(Z) || Z < 1) {
    stop("Z must be a numeric value ≥ 1.")
  }

  G <- genotypes_2locus(S, Z)

  return(G * (G - 1))
}
