#' Number of genotypes in a two-locus SI system
#'
#' Calculates the number of possible diploid genotypes in a
#' two-locus self-incompatibility system with `S` alleles at the
#' first locus and `Z` alleles at the second locus.
#'
#' The formula accounts for unordered diploid combinations at
#' each locus and excludes genotype pairs where both loci match.
#'
#' @param S Integer. Number of alleles at the S locus.
#' @param Z Integer. Number of alleles at the Z locus.
#'
#' @return Integer representing the number of possible genotypes.
#'
#' @examples
#' genotypes_2locus(3,3)
genotypes_2locus <- function(S, Z) {

  if (!is.numeric(S) || S < 2) {
    stop("S must be a numeric value ≥ 2.")
  }

  if (!is.numeric(Z) || Z < 1) {
    stop("Z must be a numeric value ≥ 1.")
  }

  # Number of diploid genotype combinations at each locus
  G_S <- S^2 - S*(S - 1)/2
  G_Z <- Z^2 - Z*(Z - 1)/2

  # Remove genotype pairs where both loci are identical
  return(G_S * G_Z - S * Z)
}
