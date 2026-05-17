#' Number of genotypes in a one-locus SI system
#'
#' Calculates the number of possible diploid genotypes in a
#' one-locus self-incompatibility system with `S` alleles.
#' Only heterozygous combinations are allowed.
#'
#' @param S Integer. Number of S alleles.
#'
#' @return Integer representing the number of possible genotypes.
#'
#' @examples
#' genotypes_1locus(4)
genotypes_1locus <- function(S) {

  if (!is.numeric(S) || length(S) != 1 || S < 2) {
    stop("S must be a numeric value ≥ 2.")
  }

  return(choose(S, 2))
}
