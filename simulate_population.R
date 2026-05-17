#' Simulate a population under a one- or two-locus SI system
#'
#' Generates diploid genotypes containing two S alleles and two Z alleles.
#' If `Z = 1`, the system behaves as a one-locus SI model.
#'
#' @param S Integer. Number of S alleles.
#' @param Z Integer. Number of Z alleles (set Z = 1 for a one-locus system).
#'
#' @return A data.frame containing simulated genotypes with columns:
#'   - Genotype
#'   - S_locus_1
#'   - S_locus_2
#'   - Z_locus_1
#'   - Z_locus_2
#'
#' @examples
#' simulate_population(3,3)
simulate_population <- function(S, Z) {

  # Input validation
  if (!is.numeric(S) || !is.numeric(Z)) {
    stop("Both S and Z must be numeric values.")
  }
  
  if (S < 2 || Z < 1) {
    stop("S must be ≥2 and Z ≥1 to generate meaningful heterozygotes.")
  }

  # Generate S allele combinations
  s_alleles <- seq_len(S)

  # If S is odd, duplicate the first allele so pairs can be formed
  if (S %% 2 == 1) {
    s_alleles <- c(s_alleles, 1)
  }

  s_matrix <- matrix(s_alleles, ncol = 2, byrow = TRUE)

  # Distribute Z alleles across S genotypes
  z_alleles <- seq_len(Z)
  z_repeat <- length(s_matrix) %/% Z
  z_remainder <- length(s_matrix) %% Z

  z_values <- rep(z_alleles, z_repeat)

  if (z_remainder > 0) {
    z_values <- c(z_values, z_alleles[seq_len(z_remainder)])
  }

  z_matrix <- t(matrix(z_values, ncol = nrow(s_matrix)))

  # Construct population data frame
  population <- data.frame(
    Genotype = paste0("G", seq_len(nrow(s_matrix))),
    S_locus_1 = s_matrix[,1],
    S_locus_2 = s_matrix[,2],
    Z_locus_1 = z_matrix[,1],
    Z_locus_2 = z_matrix[,2],
    stringsAsFactors = FALSE
  )

  return(population)
}
