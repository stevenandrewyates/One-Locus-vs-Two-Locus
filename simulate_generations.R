#' Simulate three generations of crosses in an SI population
#'
#' Starting from an initial population (F0), the function iteratively
#' predicts compatibility and generates offspring populations for
#' three generations (F1–F3). Summary statistics describing the final
#' generation are returned.
#'
#' @param sequence_data Data frame in the format returned by
#'   `simulate_population()`.
#'
#' @return Named numeric vector containing:
#'   - `Spos` : number of unique S alleles in the initial population
#'   - `Zpos` : number of unique Z alleles in the initial population
#'   - `F3_size` : number of genotypes in generation F3
#'   - `F3_mean_comp` : mean compatibility among F3 crosses (excluding selfings)
#'   - `F3_cross_pos` : fraction of F3 crosses with compatibility > 0
#'
#' @examples
#' pop <- simulate_population(3,3)
#' simulate_generations(pop)
simulate_generations <- function(sequence_data) {

  build_population <- function(offspring_matrix) {
    data.frame(
      Genotype = paste0("G", seq_len(nrow(offspring_matrix))),
      S_locus_1 = offspring_matrix[,1],
      S_locus_2 = offspring_matrix[,2],
      Z_locus_1 = offspring_matrix[,3],
      Z_locus_2 = offspring_matrix[,4],
      stringsAsFactors = FALSE
    )
  }

  # F0 → F1
  cross_F0 <- predict_compatibility(sequence_data)
  cat("Generated F0 cross data\n")

  F1 <- build_population(cross_F0$offspring)
  cat("Generated F1 population\n")

  # F1 → F2
  cross_F1 <- predict_compatibility(F1)

  F2 <- build_population(cross_F1$offspring)
  cat("Generated F2 population\n")

  # F2 → F3
  cross_F2 <- predict_compatibility(F2)

  F3 <- build_population(cross_F2$offspring)
  cross_F3 <- predict_compatibility(F3)
  cat("Generated F3 cross data\n")

  # Remove selfings
  F3_cx <- cross_F3$predicted_matrix[
    upper.tri(cross_F3$predicted_matrix) |
    lower.tri(cross_F3$predicted_matrix)
  ]

  F3_size <- nrow(F3)
  F3_mean_comp <- mean(F3_cx)
  F3_cross_pos <- mean(F3_cx > 0)

  # Allele richness in founder population
  Spos <- length(unique(c(sequence_data$S_locus_1, sequence_data$S_locus_2)))
  Zpos <- length(unique(c(sequence_data$Z_locus_1, sequence_data$Z_locus_2)))

  return(c(
    Spos = Spos,
    Zpos = Zpos,
    F3_size = F3_size,
    F3_mean_comp = F3_mean_comp,
    F3_cross_pos = F3_cross_pos
  ))
}
