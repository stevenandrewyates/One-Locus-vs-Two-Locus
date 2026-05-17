#' Predict pairwise compatibility across a population
#'
#' Iterates over all maternal × paternal genotype combinations in a population
#' and computes cross compatibility using `check_compatibility_heterozygous()`.
#' Returns both the full compatibility matrix and the unique set of viable
#' offspring genotypes produced across all crosses.
#'
#' @param seq_data Data frame in the format returned by `simulate_population()`,
#'   containing columns `Genotype`, `S_locus_1`, `S_locus_2`,
#'   `Z_locus_1`, and `Z_locus_2`.
#'
#' @return A named list with:
#'   - `predicted_matrix`: an `n x n` matrix of compatibility scores
#'   - `offspring`: unique viable offspring genotypes across all crosses
#'
#' @examples
#' pop <- simulate_population(3, 3)
#' result <- predict_compatibility(pop)
predict_compatibility <- function(seq_data) {

  required_cols <- c("Genotype", "S_locus_1", "S_locus_2", "Z_locus_1", "Z_locus_2")
  if (!all(required_cols %in% names(seq_data))) {
    stop("seq_data must contain columns: ", paste(required_cols, collapse = ", "))
  }

  n_genotypes <- nrow(seq_data)
  if (n_genotypes == 0) {
    stop("seq_data contains no rows.")
  }

  compatibility_matrix <- matrix(NA_real_, nrow = n_genotypes, ncol = n_genotypes)
  rownames(compatibility_matrix) <- seq_data$Genotype
  colnames(compatibility_matrix) <- seq_data$Genotype

  all_offspring <- NULL

  for (i in seq_len(n_genotypes)) {
    for (j in seq_len(n_genotypes)) {

      result <- tryCatch(
        check_compatibility(
          mother = seq_data$Genotype[i],
          father = seq_data$Genotype[j],
          seq_data = seq_data
        ),
        error = function(e) {
          warning(
            "Compatibility check failed for ",
            seq_data$Genotype[i], " x ", seq_data$Genotype[j],
            ": ", e$message
          )
          return(NULL)
        }
      )

      if (!is.null(result)) {
        compatibility_matrix[i, j] <- result$Compatibility

        if (!is.null(result$OffSpring)) {
          all_offspring <- rbind(all_offspring, result$OffSpring)
        }
      }
    }
  }

  if (!is.null(all_offspring)) {
    all_offspring <- unique(all_offspring)
    colnames(all_offspring) <- c("S_locus_1", "S_locus_2", "Z_locus_1", "Z_locus_2")
  }

  return(list(
    predicted_matrix = compatibility_matrix,
    offspring = all_offspring
  ))
}
