#' Plot compatibility in a two-locus SI system
#'
#' Simulates a starting population under a one- or two-locus SI system,
#' expands the population through two generations of offspring, and plots
#' the pairwise compatibility matrix among the resulting F2 genotypes.
#'
#' @param S Integer. Number of alleles at the S locus.
#' @param Z Integer. Number of alleles at the Z locus.
#'
#' @return A ggplot2 heatmap of pairwise compatibility values.
#'
#' @examples
#' fig <- plot_compatibility(3, 2)
#' fig
#'
#' pdf("compatibility_heatmap.pdf", width = 6, height = 6)
#' print(fig)
#' dev.off()
plot_compatibility <- function(S, Z) {

  if (!is.numeric(S) || length(S) != 1 || S < 2) {
    stop("S must be a numeric value >= 2.")
  }

  if (!is.numeric(Z) || length(Z) != 1 || Z < 1) {
    stop("Z must be a numeric value >= 1.")
  }

  build_population <- function(offspring) {
    if (is.null(offspring) || nrow(offspring) == 0) {
      stop("No offspring available to build the next generation.")
    }

    offspring <- as.data.frame(offspring, stringsAsFactors = FALSE)
    colnames(offspring) <- c("S_locus_1", "S_locus_2", "Z_locus_1", "Z_locus_2")

    data.frame(
      Genotype = paste0("G", seq_len(nrow(offspring))),
      S_locus_1 = offspring$S_locus_1,
      S_locus_2 = offspring$S_locus_2,
      Z_locus_1 = offspring$Z_locus_1,
      Z_locus_2 = offspring$Z_locus_2,
      stringsAsFactors = FALSE
    )
  }

  # Founder population
  population_F0 <- simulate_population(S, Z)

  # F0 -> F1
  cross_F0 <- predict_compatibility(population_F0)
  population_F1 <- build_population(cross_F0$offspring)

  # F1 -> F2
  cross_F1 <- predict_compatibility(population_F1)
  population_F2 <- build_population(cross_F1$offspring)

  # Compatibility among F2 genotypes
  cross_F2 <- predict_compatibility(population_F2)
  compatibility_matrix <- cross_F2$predicted_matrix

  if (is.null(compatibility_matrix) || nrow(compatibility_matrix) == 0) {
    stop("Compatibility matrix is empty.")
  }

  genotype_labels <- apply(population_F2, 1, function(row) {
    paste0(
      row["Genotype"],
      " (S:", row["S_locus_1"], "/", row["S_locus_2"],
      ", Z:", row["Z_locus_1"], "/", row["Z_locus_2"], ")"
    )
  })

  rownames(compatibility_matrix) <- genotype_labels
  colnames(compatibility_matrix) <- genotype_labels

  compat_df <- reshape2::melt(
    compatibility_matrix,
    varnames = c("Mother", "Father"),
    value.name = "Compatibility"
  )

  ggplot2::ggplot(
    compat_df,
    ggplot2::aes(x = Father, y = Mother, fill = Compatibility)
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::geom_text(
      ggplot2::aes(label = round(Compatibility, 2)),
      size = 3
    ) +
    ggplot2::scale_fill_gradient(
      low = "white",
      high = "steelblue",
      na.value = "gray90"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      panel.grid = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = paste0("Cross Compatibility Heatmap (S = ", S, ", Z = ", Z, ")"),
      x = "Pollen donor (father)",
      y = "Recipient (mother)",
      fill = "Compatibility"
    )
}
