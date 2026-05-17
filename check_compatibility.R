#' Check compatibility between two genotypes
#'
#' Evaluates compatibility between a maternal genotype (stigma) and a
#' paternal genotype (pollen donor) under the heterozygous two-locus model.
#' The function returns both the compatibility score and the set of viable
#' offspring genotypes produced by compatible pollen haplotypes.
#'
#' @param mother Character. Genotype ID of the maternal parent.
#' @param father Character. Genotype ID of the paternal parent.
#' @param seq_data Data frame containing genotype IDs and allele columns:
#'   `Genotype`, `S_locus_1`, `S_locus_2`, `Z_locus_1`, `Z_locus_2`.
#'
#' @return A named list with:
#'   - `Compatibility`: fraction of compatible pollen haplotypes (0 to 1)
#'   - `OffSpring`: matrix of viable offspring genotypes
#'
#' @examples
#' pop <- simulate_population(3, 3)
#' check_compatibility("G1", "G2", pop)
check_compatibility <- function(mother, father, seq_data) {

  required_cols <- c("Genotype", "S_locus_1", "S_locus_2", "Z_locus_1", "Z_locus_2")
  if (!all(required_cols %in% names(seq_data))) {
    stop("seq_data must contain columns: ", paste(required_cols, collapse = ", "))
  }

  if (!mother %in% seq_data$Genotype) {
    stop("Genotype ", mother, " not found in seq_data.")
  }

  if (!father %in% seq_data$Genotype) {
    stop("Genotype ", father, " not found in seq_data.")
  }

  mother_row <- seq_data[seq_data$Genotype == mother, , drop = FALSE]
  father_row <- seq_data[seq_data$Genotype == father, , drop = FALSE]

  # Label alleles so S and Z classes remain distinct
  s_m1 <- paste0("S", mother_row$S_locus_1)
  s_m2 <- paste0("S", mother_row$S_locus_2)
  z_m1 <- paste0("Z", mother_row$Z_locus_1)
  z_m2 <- paste0("Z", mother_row$Z_locus_2)

  s_f1 <- paste0("S", father_row$S_locus_1)
  s_f2 <- paste0("S", father_row$S_locus_2)
  z_f1 <- paste0("Z", father_row$Z_locus_1)
  z_f2 <- paste0("Z", father_row$Z_locus_2)

  all_alleles <- c(s_m1, s_m2, z_m1, z_m2, s_f1, s_f2, z_f1, z_f2)
  if (any(is.na(all_alleles))) {
    stop("At least one allele is NA. Check seq_data for completeness.")
  }

  # Four possible pollen haplotypes from the father
  pollen_haplotypes <- list(
    c(s_f1, z_f1),
    c(s_f1, z_f2),
    c(s_f2, z_f1),
    c(s_f2, z_f2)
  )

  # Maternal stigma contains all four diploid alleles
  stigma <- c(s_m1, s_m2, z_m1, z_m2)

  # A pollen haplotype is incompatible if both of its alleles are present in the stigma
  incompatible <- vapply(pollen_haplotypes, function(pollen) {
    all(pollen %in% stigma)
  }, logical(1))

  compatibility <- mean(!incompatible)

  generate_offspring <- function(pollen) {
    if (all(pollen %in% stigma)) {
      return(NULL)
    }

    offspring_1 <- c(sort(c(pollen[1], s_m1)), sort(c(pollen[2], z_m1)))
    offspring_2 <- c(sort(c(pollen[1], s_m1)), sort(c(pollen[2], z_m2)))
    offspring_3 <- c(sort(c(pollen[1], s_m2)), sort(c(pollen[2], z_m1)))
    offspring_4 <- c(sort(c(pollen[1], s_m2)), sort(c(pollen[2], z_m2)))

    offspring <- rbind(offspring_1, offspring_2, offspring_3, offspring_4)

    # Remove S/Z prefixes so output matches the population table format
    offspring <- apply(offspring, 2, function(x) sub("^[SZ]", "", x))

    return(offspring)
  }

  offspring <- do.call(
    rbind,
    lapply(pollen_haplotypes, generate_offspring)
  )

  return(list(
    Compatibility = compatibility,
    OffSpring = offspring
  ))
}
