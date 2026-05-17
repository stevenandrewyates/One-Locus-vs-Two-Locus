# Two-Locus Self-Incompatibility Simulation

This repository contains *R* functions for simulating and analysing
compatibility patterns in one- and two-locus self-incompatibility (SI)
systems.

The code allows users to:

- simulate diploid SI populations
- compute compatibility matrices among genotypes
- predict analytical properties of SI systems
- visualise compatibility patterns

## 2️⃣ Biological background

Self-incompatibility systems prevent self-fertilization and promote
outcrossing. In two-locus systems, compatibility is determined by
interactions between alleles at the *S* and *Z* loci.

Each genotype carries two *S* alleles and two *Z* alleles. Pollen haplotypes
(*S*,*Z*) are compared against the maternal stigma. If both alleles of a
pollen haplotype are present in the stigma, fertilization is rejected.

This repository provides simulation and analytical tools for exploring
compatibility patterns in such systems.

## 3️⃣ Installation

Clone the repository:
```bash
git clone https://github.com/yourname/si-simulation.git
```

Open the project in R and source the functions:

```r
list.files("R", full.names = TRUE) |> lapply(source)
```
## 4️⃣ Example

Simulate a population with 3 *S* alleles and 3 *Z* alleles.

```r
pop <- simulate_population(3,3)

result <- predict_compatibility(pop)

result$predicted_matrix
```

Example output:

G1   G2   G3

G1 0.0 0.75 0.50

G2 0.75 0.0 0.75

G3 0.50 0.75 0.0
  
  
---

## 5️⃣ Plot example

Plot compatibility heatmap

```r
fig <- plot_compatibility(3,3)
fig
```


---

## 6️⃣ Functions overview

Very helpful for users.


## 7️⃣ Functions

### Simulation

| Function | Description |
|--------|--------|
| `simulate_population()` | Generate a population with *S* and *Z* alleles |
| `check_compatibility()` | Evaluate compatibility between two genotypes |
| `predict_compatibility()` | Compute compatibility matrix across population |
| `simulate_generations()` | Simulate population expansion across generations |

### Analytical predictions

| Function | Description |
|--------|--------|
| `genotypes_1locus()` | Number of genotypes in one-locus system |
| `genotypes_2locus()` | Number of genotypes in two-locus system |
| `number_of_crosses()` | Total number of possible crosses |
| `incompatible_crosses()` | Number of incompatible crosses |
| `compatibility_fraction()` | Expected fraction of compatible crosses |

### Visualisation

| Function | Description |
|--------|--------|
| `plot_compatibility()` | Plot compatibility heatmap |

## 8️⃣ Dependencies

The following R packages are required:

- ggplot2
- reshape2

## 9️⃣ Citation

If you use this code please cite:

Author et al. (Year)  
Title of paper  
Journal

## 🔟 License

MIT License

