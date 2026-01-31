protein_weight <- function(protein = "anika") {

  aa_weights <- c(
    A = 89.09, R = 174.20, N = 132.12, D = 133.10,
    C = 121.15, E = 147.13, Q = 146.15, G = 75.07,
    H = 155.16, I = 131.18, L = 131.18, K = 146.19,
    M = 149.21, F = 165.19, P = 115.13, S = 105.09,
    T = 119.12, W = 204.23, Y = 181.19, V = 117.15
  )

  protein <- toupper(protein)
  letters <- strsplit(protein, "")[[1]]

  if (any(!letters %in% names(aa_weights))) {
    return(0)
  }

  total_weight <- sum(aa_weights[letters])
  total_weight / 1000
}

# Essay
# I worked through the protein molecular weight calculation problem in R, treating the protein sequence as a string of characters representing individual amino acids. As the molecular weight of each amino(acyl) acid in Dalton is known, the first pass was to save these values in R as a named numeric vector where one letter codes were used as names and their corresponding molecular weights, again in Dalton, forming the values.
# Then I created a custom R function, which will accept a protein sequence as an input, and use my name as a default argument. In order to ensure that the function works well both in lower as well as upper case input, the order is transformed into an upper case. Strsplit() is then used to divide the sequence of the protein into the single-amino acids after which it gives out a vector of elements with single letters.
# Before the molecular weight is computed the functionalities identify the existence of all the characters of valid protein amino acids in the protein sequence, comparing them with the names of the vector values of the amino acids. As expected, the presence of a non-protein character in the form of B would automatically return the 0. Upon taking the validity of all the characters, the respective weights of the amino acids are obtained in the look-up vector and summed up to obtain the total molecular weight in Dalton. Finally, the final value is changed to KiloDalton by dividing it by 1000 and the result is returned.
