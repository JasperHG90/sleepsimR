## Utility functions

# Transform coefficients to probabilities
mnlr <- function(coefs) {
  exp(coefs) / (1 + sum(exp(coefs)))
}

# From intercepts to TPM
intercepts_to_TPM <- function(coefs) {
  opts <- getOptions("sleepsimR_simulate")
  # Create empty matrix
  tpm <- matrix(0L, nrow = opts$m, ncol=opts$m)
  # Coefficient index
  cur_var_idx <- 1
  # Populate
  for(row_idx in 1:nrow(tpm)) {
    for(col_idx in 2:ncol(tpm)) {
      print(col_idx)
      tpm[row_idx,col_idx] <- coefs[cur_var_idx]
      cur_var_idx <- cur_var_idx + 1
    }
  }
  # Populate first column
  tpm[,1] <- rep(1, nrow(tpm))
  # To probability
  tpm_prob <- apply(tpm, 1, mnlr)
  # Return
  return(tpm_prob)
}
