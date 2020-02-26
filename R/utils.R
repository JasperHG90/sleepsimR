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

# S3 generic
burn <- function(x, ...) {
  UseMethod("burn", x)
}
# Burn function for model output
burn.mHMM_cont <- function(x) {
  # Number of burn_in samples
  burn_in <- x$input$burn_in
  J <- x$input$J
  # For each element (and nested elements), remove the burn-in samples
  inp <- x$input
  x$input <- NULL
  # Get data types for each
  dtypes <- vapply(x, function(x) mode(x), "string")
  for(idx in seq_along(x)) {
    # If character, pass
    if(mode(x[[idx]]) == "character") {
      next
    } else if (mode(x[[idx]]) == "list") {
      for(subj_idx in seq_along(x[[idx]])) {
        x[[idx]][[subj_idx]] <- x[[idx]][[subj_idx]][(burn_in+1):J,]
      }
    } else {
      if (nrow(x[[idx]] < J)) {
        next
      }
      x[[idx]] <- x[[idx]][(burn_in+1:J),]
    }
  }
  # Create new object and return
  x$input <- inp
  class(x) <- "mHMM_cont"
  # Return
  return(x)
}

# S3 generic
MAP <- function(x, ...) {
  UseMethod("MAP", x)
}
# Retrieve MAP estimates for estimates
MAP.mHMM_cont <- function(x) {
  # Remove burn-in samples
  feelnobern <- burn(x)
  # Remove input
  feelnobern$input <- NULL
  # Get data types for each
  dtypes <- vapply(feelnobern, function(x) mode(x), "string")
  # Remove character types
  feelnobern <- feelnobern[!dtypes == "character"]
  # For each, collect MAP
  map_out <- vector("list", length(feelnobern))
  # Names
  names(map_out) <- names(feelnobern)
  for(param_idx in seq_along(feelnobern)) {
    # if numeric, compute MAP
    if(mode(feelnobern[[param_idx]]) == "numeric") {
      map_out[[param_idx]] <- apply(feelnobern[[param_idx]], 2, mean)
    } else {
      map_out[[param_idx]] <- lapply(feelnobern[[param_idx]], function(x) {
        apply(x, 2, mean)
      })
    }
  }
  # Return
  return(map_out)
}
