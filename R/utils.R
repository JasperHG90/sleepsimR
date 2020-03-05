## Utility functions

#' Convert a list of probabilities into an m x m transition probability matrix
#'
#' @param x an mHMM_cont object
#'
#' @return m x m transition probability matrix, where m is the number of hidden states
#' @export
get_subject_tpm <- function(x, ...) {
  UseMethod("get_subject_tpm", x)
}
get_subject_tpm.mHMM_cont <- function(x) {
  # Select probs
  p <- x$gamma_prob_bar
  # Select states
  m <- x$input$m
  # Create m x m matrix and return
  matrix(
    p,
    ncol = m,
    nrow = m,
    byrow = TRUE
  )
}

#' Burn function for model output
#'
#' @param x an mHMM_cont object
#'
#' @return mHMM_cont object for which the burn-in samples have been removed
#'         for each parameter.
#' @export
burn <- function(x, ...) {
  UseMethod("burn", x)
}
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

#' Retrieve MAP estimates for parameters
#'
#' @param x an mHMM_cont object
#'
#' @return List. Maximum a Posteriori (MAP) estimates for each parameter.
#'         names of the elements are identical of the names of the input
#'         parameters
#' @export
MAP <- function(x, ...) {
  UseMethod("MAP", x)
}
MAP.mHMM_cont <- function(x) {
  # Remove burn-in samples
  feelthebern <- burn(x)
  # Remove input
  feelthebern$input <- NULL
  # Get data types for each
  dtypes <- vapply(feelthebern, function(x) mode(x), "string")
  # Remove character types
  feelthebern <- feelthebern[!dtypes == "character"]
  # For each, collect MAP
  map_out <- vector("list", length(feelthebern))
  # Names
  names(map_out) <- names(feelthebern)
  for(param_idx in seq_along(feelthebern)) {
    # if numeric, compute MAP
    if(mode(feelthebern[[param_idx]]) == "numeric") {
      map_out[[param_idx]] <- apply(feelthebern[[param_idx]], 2, mean)
    } else {
      map_out[[param_idx]] <- lapply(feelthebern[[param_idx]], function(x) {
        apply(x, 2, mean)
      })
    }
  }
  # Return
  return(map_out)
}

#' Plot histograms of between-subject means
#'
#' @param x an mHMM_cont object
#'
#' @importFrom magrittr '%>%'
#' @import ggplot2
#'
#' @return plots the between-subject means
#' @export
plot_posterior_means <- function(x, ...) {
  UseMethod("plot_posterior", x)
}
plot_posterior_means.mHMM_cont <- function(x, var = 1) {
  # Remove burn-in samples
  # Get between-subject means
  betmu <- x %>%
    burn() %>%
    .$emiss_mu_bar
  # Cannot select a variable using an index that doesn't exist
  assertthat::assert_that(var <= length(betmu), msg = paste0(
    "You selected ", var,
    ". But there are only ", length(betmu), " independent variables."
  ))
  betmu <- as.data.frame(betmu[[var]])
  # To long format, plot etc.
  betmu_long <- vector("list", ncol(betmu))
  for(cn in seq_along(colnames(betmu))) {
    n <- nrow(betmu)
    betmu_long[[cn]] <- data.frame(
      "var" = rep(colnames(betmu)[cn], n),
      "val" = betmu[,colnames(betmu)[cn]]
    )
  }
  # Bind
  do.call(rbind.data.frame, betmu_long) %>%
    # Plot
    ggplot(., aes(x=val)) +
      geom_histogram() +
      theme_bw() +
      facet_wrap(". ~ var", ncol=1)
}
