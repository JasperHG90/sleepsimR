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
#' @export
get_subject_tpm.mHMM_cont <- function(x) {
  # Select probs
  p <- apply(x$gamma_prob_bar,2, mean)
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
#' @export
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
    # If label switching, pass
    } else if(names(x)[idx] == "label_switch") {
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
#' @export
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
    if(mode(feelthebern[[param_idx]]) == "numeric" & names(feelthebern)[param_idx] != "label_switch") {
      map_out[[param_idx]][["mean"]] <- apply(feelthebern[[param_idx]], 2, mean)
      map_out[[param_idx]][["SE"]] <- apply(feelthebern[[param_idx]], 2, sd)
    } else {
      map_out[[param_idx]] <- lapply(feelthebern[[param_idx]], function(x) {
        list(
          "mean" = apply(x, 2, mean),
          "sd" = apply(x, 2, sd)
        )
      })
    }
  }
  # Return
  return(map_out)
}

#' Plot histograms of between-subject means
#'
#' @param x an mHMM_cont object
#' @param param string. Which of the parameters should be plotted? Must be one of 'emiss_mu_bar', 'gamma_int_bar', 'emiss_var_bar' or 'emiss_varmu_bar'
#' @param var int. Which emission distribution should be plotted? Must be an integer equal to or less than the total number of emission distributions. Index of variables is the same as the order in which they appear in the input data given to the mHMM.
#' @param ground_truth numeric. Vector of ground-truth values for the parameters equal to the number of emission distributions. Will be added as a red dotted line.
#'
#' @importFrom magrittr '%>%'
#'
#' @return plots a histogram of the parameter of interest for each of the hypothesized latent states
#' @export
plot_posterior <- function(x, ...) {
  UseMethod("plot_posterior", x)
}
#' @export
plot_posterior.mHMM_cont <- function(x, param = c("emiss_mu_bar",
                                                 "gamma_int_bar",
                                                 "emiss_var_bar",
                                                 "emiss_varmu_bar"),
                                     var = 1, ground_truth = NULL) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("assertthat", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Check length of ground_truth means. Should be equal to the number of emission distributions
  if(!is.null(ground_truth)) {
    assertthat::assert_that(length(ground_truth) == x$input$n_dep,
                            msg="Supplied more ground truth means than that there are emission distributions.")
  }
  # Match arg
  param <- match.arg(param)
  # Remove burn-in samples
  # Get parameter posterior
  betmu <- x %>%
    burn() %>%
    .[[param]]
  # Name of the variable (unless gamma)
  if(param != "gamma_int_bar") {
    var_name <- x$input$dep_labels[var]
    # Cannot select a variable using an index that doesn't exist
    assertthat::assert_that(var <= length(betmu), msg = paste0(
      "You selected ", var,
      ". But there are only ", length(betmu), " independent variables."
    ))
    betmu <- as.data.frame(betmu[[var]])
  } else {
    var_name <- "Between-subject TPM (intercepts)"
    betmu <- as.data.frame(betmu)
  }
  # To long format, plot etc.
  betmu_long <- vector("list", ncol(betmu))
  for(cn in seq_along(colnames(betmu))) {
    n <- nrow(betmu)
    betmu_long[[cn]] <- data.frame(
      "var" = rep(colnames(betmu)[cn], n),
      "val" = betmu[,colnames(betmu)[cn]],
      "mval" = mean(betmu[,colnames(betmu)[cn]]),
      "gtv" = ifelse(!is.null(ground_truth),
                     ground_truth[cn],
                     NA)
    )
  }
  # Bind
  outd <- do.call(rbind.data.frame, betmu_long)
  # Plot
  outp <- ggplot2::ggplot(outd, ggplot2::aes(x=val)) +
    ggplot2::geom_histogram() +
    ggplot2::theme_bw() +
    ggplot2::facet_wrap(". ~ var", ncol=1) +
    ggplot2::geom_vline(data=outd, ggplot2::aes(xintercept=mval),
               linetype = "dashed", color = "#2b8cbe",
               size = 1.1) +
    ggplot2::ggtitle("Between-subject means for ", var_name)
  # If ground truth supplied
  if(!is.null(ground_truth)) {
    outp +
      ggplot2::geom_vline(data = outd, ggplot2::aes(xintercept=gtv),
                 linetype = "dotdash", color = "#e34a33",
                 size = 1.1)
  } else {
    outp
  }
}

#' Make a trace plot of the data
#'
#' @param x an mHMM_cont object
#' @param param string. Which of the parameters should be plotted? Must be one of 'emiss_mu_bar', 'gamma_int_bar', 'emiss_var_bar' or 'emiss_varmu_bar'
#' @param var int. Which emission distribution should be plotted? Must be an integer equal to or less than the total number of emission distributions. Index of variables is the same as the order in which they appear in the input data given to the mHMM.
#'
#' @importFrom magrittr '%>%'
#'
#' @return XX
#'
#' @export
trace_plot <- function(x, ...) {
  UseMethod("trace_plot", x)
}
#' @export
trace_plot.mHMM_cont <- function(x, param = c("emiss_mu_bar",
                                              "gamma_int_bar",
                                              "emiss_var_bar",
                                              "emiss_varmu_bar"),
                                 var = 1) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package \"ggplot2\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("assertthat", quietly = TRUE)) {
    stop("Package \"assertthat\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  # Match arg
  param <- match.arg(param)
  # Remove burn-in samples
  # Get parameter posterior
  betmu <- x %>%
    burn() %>%
    .[[param]]
  # Name of the variable (unless gamma)
  if(param != "gamma_int_bar") {
    var_name <- x$input$dep_labels[var]
    # Cannot select a variable using an index that doesn't exist
    assertthat::assert_that(var <= length(betmu), msg = paste0(
      "You selected ", var,
      ". But there are only ", length(betmu), " independent variables."
    ))
    betmu <- as.data.frame(betmu[[var]])
  } else {
    var_name <- "Between-subject TPM (intercepts)"
    betmu <- as.data.frame(betmu)
  }
  # To long format, plot etc.
  betmu_long <- vector("list", ncol(betmu))
  for(cn in seq_along(colnames(betmu))) {
    n <- nrow(betmu)
    betmu_long[[cn]] <- data.frame(
      "iteration" = 1:n,
      "var" = rep(colnames(betmu)[cn], n),
      "val" = betmu[,colnames(betmu)[cn]]
    )
  }
  # Bind
  outd <- do.call(rbind.data.frame, betmu_long)
  # Plot
  ggplot2::ggplot(outd, ggplot2::aes(x=iteration, y=val)) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(". ~ var") +
    ggplot2::ggtitle(var_name)
}

#' Compute upper and lower values of the 95\% credible interval
#'
#' @param x posterior values for a parameter
#'
#' @return numeric vector. Element 1 is the lower 95\% CI, element 2 is the upper 95\% CI.
#'
#' @export
credible_interval <- function(x) {
  apply(x, 2, function(y) quantile(y, c(0.025, 0.975)))
}
