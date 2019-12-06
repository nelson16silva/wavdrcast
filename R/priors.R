#' Determine Scale Factor Paramenter "a" for Laplace Distribution
#'
#' The function prior_a determines the parameter a
#' of the laplace distribution that is used in
#' \href{https://www.rdocumentation.org/packages/EbayesThresh/versions/1.4-12/topics/ebayesthresh.wavelet}{empirical
#' Bayes thresholding approach}. Estimation is based on
#' the avarage of the forecast
#' error.
#'
#' @param ... Parameters for function \code{\link{error}}.
#' Argument \code{wav = TRUE} is automatically adjusted.
#' @inheritParams wavcoreinf::prior_a
#' @param wt A string "modwt" (maximal overlap
#' discrete wavelet transform) or
#' "dwt" (discrete wavelet transform).
#'
#' @return A one-dimension double vector indicating the \code{a} prior.
#' @export
#'
#' @examples
#' prior_a(df = gdp[1],
#'         lags = c(1, 1), .H = 2, .K = 8,
#'         .diff = TRUE, .var = "ar_out", vscale = "level")
prior_a <- function(..., wt = "modwt", interval = c(0.1, 5), tol = 0.01) {
  a_constructor <- function(a) {
    mean(error(..., wav = TRUE, wt = wt, a = a))
  }
  stats::optimise(a_constructor, interval, tol = tol)$minimum
}



#' Determine Hyperparameters for Bayesian Wavelet Thresholding
#'
#' The function \code{prior_alpha_beta} determines both the parameters
#' \code{alpha} and \code{beta} that are used
#' in  \href{https://www.rdocumentation.org/packages/wavethresh/versions/4.6.1/topics/BAYES.THR}{bayesian wavelet thresholding
#' of noisy data}. Estimation is based on the avarage of the forecast error.
#'
#'
#' @param ... Parameters for function error. Argument wav = TRUE is automatically adjusted.
#' @inheritParams wavcoreinf::prior_alpha_beta
#' @return A double vector of length one.
#' @export
#'
#' @examples
#' prior_alpha_beta(df = inf[1],
#' lags = c(1, 1), .H = 2, .K = 2,
#' type = "hard", boundary = FALSE)
prior_alpha_beta <- function(...,
                             par = c(0.5, 1),
                             lower = c(0, 0),
                             upper = c(3, 3),
                             control = list()) {
  ab_constructor <- function(ab) {
    mean(error(..., wav = TRUE,
               alpha = ab[1], beta = ab[2],
               policy = "BayesThresh"))
  }
  stats::optim(par,
               ab_constructor,
               lower = lower, upper = upper,
               control = control,
               method = "L-BFGS-B"
  )$par
}
