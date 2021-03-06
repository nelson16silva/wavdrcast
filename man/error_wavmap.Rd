% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wav_best.R
\name{error_wavmap}
\alias{error_wavmap}
\title{Forecast Error Mapping Wavelet Models}
\usage{
error_wavmap(
  x,
  df,
  lags,
  .H,
  .K,
  ic = BIC,
  .var = "all",
  .diff = FALSE,
  dev = "RMSE",
  xreg = NULL,
  ...
)
}
\arguments{
\item{x}{A tibble returned by the function \code{wavsigmap::\link[wavsigmap]{map_wav_args}}.}

\item{df}{A data frame. The first column of the data frame
must be the base variable for constructing the
dependent variable, the multiperiod ahead
value to be forecasted, \eqn{x_{t + h}}. If \code{wav = TRUE},
\code{ncol} of \code{df} can be equal to one (just the dependent variable),
otherwise, \code{ncol(df)} must be greater than one, even
if \code{.var = "ar"}.}

\item{lags}{An integer vector defining the lags of the
regressors. If \code{wav = FALSE}, the length of the vector \code{lags} have to
be the same as the number of columns in \code{df}. However,
if \code{wav = TRUE}, an additional element in \code{lags}
must be add such that the last element in the vector
\code{lags} is the lag associatd with the wavelet regressor.}

\item{.H}{An integer representing the maximum horizon step.}

\item{.K}{An integer, the number of pseudo-out-of-sample forecasts.}

\item{ic}{Information criterion, \code{BIC} or \code{AIC}. When searching
for the best model the dataset is adjusted so that every model
have the same data length
for appropriate comparasion.}

\item{.var}{A string to determine how the model will be
specificated: \code{"all"} (default), \code{"ar"} or \code{"ar_out"}.}

\item{.diff}{Logical \code{FALSE} or \code{TRUE}. If \code{TRUE}, the dependent
variable is differentiated. See \code{\link{wavdrcast-package}} for the
implication on the model specification and wavelet variable.}

\item{dev}{A string, \code{"RMSE"} or \code{"MAE"}.}

\item{xreg}{Data frame. Exogeunous variable not subjected
to be lagged. The number of rows must be the same as in \code{df}.}

\item{...}{Further arguments passed to
\code{wavsigmap::\link[wavsigmap]{signal}}.}
}
\value{
A tibble containing wavelet options and mean of
the forecast error.
}
\description{
\code{error_wavmap} is a functional that computes the
root-mean-squared forecast error (RMSE) or mean absolute
forecast error (MAE) of the direct forecast for
 several wavelet models.
}
\examples{

wavmap <- wavsigmap::map_wav_args(list(
wavelet = c("haar", "s8"),
thresh.fun = c("universal",  "adaptive")
))

error_wavmap(wavmap, df = inf[1],
             lags = c(1, 1),
             .H = 2, .K = 4)

error_wavmap(wavmap, df = gdp[1],
             lags = c(1, 1),
             .H = 2, .K = 4, .diff = TRUE) \%>\%
             dplyr::filter(dplyr::near(
             mean, min(mean, na.rm = TRUE)))
}
\seealso{
\code{wavsigmap::\link[wavsigmap]{map_wav_args}},
\code{wavsigmap::\link[wavsigmap]{signal}}, \code{\link{error}}
}
