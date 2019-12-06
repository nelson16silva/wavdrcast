adlerr <- function(df, lags, .h, ic = BIC,
                   .var = "all", .diff = FALSE,
                   wav = FALSE, xreg = NULL, ...) {
  dfhadj <- df[1:(nrow(df) - .h), , drop = FALSE]
  if (!is.null(xreg)) {
    xreg <- xreg[1:(nrow(xreg) - .h), , drop = FALSE]
  }
  fcast <- match.fun(fcast)(dfhadj, lags, .h, ic, .var, .diff, wav, xreg, ...)[.h]
  err <- (df[nrow(df), 1] - fcast)[[1]]
  err
}

adlerrkstep <- function(df, lags, .h, .k, ic = BIC,
                        .var = "all", .diff = FALSE,
                        wav = FALSE, xreg = NULL, ...) {
  dfkadj <- df[1:(nrow(df) - .k), , drop = FALSE]
  if (!is.null(xreg)) {
    xreg <- xreg[1:(nrow(xreg) - .k), , drop = FALSE]
  }
  adlerr(df = dfkadj, lags = lags, .h = .h, ic = ic, .var = .var, .diff = .diff, wav = wav, xreg = xreg, ...)
}

#' Forecast Error of the Direct Forecast
#'
#' Compute the root-mean-squared forecast error (RMSE) or
#' mean absolute forecast error (MAE) of direct forecast
#' OLS method for forecasting.
#'
#' @details See \code{\link{wavdrcast-package}} and
#' \code{\link{model}}.
#'
#' @inheritParams model
#' @param .H An integer representing the maximum horizon step.
#' @param .K An integer, the number of pseudo-out-of-sample forecasts.
#' @param dev A string, \code{"RMSE"} or \code{"MAE"}.
#' @return A double vector with forecasts error (RMSE or MAE) from
#' 1 to \code{.H} step-ahead.
#' @export
#' @seealso \code{\link{model}, \link{fcast}}
#'
#' @examples
#'
#' cpi <- inf[1]
#' error(df = cpi,
#' lags = c(1, 1), .H = 2, .K = 8,
#' wav = TRUE, wt = "modwt", a = 1)
#'
#' lgdp <- gdp[1]
#' error(df = lgdp,
#' lags = c(1, 1), .H = 2, .K = 8,
#' wav = TRUE, .diff = TRUE, wt = "modwt", a = 1)
#'
error <- function(df, lags, .H, .K, ic = BIC,
                  .var = "all", .diff = FALSE,
                  dev = "RMSE", wav = FALSE, xreg = NULL, ...) {
  if (!wav) {
    purrr::map_dbl(1:.H, function(x) {
      if (dev == "RMSE") {
        sqrt(mean(purrr::map_dbl(
          (.K - 1):0, ~ (adlerrkstep(df, lags,
                                     .h = x, .k = ., ic = ic, .var = .var, .diff = .diff, wav = wav, xreg = xreg
          )^2)
        )))
      } else {
        mean(purrr::map_dbl(
          (.K - 1):0, ~ abs(adlerrkstep(df, lags,
                                        .h = x, .k = ., ic = ic,  .var = .var, .diff = .diff, wav = wav, xreg = xreg
          ))
        ))
      }
    })
  } else {
    purrr::map_dbl(1:.H, function(x) {
      if (dev == "RMSE") {
        sqrt(mean(purrr::map_dbl(
          (.K - 1):0, function(y) {
            adlerrkstep(
              df = df, lags = lags,
              .h = x, .k = y, ic = ic,  .var = .var, .diff = .diff, wav = wav, xreg = xreg, ...
            )^2
          }
        )))
      } else {
        mean(purrr::map_dbl(
          (.K - 1):0, function(y) {
            abs(adlerrkstep(
              df = df, lags = lags,
              .h = x, .k = y, ic = ic,  .var = .var, .diff = .diff, wav = wav, xreg = xreg, ...
            ))
          }
        ))
      }
    })
  }
}



#' Forecast Error of the Direct Forecast From Vintage
#'
#' Compute the root-mean-squared forecast error (RMSE) or
#' mean absolute forecast error (MAE) of the
#' direct forecast from vintage. This dataset is defined
#' as the sequence of values that represent the latest
#' estimate of the data at a particular moment in time.
#'
#' @details See \code{\link{wavdrcast-package}} and
#' \code{\link{model}}.
#'
#' @param .vin List of data vintages (data frame) for each variable.
#' The first vintage is related to the dependent variable and
#' the number of columns of each data frame must be the
#' same as the number of the out-of-sample forecasts (\code{.K}).
#' The latest observation must be the last column of the data frame.
#' If \code{wav = TRUE}, the length of the list can be one,
#' otherwise, must be greater than one.
#' @param lags A integer vector defining the lags of the
#' regressors. If \code{wav = FALSE}, the length of the vector \code{lags} have to
#' be the same as the length of \code{.vin}. However,
#' if \code{wav = TRUE}, an additional element in \code{lags}
#' must be add such that the last element in the vector
#' \code{lags} is the lag of the wavelet regressor.
#' @inheritParams error
#'
#' @return A double vector with forecasts error
#' (RMSE or MAE) from 1 to .H step-ahead.
#' @export
#' @seealso \code{\link{model}, \link{fcast}, \link{error}}
#' @examples
#' set.seed(1)
#'
#' v1 <- tibble::tibble(v1_t1 = c(sample(100, 98, replace = TRUE), NA, NA),
#'                      v1_t2 = c(v1_t1[1:98], 500, NA),
#'                      v1_t3 = c(v1_t2[1:99], 280))
#'
#'  set.seed(1)
#' v2 <- tibble::tibble(v2_t1 = 0.4 * v1[["v1_t1"]] + rnorm(100),
#'                      v2_t2 = c(v2_t1[1:98], 500 + rnorm(1), NA),
#'                      v2_t3 = c(v2_t2[1:99], 600 + rnorm(1)))
#'
#' error_vin(list(v1, v2), lags = c(2, 2), .H = 3, .K = 3)
#' error_vin(list(v1), lags = c(2, 2), .H = 3, .K = 3, wav = TRUE)
error_vin <- function(.vin, lags, .H, .K, ic = BIC,
                      .var = "all", .diff = FALSE,
                      dev = "RMSE", wav = FALSE, xreg = NULL, ...) {
  v <- length(.vin)
  df <- rev(eval(rlang::expr(vint(!!!.vin))))
  if (!wav) {
    purrr::map_dbl(1:.H, function(x) {
      if (dev == "RMSE") {
        sqrt(mean(purrr::map_dbl(
          (.K - 1):0, ~ (adlerrkstep(df[, rev((v * . + 1):(v * (. + 1)))], lags,
                                     .h = x, .k = ., ic = ic, .var = .var, .diff = .diff, xreg = xreg
          )^2)
        )))
      } else {
        mean(purrr::map_dbl(
          (.K - 1):0, ~ abs(adlerrkstep(df[, rev((v * . + 1):(v * (. + 1)))], lags,
                                        .h = x, .k = ., ic = ic, .var = .var, .diff = .diff, xreg = xreg
          ))
        ))
      }
    })
  } else {
    purrr::map_dbl(1:.H, function(x) {
      if (dev == "RMSE") {
        sqrt(mean(purrr::map_dbl(
          (.K - 1):0, function(y) {
            adlerrkstep(
              df = df[, rev((v * y + 1):(v * (y + 1)))], lags = lags,
              .h = x, .k = y, ic = ic, .var = .var, .diff = .diff, wav = wav, xreg = xreg, ...
            )^2
          }
        )))
      } else {
        mean(purrr::map_dbl(
          (.K - 1):0, function(y) {
            abs(adlerrkstep(
              df = df[, rev((v * y + 1):(v * (y + 1)))], lags = lags,
              .h = x, .k = y, ic = ic, .var = .var, .diff = .diff, wav = wav, xreg = xreg, ...
            ))
          }
        ))
      }
    })
  }
}

