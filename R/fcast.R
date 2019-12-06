#' Direct Multistep Forecast
#'
#' Make forecast using a horizon-specific estimated OLS model.
#'
#' @details See \code{\link{wavdrcast-package}} and
#' \code{\link{model}}.
#'
#' @inheritParams model
#' @return A double vector with forecasts from
#' 1 to \eqn{h} step-ahead.
#' @export
#' @seealso \code{\link{model}}
#'
#' @examples
#' fcast(df = inf[1], lags = c(2, 1), .h = 12, wav = TRUE)
#' fcast(df = gdp[1], lags = c(2, 1), .h = 12, wav = TRUE, .diff = TRUE)
#' fcast(df = gdp, lags = c(2, 1), .h = 12, .diff = TRUE)
fcast <- function(df, lags, .h, ic = BIC,
                  .var = "all", .diff = FALSE,
                  wav = FALSE, xreg = NULL, ...) {
  object <- model(df, lags, .h, ic, .var, .diff, wav, xreg, ...)
  rowend <- nrow(object$xreg)
  rowstart <- rowend - (.h - 1)
  newdata <- object$xreg[rowstart:rowend, ]
  fcast <- forecast::forecast(object$model, newdata = newdata, h = .h)$mean
  rm(object)
  if (.diff) {
    y_to_add <- df[[1]][rowstart:rowend]
    fcast + y_to_add
  } else {
    fcast
  }
}
