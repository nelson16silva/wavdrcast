#' Forecast Error Mapping Wavelet Models
#'
#' \code{error_wavmap} is a functional that computes the
#' root-mean-squared forecast error (RMSE) or mean absolute
#' forecast error (MAE) of the direct forecast for
#'  several wavelet models.
#' @param x A tibble returned by the function \code{wavsigmap::\link[wavsigmap]{map_wav_args}}.
#' @inheritParams error
#' @param ... Further arguments passed to
#' \code{wavsigmap::\link[wavsigmap]{signal}}.
#'
#' @return A tibble containing wavelet options and mean of
#' the forecast error.
#' @export
#' @seealso \code{wavsigmap::\link[wavsigmap]{map_wav_args}},
#' \code{wavsigmap::\link[wavsigmap]{signal}}, \code{\link{error}}
#'
#' @examples
#'
#' wavmap <- wavsigmap::map_wav_args(list(
#' wavelet = c("haar", "s8"),
#' thresh.fun = c("universal",  "adaptive")
#' ))
#'
#' error_wavmap(wavmap, df = inf[1],
#'              lags = c(1, 1),
#'              .H = 2, .K = 4)
#'
#' error_wavmap(wavmap, df = gdp[1],
#'              lags = c(1, 1),
#'              .H = 2, .K = 4, .diff = TRUE) %>%
#'              dplyr::filter(dplyr::near(
#'              mean, min(mean, na.rm = TRUE)))
error_wavmap <- function(x, df, lags, .H, .K, ic = BIC, .var = "all", .diff = FALSE,
                         dev = "RMSE", xreg = NULL, ...) {
  a <- rlang::enexprs(df, lags, .H, .K, ic, .var, .diff, dev, xreg)
  error_map <- purrr::pmap(x, function(...) {
    wav_args <- rlang::enexprs(...)
    purrr::possibly(eval, NA)(rlang::expr(error(!!!a, wav = TRUE, !!!wav_args)))
  })
  x %>% dplyr::mutate(mean = purrr::map_dbl(error_map, mean))
}


#' Forecast Error Mapping Wavelet Models for Vintages
#'
#' \code{error_vin_wavmap} is a functional that computes the
#' root-mean-squared forecast error (RMSE) or mean absolute
#' forecast error (MAE) of the direct forecast for
#'  several wavelet models using data vintages.
#' @param x A tibble returned by the function
#' \code{wavsigmap::\link[wavsigmap]{map_wav_args}}.
#' @inheritParams error_vin
#' @param ... Further arguments passed to
#' \code{wavsigmap::\link[wavsigmap]{signal}}.
#'
#' @return A tibble containing wavelet options and mean of
#' the forecast error.
#' @export
#' @seealso \code{wavsigmap::\link[wavsigmap]{map_wav_args}},
#' \code{wavsigmap::\link[wavsigmap]{signal}}, \code{\link{error_vin}}
#'
#' @examples
#'  set.seed(1)
#' v1 <- tibble::tibble(v1_t1 = c(sample(100, 98, replace = TRUE), NA, NA),
#'                      v1_t2 = c(v1_t1[1:98], 500, NA),
#'                      v1_t3 = c(v1_t2[1:99], 280))
#'
#' wavmap <- wavsigmap::map_wav_args(list(
#' wavelet = c("haar", "s8"),
#' thresh.fun = c("universal",  "adaptive")
#' ))
#'
#' error_vin_wavmap(wavmap, .vin = list(v1),
#' lags = c(1, 1),
#' .H = 2, .K = 3)
error_vin_wavmap <- function(x, .vin, lags, .H, .K, ic = BIC, .var = "all", .diff = FALSE,
                             dev = "RMSE", xreg = NULL, ...) {
  a <- rlang::enexprs(.vin, lags, .H, .K, ic, .var, .diff, dev, xreg)
  error_map <- purrr::pmap(x, function(...) {
    wav_args <- rlang::enexprs(...)
    purrr::possibly(eval, NA)(rlang::expr(error_vin(!!!a, wav = TRUE, !!!wav_args)))
  })
  x %>% dplyr::mutate(mean = purrr::map_dbl(error_map, mean))
}

