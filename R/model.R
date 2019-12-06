adloptim_norec_map <- function(df, lags, .h, ic = BIC,
                               .var = "all", .diff = FALSE, xreg = NULL, ...) {
  # number of rows of the true table (see:adlind_multdf)
  if (.var == "ar") {
    lags[-1] <- 0
  } else if (.var == "ar_out") {
    lags[1] <- 0
  } else {
    lags <- lags
  }
  nrows <- prod(lags + 2)
  i <- 1:nrows
  adlic_norec <- switch_adlic_norec(.var)
  which.min(purrr::map_dbl(
    i,
    ~adlic_norec(
      .x, df = df, lags = lags, .h = .h, ic = ic, .diff = .diff, xreg = xreg
    )
  )
  )
}

lm_tidy <- function(df) {
  yx_names <- rlang::syms(names(df))
  exogenous <- purrr::reduce(yx_names[-1], ~ rlang::expr(!!.x + !!.y))
  equation <- rlang::expr(!!yx_names[[1]] ~ !!exogenous)
  lm_call <- (rlang::quo(stats::lm(!!equation)))
  rlang::eval_tidy(lm_call, df)
}

#' Direct Model
#'
#' This function estimates a direct least square
#' horizon-specific model.
#'
#' @details General OLS regression model is writting as:
#'
#'     \eqn{x_{t + h} = \alpha + \mathbf{{\beta}}'\mathbf{x}_t
#'     + \mathbf{{\gamma}}'\mathbf{z}_t +
#'     \mathbf{{\theta}}'\mathbf{d}_t + \epsilon_{t + h}},
#'
#' where variables are explained in \code{\link{wavdrcast-package}} details
#' section. Arguments \code{.var, .diff} and \code{wav} allow one to
#' estimate subsets of this general model,
#' as discussed in \code{\link{wavdrcast-package}}. If
#' \code{.var = "all"}, the general model is estimated (unless
#' \code{xreg = NULL}, so that \eqn{\mathbf{d}} is out). If
#' \code{.var = "ar"}, \eqn{\mathbf{z}} terms are excluded
#' (not \eqn{\mathbf{d}}, if included). Finally, if
#' \code{.var = "ar_out"}, the model is estimated without
#' \eqn{\mathbf{x}} regressors.
#'
#' @param df A data frame. The first column of the data frame
#'  must be the base variable for constructing the
#'  dependent variable, the multiperiod ahead
#'  value to be forecasted, \eqn{x_{t + h}}. If \code{wav = TRUE},
#'  \code{ncol} of \code{df} can be equal to one (just the dependent variable),
#'  otherwise, \code{ncol(df)} must be greater than one, even
#'  if \code{.var = "ar"}.
#' @param lags An integer vector defining the lags of the
#' regressors. If \code{wav = FALSE}, the length of the vector \code{lags} have to
#' be the same as the number of columns in \code{df}. However,
#' if \code{wav = TRUE}, an additional element in \code{lags}
#' must be add such that the last element in the vector
#' \code{lags} is the lag associatd with the wavelet regressor.
#' @param .h \eqn{h} step-ahead.
#' @param ic Information criterion, \code{BIC} or \code{AIC}. When searching
#' for the best model the dataset is adjusted so that every model
#' have the same data length
#' for appropriate comparasion.
#' @param .var A string to determine how the model will be
#' specificated: \code{"all"} (default), \code{"ar"} or \code{"ar_out"}.
#' @param .diff Logical \code{FALSE} or \code{TRUE}. If \code{TRUE}, the dependent
#' variable is differentiated. See \code{\link{wavdrcast-package}} for the
#' implication on the model specification and wavelet variable.
#' @param wav Logical. If \code{TRUE}, a wavelet-based signal
#' is add to \code{df}, where the signal is related
#' to the explained variable, the first column of \code{df}.
#' @param xreg Data frame. Exogeunous variable not subjected
#' to be lagged. The number of rows must be the same as in \code{df}.
#' @param ... Further arguments passed to \code{wavsigmap::\link[wavsigmap]{signal}}.
#'
#' @return A list with the following elements:
#' \describe{
#' \item{xreg}{A tibble with the data regressors used to run the
#' model choosed by the information criterion. This can be
#' a different data set from that one returned by the \code{lm} object.}
#' \item{model}{An object of class \code{lm}.}
#' }
#' @export
#' @seealso \code{\link{fcast}}
#'
#' @examples
#' model(df = inf, lags = c(2, 0), .h = 2, .var = "ar")
#' model(df = inf, lags = c(2, 2), .h = 2)
#' model(df = inf[1], lags = c(2, 2), .h = 2, wav = TRUE)
#' model(df = gdp, lags = c(2, 2), .h = 2, .diff = TRUE)
#' model(df = gdp[1], lags = c(2, 2), .h = 2, .diff = TRUE, wav = TRUE)
#' model(df = gdp[1], lags = c(0, 0), .h = 2, .diff = TRUE, wav = TRUE, .var = "ar_out")
#'
model <- function(df, lags, .h, ic = BIC,
                  .var = "all", .diff = FALSE,
                  wav = FALSE, xreg = NULL, ...) {
  if (wav) {
    df <- include_wav(df, .diff, ...)
  }
  if (.var == "ar") {
    lags[-1] <- 0
  } else if (.var == "ar_out") {
    lags[1] <- 0
  } else {
    lags <- lags
  }
  df_input <- df
  if (.diff) {
    diff_name <- rlang::sym(paste0(names(df[1]), "_diff"))
    df <- df %>%
      tibble::add_column(!!diff_name := df[[1]] - dplyr::lag(df[[1]]), .before = 1) %>%
      dplyr::select(-2)
  } else {
    df <- df
  }
  data_tbl <- adldf(df, lags, xreg)
  index <- adloptim_norec_map(df_input, lags, .h, ic, .var, .diff, xreg, ...)
  lgl_to_int <- adlind_multdf(lags) %>% dplyr::mutate_all(as.integer)
  slct <- lgl_to_int[index, ]
  if (!is.null(xreg)) {
    col_slct_xreg <- rep(TRUE, ncol(xreg))
  } else {
    col_slct_xreg <- NULL
  }
  col_slct_df <- as.logical(slct)
  col_slct <- c(col_slct_df, col_slct_xreg)
  df2 <- data_tbl[, col_slct]
  if (.diff) {
    name_lead_var <- rlang::sym(paste0(names(df_input[1]), "_diff_lead", .h))
    df3 <- tibble::add_column(df2, !!name_lead_var := dplyr::lead(df_input[[1]], .h) - df_input[[1]], .before = 1)
  } else {
    name_lead_var <- rlang::sym(paste0(names(df_input[1]), "_lead", .h))
    df3 <- tibble::add_column(df2, !!name_lead_var := dplyr::lead(df_input[[1]], .h), .before = 1)
  }
  lm_model <- df3 %>% lm_tidy()
  list(xreg = df2, model = lm_model)
}

