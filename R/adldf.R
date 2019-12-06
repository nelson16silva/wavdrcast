adldf <- function(df, lags, xreg = NULL) {
  lagmatch <- unlist(purrr::map(lags, ~c(0:.)))
  var_names <- names(df)
  var_names_rep <- rep(var_names, times = lags + 1)
  col_names <- paste0(var_names_rep, "_lag", lagmatch)
  dfmatch <- data.frame(rep(df, times = lags + 1))
  tbl_lags <- purrr::map2_df(dfmatch, lagmatch, dplyr::lag)
  names(tbl_lags) <- col_names
  dplyr::bind_cols(tbl_lags, xreg)
}
