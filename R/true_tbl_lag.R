adlind_unidf <- function(p) {
  names <- paste0("lag", 0:p)
  nrows <- p + 2
  ncols <- p + 1
  lag_list <- vector("list", ncols)
  for (i in 1:ncols) {
    lag_list[[i]] <- c(rep(TRUE, (nrows - i)), rep(FALSE, i))
  }
  lag_ind_df <- data.frame(lag_list)
  names(lag_ind_df) <- names
  tibble::as_tibble(lag_ind_df)
}

adlind_multdf <- function(lags) {
  lag_ind_dfs <- purrr::map(lags, adlind_unidf)
  lag_ind_dfs <- purrr::map2(
    lag_ind_dfs, seq_along(lag_ind_dfs),
    function(x, y) `names<-`(x, paste0("x", y, names(x)))
  )

  df_cbind <- function(df1, df2) {
    n2 <- nrow(df2)
    df1 %>%
      dplyr::mutate(count = n2) %>%
      tidyr::uncount(count) %>%
      cbind(df2) %>%
      tibble::as_tibble()
  }
  lag_ind_dfs %>%
    purrr::reduce(df_cbind)
}
