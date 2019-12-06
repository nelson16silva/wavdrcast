adlic_norec_all <- function(i, df, lags, .h, ic = BIC,
                            .diff = FALSE, xreg = NULL) {
  if (.diff) {
    diff_name <- rlang::sym(paste0(names(df[1]), "_diff"))
    # add difference of dependent variable to df in position 2
    diff_var1 <- df %>%
      tibble::add_column(!!diff_name := df[[1]] - dplyr::lag(df[[1]]), .before = 2)
    # data table to get consistent observations of dependent variable.
    # an extra 0 is add to the lag argument because adldf needs the same number of lags as variables
    tbl_diff_var1 <- tidyr::drop_na(adldf(diff_var1, lags = c(0, lags), xreg))
    # data frame without level of the dependent variable that is in position 1 of diff_var1
    df <- diff_var1 %>% dplyr::select(-1)
  } else {
    df <- df
  }
  data_tbl <- tidyr::drop_na(adldf(df, lags, xreg))
  lgl_to_int <- adlind_multdf(lags) %>% dplyr::mutate_all(as.integer)
  slct <- lgl_to_int[i, ]
  if (
    slct[1] > 0
    && sum(slct[(lags[1] + 2):(sum(lags + 1))]) > 0
  ) {
    if (!is.null(xreg)) {
      col_slct_xreg <- rep(TRUE, ncol(xreg))
    } else {
      col_slct_xreg <- NULL
    }
    col_slct_df <- as.logical(slct)
    col_slct <- c(col_slct_df, col_slct_xreg)
    df2 <- data_tbl[, col_slct]
    if (.diff) {
      df3 <- tibble::add_column(df2, y2 = dplyr::lead(tbl_diff_var1[[1]], .h) - tbl_diff_var1[[1]], .before = 1)
    } else {
      df3 <- tibble::add_column(df2, y2 = dplyr::lead(data_tbl[[1]], .h), .before = 1)
    }
    ic(stats::lm(y2 ~ ., data = df3))
  } else {
    NA
  }
}

adlic_norec_ar <- function(i, df, lags, .h, ic = BIC,
                           .diff = FALSE, xreg = NULL) {
  lags[-1] <- 0
  if (.diff) {
    diff_name <- rlang::sym(paste0(names(df[1]), "_diff"))
    # add difference of dependent variable to df in position 2
    diff_var1 <- df %>%
      tibble::add_column(!!diff_name := df[[1]] - dplyr::lag(df[[1]]), .before = 2)
    # data table to get consistent observations of dependent variable.
    # an extra 0 is add to the lag argument because adldf needs the same number of lags as variables
    tbl_diff_var1 <- tidyr::drop_na(adldf(diff_var1, lags = c(0, lags), xreg))
    # data frame without level of the dependent variable that is in position 1 of diff_var1
    df <- diff_var1 %>% dplyr::select(-1)
  } else {
    df <- df
  }
  data_tbl <- tidyr::drop_na(adldf(df, lags, xreg))
  lgl_to_int <- adlind_multdf(lags) %>% dplyr::mutate_all(as.integer)
  slct <- lgl_to_int[i, ]
  if (
    slct[1] > 0
    && sum(slct[(lags[1] + 2):(sum(lags + 1))]) == 0
  ) {
    if (!is.null(xreg)) {
      col_slct_xreg <- rep(TRUE, ncol(xreg))
    } else {
      col_slct_xreg <- NULL
    }
    col_slct_df <- as.logical(slct)
    col_slct <- c(col_slct_df, col_slct_xreg)
    df2 <- data_tbl[, col_slct]
    if (.diff) {
      df3 <- tibble::add_column(df2, y2 = dplyr::lead(tbl_diff_var1[[1]], .h) - tbl_diff_var1[[1]], .before = 1)
    } else {
      df3 <- tibble::add_column(df2, y2 = dplyr::lead(data_tbl[[1]], .h), .before = 1)
    }
    ic(stats::lm(y2 ~ ., data = df3))
  } else {
    NA
  }
}

adlic_norec_ar_out <- function(i, df, lags, .h, ic = BIC,
                               .diff = FALSE, xreg = NULL) {
  lags[1] <- 0
  data_tbl <- tidyr::drop_na(adldf(df, lags, xreg))
  lgl_to_int <- adlind_multdf(lags) %>% dplyr::mutate_all(as.integer)
  slct <- lgl_to_int[i, ]
  if (
    slct[1] == 0
    && sum(slct[(lags[1] + 2):(sum(lags + 1))]) > 0
  ) {
    if (!is.null(xreg)) {
      col_slct_xreg <- rep(TRUE, ncol(xreg))
    } else {
      col_slct_xreg <- NULL
    }
    col_slct_df <- as.logical(slct)
    col_slct <- c(col_slct_df, col_slct_xreg)
    df2 <- data_tbl[, col_slct]
    if (.diff) {
      df3 <- tibble::add_column(df2, y2 = dplyr::lead(data_tbl[[1]], .h) - data_tbl[[1]], .before = 1)
    } else {
      df3 <- tibble::add_column(df2, y2 = dplyr::lead(data_tbl[[1]], .h), .before = 1)
    }
    ic(stats::lm(y2 ~ ., data = df3))
  } else {
    NA
  }
}

switch_adlic_norec <- function(x) {
  if (!is.character(x)) {
    stop(paste0("Function argument must be a character, not ", class(x), "."))
  }
  switch(x,
         all = adlic_norec_all,
         ar = adlic_norec_ar,
         ar_out = adlic_norec_ar_out,
         stop(paste0("Invalid argument. It must be 'all', 'ar' or 'ar_out', not ", rlang::enexpr(x), "." )))
}
