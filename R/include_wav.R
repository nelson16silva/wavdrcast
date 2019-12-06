include_wav <- function(df, .diff = FALSE, ...) {
  wavsignal <- wavsigmap::signal(df[[1]], ...)
  if (!.diff) {
    wav_var <- rlang::sym(paste0(names(df[1]), "_wav"))
    dfwav <- df %>% tibble::add_column(!!wav_var := wavsignal)
  } else {
    wav_var <- rlang::sym(paste0(names(df[1]), "_wav_diff"))
    dfwav <- df %>% tibble::add_column(!!wav_var := df[[1]] - wavsignal)
  }
  dfwav
}
