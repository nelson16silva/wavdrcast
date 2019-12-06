vint <- function(...) {
  vl <- list(...)
  vlncol <- ncol(vl[[1]])
  colnam <- paste0("V", 1:vlncol)
  vl <- lapply(vl, function(x) {
    colnames(x) <- colnam
    x
  })
  v <- dplyr::bind_rows(vl)
  m <- matrix(unlist(v), nrow = nrow(vl[[1]]))
  mname <- paste0("V", 1:(vlncol * length(vl)))
  tibble::as_tibble(m)
}

#' Wavelet Vintage
#'
#' @param x A numeric vector.
#' @param k Number of vintages
#' @param ... Addition parameters for wavsigmap::signal
#'
#' @return A tibble
#' @export
#'
#' @examples
#' lgdp <- gdp[[1]]
#'
#' vin_wavgap <- wavint(lgdp, 24,
#'                      wf = "haar",
#'                      vscale = "level", a = 1,
#'                      threshrule = "hard"
#' ) %>%
#'   dplyr::mutate_all(~ `-`(lgdp, .))
#'
wavint <- function(x, k, ...) {
  symx <- rlang::ensym(x)
  names <- paste0(symx, 0:k)
  n <- length(x)
  m <- matrix(nrow = n, ncol = k + 1)
  for (i in 0:k) {
    m[1:(n - i), i + 1] <- wavsigmap::signal(x[1:(n - i)], ...)
  }
  colnames(m) <- names
  m <- m[,(k+1):1]
  tibble::as_tibble(m)
}
