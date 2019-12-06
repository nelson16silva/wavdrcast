#' wavdrcast: Wavelet-based direct multistep forecast
#'
#' This package provides forecasts of time series using a
#' direct multistep model where regressors can be composed by
#' AR components, exogenous variables and
#' wavelet-based signal estimation.
#'
#' \code{wavdrcast} is a package for forecasting
#' time series using a direct multistep model estimation. In particular,
#' wavelet-based signal estimation of the time series
#' is used as an additional regressor or even the only one. This is useful for
#' evaluation of core inflation and outupt gap measures
#' constructed from wavelet methods, for example.
#' A functional available in  \code{wavdrcast} package allows one to
#' choose a good wavelet specification for sinal extraction
#' considering  the forecast
#' criterion.  The scope is not limited to wavelet regressors,
#' however. In general, any other data set can be used as
#' a regressor, including just AR components of the time series. This can be
#' interesting when one is comparing the forecast property of
#' candidates model to be a good estimator of the core inflation
#' or output gap. If the data set are subjected to revisions,
#' functions for vintages are also available.
#'
#' @section Model:
#'
#' The most general OLS regression model is writting as:
#'
#'     \eqn{x_{t + h} = \alpha + \mathbf{{\beta}}'\mathbf{x}_t
#'     + \mathbf{{\gamma}}'\mathbf{z}_t +
#'     \mathbf{{\theta}}'\mathbf{d}_t + \epsilon_{t + h}},
#'
#' where:
#' \itemize{
#' \item \eqn{x_{t + h}}, regressed \eqn{x} leaded \eqn{h}
#'  step-ahead;
#'
#'  \item \eqn{\alpha}, constant;
#'
#'  \item \eqn{\mathbf{{\beta}}' = (\beta_0, \ldots, \beta_p)}, vector of \eqn{p + 1}  coefficients;

#'  \item \eqn{\mathbf{x}_t = (x_t, \ldots, x_{t - p})'}, variable \eqn{x} in \eqn{t} and its \eqn{p} lags;

#'  \item \eqn{\mathbf{{\gamma}}' = (\gamma^1_0, \ldots, \gamma^1_{q_1}, \ldots, \gamma^n_0, \ldots, \gamma^n_{q_n}, \gamma^w_0, \ldots, \gamma^w_{q_w})}, vector of coefficients on \eqn{\mathbf{z}_t};

#'  \item \eqn{\mathbf{z}_t = (z^1_t, \ldots, z^1_{t - q_1}, \ldots, z^n_t, \ldots, z^n_{t - q_n},  z^w_t, \ldots, z^w_{t - q_w})'}, vector of exogenous variables and its \eqn{q_i}, \eqn{i = (1, \ldots, n, w)} lags. By convention, the exogenous variable from wavelet-based signal estimation (\eqn{z^w_t}), if included, is ordered last;

#'  \item \eqn{\mathbf{{\theta}}' = (\theta_1, \ldots, \theta_m)}, coefficients;

#'  \item \eqn{\mathbf{d}_t = (d_1, \ldots, d_m)'}, exogenous variables not subjected to be lagged, possibly dummy variables;
#'
#'  \item \eqn{\epsilon_{t + h}}, white noise error term.
#' }
#'
#' If \eqn{x_{t + h} = y_{t + h} - y_t}, then
#' \eqn{x_t = \Delta x_t = y_t - y_{t - 1}} and
#' \eqn{z^w_t = y_t - y^w_t}, where \eqn{y^w_t} is a
#' wavelet-based signal estimation of \eqn{y_t}.
#' For example, if \eqn{y_t} is the logarithm of the output,
#' then  \eqn{z^w_t = y_t - y^w_t} is the wavelet-base
#' estimation of the output gap. Other variables
#' \eqn{z^i_t, i = (1, \ldots, n)} are never automatically
#' differentiated, so it is a user's decision to include them
#' in level or difference.
#'
#' From this specification, it is possible to estimate
#' particular models like:
#'
#' \itemize{
#' \item \eqn{y_{t + h} - y_t = \alpha + \mathbf{{\beta}}\Delta \mathbf{y}_t + \epsilon_{t + h}}, (AR case);
#'  \item \eqn{y_{t + h} - y_t = \alpha + \gamma_0^w z^w_t + \epsilon_{t + h}}, (evaluation of the wavelet-based output gap, for example);
#'  \item \eqn{x_{t + h}  = \alpha + \sum^p_{i = 0}\beta_ix_{t - i} + \sum^q_{i = 0}\gamma^1_i z^1_{t - i} + \epsilon_{t + h}}, (evaluation of core inflation measure that exclude food and energy, for example).
#' }
#'
#' In the last equation above, it was assumed that the variable
#' \eqn{z} are not revised, as it is almost always the case
#' for core inflation measures that exclude food and energy.
#' If not, there is a function in the package that considers
#' the case in which \eqn{z} can be revised in each point of time when new
#' data are available. If
#' \eqn{z} is the core inflation measure from HP filter,
#' for example, then an evaluation of the forecast property
#' should consider that the  estimation of all element in \eqn{z} can be
#' different for each out-of-sample observation included in the sample. If
#' \eqn{z} includes \eqn{z^w}, as is the case of the second
#' equation above, then for each point in time a new
#' estimation of \eqn{z^w} is did automatically.
#'
#'
"_PACKAGE"

