#' US Inflation and Core Inflation Data
#'
#' A dataset containing the variation of the US headline Consumer Price Index
#' along with a core inflation measure that exclude food and energy.
#'
#' @format An object of class (inherits from tbl, data.frame) with 598 rows and 2 columns.
#' Monthly \% variation from 1970-01-01 to 2019-10-01.
#'
#' \describe{
#' \item{cpi}{\emph{Consumer Price Index for All Urban Consumers: All Items in U.S. City Average  (CPIAUCNS)}. \code{cpi}
#' represents the variation in consumer price index. Based on Index 1982-1984=100,
#' not seasonally adjusted}
#'
#' \item{core}{\emph{Consumer Price Index for All Urban Consumers: All Items Less Food and Energy in U.S. City Average  (CPILFENS)}.
#' Excludes food and energy. Based on Index 1982-1984=100, not seasonally adjusted}
#' }
#'
#' @source Series can be obtained  in: \cr
#' \href{https://fred.stlouisfed.org/graph/?id=CPIAUCNS,CPILFENS}{Federal Reserve Economic Data | FRED | St. Louis Fed}
#'
#' @references
#'
#' U.S. Bureau of Labor Statistics,
#' Consumer Price Index for All Urban Consumers:
#' All Items Less Food and Energy in U.S.
#' City Average [CPILFENS], retrieved from FRED,
#' \href{https://fred.stlouisfed.org/series/CPIAUCNS}{Federal Reserve Bank of St. Louis}, November 26, 2019.
#'
#' U.S. Bureau of Labor Statistics,
#' Consumer Price Index for All Urban Consumers:
#' All Items Less Food and Energy in U.S.
#' City Average [CPILFENS], retrieved from FRED,
#' \href{https://fred.stlouisfed.org/series/CPILFENS}{Federal Reserve Bank of St. Louis}, November 26, 2019.
#'
#'
"inf"


#' Real Gross Domestic Product
#'
#' Real gross domestic product is the inflation adjusted value of
#' the goods and services produced by labor and property located in the United States.
#'
#' @format An object of class \code{tibble}. From 1949Qtr1 to 2019Qtr2.
#'
#' \describe{
#' \item{lgdp}{\emph{Log of real gross domestic product}. \code{lgdp}
#' represents the natural logarithm of the real gross
#' domestic product, that is the inflation adjusted value of
#' the goods and services produced by labor and property
#' located in the United States.}
#'
#' \item{gap}{\emph{Output gap}.
#' Defined as log(real gross domestic product) - log(potential gross domestic product). Real potential
#' GDP is the CBO’s estimate of the output the
#' economy would produce with a high rate of use of its
#' capital and labor resources. The data is adjusted to
#' remove the effects of inflation.}
#' }
#'
#' @references U.S. Bureau of Economic Analysis,
#' Real Gross Domestic Product [GDPC1], retrieved from FRED,
#' \href{https://fred.stlouisfed.org/series/GDPC1}{Federal Reserve Bank of St. Louis}, September 11, 2019.
#'
#' U.S. Congressional Budget Office, Real Potential
#' Gross Domestic Product [GDPPOT], retrieved from FRED, Federal
#' \href{https://fred.stlouisfed.org/series/GDPPOT}{Reserve Bank of St. Louis}, September 11, 2019.
#'
#' @source Series can be obtained  in: \cr
#' \href{https://fred.stlouisfed.org/graph/?id=GDPC1,GDPPOT}{Federal Reserve Economic Data | FRED | St. Louis Fed}
#'
"gdp"


utils::globalVariables(c("count", "."))
