#' Estimate Gamma Distribution Parameters and Compute PDF or CDF
#'
#' Estimates the shape (`alpha`) and scale (`theta`) parameters of a Gamma
#' distribution from a numeric variable using the method of moments, and
#' computes either the probability density function (PDF) or cumulative
#' distribution function (CDF) values for each observation. Optionally supports
#' grouped estimation when grouping variables are provided.
#'
#' @param data A data frame containing the variable to be analyzed.
#' @param variable A string or unquoted column name specifying the numeric variable
#'   for which the Gamma distribution parameters should be estimated.
#' @param group Optional. A character vector of column names used to group the data
#'   before parameter estimation. If `NULL` (default), parameters are estimated
#'   using all data.
#' @param func An integer indicating which function values to return:
#'   \describe{
#'     \item{1}{Return the PDF (default).}
#'     \item{2}{Return the CDF.}
#'   }
#'
#' @returns A tibble containing the original data with additional columns:
#' \describe{
#'   \item{alpha_hat}{Estimated shape parameter of the Gamma distribution.}
#'   \item{theta_hat}{Estimated scale parameter of the Gamma distribution.}
#'   \item{PDF}{Gamma probability density function values (if `func = 1`).}
#'   \item{CDF}{Gamma cumulative distribution function values (if `func = 2`).}
#' }
#'
#' @export
#'
#' @examples
#' # to install the package
#' if(!require(remotes)) install.packages("remotes")
#' if(!requireNamespace("gammacurve", quietly = TRUE)) {
#'   remotes::install_github("agronomy4future/gammacurve", force= TRUE)
#' }
#' library(remotes)
#' library(gammacurve)
#'
#' # to upload dataset for practice
#' if(!require(remotes)) install.packages("readr")
#'library (readr)
#'github="https://raw.githubusercontent.com/agronomy4future/raw_data_practice/refs/heads/main/sweet_potato_weight.csv"
#'df=data.frame(read_csv(url(github),show_col_types = FALSE))
#'
#'print(head(df,3))
#'       Season Nitrogen Block   weight
#'1 2020_Season       N1     I 111.8380
#'2 2020_Season       N1     I   9.7788
#'3 2020_Season       N1     I  57.3240
#'
#' # Example 1: Estimate Gamma parameters and compute PDF without grouping
#'output= gammacurve(df, variable="weight",
#'                   func=1) #func=1, PDF / func=2, CDF
#'print(head(output,3))
#'
#' # Example 2: Estimate Gamma parameters and compute PDF with grouping
#'output= gammacurve(df, variable="weight",
#'                   group=c("Season", "Nitrogen"),
#'                   func=1) #func=1, PDF / func=2, CDF
#'print(head(output,3))
#'
#'Github: https://github.com/agronomy4future/gammacurve
#'Website: https://agronomy4future.com
#'Contact: kimjk@agronomy4future
#'
gammacurve = function(data, variable, group = NULL, func = 1) {
  if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
  if (!requireNamespace("rlang", quietly = TRUE)) install.packages("rlang")
  if (!requireNamespace("tidyr", quietly = TRUE)) install.packages("tidyr")
  library(dplyr)
  library(rlang)
  library(tidyr)

  var_name = if (is.character(variable)) variable else as_string(ensym(variable))
  if (!(var_name %in% names(data))) stop(paste("Column", var_name, "not found in data"))

  group = group[group %in% names(data)]
  if (length(group) == 0) group = NULL

  fit_gamma = function(x) {
    x = x[!is.na(x) & x > 0]
    if (length(x) < 2) return(tibble(alpha_hat = NA, theta_hat = NA))
    rhs = log(mean(x)) - mean(log(x))
    f = function(alpha) log(alpha) - digamma(alpha) - rhs
    alpha_hat = tryCatch(uniroot(f, c(0.01, 100))$root, error = function(e) NA)
    theta_hat = if (!is.na(alpha_hat)) mean(x) / alpha_hat else NA
    tibble(alpha_hat = alpha_hat, theta_hat = theta_hat)
  }

  data = data %>% arrange(.data[[var_name]])

  if (is.null(group)) {
    params = fit_gamma(data[[var_name]])
    alpha_hat = params$alpha_hat
    theta_hat = params$theta_hat

    # Add 0 point to ensure curve touches axis
    data = bind_rows(tibble(!!var_name := 0), data)

    data = data %>%
      mutate(
        alpha_hat = alpha_hat,
        theta_hat = theta_hat,
        PDF = dgamma(.data[[var_name]], shape = alpha_hat, scale = theta_hat),
        CDF = pgamma(.data[[var_name]], shape = alpha_hat, scale = theta_hat)
      )
  } else {
    params = data %>%
      group_by(across(all_of(group))) %>%
      group_modify(~ fit_gamma(.x[[var_name]]), .keep = TRUE)

    data = data %>%
      left_join(params, by = group) %>%
      group_by(across(all_of(group))) %>%
      group_modify(~ {
        # add 0 weight to each group
        add0 = tibble(!!var_name := 0)
        d = bind_rows(add0, .x)
        d %>%
          mutate(
            PDF = dgamma(.data[[var_name]],
                         shape = unique(.x$alpha_hat),
                         scale = unique(.x$theta_hat)),
            CDF = pgamma(.data[[var_name]],
                         shape = unique(.x$alpha_hat),
                         scale = unique(.x$theta_hat)),
            alpha_hat = unique(.x$alpha_hat),
            theta_hat = unique(.x$theta_hat)
          )
      })
  }

  if (func == 1) data = select(data, -CDF)
  if (func == 2) data = select(data, -PDF)

  data
}
