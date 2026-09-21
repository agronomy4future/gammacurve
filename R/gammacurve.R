#' Estimate Gamma Distribution Parameters and Compute PDF or CDF
#'
#' Estimates the shape (`alpha`) and scale (`theta`) parameters of a Gamma
#' distribution from a numeric variable using the method of moments. By
#' default, PDF/CDF values are returned for each observed row (same behavior
#' as before). If `resolution` is provided, the PDF/CDF curve is instead
#' evaluated on a fine, evenly-spaced grid (e.g. by = 0.1) so the curve is
#' smooth even when the number of observed data points is small (e.g. only
#' 2, 4, 8, 10, 16, 20). Optionally supports grouped estimation when grouping
#' variables are provided.
#'
#' @param data A data frame containing the variable to be analyzed.
#' @param variable A string or unquoted column name specifying the numeric variable
#' for which the Gamma distribution parameters should be estimated.
#' @param group Optional. A character vector of column names used to group the data
#' before parameter estimation. If `NULL` (default), parameters are estimated
#' using all data.
#' @param func An integer indicating which function values to return:
#' \describe{
#' \item{1}{Return the PDF (default).}
#' \item{2}{Return the CDF.}
#' }
#' @param resolution Optional. A numeric step size (e.g. 0.1) used to build a
#' fine, evenly-spaced grid of `variable` values from 0 to `xmax` for a smooth
#' curve. If `NULL` (default), PDF/CDF are computed only at the observed data
#' points (original behavior), which can look "kinked" when data are sparse.
#' @param xmax Optional. The upper bound of the smooth grid used when
#' `resolution` is supplied. If `NULL` (default), the grid is extended past
#' the observed maximum out to the `tail_prob` quantile of the fitted Gamma
#' distribution, so the curve visibly tapers to (near) zero instead of
#' stopping abruptly at the last data point.
#' @param tail_prob Optional. Used only when `resolution` is supplied and
#' `xmax` is `NULL`. The Gamma quantile (default `0.999`) used to decide how
#' far past the observed maximum the smooth grid should extend, so the curve
#' ends already close to 0 rather than being cut off mid-slope.
#'
#' @returns A tibble.
#' \describe{
#' \item{If `resolution` is NULL}{The original data with `alpha_hat`, `theta_hat`,
#' and `PDF` (if `func = 1`) or `CDF` (if `func = 2`) added — same as before.}
#' \item{If `resolution` is supplied}{A smooth curve table with `variable`
#' replaced by an evenly-spaced grid (step = `resolution`), plus `alpha_hat`,
#' `theta_hat`, and `PDF`/`CDF`. Group columns are included when `group` is set.}
#' }
#'
#' @export
#'
#' @examples
#' # to install the package
#' if(!require(remotes)) install.packages("remotes")
#' if(!requireNamespace("gammacurve", quietly = TRUE)) {
#' remotes::install_github("agronomy4future/gammacurve", force= TRUE)
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
#' # Example 1: original behavior (PDF at each observed row, can look kinked
#' # when data are sparse)
#'output= gammacurve(df, variable="weight", func=1)
#'
#' # Example 2: smooth curve — evaluate PDF on a 0.1-step grid instead of only
#' # at the (possibly few) observed values. The grid automatically extends
#' # past the observed max out to the 99.9th percentile of the fitted Gamma,
#' # so the tail visibly tapers to ~0 instead of stopping mid-slope.
#'curve= gammacurve(df, variable="weight", func=1, resolution=0.1)
#'
#' # Example 3: smooth curve with grouping — each group gets its own alpha/theta
#' # and its own extended grid, so every group's curve tapers to ~0 on its own
#'curve_grp= gammacurve(df, variable="weight",
#' group=c("Season", "Nitrogen"),
#' func=1, resolution=0.1)
#'
#' # Example 4: force all curves to share the same x-axis end point (e.g. 40)
#' # instead of each group extending on its own
#'curve_shared= gammacurve(df, variable="weight",
#' group=c("Season", "Nitrogen"),
#' func=1, resolution=0.1, xmax=40)
#'
#'Github: https://github.com/agronomy4future/gammacurve
#'Website: https://agronomy4future.com
#'Contact: kimjk@agronomy4future
#'
#' @param data
#' @param variable
#' @param group
#' @param func
#' @param resolution
#' @param xmax
#' @param tail_prob
#'
#' @return
#' @export
#'
#' @examples
gammacurve = function(data, variable, group = NULL, func = 1, resolution = NULL, xmax = NULL, tail_prob = 0.999) {

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

  # decide how far the smooth grid should extend so the tail visibly reaches
  # (near) 0 instead of stopping abruptly at the last observed value
  curve_end = function(alpha_hat, theta_hat, observed_max) {
    if (!is.null(xmax)) return(xmax)
    if (is.na(alpha_hat) || is.na(theta_hat)) return(observed_max)
    max(observed_max, qgamma(tail_prob, shape = alpha_hat, scale = theta_hat))
  }

  # build a smooth curve table from estimated params + a fine grid
  make_curve = function(alpha_hat, theta_hat, grid_max) {
    if (is.na(alpha_hat) || is.na(theta_hat)) {
      return(tibble(!!var_name := numeric(0), alpha_hat = numeric(0),
                    theta_hat = numeric(0), PDF = numeric(0), CDF = numeric(0)))
    }
    xseq = seq(0, grid_max, by = resolution)
    tibble(
      !!var_name := xseq,
      alpha_hat = alpha_hat,
      theta_hat = theta_hat,
      PDF = dgamma(xseq, shape = alpha_hat, scale = theta_hat),
      CDF = pgamma(xseq, shape = alpha_hat, scale = theta_hat)
    )
  }

  data = data %>% arrange(.data[[var_name]])

  if (is.null(group)) {

    params = fit_gamma(data[[var_name]])
    alpha_hat = params$alpha_hat
    theta_hat = params$theta_hat

    if (is.null(resolution)) {
      # original behavior: PDF/CDF at observed points only
      data = bind_rows(tibble(!!var_name := 0), data)
      data = data %>%
        mutate(
          alpha_hat = alpha_hat,
          theta_hat = theta_hat,
          PDF = dgamma(.data[[var_name]], shape = alpha_hat, scale = theta_hat),
          CDF = pgamma(.data[[var_name]], shape = alpha_hat, scale = theta_hat)
        )
    } else {
      grid_max = curve_end(alpha_hat, theta_hat, max(data[[var_name]], na.rm = TRUE))
      data = make_curve(alpha_hat, theta_hat, grid_max)
    }

  } else {

    params = data %>%
      group_by(across(all_of(group))) %>%
      group_modify(~ fit_gamma(.x[[var_name]]), .keep = TRUE)

    if (is.null(resolution)) {
      # original behavior: PDF/CDF at observed points only, per group
      data = data %>%
        left_join(params, by = group) %>%
        group_by(across(all_of(group))) %>%
        group_modify(~ {
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
    } else {
      # smooth curve per group: each group's own alpha/theta, evaluated on a
      # fine grid up to that group's max (or a shared xmax if supplied)
      data = data %>%
        left_join(params, by = group) %>%
        group_by(across(all_of(group))) %>%
        group_modify(~ {
          a_hat = unique(.x$alpha_hat)
          t_hat = unique(.x$theta_hat)
          grid_max = curve_end(a_hat, t_hat, max(.x[[var_name]], na.rm = TRUE))
          make_curve(a_hat, t_hat, grid_max)
        }) %>%
        ungroup()
    }
  }

  if (func == 1) data = select(data, -CDF)
  if (func == 2) data = select(data, -PDF)

  data
}
