<!-- README.md is generated from README.Rmd. Please edit that file -->
# gammacurve
<!-- badges: start -->
<!-- badges: end -->

The goal of the gammacurve package is to provides a function to estimate the shape (alpha) and scale (theta) 
    parameters of a Gamma distribution from observed data using the method of moments. 
    The function also computes the corresponding probability density function (PDF) and cumulative distribution 
    function (CDF) values.

By default, PDF/CDF values are returned only at the observed data points, which can look "kinked" when the
    number of observations is small. Optionally, the function can instead evaluate the PDF/CDF on a fine,
    evenly-spaced grid (`resolution`), automatically extended past the observed maximum so the curve visibly
    tapers to (near) zero instead of stopping abruptly.

□ Code detailed: https://github.com/agronomy4future/r_code/blob/main/Estimate_Gamma_Distribution_Parameters_and_Compute_PDF_or_CDF.ipynb </br>
□ Website: https://agronomy4future.com/archives/24792 </br>
□ Contact: kimjk@agronomy4future.com </br>

## Installation
You can install gammacurve() like so:
Before installing, please download Rtools (https://cran.r-project.org/bin/windows/Rtools)

``` r
if(!require(remotes)) install.packages("remotes")
if (!requireNamespace("gammacurve", quietly = TRUE)) {
  remotes::install_github("agronomy4future/gammacurve", force= TRUE)
}
library(remotes)
library(gammacurve)
```

## Function
``` r
gammacurve(data, variable, group = NULL, func = 1,
           resolution = NULL, xmax = NULL, tail_prob = 0.999)
```
- `data`: a data frame containing the variable to be analyzed
- `variable`: the numeric column to fit
- `group`: optional column(s) to estimate parameters separately by group
- `func`: `1` returns PDF, `2` returns CDF
- `resolution`: optional step size (e.g. `0.1`). If `NULL` (default), PDF/CDF are computed only at the
  observed data points (original behavior). If supplied, a smooth curve is returned instead, evaluated on
  an evenly-spaced grid from 0 up to the endpoint described below
- `xmax`: optional fixed upper bound for the smooth grid (used with `resolution`). If `NULL` (default), the
  endpoint is chosen automatically using `tail_prob` instead
- `tail_prob`: used only when `resolution` is supplied and `xmax` is `NULL`. The Gamma quantile (default
  `0.999`) used to decide how far past the observed maximum the smooth grid should extend, so the curve ends
  already close to 0 rather than being cut off mid-slope

## Code practice
``` r
# data upload
if(!require(remotes)) install.packages("readr")
library (readr)
github="https://raw.githubusercontent.com/agronomy4future/raw_data_practice/refs/heads/main/sweet_potato_weight.csv"
df=data.frame(read_csv(url(github),show_col_types = FALSE))

print(head(df,3))
 Season        Nitrogen  Block  weight
'2020_Season'        N1      I  111.8380
'2020_Season'        N1      I  9.7788
'2020_Season'        N1      I  57.3240

# Example 1: Estimate Gamma parameters and compute PDF without grouping
output= gammacurve(df, variable="weight", func=1) #func=1, PDF / func=2, CDF

print(head(output,3))
weight  Season         Nitrogen  Block  alpha_hat  theta_hat  PDF
0       NA             NA        NA     1.24       48.5       0     
0       '2021_Season'  N0        I      1.24       48.5       0     
2.14    '2020_Season'  N0        IV     1.24       48.5       0.0102
.
.
.

# Example 2: Estimate Gamma parameters and compute PDF with grouping
output= gammacurve(df, variable="weight", group=c("Season", "Nitrogen"),
                   func=1) #func=1, PDF / func=2, CDF

print(head(output,3))
Season         Nitrogen  weight  Block  alpha_hat  theta_hat     PDF
'2020_Season'  N0        0       NA     1.33       41.4          0      
'2020_Season'  N0        2.14    IV     1.33       41.4          0.00956
'2020_Season'  N0        2.47    IV     1.33       41.4          0.00996
.
.
.

# Example 3: Smooth curve without grouping
# With few or unevenly spaced observations (e.g. 2, 4, 8, 10, 16, 20), the PDF/CDF
# computed only at those points can look kinked instead of smooth. Setting
# resolution=0.1 evaluates the curve on a fine grid instead, and the grid is
# automatically extended past the observed max (using tail_prob) so the curve
# tapers to ~0 rather than stopping abruptly.
curve= gammacurve(df, variable="weight", func=1, resolution=0.1)

print(head(curve,3))
weight  alpha_hat  theta_hat  PDF
0.0     1.24       48.5       0     
0.1     1.24       48.5       0.00046
0.2     1.24       48.5       0.00072
.
.
.

# Example 4: Smooth curve with grouping
# Each group keeps its own alpha_hat/theta_hat, and each group's curve is
# independently extended so it also tapers to ~0 on its own scale.
curve_grp= gammacurve(df, variable="weight", group=c("Season", "Nitrogen"),
                      func=1, resolution=0.1)

print(head(curve_grp,3))
Season         Nitrogen  weight  alpha_hat  theta_hat  PDF
'2020_Season'  N0        0.0     1.33       41.4       0      
'2020_Season'  N0        0.1     1.33       41.4       0.00062
'2020_Season'  N0        0.2     1.33       41.4       0.00089
.
.
.

# Example 5: Smooth curves compared on a shared x-axis
# If groups have very different scales and you want every curve drawn out to
# the same x-axis endpoint for a fair visual comparison, set xmax directly
# instead of letting each group extend on its own.
curve_shared= gammacurve(df, variable="weight", group=c("Season", "Nitrogen"),
                         func=1, resolution=0.1, xmax=100)
```
