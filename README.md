<!-- README.md is generated from README.Rmd. Please edit that file -->
# gammacurve
<!-- badges: start -->
<!-- badges: end -->

The goal of the gammacurve package is to provides a function to estimate the shape (alpha) and scale (theta) 
    parameters of a Gamma distribution from observed data using the method of moments. 
    The function also computes the corresponding probability density function (PDF) and cumulative distribution 
    function (CDF) values.

□ Code detailed: https://github.com/agronomy4future/r_code/blob/main/Estimate_Gamma_Distribution_Parameters_and_Compute_PDF_or_CDF.ipynb
□ Website: https://agronomy4future.com/archives/24792
□ Contact: kimjk@agronomy4future.com

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
