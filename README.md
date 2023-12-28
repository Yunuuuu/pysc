
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pysc

<!-- badges: start -->

<!-- badges: end -->

The goal of `pysc` is to integrate Miscellaneous Python functions for
Single Cell analysis.

## Installation

You can install the development version of `pysc` from
[GitHub](https://github.com/) with:

``` r
if (!requireNamespace("pak")) {
  install.packages("pak",
    repos = sprintf(
      "https://r-lib.github.io/p/pak/devel/%s/%s/%s",
      .Platform$pkgType, R.Version()$os, R.Version()$arch
    )
  )
}
pak::pkg_install("Yunuuuu/pysc")
```

## Implemented Algorithm

  - `scCODA`: <https://github.com/theislab/scCODA>

## Examples

``` r
library(pysc)
```

## scCODA - Single-cell differential composition analysis

This is a basic example which shows you how to solve a common problem:

``` r
data <- reticulate::py_to_r(sccoda_datasets("haber"))
data$Condition <- gsub("_[0-9]$", "", data$Mouse)
data <- data[data$Condition %in% c("Control", "Salm")]
data <- sccoda_data(
  data, c("Mouse", "Condition"),
  setdiff(names(data), c("Mouse", "Condition"))
)
sccoda_out <- sccoda(data, "Condition")
pysc_tidy(sccoda_out)
#>           Intercept terms Intercept Final Parameter Intercept HDI 3%
#>                    <char>                     <num>            <num>
#>  1:             Endocrine                     0.858            0.157
#>  2:            Enterocyte                     1.838            1.023
#>  3: Enterocyte.Progenitor                     2.204            1.511
#>  4:                  Tuft                     0.495           -0.226
#>  5:             Endocrine                     0.858            0.157
#>  6:            Enterocyte                     1.838            1.023
#>  7: Enterocyte.Progenitor                     2.204            1.511
#>  8:                  Tuft                     0.495           -0.226
#>  9:             Endocrine                     0.858            0.157
#> 10:            Enterocyte                     1.838            1.023
#> 11: Enterocyte.Progenitor                     2.204            1.511
#> 12:                  Tuft                     0.495           -0.226
#>     Intercept HDI 97% Intercept SD Intercept Expected Sample
#>                 <num>        <num>                     <num>
#>  1:             1.495        0.357                  49.60943
#>  2:             2.622        0.431                 132.18216
#>  3:             2.902        0.373                 190.60076
#>  4:             1.162        0.374                  34.50765
#>  5:             1.495        0.357                  49.60943
#>  6:             2.622        0.431                 132.18216
#>  7:             2.902        0.373                 190.60076
#>  8:             1.162        0.374                  34.50765
#>  9:             1.495        0.357                  49.60943
#> 10:             2.622        0.431                 132.18216
#> 11:             2.902        0.373                 190.60076
#> 12:             1.162        0.374                  34.50765
#>     Effect Final Parameter Effect HDI 3% Effect HDI 97% Effect SD
#>                      <num>         <num>          <num>     <num>
#>  1:               0.000000         0.000          0.000     0.000
#>  2:               0.000000        -1.714          0.106     0.566
#>  3:               0.000000        -0.496          1.139     0.299
#>  4:               0.000000        -0.155          1.853     0.588
#>  5:               0.000000         0.000          0.000     0.000
#>  6:               0.000000        -1.236          0.161     0.346
#>  7:               0.000000        -0.707          0.489     0.208
#>  8:               0.000000        -0.413          1.111     0.317
#>  9:               0.000000         0.000          0.000     0.000
#> 10:               1.390214         0.516          2.181     0.473
#> 11:               0.000000        -0.988          0.773     0.259
#> 12:               0.000000        -1.417          0.578     0.395
#>     Effect Inclusion probability Effect Expected Sample Effect log2-fold change
#>                            <num>                  <num>                   <num>
#>  1:                    0.0000000               49.60943               0.0000000
#>  2:                    0.7786000              132.18216               0.0000000
#>  3:                    0.4519333              190.60076               0.0000000
#>  4:                    0.6890667               34.50765               0.0000000
#>  5:                    0.0000000               49.60943               0.0000000
#>  6:                    0.5237333              132.18216               0.0000000
#>  7:                    0.4362667              190.60076               0.0000000
#>  8:                    0.4873333               34.50765               0.0000000
#>  9:                    0.0000000               25.05959              -0.9852517
#> 10:                    0.9821333              268.12969               1.0204035
#> 11:                    0.3207333               96.27961              -0.9852517
#> 12:                    0.4284000               17.43111              -0.9852517
```
