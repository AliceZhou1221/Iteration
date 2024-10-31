Iteration_and_list columns
================
2024-10-31

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'
    ## 
    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## here are some lists:

``` r
l = list(
  vec_numeric = 1:4,
  unif_sample = runif(100),
  mat = matrix(1:8, nrow = 2, ncol = 4, byrow = TRUE),
  summary = summary(rnorm(1000))
)

l
```

    ## $vec_numeric
    ## [1] 1 2 3 4
    ## 
    ## $unif_sample
    ##   [1] 0.873708779 0.157798286 0.155636469 0.305929961 0.392032627 0.492051984
    ##   [7] 0.391571910 0.278916029 0.646570833 0.765892242 0.389287885 0.221435090
    ##  [13] 0.572093924 0.362099376 0.196071023 0.999073290 0.774396921 0.991345969
    ##  [19] 0.919444454 0.051467127 0.540052644 0.709473677 0.487100130 0.213730186
    ##  [25] 0.594250714 0.436866058 0.247158588 0.760278471 0.252076330 0.451475166
    ##  [31] 0.183198952 0.788817877 0.358673616 0.329786031 0.640949551 0.839968128
    ##  [37] 0.599105079 0.220367520 0.782747014 0.725218160 0.622811466 0.627346451
    ##  [43] 0.655300899 0.416664325 0.021413308 0.004370226 0.457936657 0.829457836
    ##  [49] 0.622271952 0.588856833 0.551866015 0.824392635 0.117958579 0.171104511
    ##  [55] 0.352350452 0.721185860 0.024374217 0.468015832 0.092543689 0.978568531
    ##  [61] 0.592667963 0.855139679 0.583911901 0.737832428 0.499132801 0.013044419
    ##  [67] 0.535206509 0.618554421 0.821190981 0.227707642 0.943593158 0.394210231
    ##  [73] 0.637503262 0.048377889 0.747231973 0.959491499 0.656306419 0.184060391
    ##  [79] 0.528846489 0.543093531 0.726235276 0.809655267 0.327800100 0.975936313
    ##  [85] 0.187655287 0.158740901 0.957154105 0.155534885 0.837335976 0.264744760
    ##  [91] 0.651746231 0.058653747 0.881317967 0.878073418 0.652028752 0.520478146
    ##  [97] 0.443407050 0.766003903 0.076592876 0.413769307
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -3.261980 -0.662967 -0.001662 -0.008848  0.676447  2.993457

access lists

``` r
l$mat
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8

``` r
l[["mat"]]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8

make a list hopefully a bit more useful

``` r
list_norm = 
  list(
    a = rnorm(20, 0, 5),
    b = rnorm(20, 4, 5),
    c = rnorm(20, 0, 10),
    d = rnorm(20, 4, 10)
  )

list_norm$b
```

    ##  [1] -3.1327942 -0.8441767  0.2483828  2.3681494  3.3910241 -0.8061356
    ##  [7]  2.4334903  6.9551426  6.3553362 13.1666323  2.2331728  2.0093725
    ## [13]  1.3939489  2.8817040 -8.9223071  6.7087086 -1.6076869  8.6333051
    ## [19]  5.1602165 13.7796931

reuse function we wrote last time

``` r
mean_and_sd = function(x) {
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  out_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  return(out_df)
}
```

use the function to take mean and sd for all normal samples

``` r
mean_and_sd(list_norm[["a"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0440  5.12

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.12  5.33

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.50  12.8

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.58  8.33

create output list from a for loop: a vector of type list and length 4

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  output[[i]] = mean_and_sd(list_norm[[i]])
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 0.0440  5.12
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.12  5.33
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.50  12.8
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.58  8.33
