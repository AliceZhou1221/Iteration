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
    ##   [1] 0.86030328 0.37862808 0.88642303 0.95763186 0.44218234 0.56433832
    ##   [7] 0.87707722 0.19602991 0.58634460 0.14398767 0.94939161 0.40423784
    ##  [13] 0.41335368 0.26311652 0.47397183 0.43469382 0.15449325 0.30354520
    ##  [19] 0.95523376 0.96486979 0.13594090 0.82534514 0.64037572 0.51169398
    ##  [25] 0.37351549 0.16205866 0.98824022 0.82805297 0.10518634 0.49950616
    ##  [31] 0.61037381 0.66042240 0.15162272 0.63354511 0.60316614 0.45316544
    ##  [37] 0.73350037 0.82752868 0.55505522 0.17529885 0.36728940 0.76181876
    ##  [43] 0.23870753 0.52652211 0.70840408 0.94202900 0.42398538 0.46053206
    ##  [49] 0.53264533 0.66654857 0.60825373 0.71461104 0.01662753 0.41959959
    ##  [55] 0.26410223 0.76592909 0.18615245 0.93465731 0.45814306 0.44574587
    ##  [61] 0.93843156 0.49565101 0.31824367 0.65965924 0.35839328 0.70798990
    ##  [67] 0.30653563 0.59606120 0.97501859 0.71614128 0.24933675 0.75953772
    ##  [73] 0.70465139 0.64899319 0.43431466 0.36446789 0.43429601 0.71511038
    ##  [79] 0.10959054 0.69346759 0.84517546 0.42155442 0.92843224 0.96882058
    ##  [85] 0.59085983 0.40623166 0.13437519 0.42866850 0.02457659 0.40780470
    ##  [91] 0.03176346 0.56649317 0.26020410 0.20747280 0.06489228 0.46663441
    ##  [97] 0.85321451 0.04444270 0.82536589 0.55497484
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.01923 -0.63684  0.01540  0.02467  0.68303  3.19410

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

    ##  [1]  8.2816912  5.4258793  4.4706384 -3.4136717 -0.9824888  4.1479033
    ##  [7]  1.3657576 -0.2841380  7.9748564  3.9268079 -1.5862408 10.8572515
    ## [13] 10.4030815  3.9379171  1.1466810  4.9892829 18.6082316  1.6774522
    ## [19] -2.0778084  1.4998604

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
    ## 1 -0.880  5.48

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.02  5.29

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.30  11.9

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.39  9.88

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
    ## 1 -0.880  5.48
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.02  5.29
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.30  11.9
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.39  9.88

## do the same thing with `map` instead

here is the list that I want, please apply the function mean_and_sd

``` r
output = map(list_norm, mean_and_sd)

output
```

    ## $a
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.880  5.48
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.02  5.29
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.30  11.9
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.39  9.88

let’s do a couple other things

combine lists

``` r
output = map(list_norm, IQR)

output = map_dbl(list_norm, IQR)
```

combine a list of dataframes

``` r
output = map_dfr(list_norm, mean_and_sd)

#equivalent to
output = map(list_norm, mean_and_sd) %>% 
  bind_rows()
```

## LIST COLUMNS!!

you can do normal df operations

``` r
listcol_df =
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norm
  )

listcol_df %>% 
  filter(name %in% c("a", "b"))
```

    ## # A tibble: 2 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>  
    ## 2 b     <dbl [20]>

``` r
listcol_df %>% 
  select(-samp)
```

    ## # A tibble: 4 × 1
    ##   name 
    ##   <chr>
    ## 1 a    
    ## 2 b    
    ## 3 c    
    ## 4 d

pull out a sample column

``` r
listcol_df[["samp"]][["a"]]
```

    ##  [1]  -0.56112733   6.65938116  -2.12003376   5.47074335   1.15411537
    ##  [6]   4.23670825  -3.70241766   4.72707745 -12.70372504  -0.50581893
    ## [11]  -2.54674952  -3.42626781  -7.11832785  -0.85015137  -3.73149186
    ## [16]   6.74729023   1.15306573 -12.48080102   0.02734026   1.97714046

compute mean and sd

``` r
mean_and_sd(listcol_df[["samp"]][["a"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.880  5.48

``` r
#do the same thing as before: map over a list and compute mean and sd
map(listcol_df[["samp"]], mean_and_sd)
```

    ## $a
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.880  5.48
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.02  5.29
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.30  11.9
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.39  9.88

ADD A LIST COLUMN!!!

``` r
listcol_df %>% 
  mutate(
    output = map(samp, mean_and_sd),
    iqr = map_dbl(samp, IQR))
```

    ## # A tibble: 4 × 4
    ##   name  samp         output             iqr
    ##   <chr> <named list> <named list>     <dbl>
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>  6.04
    ## 2 b     <dbl [20]>   <tibble [1 × 2]>  5.27
    ## 3 c     <dbl [20]>   <tibble [1 × 2]> 16.4 
    ## 4 d     <dbl [20]>   <tibble [1 × 2]> 16.1

``` r
listcol_df %>% 
  mutate(
    output = map(samp, mean_and_sd),
    iqr = map_dbl(samp, IQR)) %>% 
  select(-samp) %>% 
  unnest(output)
```

    ## # A tibble: 4 × 4
    ##   name    mean    sd   iqr
    ##   <chr>  <dbl> <dbl> <dbl>
    ## 1 a     -0.880  5.48  6.04
    ## 2 b      4.02   5.29  5.27
    ## 3 c      1.30  11.9  16.4 
    ## 4 d      2.39   9.88 16.1

### NSDUH

goal: have a df that has everything

new version of the nsduh function

``` r
nsduh_import = function(html, nth_table) {
  
  out_df = 
    html %>% 
    html_table() %>% 
    nth(nth_table) %>% 
    slice(-1) %>% 
    select(-contains("P Value"))
  
  return(out_df)
}
```

we need to import the html, extract correct tables

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)
```

``` r
nsduh_import(html = nsduh_html, nth_table = 1)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 12.90a           13.36            13.28b             12.86             
    ##  2 Nort… 13.88a           14.66            13.98              13.51             
    ##  3 Midw… 12.40b           12.76            12.45              12.33             
    ##  4 South 11.24a           11.64            12.02              11.88             
    ##  5 West  15.27            15.62            15.53a             14.43             
    ##  6 Alab… 9.98             9.60             9.90               9.71              
    ##  7 Alas… 19.60a           21.92            17.30              18.44             
    ##  8 Ariz… 13.69            13.12            15.12              13.45             
    ##  9 Arka… 11.37            11.59            12.79              12.14             
    ## 10 Cali… 14.49            15.25            15.03              14.11             
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_import(html = nsduh_html, nth_table = 4)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 1.66a            1.76             0.60               0.64              
    ##  2 Nort… 1.94a            2.18             0.60               0.66              
    ##  3 Midw… 1.37             1.43             0.48               0.54              
    ##  4 South 1.45b            1.56             0.53               0.57              
    ##  5 West  2.03             2.05             0.82               0.85              
    ##  6 Alab… 1.23             1.22             0.42               0.41              
    ##  7 Alas… 1.54a            2.00             0.51               0.65              
    ##  8 Ariz… 2.25             2.29             1.01               0.85              
    ##  9 Arka… 0.93             1.07             0.41               0.48              
    ## 10 Cali… 2.14             2.16             0.89               0.94              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_import(html = nsduh_html, nth_table = 5)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 0.30             0.33             0.12               0.10              
    ##  2 Nort… 0.43a            0.54             0.13               0.13              
    ##  3 Midw… 0.30             0.31             0.11               0.10              
    ##  4 South 0.27             0.26             0.12               0.08              
    ##  5 West  0.25             0.29             0.13               0.11              
    ##  6 Alab… 0.22             0.27             0.10               0.08              
    ##  7 Alas… 0.70a            1.23             0.11               0.08              
    ##  8 Ariz… 0.32a            0.55             0.17               0.20              
    ##  9 Arka… 0.19             0.17             0.10               0.07              
    ## 10 Cali… 0.20             0.20             0.13               0.09              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_listcol_df =
  tibble(
    drug = c("marj", "cocaine", "heroin"),
    nth_table = c(1, 4, 5),
  ) %>% 
  mutate(table = map(nth_table, nsduh_import, html = nsduh_html)) %>% 
#html is the only argument left, add as additional argument
  unnest(table)
```

Now we have a well-organized df

``` r
nsduh_listcol_df %>% 
  filter(State == "New York")
```

    ## # A tibble: 3 × 13
    ##   drug    nth_table State   `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)`
    ##   <chr>       <dbl> <chr>   <chr>            <chr>            <chr>             
    ## 1 marj            1 New Yo… 14.24b           15.04            13.94             
    ## 2 cocaine         4 New Yo… 2.28             2.54             0.71              
    ## 3 heroin          5 New Yo… 0.38a            0.52             0.13              
    ## # ℹ 7 more variables: `12-17(2014-2015)` <chr>, `18-25(2013-2014)` <chr>,
    ## #   `18-25(2014-2015)` <chr>, `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>,
    ## #   `18+(2013-2014)` <chr>, `18+(2014-2015)` <chr>

## Operations on nested data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USW00022534", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2021-01-01",
    date_max = "2022-12-31") |>
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USW00022534 = "Molokai_HI",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) |>
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: /Users/alicezhou/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2024-09-26 10:17:55.5208 (8.651)

    ## file min/max dates: 1869-01-01 / 2024-09-30

    ## using cached file: /Users/alicezhou/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USW00022534.dly

    ## date created (size, mb): 2024-09-26 10:18:03.380233 (3.932)

    ## file min/max dates: 1949-10-01 / 2024-09-30

    ## using cached file: /Users/alicezhou/Library/Caches/org.R-project.R/R/rnoaa/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2024-09-26 10:18:05.997975 (1.036)

    ## file min/max dates: 1999-09-01 / 2024-09-30

create a list column.

``` r
weather_nest= 
  weather_df %>% 
  nest(data = date:tmin)
```

``` r
weather_nest[["data"]][[1]]
```

    ## # A tibble: 730 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2021-01-01   157   4.4   0.6
    ##  2 2021-01-02    13  10.6   2.2
    ##  3 2021-01-03    56   3.3   1.1
    ##  4 2021-01-04     5   6.1   1.7
    ##  5 2021-01-05     0   5.6   2.2
    ##  6 2021-01-06     0   5     1.1
    ##  7 2021-01-07     0   5    -1  
    ##  8 2021-01-08     0   2.8  -2.7
    ##  9 2021-01-09     0   2.8  -4.3
    ## 10 2021-01-10     0   5    -1.6
    ## # ℹ 720 more rows

regressing tmax on tmin.

``` r
lm(tmax ~ tmin, data = weather_nest[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.514        1.034

``` r
lm(tmax ~ tmin, data = weather_nest[["data"]][[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     21.7547       0.3222

``` r
lm(tmax ~ tmin, data = weather_nest[["data"]][[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest[["data"]][[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.532        1.137

``` r
#here is my data, call it x, then plug it into the lm function
weather_nest %>% 
  mutate(model_fit = map(data, \(x) lm(tmax~tmin, data = x)))
```

    ## # A tibble: 3 × 4
    ##   name           id          data               model_fit
    ##   <chr>          <chr>       <list>             <list>   
    ## 1 CentralPark_NY USW00094728 <tibble [730 × 4]> <lm>     
    ## 2 Molokai_HI     USW00022534 <tibble [730 × 4]> <lm>     
    ## 3 Waterhole_WA   USS0023B17S <tibble [730 × 4]> <lm>
