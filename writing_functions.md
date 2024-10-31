writing_functions
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

z-score computation

``` r
x_vec = rnorm(n = 25, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.670228910  0.024668902 -0.842691524  1.073076397  0.582378825
    ##  [6] -0.150097738 -1.540703846 -0.102070328  1.898878735  0.767415468
    ## [11] -0.216576826  0.352785191  1.220199588  1.644506437 -0.049943605
    ## [16] -1.939514932 -1.488098099  0.013395089 -0.751930681  0.135938916
    ## [21] -0.770917598 -0.359123652  0.001531356 -0.515593315  1.682716153

write a function that does this

``` r
z_scores = function(x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if(length(x) < 5 ) {
    stop("you need at least 5 numbers to compute z score")
  }
  
  z = (x - mean(x))/sd(x)
  
  return(z)
  
}
```

does this always work?

``` r
z_scores(x = 3)
```

    ## Error in z_scores(x = 3): you need at least 5 numbers to compute z score

``` r
z_scores(x = c("my", "name", "is", "Alice"))
```

    ## Error in z_scores(x = c("my", "name", "is", "Alice")): x needs to be numeric

## a new function!

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

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.87  3.52

## check stuff using a simulation

``` r
sim_df =
  tibble(
    x = rnorm(30, 10, 5)
  )

sim_df %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.31  5.08

simulation function to check sample mean and sd

``` r
sim_mean_sd = function(samp_size, true_mean = 10, true_sd = 5) {
  sim_df =
  tibble(
    x = rnorm(samp_size, true_mean, true_sd)
  )

  out_df = 
    sim_df %>%
    summarize(
      mean = mean(x),
      sd = sd(x)
  )
  
  return(out_df)
}

sim_mean_sd(samp_size = 30, true_mean = 4, true_sd = 12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.15  11.3

``` r
sim_mean_sd(30, 4, 12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.31  13.8

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.5  6.02

## revisit lord of the rings words

``` r
fellowship_ring = readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") |>
  mutate(movie = "fellowship") %>% 
  janitor::clean_names()

two_towers = readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers") %>% 
  janitor::clean_names()

return_king = readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king") %>% 
  janitor::clean_names()
```

write a function that does this

``` r
lotr_import = function(cell_range, movie_title) {
 movie_df = readxl::read_excel("./data/LotR_Words.xlsx", range = cell_range) |>
  mutate(movie = movie_title) %>% 
  janitor::clean_names() %>% 
   pivot_longer(
     cols = female:male,
     names_to = "sex",
     values_to = "words"
   ) %>% 
   select(movie, everything())
 
 return(movie_df)
}

lotr_df = 
  bind_rows(
lotr_import(cell_range = "B3:D6", movie_title = "fellowship"),
lotr_import(cell_range = "F3:H6", movie_title = "fellowship"),
lotr_import(cell_range = "J3:L6", movie_title = "fellowship")
)
```

## NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

marj_table = 
  nsduh_html %>% 
  html_table() %>% 
  nth(1) %>% 
  slice(-1) %>% 
  mutate(drug = "marj")

cocaine_table = 
  nsduh_html %>% 
  html_table() %>% 
  nth(4) %>% 
  slice(-1) %>% 
  mutate(drug = "cocaine")

marj_table = 
  nsduh_html %>% 
  html_table() %>% 
  nth(5) %>% 
  slice(-1) %>% 
  mutate(drug = "heroin")
```

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

nsduh_import = function(html, nth_table, table_name) {

out_df = 
  html %>% 
  html_table() %>% 
  nth(nth_table) %>% 
  slice(-1) %>% 
  mutate(drug = table_name)

return(out_df)
}

nsduh_df = bind_rows(
    nsduh_import(html = nsduh_html, nth_table = 1 , table_name = "marj"),
    nsduh_import(html = nsduh_html, nth_table = 4 , table_name = "cocaine"),
    nsduh_import(html = nsduh_html, nth_table = 5 , table_name = "heroin")
    )
```
