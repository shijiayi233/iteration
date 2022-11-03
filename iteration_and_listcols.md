iteration_and_listcols
================
Jiayi Shi
2022-11-01

``` r
l = list(
  vec_numeric = 5:8,
  mat         = matrix(1:8, 2, 4),
  vec_logical = c(TRUE, FALSE),
  summary     = summary(rnorm(1000)))
l
```

    ## $vec_numeric
    ## [1] 5 6 7 8
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8
    ## 
    ## $vec_logical
    ## [1]  TRUE FALSE
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.00805 -0.69737 -0.03532 -0.01165  0.68843  3.81028

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
l[[1]][1:2]
```

    ## [1] 5 6

``` r
l[["mat"]]
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    3    5    7
    ## [2,]    2    4    6    8

## for loop

Given an input vector/list, apply a function to each element, save the
result to a output vector/list.

``` r
# input list
list_norms = 
  list(
    a = rnorm(20, 3, 1),
    b = rnorm(20, 0, 5),
    c = rnorm(20, 10, .2),
    d = rnorm(20, -3, 1)
  )

is.list(list_norms)
```

    ## [1] TRUE

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  #list(mean = mean_x, sd = sd_x)
  # return a list
  
  tibble(
    mean = mean_x, 
    sd = sd_x
  ) # return a data frame
}
```

``` r
output = vector("list", length = 4)
# how to create an empty list 
for (i in 1:4) {
  output[[i]] = mean_and_sd(list_norms[[i]])
}

#same
output = map(list_norms, mean_and_sd)
output = map(.x = list_norms, ~ mean_and_sd(.x))
```

## map (function in `purr`, better than for loop, = `lappy` in base R)

``` r
output = map(list_norms, mean_and_sd)
output
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70  1.12
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.416  4.08
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.1 0.191
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.43  1.18

``` r
output = map(list_norms, median)
output
```

    ## $a
    ## [1] 2.621376
    ## 
    ## $b
    ## [1] 0.7210996
    ## 
    ## $c
    ## [1] 10.05016
    ## 
    ## $d
    ## [1] -3.521665

- If know the output type, use map\_\* to simplify outputs.

``` r
output = map_dbl(list_norms, median)#output is numeric
output
```

    ##          a          b          c          d 
    ##  2.6213757  0.7210996 10.0501641 -3.5216649

``` r
output = map_df(list_norms, mean_and_sd)#simplified dataframe
output
```

    ## # A tibble: 4 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1  2.70  1.12 
    ## 2  0.416 4.08 
    ## 3 10.1   0.191
    ## 4 -3.43  1.18

- when your function has two arguments: map2\_\*

``` r
#output = map2(.x = input_1, .y = input_2, ~func(arg_1 = .x, arg_2 = .y))
```

## list, dataframe

list can contain anything: list(a=…,b=…)

dataframe is a special kind of list, but with same length and printed as
a table data_frame(a=…, b=…)

## list column

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norms
  )
```

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1] 4.134965 4.111932 2.129222 3.210732 3.069396 1.337351 3.810840 1.087654
    ##  [9] 1.753247 3.998154 2.459127 2.783624 1.378063 1.549036 3.350910 2.825453
    ## [17] 2.408572 1.665973 1.902701 5.036104
    ## 
    ## $b
    ##  [1] -1.63244797  3.87002606  3.92503200  3.81623040  1.47404380 -6.26177962
    ##  [7] -5.04751876  3.75695597 -6.54176756  2.63770049 -2.66769787 -1.99188007
    ## [13] -3.94784725 -1.15070568  4.38592421  2.26866589 -1.16232074  4.35002762
    ## [19]  8.28001867 -0.03184464
    ## 
    ## $c
    ##  [1] 10.094098 10.055644  9.804419  9.814683 10.383954 10.176256 10.148416
    ##  [8] 10.029515 10.097078 10.030371 10.008400 10.044684  9.797907 10.480244
    ## [15] 10.160392  9.949758 10.242578  9.874548 10.342232  9.921125
    ## 
    ## $d
    ##  [1] -5.321491 -1.635881 -1.867771 -3.774316 -4.410375 -4.834528 -3.269014
    ##  [8] -4.833929 -3.814468 -2.836428 -2.144481 -3.819963 -3.123603 -2.745052
    ## [15] -1.281074 -3.958544 -4.604310 -4.845609 -2.444263 -3.060119

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70  1.12

## Nested data

``` r
weather_df = 
  rnoaa::meteo_pull_monitors(
    c("USW00094728", "USC00519397", "USS0023B17S"),
    var = c("PRCP", "TMIN", "TMAX"), 
    date_min = "2017-01-01",
    date_max = "2017-12-31") %>%
  mutate(
    name = recode(
      id, 
      USW00094728 = "CentralPark_NY", 
      USC00519397 = "Waikiki_HA",
      USS0023B17S = "Waterhole_WA"),
    tmin = tmin / 10,
    tmax = tmax / 10) %>%
  select(name, id, everything())
```

    ## Registered S3 method overwritten by 'hoardr':
    ##   method           from
    ##   print.cache_info httr

    ## using cached file: C:\Users\sjy99\AppData\Local/Cache/R/noaa_ghcnd/USW00094728.dly

    ## date created (size, mb): 2022-09-29 10:32:27 (8.418)

    ## file min/max dates: 1869-01-01 / 2022-09-30

    ## using cached file: C:\Users\sjy99\AppData\Local/Cache/R/noaa_ghcnd/USC00519397.dly

    ## date created (size, mb): 2022-09-29 10:32:47 (1.703)

    ## file min/max dates: 1965-01-01 / 2020-03-31

    ## using cached file: C:\Users\sjy99\AppData\Local/Cache/R/noaa_ghcnd/USS0023B17S.dly

    ## date created (size, mb): 2022-09-29 10:32:58 (0.952)

    ## file min/max dates: 1999-09-01 / 2022-09-30

``` r
weather_df %>% head()
```

    ## # A tibble: 6 × 6
    ##   name           id          date        prcp  tmax  tmin
    ##   <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ## 1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ## 2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ## 3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ## 4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ## 5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ## 6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8

The station name and id are constant across the year’s temperature and
precipitation data. For that reason, we can reorganize these data into a
new data frame with a single row for each station. Weather data will be
separated into three station-specific data frames, each of which is the
data “observation” for the respective station.

``` r
weather_nest_df = 
  nest(weather_df, data = date:tmin)

weather_nest_df[["data"]]
```

    ## [[1]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0   8.9   4.4
    ##  2 2017-01-02    53   5     2.8
    ##  3 2017-01-03   147   6.1   3.9
    ##  4 2017-01-04     0  11.1   1.1
    ##  5 2017-01-05     0   1.1  -2.7
    ##  6 2017-01-06    13   0.6  -3.8
    ##  7 2017-01-07    81  -3.2  -6.6
    ##  8 2017-01-08     0  -3.8  -8.8
    ##  9 2017-01-09     0  -4.9  -9.9
    ## 10 2017-01-10     0   7.8  -6  
    ## # … with 355 more rows
    ## 
    ## [[2]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01     0  26.7  16.7
    ##  2 2017-01-02     0  27.2  16.7
    ##  3 2017-01-03     0  27.8  17.2
    ##  4 2017-01-04     0  27.2  16.7
    ##  5 2017-01-05     0  27.8  16.7
    ##  6 2017-01-06     0  27.2  16.7
    ##  7 2017-01-07     0  27.2  16.7
    ##  8 2017-01-08     0  25.6  15  
    ##  9 2017-01-09     0  27.2  15.6
    ## 10 2017-01-10     0  28.3  17.2
    ## # … with 355 more rows
    ## 
    ## [[3]]
    ## # A tibble: 365 × 4
    ##    date        prcp  tmax  tmin
    ##    <date>     <dbl> <dbl> <dbl>
    ##  1 2017-01-01   432  -6.8 -10.7
    ##  2 2017-01-02    25 -10.5 -12.4
    ##  3 2017-01-03     0  -8.9 -15.9
    ##  4 2017-01-04     0  -9.9 -15.5
    ##  5 2017-01-05     0  -5.9 -14.2
    ##  6 2017-01-06     0  -4.4 -11.3
    ##  7 2017-01-07    51   0.6 -11.5
    ##  8 2017-01-08    76   2.3  -1.2
    ##  9 2017-01-09    51  -1.2  -7  
    ## 10 2017-01-10     0  -5   -14.2
    ## # … with 355 more rows

unnesting

``` r
weather_nest_df %>% 
  unnest(data)
```

    ## # A tibble: 1,095 × 6
    ##    name           id          date        prcp  tmax  tmin
    ##    <chr>          <chr>       <date>     <dbl> <dbl> <dbl>
    ##  1 CentralPark_NY USW00094728 2017-01-01     0   8.9   4.4
    ##  2 CentralPark_NY USW00094728 2017-01-02    53   5     2.8
    ##  3 CentralPark_NY USW00094728 2017-01-03   147   6.1   3.9
    ##  4 CentralPark_NY USW00094728 2017-01-04     0  11.1   1.1
    ##  5 CentralPark_NY USW00094728 2017-01-05     0   1.1  -2.7
    ##  6 CentralPark_NY USW00094728 2017-01-06    13   0.6  -3.8
    ##  7 CentralPark_NY USW00094728 2017-01-07    81  -3.2  -6.6
    ##  8 CentralPark_NY USW00094728 2017-01-08     0  -3.8  -8.8
    ##  9 CentralPark_NY USW00094728 2017-01-09     0  -4.9  -9.9
    ## 10 CentralPark_NY USW00094728 2017-01-10     0   7.8  -6  
    ## # … with 1,085 more rows

``` r
lm(tmax~tmin, data = weather_nest_df[["data"]][[1]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[1]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039

``` r
lm(tmax~tmin, data = weather_nest_df[["data"]][[2]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[2]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509

``` r
lm(tmax~tmin, data = weather_nest_df[["data"]][[3]])
```

    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = weather_nest_df[["data"]][[3]])
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

- use map to simplify:

``` r
weather_lm = function(df){
  lm(tmax~tmin, data = df)
}
```

``` r
map(weather_nest_df[["data"]], weather_lm)
```

    ## [[1]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.209        1.039  
    ## 
    ## 
    ## [[2]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##     20.0966       0.4509  
    ## 
    ## 
    ## [[3]]
    ## 
    ## Call:
    ## lm(formula = tmax ~ tmin, data = df)
    ## 
    ## Coefficients:
    ## (Intercept)         tmin  
    ##       7.499        1.221

## Napoleon reviews

``` r
read_page_reviews <- function(url) {
  
  html = read_html(url)
  
  review_titles = 
    html %>%
    html_nodes(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    html %>%
    html_nodes("#cm_cr-review_list .review-rating") %>%
    html_text() %>%
    str_extract("^\\d") %>%
    as.numeric()
  
  review_text = 
    html %>%
    html_nodes(".review-text-content span") %>%
    html_text() %>% 
    str_replace_all("\n", "") %>% 
    str_trim() %>% 
    str_subset("The media could not be loaded.", negate = TRUE) %>% 
    str_subset("^$", negate = TRUE)
  
  tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
}
```

``` r
url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

vec_urls = str_c(url_base, 1:5)
```

``` r
np_reviews = 
  tibble(
  page = 1:5,
  page_url = str_c(url_base, page)
  ) %>% 
  mutate(reviews = map(page_url, read_page_reviews)) %>% 
  select(-page_url) %>% 
  unnest(reviews)
```

``` r
output = map(vec_urls, read_page_reviews)
dynamite_reviews = bind_rows(output)
```
