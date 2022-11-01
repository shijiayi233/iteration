write_function
================
Jiayi Shi
2022-10-27

``` r
library(tidyverse)
library(rvest)
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

``` r
set.seed(1)
```

## z-scores (single output)

``` r
z_scores = function(x){
  
  if (!is.numeric(x)){
    stop("Z scores only work for numbers")
  }
  
  if (length(x)<3){
    stop("Z scores really only work if you have >=3 numbers")
  }
  
  z = (x-mean(x))/sd(x)
  
  z
}
```

## multiple outputs

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

## multiple inputs

``` r
sim_data = tibble(
  x = rnorm(30, mean = 2, sd = 3)
)

sim_data %>% 
  summarize(
    mu_hat = mean(x),
    sigma_hat = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1   2.25      2.77

Do this using a function.

``` r
sim_mean_sd = function(n, mu, sigma) {
  
  sim_data = tibble(
    x = rnorm(n, mean = mu, sd = sigma),
  )
  
  sim_data %>% 
    summarize(
      mu_hat = mean(x),
      sigma_hat = sd(x)
    )
}
```

``` r
sim_mean_sd(10,0,1)
```

    ## # A tibble: 1 × 2
    ##   mu_hat sigma_hat
    ##    <dbl>     <dbl>
    ## 1  0.121     0.809

## read page reviews

``` r
cur = function(x){
  for (i in 1:length(x)){
     substr(x[i],1,3) 
  }
}
```

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)
```

``` r
read_page_reviews = function(url){
  
dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text() #%>% 
  #str_replace_all("\n","") %>% 
  #str_trim() %>% 
  #str_subset("The media could not be loaded.", negate = TRUE) %>% 
  #str_subset("^$", negate = TRUE)
#str_trim("\n\nString with trailing and leading white space\n\n")
#> [1] "String with trailing and leading white space"
#> ?\n

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
reviews
}
```

``` r
read_page_reviews(url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1")
```

    ## # A tibble: 10 × 3
    ##    title                                stars text                              
    ##    <chr>                                <dbl> <chr>                             
    ##  1 70’s and 80’s Schtick Comedy             5 "…especially funny if you have ev…
    ##  2 Amazon Censorship                        5 "I hope Amazon does not censor my…
    ##  3 Watch to say you did                     3 "I know it's supposed to be a cul…
    ##  4 Best Movie Ever!                         5 "We just love this movie and even…
    ##  5 Quirky                                   5 "Good family film"                
    ##  6 Funny movie - can't play it !            1 "Sony 4k player won't even recogn…
    ##  7 A brilliant story about teenage life     5 "Napoleon Dynamite delivers dry h…
    ##  8 HUHYAH                                   5 "Spicy"                           
    ##  9 Cult Classic                             4 "Takes a time or two to fully app…
    ## 10 Sweet                                    5 "Timeless Movie. My Grandkids are…

``` r
base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

vec_url = str_c(base_url, c(1,2,4,5))

reviews = bind_rows(
  read_page_reviews(vec_url[1]),
  read_page_reviews(vec_url[2]),
  read_page_reviews(vec_url[3]),
  read_page_reviews(vec_url[4]),
)
```

## movie example

``` r
  fellowship_ring = readxl::read_excel("./data/LotR_Words.xlsx", range = "B3:D6") %>%
  mutate(movie = "fellowship_ring")

two_towers = readxl::read_excel("./data/LotR_Words.xlsx", range = "F3:H6") %>%
  mutate(movie = "two_towers")

return_king = readxl::read_excel("./data/LotR_Words.xlsx", range = "J3:L6") %>%
  mutate(movie = "return_king")

lotr_tidy = bind_rows(fellowship_ring, two_towers, return_king) %>%
  janitor::clean_names() %>%
  gather(key = sex, value = words, female:male) %>%
  mutate(race = str_to_lower(race)) %>% 
  select(movie, everything())
```

``` r
lotr_load_and_tidy = function(path, range, movie_name) {
  
  df = readxl::read_excel(path, range = range) %>%
    janitor::clean_names() %>%
    gather(key = sex, value = words, female:male) %>%
    mutate(race = str_to_lower(race),
           movie = movie_name)
  
  df
  
}

lotr_tidy = 
  bind_rows(
    lotr_load_and_tidy("./data/LotR_Words.xlsx", "B3:D6", "fellowship_ring"),
    lotr_load_and_tidy("./data/LotR_Words.xlsx", "F3:H6", "two_towers"),
    lotr_load_and_tidy("./data/LotR_Words.xlsx", "J3:L6", "return_king")) %>%
  select(movie, everything()) 
```
