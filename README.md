
<!-- README.md is generated from README.Rmd. Please edit that file -->

# casewhen

[![Travis build
status](https://travis-ci.org/RLesur/casewhen.svg?branch=master)](https://travis-ci.org/RLesur/casewhen)
[![Coverage
status](https://codecov.io/gh/RLesur/casewhen/branch/master/graph/badge.svg)](https://codecov.io/github/RLesur/casewhen?branch=master)

The goal of casewhen is to create reusable `dplyr::case_when()`
functions.  
`SAS` users may recognise a behavior close to the `SAS FORMATS`.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("RLesur/casewhen")
```

## Motivation

During data wrangling with `dplyr`, one may use several times identical
`case_when()` clauses in different steps. This can lead to a non-DRY
code. This package provides a convenient mean to define and reuse
`dplyr::case_when()` functions.

## Examples

With `casewhen`, you can easily create reusable `dplyr::case_when()`
functions with the function `create_case_when()`:

``` r
library(dplyr)
library(casewhen)

people <-
  tribble(
    ~name, ~sex, ~seek,
    "Mary", "F", "M",
    "Henry", "M", "F"
  )

cw_sex <- create_case_when(x == "F" ~ "Woman",
                           x == "M" ~ "Man",
                           TRUE ~ as.character(x),
                           vars = "x")

people %>% 
  mutate(sex_label = cw_sex(sex), 
         seek_label = cw_sex(seek))
#> # A tibble: 2 x 5
#>   name  sex   seek  sex_label seek_label
#>   <chr> <chr> <chr> <chr>     <chr>     
#> 1 Mary  F     M     Woman     Man       
#> 2 Henry M     F     Man       Woman
```

Reusing a `case_when()` function is mainly convenient when the same
transformation is performed on different
datasets.

``` r
cw_sex <- create_case_when(x == "F" | x == "female" & y == "Human" ~ "Woman",
                           x == "M" | x == "male" & y == "Human" ~ "Man",
                           TRUE ~ as.character(x),
                           vars = c("x", "y"))

people %>% 
  mutate(sex_label = cw_sex(sex, "Human"))
#> # A tibble: 2 x 4
#>   name  sex   seek  sex_label
#>   <chr> <chr> <chr> <chr>    
#> 1 Mary  F     M     Woman    
#> 2 Henry M     F     Man

starwars %>%
  mutate(sex_label = cw_sex(gender, species)) %>%
  select(name, gender, species, sex_label) %>%
  head()
#> # A tibble: 6 x 4
#>   name           gender species sex_label
#>   <chr>          <chr>  <chr>   <chr>    
#> 1 Luke Skywalker male   Human   Man      
#> 2 C-3PO          <NA>   Droid   <NA>     
#> 3 R2-D2          <NA>   Droid   <NA>     
#> 4 Darth Vader    male   Human   Man      
#> 5 Leia Organa    female Human   Woman    
#> 6 Owen Lars      male   Human   Man
```

## `dbplyr` support

*`dbplyr` support is experimental.*  
You first have to register the `SQL` translation to your connection:

``` r
library(dbplyr)

con <- NULL # use a connection instead
# register the case_when function:
add_sql_translate(cw_sex, con = con)
# see the translation:
translate_sql(cw_sex(gender, species), con = con)
#> <SQL> CASE
#> WHEN ("gender" = 'F' OR "gender" = 'female' AND "species" = 'Human') THEN ('Woman')
#> WHEN ("gender" = 'M' OR "gender" = 'male' AND "species" = 'Human') THEN ('Man')
#> WHEN (TRUE) THEN (CAST("gender" AS TEXT))
#> END
```
