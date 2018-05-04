
<!-- README.md is generated from README.Rmd. Please edit that file -->

# casewhen

[![Travis build
status](https://travis-ci.org/RLesur/casewhen.svg?branch=master)](https://travis-ci.org/RLesur/casewhen)
[![Coverage
status](https://codecov.io/gh/RLesur/casewhen/branch/master/graph/badge.svg)](https://codecov.io/github/RLesur/casewhen?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/casewhen)](https://cran.r-project.org/package=casewhen)

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
code.  
This package provides a convenient mean to define and reuse
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
transformation is performed across different datasets.

``` r
cw_sexyverse <- 
  create_case_when(x == "F" | x == "female" & y == "Human" ~ "Woman",
                   x == "M" | x == "male" & y == "Human" ~ "Man",
                   TRUE ~ as.character(x),
                   vars = c("x", "y")
                   )

people %>% 
  mutate(sex_label = cw_sexyverse(sex, "Human"))
#> # A tibble: 2 x 4
#>   name  sex   seek  sex_label
#>   <chr> <chr> <chr> <chr>    
#> 1 Mary  F     M     Woman    
#> 2 Henry M     F     Man

starwars %>%
  mutate(sex_label = cw_sexyverse(gender, species)) %>%
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

You only have to register the `case_when` functions to your connection:

``` r
con <- 
  DBI::dbConnect(RSQLite::SQLite(), ":memory:") %>%
  add_case_when(cw_sex, cw_sexyverse)

people_db <- copy_to(con, people)
starwars_db <- copy_to(con, starwars %>% select(name, gender, species))

people_db %>% 
  mutate(sex_label = cw_sex(sex), 
         seek_label = cw_sex(seek)) %>%
  show_query()
```

``` sql
SELECT `name`, `sex`, `seek`, 
    CASE
      WHEN (`sex` = 'F') THEN ('Woman')
      WHEN (`sex` = 'M') THEN ('Man')
      WHEN (1) THEN (CAST(`sex` AS TEXT))
    END AS `sex_label`, 
    CASE
      WHEN (`seek` = 'F') THEN ('Woman')
      WHEN (`seek` = 'M') THEN ('Man')
      WHEN (1) THEN (CAST(`seek` AS TEXT))
    END AS `seek_label`
FROM `people`
```

``` r
people_db %>%
  mutate(sex_label = cw_sexyverse(sex, "Human")) %>%
  show_query()
```

``` sql
SELECT `name`, `sex`, `seek`, 
    CASE
      WHEN (`sex` = 'F' OR `sex` = 'female' AND 'Human' = 'Human') THEN ('Woman')
      WHEN (`sex` = 'M' OR `sex` = 'male' AND 'Human' = 'Human') THEN ('Man')
      WHEN (1) THEN (CAST(`sex` AS TEXT))
    END AS `sex_label`
FROM `people`
```

``` r
starwars_db %>%
  mutate(sex_label = cw_sexyverse(gender, species)) %>%
  show_query()
```

``` sql
SELECT `name`, `gender`, `species`, 
    CASE
      WHEN (`gender` = 'F' OR `gender` = 'female' AND `species` = 'Human') THEN ('Woman')
      WHEN (`gender` = 'M' OR `gender` = 'male' AND `species` = 'Human') THEN ('Man')
      WHEN (1) THEN (CAST(`gender` AS TEXT))
    END AS `sex_label`
FROM `starwars %>% select(name, gender, species)`
```

``` r
DBI::dbDisconnect(con)
```
