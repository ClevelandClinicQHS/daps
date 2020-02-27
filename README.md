
<!-- README.md is generated from README.Rmd. Please edit that file -->

# daps

<!-- badges: start -->

<!-- badges: end -->

The goal of daps is to …

## Installation

<!-- You can install the released version of daps from [CRAN](https://CRAN.R-project.org) with: -->

<!-- -->

<!-- ``` r -->

<!-- install.packages("daps") -->

<!-- ``` -->

You can install the development version of `daps` from
[GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("NikKrieger/daps")
```

## Create a `daps` object

Create a `daps-class` object and add a metastate model as well as
individual variable models.

``` r
library(ordinal)
library(nnet)
library(tidyverse)
#> ── Attaching packages ────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
#> ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
#> ✓ tibble  2.1.3     ✓ dplyr   0.8.3
#> ✓ tidyr   1.0.2     ✓ stringr 1.4.0
#> ✓ readr   1.3.1     ✓ forcats 0.4.0
#> ── Conflicts ───────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
#> x dplyr::slice()  masks ordinal::slice()
library(daps)
#> 
#> Attaching package: 'daps'
#> The following objects are masked from 'package:stats':
#> 
#>     lag, simulate
```

topological sorting: first do all the static variables and then temporal
check for intra-timeslice dependencies.

# For each variable:

    #   (1) get model (models have binary flag that indicate intra-timeslice dependencies)
    #       if max of flags is 1, go into subroutine in order to generate dag and topological sort

``` r
daps1 <-
  daps() %>% 
  
  add_models(
    
    diabetes(t) := glm(~sex + lag(dbp, 2) + lag(sbp) + lag(dbp) + lag(sbp, 2) + race + age, family = binomial),
    
    age(t) := lag(age) + 1,
    
    sbp(t) := list(
      lm(~ lag(sbp) + lag(dbp) + sex + race + age),
      lm(~ sex + race + age),
      lag(sbp),
      rnorm(1, 130, 15)
    ),
    
    dbp(t) := lm(~lag(sbp) + lag(dbp) + sex + race + age),
    dbp(t) := lm(~sex + race + age),
    
    dbp(t) := lm(~sbp),
    
    # Find a way to determine the intratimeslice variables each model is using.
    # During simulation, when you come across one that has at least one intraslice model, do the others first.
    
    diabetes(t) := glm(~sex + race + age + lag(sbp) + lag(dbp), family = binomial),
    diabetes(t) := glm(~sex + race + age, family = binomial),
    
    chd_risk(t) := list(
      clm(~ lag(sbp) + lag(dbp) + sex + race + age),
      multinom(~ lag(sbp) + slide_mean(dbp) + sex + race + age)
    ),
    
    glucose(t) := lm(~sbp + dbp + diabetes + age + sex + race),
    glucose(t) := lm(~sbp + dbp + age + sex + race),
    glucose(t) := lm(~diabetes + age + sex + race),
    glucose(t) := lm(~age + sex + race),
    glucose(t) := lag(glucose)
    
    # sex := categorical(c("M", "F"), M = .495),
    # sex := factor(rbinom(1, 1, .505), 0:1, c("Male", "Female")),
    # 
    # 
    # race := categorical(M = .495, F = .505),
    #   factor(
    #     1:3 %*% rmultinom(1, 1, c(.7, .25, .05)),
    #     1:3,
    #     c("White", "Black", "Other")
    #   )
      
  ) %>%
  
  add_metastate_model(
    metastate = c("state1", "state2", "state3"),
    nodes =
      list(
        c("age", "sbp", "dbp", "chd_risk", "diabetes"),
        c("age", "sbp", "dbp", "chd_risk", "diabetes"),
        c("age", "sbp", "dbp", "chd_risk", "diabetes", "glucose")
      ),
    transitions =
      list(
        ~case_when(
          lag(sbp) >= 135 | lag(dbp) >= 80                            ~ "state3",
          lag(chd_risk) == "elevated" | sex == "male" & lag(age) > 35 ~ "state2",
          TRUE                                                        ~ "state1"
        ),
        ~case_when(
          lag(sbp) >= 135 | lag(dbp) >= 80                            ~ "state3",
          TRUE                                                        ~ "state2"
        ),
        ~case_when(
          TRUE                                                        ~ "state3"
        )
      )
  )

daps1
#> $model_table
#> # A tibble: 18 x 4
#>    model_id var      dynamic model_quo 
#>  *    <int> <chr>    <lgl>   <list>    
#>  1        1 diabetes TRUE    <dps_ftt_>
#>  2        2 age      TRUE    <dps_nnf_>
#>  3        3 sbp      TRUE    <dps_ftt_>
#>  4        4 sbp      TRUE    <dps_ftt_>
#>  5        5 sbp      TRUE    <dps_nnf_>
#>  6        6 sbp      TRUE    <dps_nnf_>
#>  7        7 dbp      TRUE    <dps_ftt_>
#>  8        8 dbp      TRUE    <dps_ftt_>
#>  9        9 dbp      TRUE    <dps_ftt_>
#> 10       10 diabetes TRUE    <dps_ftt_>
#> 11       11 diabetes TRUE    <dps_ftt_>
#> 12       12 chd_risk TRUE    <dps_ftt_>
#> 13       13 chd_risk TRUE    <dps_ftt_>
#> 14       14 glucose  TRUE    <dps_ftt_>
#> 15       15 glucose  TRUE    <dps_ftt_>
#> 16       16 glucose  TRUE    <dps_ftt_>
#> 17       17 glucose  TRUE    <dps_ftt_>
#> 18       18 glucose  TRUE    <dps_nnf_>
#> 
#> $metastate_model
#> # A tibble: 3 x 3
#>   metastate nodes     transitions
#> * <chr>     <list>    <list>     
#> 1 state1    <chr [5]> <quosure>  
#> 2 state2    <chr [5]> <quosure>  
#> 3 state3    <chr [6]> <quosure>  
#> 
#> attr(,"class")
#> [1] "daps"
```

``` r
teststatic <- 
  tribble(
    ~id, ~sex,     ~race,
    1,   "male",   "black",
    2,   "female", "white",
    3,   "male",   "black",
    4,   "male",   "white",
    6,   "female", "black"
  )

testtemporal <-
  tribble(
    ~id, ~t, ~sbp, ~dbp, ~age, ~diabetes, ~glucose, ~chd_risk,
    1,   1,  120,  65,   35,   FALSE,     NA,       "low",
    1,   2,  119,  66,   36,   FALSE,     NA,       "normal",
    1,   3,  118,  68,   50,   FALSE,     NA,       "normal",
    1,   4,  150,  100,  51,   TRUE,      NA,       "elevated",
     
    2,   1,  139,  81,   64,   FALSE,     NA,       "normal",
    2,   2,  140,  111,  66,   FALSE,     90,       "elevated",
    2,   3,  137,  85,   66,   TRUE,      100,      "elevated",
    2,   4,  155,  90,   66,   TRUE,      99,       "elevated",
     
    3,   1,  100,  40,   34,   TRUE,      NA,       "low",
    3,   2,  114,  45,   34,   FALSE,     NA,       "low",
    3,   3,  100,  50,   34,   FALSE,     NA,       "low",
    3,   4,  103,  56,   34,   FALSE,     NA,       "low",
     
    4,   1,  115,  110,  85,   FALSE,     125,      "elevated",
    4,   2,  140,  125,  86,   TRUE,      NA,       "elevated",
    4,   3,  NA,   NA,   87,   TRUE,      100,      "elevated",
    4,   4,  NA,   NA,   88,   TRUE,      NA,       "elevated",
    
    6,   1,  114,  111,  86,   FALSE,     99,       "normal"
  ) %>% 
  mutate_at("chd_risk", ordered, levels = c("low", "normal", "elevated"))
```

``` r
daps_trained <- daps1 %>% train(teststatic, testtemporal)
#> Warning: glm.fit: algorithm did not converge
#> Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
#> Warning: (1) Hessian is numerically singular: parameters are not uniquely determined 
#> In addition: Absolute convergence criterion was met, but relative criterion was not met
#> # weights:  21 (12 variable)
#> initial  value 12.084735 
#> iter  10 value 1.962247
#> iter  20 value 0.075340
#> iter  30 value 0.000969
#> final  value 0.000040 
#> converged

daps_trained
#> $model_table
#> # A tibble: 18 x 4
#>    model_id var      dynamic model_quo 
#>  *    <int> <chr>    <lgl>   <list>    
#>  1        1 diabetes TRUE    <dps_ftt_>
#>  2        2 age      TRUE    <dps_nnf_>
#>  3        3 sbp      TRUE    <dps_ftt_>
#>  4        4 sbp      TRUE    <dps_ftt_>
#>  5        5 sbp      TRUE    <dps_nnf_>
#>  6        6 sbp      TRUE    <dps_nnf_>
#>  7        7 dbp      TRUE    <dps_ftt_>
#>  8        8 dbp      TRUE    <dps_ftt_>
#>  9        9 dbp      TRUE    <dps_ftt_>
#> 10       10 diabetes TRUE    <dps_ftt_>
#> 11       11 diabetes TRUE    <dps_ftt_>
#> 12       12 chd_risk TRUE    <dps_ftt_>
#> 13       13 chd_risk TRUE    <dps_ftt_>
#> 14       14 glucose  TRUE    <dps_ftt_>
#> 15       15 glucose  TRUE    <dps_ftt_>
#> 16       16 glucose  TRUE    <dps_ftt_>
#> 17       17 glucose  TRUE    <dps_ftt_>
#> 18       18 glucose  TRUE    <dps_nnf_>
#> 
#> $metastate_model
#> # A tibble: 3 x 3
#>   metastate nodes     transitions
#> * <chr>     <list>    <list>     
#> 1 state1    <chr [5]> <quosure>  
#> 2 state2    <chr [5]> <quosure>  
#> 3 state3    <chr [6]> <quosure>  
#> 
#> $trained_models
#> # A tibble: 18 x 3
#>    model_id var      model     
#>       <int> <chr>    <list>    
#>  1        1 diabetes <glm>     
#>  2        2 age      <dps_nnf_>
#>  3        3 sbp      <lm>      
#>  4        4 sbp      <lm>      
#>  5        5 sbp      <dps_nnf_>
#>  6        6 sbp      <dps_nnf_>
#>  7        7 dbp      <lm>      
#>  8        8 dbp      <lm>      
#>  9        9 dbp      <lm>      
#> 10       10 diabetes <glm>     
#> 11       11 diabetes <glm>     
#> 12       12 chd_risk <clm>     
#> 13       13 chd_risk <multinom>
#> 14       14 glucose  <lm>      
#> 15       15 glucose  <lm>      
#> 16       16 glucose  <lm>      
#> 17       17 glucose  <lm>      
#> 18       18 glucose  <dps_nnf_>
#> 
#> $pred_setup
#> $pred_setup$models
#> # A tibble: 18 x 6
#>    var   check_nonimputa… check_imputable imputable_vars imputable_lags
#>    <chr> <list>           <list>          <list>         <list>        
#>  1 diab… <quosure>        <quosure>       <chr [4]>      <dbl [4]>     
#>  2 age   <quosure>        <quosure>       <chr [1]>      <int [1]>     
#>  3 sbp   <quosure>        <quosure>       <chr [2]>      <int [2]>     
#>  4 sbp   <quosure>        <quosure>       <chr [0]>      <int [0]>     
#>  5 sbp   <quosure>        <quosure>       <chr [1]>      <int [1]>     
#>  6 sbp   <quosure>        <quosure>       <chr [0]>      <int [0]>     
#>  7 dbp   <quosure>        <quosure>       <chr [2]>      <int [2]>     
#>  8 dbp   <quosure>        <quosure>       <chr [0]>      <int [0]>     
#>  9 dbp   <quosure>        <quosure>       <chr [0]>      <int [0]>     
#> 10 diab… <quosure>        <quosure>       <chr [2]>      <int [2]>     
#> 11 diab… <quosure>        <quosure>       <chr [0]>      <int [0]>     
#> 12 chd_… <quosure>        <quosure>       <chr [2]>      <int [2]>     
#> 13 chd_… <quosure>        <quosure>       <chr [1]>      <int [1]>     
#> 14 gluc… <quosure>        <quosure>       <chr [0]>      <int [0]>     
#> 15 gluc… <quosure>        <quosure>       <chr [0]>      <int [0]>     
#> 16 gluc… <quosure>        <quosure>       <chr [0]>      <int [0]>     
#> 17 gluc… <quosure>        <quosure>       <chr [0]>      <int [0]>     
#> 18 gluc… <quosure>        <quosure>       <chr [1]>      <int [1]>     
#> # … with 1 more variable: pred_expr <list>
#> 
#> $pred_setup$mm
#> # A tibble: 3 x 3
#>   metastate nodes     transitions
#>   <chr>     <list>    <list>     
#> 1 state1    <chr [5]> <quosure>  
#> 2 state2    <chr [5]> <quosure>  
#> 3 state3    <chr [6]> <quosure>  
#> 
#> $pred_setup$sim_row_env
#> <environment: 0xa0d0a40>
#> 
#> 
#> attr(,"class")
#> [1] "daps"
```

``` r
daps_trained %>% 
simulate(
  static = teststatic, 
  longitudinal = testtemporal, 
  h = NULL,
  from = "last",
  to = 10,
  impute = TRUE,
  seed = 20200123
)
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> # A tibble: 33 x 11
#>       id sex   race      t metastate   sbp   dbp   age diabetes glucose chd_risk
#>    <int> <chr> <chr> <int> <chr>     <dbl> <dbl> <dbl> <lgl>      <dbl> <ord>   
#>  1     1 male  black     5 state3     134.  95.9    52 TRUE       -391. elevated
#>  2     1 male  black     6 state3     137.  90.2    53 FALSE       523. elevated
#>  3     1 male  black     7 state3     139.  95.1    54 TRUE       -307. elevated
#>  4     1 male  black     8 state3     140.  96.8    55 FALSE       381. elevated
#>  5     1 male  black     9 state3     142.  99.0    56 FALSE       343. elevated
#>  6     1 male  black    10 state3     143. 101.     57 FALSE       303. elevated
#>  7     2 fema… white     5 state3     145. 107.     67 FALSE       264. elevated
#>  8     2 fema… white     6 state3     146.  98.8    68 FALSE       512. elevated
#>  9     2 fema… white     7 state3     149. 103.     69 FALSE       411. elevated
#> 10     2 fema… white     8 state3     150. 105.     70 FALSE       376. elevated
#> # … with 23 more rows

daps_trained %>% 
simulate(
  static = teststatic, 
  longitudinal = testtemporal, 
  h = 1:3,
  from = "last",
  to = 3,
  impute = FALSE,
  seed = 20200123
)
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> # A tibble: 33 x 12
#>       id sex   race      t     h metastate   sbp   dbp   age diabetes glucose
#>    <int> <chr> <chr> <int> <int> <chr>     <dbl> <dbl> <dbl> <lgl>      <dbl>
#>  1     1 male  black     4     1 state3     134.  95.9    52 TRUE       -391.
#>  2     1 male  black     4     2 state3     137.  90.2    53 FALSE       523.
#>  3     1 male  black     4     3 state3     139.  95.1    54 TRUE       -307.
#>  4     1 male  black     3     1 state2     136.  85.3    51 TRUE         NA 
#>  5     1 male  black     3     2 state3     136.  92.2    52 TRUE       -270.
#>  6     1 male  black     3     3 state3     137.  92.1    53 FALSE       470.
#>  7     2 fema… white     4     1 state3     145. 107.     67 FALSE       264.
#>  8     2 fema… white     4     2 state3     146.  98.8    68 FALSE       512.
#>  9     2 fema… white     4     3 state3     149. 103.     69 FALSE       411.
#> 10     2 fema… white     3     1 state3     146.  98.3    67 TRUE       -199.
#> # … with 23 more rows, and 1 more variable: chd_risk <ord>

daps_trained %>% 
  simulate(
    static = teststatic,
    longitudinal = testtemporal,
    h = NULL,
    from = 5,
    to = 7,
    impute = "locf"
  )
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading

#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(object, newdata, se.fit, scale = 1, type = ifelse(type
#> == : prediction from a rank-deficient fit may be misleading
#> Warning in predict.lm(structure(list(coefficients = c(`(Intercept)` =
#> 1464.99999999999, : prediction from a rank-deficient fit may be misleading
#> # A tibble: 15 x 11
#>       id sex   race      t metastate   sbp   dbp   age diabetes glucose chd_risk
#>    <int> <chr> <chr> <int> <chr>     <dbl> <dbl> <dbl> <lgl>      <dbl> <ord>   
#>  1     1 male  black     5 state3     134.  95.9    52 TRUE       -391. elevated
#>  2     1 male  black     6 state3     137.  90.2    53 FALSE       523. elevated
#>  3     1 male  black     7 state3     139.  95.1    54 TRUE       -307. elevated
#>  4     2 fema… white     5 state3     145. 107.     67 FALSE       264. elevated
#>  5     2 fema… white     6 state3     146.  98.8    68 FALSE       512. elevated
#>  6     2 fema… white     7 state3     149. 103.     69 FALSE       411. elevated
#>  7     3 male  black     5 state1     109.  50.1    35 TRUE         NA  normal  
#>  8     3 male  black     6 state1     111.  56.9    36 TRUE         NA  normal  
#>  9     3 male  black     7 state2     112.  58.1    37 TRUE         NA  normal  
#> 10     4 male  white     5 state3     143. 140.     89 FALSE      -480. elevated
#> 11     4 male  white     6 state3     144. 140.     90 FALSE      -451. elevated
#> 12     4 male  white     7 state3     145. 142.     91 FALSE      -491. elevated
#> 13     6 fema… black     5 state3     235. 130.     87 TRUE       -188. elevated
#> 14     6 fema… black     6 state3     231. 193.     88 TRUE      -2024. elevated
#> 15     6 fema… black     7 state3     228. 175.     89 FALSE      -816. elevated

# Add in a count of patients/observations with incomplete predictions
# impute options:
#   - default: FALSE
#   - locf
#   - simulate at the preceding rows with missingness (cut it off if you have to back more than lookback_steps steps)

# daps() %>%
#   add_models() %>% 
#   add_metastate_model() %>%
#   train() %>%
#   simulate()
```
