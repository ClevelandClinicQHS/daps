
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
#> ── Attaching packages ──────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse 1.2.1 ──
#> ✓ ggplot2 3.2.1     ✓ purrr   0.3.3
#> ✓ tibble  2.1.3     ✓ dplyr   0.8.3
#> ✓ tidyr   1.0.2     ✓ stringr 1.4.0
#> ✓ readr   1.3.1     ✓ forcats 0.4.0
#> ── Conflicts ─────────────────────────────────────────────────────────────────────────────────────────────────────── tidyverse_conflicts() ──
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

``` r
daps1 <-
  daps() %>% 
  
  add_models(
    
    age(t) := lag(age) + 1,
    
    sbp(t) := list(
      lm(~ lag(sbp) + lag(dbp) + sex + race + age),
      lm(~ sex + race + age),
      lag(sbp),
      rnorm(1, 130, 15)
    ),
    
    dbp(t) := lm(~lag(sbp) + lag(dbp) + sex + race + age),
    dbp(t) := lm(~sex + race + age),
    
    diabetes(t) := glm(~sex + race + age + lag(sbp) + lag(sbp, 2) + lag(dbp) + lag(dbp, 2), family = binomial),
    diabetes(t) := glm(~sex + race + age + lag(sbp) + lag(dbp), family = binomial),
    diabetes(t) := glm(~sex + race + age, family = binomial),
    
    chd_risk(t) := list(
      clm(~ lag(sbp) + lag(dbp) + sex + race + age),
      multinom(~ lag(sbp) + lag(dbp) + sex + race + age)
    ),
    
    glucose(t) := lm(~sbp + dbp + diabetes + age + sex + race),
    glucose(t) := lm(~sbp + dbp + age + sex + race),
    glucose(t) := lm(~diabetes + age + sex + race),
    glucose(t) := lm(~age + sex + race),
    glucose(t) := lag(glucose)
    
  ) %>%
  
  add_metastate_model(
    # implement a "default_metastate", which is the first "state" unless the user
    # specifies
    # 
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
    # default_state = NULL
  )

daps1
#> $model_table
#> # A tibble: 17 x 4
#>    var      dynamic predictors model_quo 
#>  * <chr>    <lgl>   <list>     <list>    
#>  1 age      TRUE    <chr [1]>  <dps_nnf_>
#>  2 sbp      TRUE    <chr [5]>  <dps_ftt_>
#>  3 sbp      TRUE    <chr [3]>  <dps_ftt_>
#>  4 sbp      TRUE    <chr [1]>  <dps_nnf_>
#>  5 sbp      TRUE    <chr [0]>  <dps_nnf_>
#>  6 dbp      TRUE    <chr [5]>  <dps_ftt_>
#>  7 dbp      TRUE    <chr [3]>  <dps_ftt_>
#>  8 diabetes TRUE    <chr [5]>  <dps_ftt_>
#>  9 diabetes TRUE    <chr [5]>  <dps_ftt_>
#> 10 diabetes TRUE    <chr [3]>  <dps_ftt_>
#> 11 chd_risk TRUE    <chr [5]>  <dps_ftt_>
#> 12 chd_risk TRUE    <chr [5]>  <dps_ftt_>
#> 13 glucose  TRUE    <chr [6]>  <dps_ftt_>
#> 14 glucose  TRUE    <chr [5]>  <dps_ftt_>
#> 15 glucose  TRUE    <chr [4]>  <dps_ftt_>
#> 16 glucose  TRUE    <chr [3]>  <dps_ftt_>
#> 17 glucose  TRUE    <chr [1]>  <dps_nnf_>
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
#> iter  10 value 2.797158
#> iter  20 value 0.425688
#> iter  30 value 0.001153
#> final  value 0.000068 
#> converged

daps_trained
#> $model_table
#> # A tibble: 17 x 4
#>    var      dynamic predictors model_quo 
#>  * <chr>    <lgl>   <list>     <list>    
#>  1 age      TRUE    <chr [1]>  <dps_nnf_>
#>  2 sbp      TRUE    <chr [5]>  <dps_ftt_>
#>  3 sbp      TRUE    <chr [3]>  <dps_ftt_>
#>  4 sbp      TRUE    <chr [1]>  <dps_nnf_>
#>  5 sbp      TRUE    <chr [0]>  <dps_nnf_>
#>  6 dbp      TRUE    <chr [5]>  <dps_ftt_>
#>  7 dbp      TRUE    <chr [3]>  <dps_ftt_>
#>  8 diabetes TRUE    <chr [5]>  <dps_ftt_>
#>  9 diabetes TRUE    <chr [5]>  <dps_ftt_>
#> 10 diabetes TRUE    <chr [3]>  <dps_ftt_>
#> 11 chd_risk TRUE    <chr [5]>  <dps_ftt_>
#> 12 chd_risk TRUE    <chr [5]>  <dps_ftt_>
#> 13 glucose  TRUE    <chr [6]>  <dps_ftt_>
#> 14 glucose  TRUE    <chr [5]>  <dps_ftt_>
#> 15 glucose  TRUE    <chr [4]>  <dps_ftt_>
#> 16 glucose  TRUE    <chr [3]>  <dps_ftt_>
#> 17 glucose  TRUE    <chr [1]>  <dps_nnf_>
#> 
#> $metastate_model
#> # A tibble: 3 x 3
#>   metastate nodes     transitions
#> * <chr>     <list>    <list>     
#> 1 state1    <chr [5]> <quosure>  
#> 2 state2    <chr [5]> <quosure>  
#> 3 state3    <chr [6]> <quosure>  
#> 
#> $trained_fits
#> # A tibble: 17 x 3
#>    var      predictors model     
#>  * <chr>    <list>     <list>    
#>  1 age      <chr [1]>  <dps_nnf_>
#>  2 sbp      <chr [5]>  <lm>      
#>  3 sbp      <chr [3]>  <lm>      
#>  4 sbp      <chr [1]>  <dps_nnf_>
#>  5 sbp      <chr [0]>  <dps_nnf_>
#>  6 dbp      <chr [5]>  <lm>      
#>  7 dbp      <chr [3]>  <lm>      
#>  8 diabetes <chr [5]>  <glm>     
#>  9 diabetes <chr [5]>  <glm>     
#> 10 diabetes <chr [3]>  <glm>     
#> 11 chd_risk <chr [5]>  <clm>     
#> 12 chd_risk <chr [5]>  <multinom>
#> 13 glucose  <chr [6]>  <lm>      
#> 14 glucose  <chr [5]>  <lm>      
#> 15 glucose  <chr [4]>  <lm>      
#> 16 glucose  <chr [3]>  <lm>      
#> 17 glucose  <chr [1]>  <dps_nnf_>
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
  impute = FALSE,
  seed = 20200123
)
#> # A tibble: 33 x 11
#>       id sex   race      t metastate   sbp   dbp   age diabetes glucose chd_risk
#>    <int> <chr> <chr> <int> <chr>     <dbl> <dbl> <dbl> <lgl>      <dbl> <ord>   
#>  1     1 male  black     5 state3     134.  95.9    52 FALSE       325. elevated
#>  2     1 male  black     6 state3     137.  90.2    53 FALSE       523. elevated
#>  3     1 male  black     7 state3     139.  95.1    54 FALSE       409. elevated
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
#> # A tibble: 33 x 12
#>       id sex   race      t     h metastate   sbp   dbp   age diabetes glucose
#>    <int> <chr> <chr> <int> <int> <chr>     <dbl> <dbl> <dbl> <lgl>      <dbl>
#>  1     1 male  black     4     1 state3     134.  95.9    52 FALSE       325.
#>  2     1 male  black     4     2 state3     137.  90.2    53 FALSE       523.
#>  3     1 male  black     4     3 state3     139.  95.1    54 FALSE       409.
#>  4     1 male  black     3     1 state2     136.  85.3    51 TRUE         NA 
#>  5     1 male  black     3     2 state3     136.  92.2    52 FALSE       445.
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
    impute = TRUE
  )
#> # A tibble: 15 x 11
#>       id sex   race      t metastate   sbp   dbp   age diabetes glucose chd_risk
#>    <int> <chr> <chr> <int> <chr>     <dbl> <dbl> <dbl> <lgl>      <dbl> <ord>   
#>  1     1 male  black     5 state3     134.  95.9    52 FALSE       325. elevated
#>  2     1 male  black     6 state3     137.  90.2    53 FALSE       523. elevated
#>  3     1 male  black     7 state3     139.  95.1    54 FALSE       409. elevated
#>  4     2 fema… white     5 state3     145. 107.     67 FALSE       264. elevated
#>  5     2 fema… white     6 state3     146.  98.8    68 FALSE       512. elevated
#>  6     2 fema… white     7 state3     149. 103.     69 FALSE       411. elevated
#>  7     3 male  black     5 state1     109.  50.1    35 TRUE         NA  normal  
#>  8     3 male  black     6 state1     111.  56.9    36 TRUE         NA  normal  
#>  9     3 male  black     7 state2     112.  58.1    37 FALSE        NA  normal  
#> 10     4 male  white     5 state3     142. 138.     89 FALSE      -416. elevated
#> 11     4 male  white     6 state3     144. 140.     90 FALSE      -455. elevated
#> 12     4 male  white     7 state3     145. 142.     91 FALSE      -493. elevated
#> 13     6 fema… black     5 state3     231. 180.     90 FALSE      -924. elevated
#> 14     6 fema… black     6 state3     232. 183.     91 FALSE      -970. elevated
#> 15     6 fema… black     7 state3     234. 185.     92 FALSE     -1003. elevated

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

# TO DO list 2020-01-13

  - Revise the example model
      - Add a treatment variable (“none”, “antihypertensives”,
        “statins”, “both”)
      - Add incident outcomes within each observation year
  - Make a sufficiently deidentified data set, ideally using EHR
      - Apply for IRB to create a public data set
          - ADI, year, etc.
          - (talk to Amy Nowacki)
  - Incorporate “static” versus “dynamic” simuation. Put this switch in
    the initialize() function (mode = c(“static”, “dynamic”); add
    “horizon” parameter–NULL implies).
  - seed parameter in simulate(); t0 parameter in simulate

## Future packages

1.  Forecast accuracy - functions to join original data back up with
    simulated data in order to test the accuracy of the predictions
2.  Parameter uncertainty
3.  Adapting irregular temporal data to discrete time

<!-- end list -->

``` r
# nonparsnip <- 
#   lm(
#     sbp ~ lag(sbp) + lag(dbp) + sex + race + age, 
#     data = full_join(teststatic, testtemporal)
#   )
# 
# nonparsnip
# 
# parsnip <-
#   linear_reg() %>% 
#   set_engine(engine = "lm") %>% 
#   fit(
#     formula = sbp ~ lag(sbp) + lag(dbp) + sex + race + age,
#     data = full_join(teststatic, testtemporal)
#   )
# 
# parsnip
# 
# parsnip_xy <-
#   linear_reg() %>% 
#   set_engine(engine = "lm") %>% 
#   fit_xy(
#     x =
#       with(
#         full_join(teststatic, testtemporal, by = "id"),
#         cbind(lag(sbp), lag(dbp), sex, race, age)
#       ),
#     y = full_join(teststatic, testtemporal)$sbp
#   )
# 
# parsnip_xy
# 
# predict(nonparsnip, tibble(sbp = c(90, 95), dbp = c(55, 70), sex = c("male", "male"), race = c("white", "white"), age = c(50, 51)))
# 
# predict(parsnip, new_data = tibble(sbp = c(90, 95), dbp = c(55, 70), sex = c("male", "male"), race = c("white", "white"), age = c(50, 51)))
# 
# predict.model_fit(parsnip, new_data = tibble(sbp = c(90, 95), dbp = c(55, 70), sex = c("male", "male"), race = c("white", "white"), age = c(50, 51)))
```
