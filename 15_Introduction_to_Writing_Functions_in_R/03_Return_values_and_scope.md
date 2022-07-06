Return values and scope
================
Mohamad Osman
2022-07-06

# **Section 03: Return values and scope**

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ stringr 1.4.0
    ## ✔ tidyr   1.2.0     ✔ forcats 0.5.1
    ## ✔ readr   2.1.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(assertive)
```

    ## 
    ## Attaching package: 'assertive'

    ## The following objects are masked from 'package:purrr':
    ## 
    ##     is_atomic, is_character, is_double, is_empty, is_formula,
    ##     is_function, is_integer, is_list, is_logical, is_null, is_numeric,
    ##     is_vector

    ## The following object is masked from 'package:tibble':
    ## 
    ##     has_rownames

``` r
library(zeallot)
```

### **`01-Returning early`**

Sometimes, you don’t need to run through the whole body of a function to
get the answer. In that case you can return early from that function
using `return()`.

To check if `x` is divisible by `n`, you can use `is_divisible_by(x, n)`
from `assertive`.

Alternatively, use the modulo operator, `%%`. `x %% n` gives the
remainder when dividing `x` by `n`, so `x %% n == 0` determines whether
`x` is divisible by `n`. Try `1:10 %% 3 == 0` in the console.

To solve this exercise, you need to know that a leap year is every 400th
year (like the year 2000) or every 4th year that isn’t a century (like
1904 but not 1900 or 1905).

`assertive` is loaded.

-   Complete the definition of `is_leap_year()`, checking for the cases
    of `year` being divisible by 400, then 100, then 4, returning early
    from the function in each case.

``` r
is_leap_year <- function(year) {
  # If year is div. by 400 return TRUE
  if(year %% 400 == 0) {
    return(TRUE)
  }
  # If year is div. by 100 return FALSE
  if(year %% 100 == 0) {
    return(FALSE)
  }  
  # If year is div. by 4 return TRUE
  if(year %% 4 == 0) {
    return (TRUE)
  }
  
  
  # Otherwise return FALSE
  return(FALSE)

}
```

### **`02-Returning invisibly`**

When the main purpose of a function is to generate output, like drawing
a plot or printing something in the console, you may not want a return
value to be printed as well. In that case, the value should be
[**invisibly
returned**](https://www.rdocumentation.org/packages/base/topics/invisible).

-   Use the `cars` dataset and the formula interface to `plot()`, draw a
    scatter plot of `dist` versus `speed`.

``` r
hotdogs_path <- file.path("..","00_Datasets","cars.txt")

# Import hotdogs.txt: hotdogs
hotdogs <- read.delim(hotdogs_path, sep = "\t", header = TRUE,
                      stringsAsFactors = FALSE)

# Using cars, draw a scatter plot of dist vs. speed
plt_dist_vs_speed <- plot(dist ~ speed, data = cars)
```

![](03_Return_values_and_scope_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
# Oh no! The plot object is NULL
plt_dist_vs_speed
```

    ## NULL

-   Give `pipeable_plot()` `data` and `formula` arguments (in that
    order) and make it draw the plot, then invisibly return `data`.

-   Draw the scatter plot of dist vs. speed again by calling
    `pipeable_plot()`.

``` r
# Define a pipeable plot fn with data and formula args
pipeable_plot <- function(data, formula) {
  # Call plot() with the formula interface
  plot(formula, data)
  # Invisibly return the input dataset
  invisible(data)
}

# Draw the scatter plot of dist vs. speed again
plt_dist_vs_speed <- cars %>% 
  pipeable_plot(dist ~ speed)
```

![](03_Return_values_and_scope_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
# Now the plot object has a value
plt_dist_vs_speed
```

    ##    speed dist
    ## 1      4    2
    ## 2      4   10
    ## 3      7    4
    ## 4      7   22
    ## 5      8   16
    ## 6      9   10
    ## 7     10   18
    ## 8     10   26
    ## 9     10   34
    ## 10    11   17
    ## 11    11   28
    ## 12    12   14
    ## 13    12   20
    ## 14    12   24
    ## 15    12   28
    ## 16    13   26
    ## 17    13   34
    ## 18    13   34
    ## 19    13   46
    ## 20    14   26
    ## 21    14   36
    ## 22    14   60
    ## 23    14   80
    ## 24    15   20
    ## 25    15   26
    ## 26    15   54
    ## 27    16   32
    ## 28    16   40
    ## 29    17   32
    ## 30    17   40
    ## 31    17   50
    ## 32    18   42
    ## 33    18   56
    ## 34    18   76
    ## 35    18   84
    ## 36    19   36
    ## 37    19   46
    ## 38    19   68
    ## 39    20   32
    ## 40    20   48
    ## 41    20   52
    ## 42    20   56
    ## 43    20   64
    ## 44    22   66
    ## 45    23   54
    ## 46    24   70
    ## 47    24   92
    ## 48    24   93
    ## 49    24  120
    ## 50    25   85

### **`03-Returning many things`**

Functions can only return one value. If you want to return multiple
things, then you can store them all in a list.

If users want to have the list items as separate variables, they can
assign each list element to its own variable using `zeallot`’s
multi-assignment operator, `%<-%`.

`glance()`, `tidy()`, and `augment()` each take the model object as
their only argument.

The Poisson regression model of Snake River visits is available as
`model`. `broom` and `zeallot` are loaded.

``` r
file_path <- file.path("..", "00_Datasets", "snake_river_visits.rds")
snake_river_visits <- readRDS(file_path)

snake_river_explanatory <- snake_river_visits

# From previous step
run_poisson_regression <- function(data, formula) {
  glm(formula, data, family = poisson)
}

# Re-run the Poisson regression, using your function
model <- snake_river_visits %>%
  run_poisson_regression(n_visits ~ gender + income + travel)
```

-   Examine the structure of `model`.

-   Use `broom` functions on `model` to create a list containing the
    model-, coefficient-, and observation-level parts of `model`.

``` r
library(broom)

# Look at the structure of model (it's a mess!)
str(model)
```

    ## List of 31
    ##  $ coefficients     : Named num [1:7] 4.0864 0.374 -0.0199 -0.5807 -0.5782 ...
    ##   ..- attr(*, "names")= chr [1:7] "(Intercept)" "genderfemale" "income($25k,$55k]" "income($55k,$95k]" ...
    ##  $ residuals        : Named num [1:346] -0.535 -0.768 -0.944 -0.662 -0.767 ...
    ##   ..- attr(*, "names")= chr [1:346] "25" "26" "27" "29" ...
    ##  $ fitted.values    : Named num [1:346] 4.3 4.3 17.83 2.96 4.29 ...
    ##   ..- attr(*, "names")= chr [1:346] "25" "26" "27" "29" ...
    ##  $ effects          : Named num [1:346] -360 -29.2 20.3 -10 23.4 ...
    ##   ..- attr(*, "names")= chr [1:346] "(Intercept)" "genderfemale" "income($25k,$55k]" "income($55k,$95k]" ...
    ##  $ R                : num [1:7, 1:7] -97.4 0 0 0 0 ...
    ##   ..- attr(*, "dimnames")=List of 2
    ##   .. ..$ : chr [1:7] "(Intercept)" "genderfemale" "income($25k,$55k]" "income($55k,$95k]" ...
    ##   .. ..$ : chr [1:7] "(Intercept)" "genderfemale" "income($25k,$55k]" "income($55k,$95k]" ...
    ##  $ rank             : int 7
    ##  $ qr               :List of 5
    ##   ..$ qr   : num [1:346, 1:7] -97.3861 0.0213 0.0434 0.0177 0.0213 ...
    ##   .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. ..$ : chr [1:346] "25" "26" "27" "29" ...
    ##   .. .. ..$ : chr [1:7] "(Intercept)" "genderfemale" "income($25k,$55k]" "income($55k,$95k]" ...
    ##   ..$ rank : int 7
    ##   ..$ qraux: num [1:7] 1.02 1.02 1.04 1.01 1 ...
    ##   ..$ pivot: int [1:7] 1 2 3 4 5 6 7
    ##   ..$ tol  : num 1e-11
    ##   ..- attr(*, "class")= chr "qr"
    ##  $ family           :List of 12
    ##   ..$ family    : chr "poisson"
    ##   ..$ link      : chr "log"
    ##   ..$ linkfun   :function (mu)  
    ##   ..$ linkinv   :function (eta)  
    ##   ..$ variance  :function (mu)  
    ##   ..$ dev.resids:function (y, mu, wt)  
    ##   ..$ aic       :function (y, n, mu, wt, dev)  
    ##   ..$ mu.eta    :function (eta)  
    ##   ..$ initialize:  expression({  if (any(y < 0))  stop("negative values not allowed for the 'Poisson' family")  n <- rep.int(1, nobs| __truncated__
    ##   ..$ validmu   :function (mu)  
    ##   ..$ valideta  :function (eta)  
    ##   ..$ simulate  :function (object, nsim)  
    ##   ..- attr(*, "class")= chr "family"
    ##  $ linear.predictors: Named num [1:346] 1.46 1.46 2.88 1.09 1.46 ...
    ##   ..- attr(*, "names")= chr [1:346] "25" "26" "27" "29" ...
    ##  $ deviance         : num 11529
    ##  $ aic              : num 12864
    ##  $ null.deviance    : num 18850
    ##  $ iter             : int 6
    ##  $ weights          : Named num [1:346] 4.3 4.3 17.83 2.96 4.29 ...
    ##   ..- attr(*, "names")= chr [1:346] "25" "26" "27" "29" ...
    ##  $ prior.weights    : Named num [1:346] 1 1 1 1 1 1 1 1 1 1 ...
    ##   ..- attr(*, "names")= chr [1:346] "25" "26" "27" "29" ...
    ##  $ df.residual      : int 339
    ##  $ df.null          : int 345
    ##  $ y                : Named num [1:346] 2 1 1 1 1 1 80 104 55 350 ...
    ##   ..- attr(*, "names")= chr [1:346] "25" "26" "27" "29" ...
    ##  $ converged        : logi TRUE
    ##  $ boundary         : logi FALSE
    ##  $ model            :'data.frame':   346 obs. of  4 variables:
    ##   ..$ n_visits: num [1:346] 2 1 1 1 1 1 80 104 55 350 ...
    ##   ..$ gender  : Factor w/ 2 levels "male","female": 2 2 1 1 2 1 2 2 1 2 ...
    ##   ..$ income  : Factor w/ 4 levels "[$0,$25k]","($25k,$55k]",..: 4 4 4 4 3 1 1 4 2 2 ...
    ##   ..$ travel  : Factor w/ 3 levels "[0h,0.25h]","(0.25h,4h]",..: 3 3 2 3 3 1 1 1 2 1 ...
    ##   ..- attr(*, "terms")=Classes 'terms', 'formula'  language n_visits ~ gender + income + travel
    ##   .. .. ..- attr(*, "variables")= language list(n_visits, gender, income, travel)
    ##   .. .. ..- attr(*, "factors")= int [1:4, 1:3] 0 1 0 0 0 0 1 0 0 0 ...
    ##   .. .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. .. ..$ : chr [1:4] "n_visits" "gender" "income" "travel"
    ##   .. .. .. .. ..$ : chr [1:3] "gender" "income" "travel"
    ##   .. .. ..- attr(*, "term.labels")= chr [1:3] "gender" "income" "travel"
    ##   .. .. ..- attr(*, "order")= int [1:3] 1 1 1
    ##   .. .. ..- attr(*, "intercept")= int 1
    ##   .. .. ..- attr(*, "response")= int 1
    ##   .. .. ..- attr(*, ".Environment")=<environment: 0x000002b9be3b1a88> 
    ##   .. .. ..- attr(*, "predvars")= language list(n_visits, gender, income, travel)
    ##   .. .. ..- attr(*, "dataClasses")= Named chr [1:4] "numeric" "factor" "factor" "factor"
    ##   .. .. .. ..- attr(*, "names")= chr [1:4] "n_visits" "gender" "income" "travel"
    ##   ..- attr(*, "na.action")= 'omit' Named int [1:64] 1 2 3 4 5 6 7 8 9 10 ...
    ##   .. ..- attr(*, "names")= chr [1:64] "1" "2" "3" "4" ...
    ##  $ na.action        : 'omit' Named int [1:64] 1 2 3 4 5 6 7 8 9 10 ...
    ##   ..- attr(*, "names")= chr [1:64] "1" "2" "3" "4" ...
    ##  $ call             : language glm(formula = formula, family = poisson, data = data)
    ##  $ formula          :Class 'formula'  language n_visits ~ gender + income + travel
    ##   .. ..- attr(*, ".Environment")=<environment: 0x000002b9be3b1a88> 
    ##  $ terms            :Classes 'terms', 'formula'  language n_visits ~ gender + income + travel
    ##   .. ..- attr(*, "variables")= language list(n_visits, gender, income, travel)
    ##   .. ..- attr(*, "factors")= int [1:4, 1:3] 0 1 0 0 0 0 1 0 0 0 ...
    ##   .. .. ..- attr(*, "dimnames")=List of 2
    ##   .. .. .. ..$ : chr [1:4] "n_visits" "gender" "income" "travel"
    ##   .. .. .. ..$ : chr [1:3] "gender" "income" "travel"
    ##   .. ..- attr(*, "term.labels")= chr [1:3] "gender" "income" "travel"
    ##   .. ..- attr(*, "order")= int [1:3] 1 1 1
    ##   .. ..- attr(*, "intercept")= int 1
    ##   .. ..- attr(*, "response")= int 1
    ##   .. ..- attr(*, ".Environment")=<environment: 0x000002b9be3b1a88> 
    ##   .. ..- attr(*, "predvars")= language list(n_visits, gender, income, travel)
    ##   .. ..- attr(*, "dataClasses")= Named chr [1:4] "numeric" "factor" "factor" "factor"
    ##   .. .. ..- attr(*, "names")= chr [1:4] "n_visits" "gender" "income" "travel"
    ##  $ data             :'data.frame':   410 obs. of  4 variables:
    ##   ..$ n_visits: num [1:410] 0 0 0 0 0 0 0 0 0 0 ...
    ##   ..$ gender  : Factor w/ 2 levels "male","female": 1 1 1 2 1 2 2 2 1 1 ...
    ##   ..$ income  : Factor w/ 4 levels "[$0,$25k]","($25k,$55k]",..: 4 2 4 2 4 2 4 4 4 4 ...
    ##   ..$ travel  : Factor w/ 3 levels "[0h,0.25h]","(0.25h,4h]",..: NA NA NA NA NA NA NA NA NA NA ...
    ##  $ offset           : NULL
    ##  $ control          :List of 3
    ##   ..$ epsilon: num 1e-08
    ##   ..$ maxit  : num 25
    ##   ..$ trace  : logi FALSE
    ##  $ method           : chr "glm.fit"
    ##  $ contrasts        :List of 3
    ##   ..$ gender: chr "contr.treatment"
    ##   ..$ income: chr "contr.treatment"
    ##   ..$ travel: chr "contr.treatment"
    ##  $ xlevels          :List of 3
    ##   ..$ gender: chr [1:2] "male" "female"
    ##   ..$ income: chr [1:4] "[$0,$25k]" "($25k,$55k]" "($55k,$95k]" "($95k,$Inf)"
    ##   ..$ travel: chr [1:3] "[0h,0.25h]" "(0.25h,4h]" "(4h,Infh)"
    ##  - attr(*, "class")= chr [1:2] "glm" "lm"

``` r
# Use broom tools to get a list of 3 data frames
list(
  # Get model-level values
  model = glance(model),
  # Get coefficient-level values
  coefficients = tidy(model),
  # Get observation-level values
  observations = augment(model)
)
```

    ## $model
    ## # A tibble: 1 × 8
    ##   null.deviance df.null logLik    AIC    BIC deviance df.residual  nobs
    ##           <dbl>   <int>  <dbl>  <dbl>  <dbl>    <dbl>       <int> <int>
    ## 1        18850.     345 -6425. 12864. 12891.   11529.         339   346
    ## 
    ## $coefficients
    ## # A tibble: 7 × 5
    ##   term              estimate std.error statistic   p.value
    ##   <chr>                <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         4.09      0.0279   146.    0        
    ## 2 genderfemale        0.374     0.0212    17.6   2.18e- 69
    ## 3 income($25k,$55k]  -0.0199    0.0267    -0.746 4.56e-  1
    ## 4 income($55k,$95k]  -0.581     0.0343   -16.9   3.28e- 64
    ## 5 income($95k,$Inf)  -0.578     0.0310   -18.7   6.88e- 78
    ## 6 travel(0.25h,4h]   -0.627     0.0217   -28.8   5.40e-183
    ## 7 travel(4h,Infh)    -2.42      0.0492   -49.3   0        
    ## 
    ## $observations
    ## # A tibble: 346 × 11
    ##    .rownames n_visits gender income    travel .fitted  .resid .std.resid    .hat
    ##    <chr>        <dbl> <fct>  <fct>     <fct>    <dbl>   <dbl>      <dbl>   <dbl>
    ##  1 25               2 female ($95k,$I… (4h,I…    1.46  -1.24      -1.25  0.0109 
    ##  2 26               1 female ($95k,$I… (4h,I…    1.46  -1.92      -1.93  0.0109 
    ##  3 27               1 male   ($95k,$I… (0.25…    2.88  -5.28      -5.32  0.0129 
    ##  4 29               1 male   ($95k,$I… (4h,I…    1.09  -1.32      -1.33  0.00711
    ##  5 30               1 female ($55k,$9… (4h,I…    1.46  -1.92      -1.93  0.0121 
    ##  6 31               1 male   [$0,$25k] [0h,0…    4.09 -10.4      -10.7   0.0465 
    ##  7 33              80 female [$0,$25k] [0h,0…    4.46  -0.710     -0.728 0.0479 
    ##  8 34             104 female ($95k,$I… [0h,0…    3.88   6.90       7.02  0.0332 
    ##  9 35              55 male   ($25k,$5… (0.25…    3.44   3.85       3.88  0.0153 
    ## 10 36             350 female ($25k,$5… [0h,0…    4.44  21.5       21.9   0.0360 
    ## # … with 336 more rows, and 2 more variables: .sigma <dbl>, .cooksd <dbl>

-   Wrap the code into a function, `groom_model()`, that accepts `model`
    as its only argument.

``` r
# Wrap this code into a function, groom_model
groom_model <- function(model){
  list(
    model = glance(model),
    coefficients = tidy(model),
    observations = augment(model)
  )
}
```

-   Call `groom_model()` on `model`, multi-assigning the result to three
    variables at once: `mdl`, `cff`, and `obs`.

``` r
library("magrittr")
```

    ## 
    ## Attaching package: 'magrittr'

    ## The following objects are masked from 'package:assertive':
    ## 
    ##     is_greater_than, is_less_than

    ## The following object is masked from 'package:purrr':
    ## 
    ##     set_names

    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract

``` r
# From previous step
groom_model <- function(model) {
  list(
    model = glance(model),
    coefficients = tidy(model),
    observations = augment(model)
  )
}

# Call groom_model on model, assigning to 3 variables
groom_model(model)
```

    ## $model
    ## # A tibble: 1 × 8
    ##   null.deviance df.null logLik    AIC    BIC deviance df.residual  nobs
    ##           <dbl>   <int>  <dbl>  <dbl>  <dbl>    <dbl>       <int> <int>
    ## 1        18850.     345 -6425. 12864. 12891.   11529.         339   346
    ## 
    ## $coefficients
    ## # A tibble: 7 × 5
    ##   term              estimate std.error statistic   p.value
    ##   <chr>                <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         4.09      0.0279   146.    0        
    ## 2 genderfemale        0.374     0.0212    17.6   2.18e- 69
    ## 3 income($25k,$55k]  -0.0199    0.0267    -0.746 4.56e-  1
    ## 4 income($55k,$95k]  -0.581     0.0343   -16.9   3.28e- 64
    ## 5 income($95k,$Inf)  -0.578     0.0310   -18.7   6.88e- 78
    ## 6 travel(0.25h,4h]   -0.627     0.0217   -28.8   5.40e-183
    ## 7 travel(4h,Infh)    -2.42      0.0492   -49.3   0        
    ## 
    ## $observations
    ## # A tibble: 346 × 11
    ##    .rownames n_visits gender income    travel .fitted  .resid .std.resid    .hat
    ##    <chr>        <dbl> <fct>  <fct>     <fct>    <dbl>   <dbl>      <dbl>   <dbl>
    ##  1 25               2 female ($95k,$I… (4h,I…    1.46  -1.24      -1.25  0.0109 
    ##  2 26               1 female ($95k,$I… (4h,I…    1.46  -1.92      -1.93  0.0109 
    ##  3 27               1 male   ($95k,$I… (0.25…    2.88  -5.28      -5.32  0.0129 
    ##  4 29               1 male   ($95k,$I… (4h,I…    1.09  -1.32      -1.33  0.00711
    ##  5 30               1 female ($55k,$9… (4h,I…    1.46  -1.92      -1.93  0.0121 
    ##  6 31               1 male   [$0,$25k] [0h,0…    4.09 -10.4      -10.7   0.0465 
    ##  7 33              80 female [$0,$25k] [0h,0…    4.46  -0.710     -0.728 0.0479 
    ##  8 34             104 female ($95k,$I… [0h,0…    3.88   6.90       7.02  0.0332 
    ##  9 35              55 male   ($25k,$5… (0.25…    3.44   3.85       3.88  0.0153 
    ## 10 36             350 female ($25k,$5… [0h,0…    4.44  21.5       21.9   0.0360 
    ## # … with 336 more rows, and 2 more variables: .sigma <dbl>, .cooksd <dbl>

``` r
# See these individual variables
c(mdl, cff, obs)  %<-% groom_model(model)
```

### **`04-Returning metadata`**

In that case, you can store the metadata in attributes. Recall the
syntax for assigning attributes is as follows.

    attr(object, "attribute_name") <- attribute_value

-   Update `pipeable_plot()` so the result has an attribute named
    `"formula"` with the value of `formula`.

-   *`plt_dist_vs_speed`, that you previously created, is shown.*
    Examine its updated structure.

``` r
pipeable_plot <- function(data, formula) {
  plot(formula, data)
  # Add a "formula" attribute to data
  attr(data, "formula") <- formula
  invisible(data)
}

# From previous exercise
plt_dist_vs_speed <- cars %>% 
  pipeable_plot(dist ~ speed)
```

![](03_Return_values_and_scope_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
# Examine the structure of the result
str(plt_dist_vs_speed)
```

    ## 'data.frame':    50 obs. of  2 variables:
    ##  $ speed: num  4 4 7 7 8 9 10 10 10 11 ...
    ##  $ dist : num  2 10 4 22 16 10 18 26 34 17 ...
    ##  - attr(*, "formula")=Class 'formula'  language dist ~ speed
    ##   .. ..- attr(*, ".Environment")=<environment: 0x000002b9c6653f90>

### **`05-Creating and exploring environments`**

Environments are used to store other variables. Mostly, you can think of
them as lists, but there’s an important extra property that is relevant
to writing functions. Every environment has a **parent environment**
(except the **empty environment**, at the root of the environment tree).
This determines which variables R know about at different places in your
code.

Facts about the Republic of South Africa are contained in `capitals`,
`national_parks`, and `population`.

``` r
city <- c("Cape Town", "Bloemfontein", "Pretoria")
type_of_capital <- c("Legislative", "Judicial", "Administrative")

capitals <- data.frame(city, type_of_capital)
national_parks <- 
  
national_parks <- c("Addo Elephant National Park" ,"Agulhas National Park" ,"Ai-Ais/Richtersveld Transfrontier Park","Augrabies Falls National Park","Bontebok National Park" ,"Camdeboo National Park","Golden Gate Highlands National Park","Hluhluwe–Imfolozi Park","Karoo National Park","Kgalagadi Transfrontier Park" ,"Knysna National Lake Area","Kruger National Park","Mapungubwe National Park"    ,"Marakele National Park","Mokala National Park","Mountain Zebra National Park"   ,"Namaqua National Park","Table Mountain National Park","Tankwa Karoo National Park","Tsitsikamma National Park","West Coast National Park","Wilderness National Park")
pop_vector <- c(40583573, 44819778, 47390900, 51770560, 55908900)
population  <- ts(pop_vector, start=c(1996), end=c(2016), frequency=0.2)
```

-   Create `rsa_lst`, a named list from `capitals`, `national_parks`,
    and `population`. Use those values as the names.

-   List the structure of each element of `rsa_lst` using `ls.str()`.

``` r
# Add capitals, national_parks, & population to a named list
rsa_lst <- list(
  capitals = capitals,
  national_parks = national_parks,
  population = population
)

# List the structure of each element of rsa_lst
ls.str(rsa_lst)
```

    ## capitals : 'data.frame': 3 obs. of  2 variables:
    ##  $ city           : chr  "Cape Town" "Bloemfontein" "Pretoria"
    ##  $ type_of_capital: chr  "Legislative" "Judicial" "Administrative"
    ## national_parks :  chr [1:22] "Addo Elephant National Park" "Agulhas National Park" ...
    ## population :  Time-Series [1:5] from 1996 to 2016: 40583573 44819778 47390900 51770560 55908900

-   Convert the list to an environment, `rsa_env`, using `list2env()`.

-   List the structure of each element of `rsa_env`

``` r
# From previous step
rsa_lst <- list(
  capitals = capitals,
  national_parks = national_parks,
  population = population
)

# Convert the list to an environment
rsa_env <- list2env(rsa_lst)

# List the structure of each variable
ls.str(rsa_env)
```

    ## capitals : 'data.frame': 3 obs. of  2 variables:
    ##  $ city           : chr  "Cape Town" "Bloemfontein" "Pretoria"
    ##  $ type_of_capital: chr  "Legislative" "Judicial" "Administrative"
    ## national_parks :  chr [1:22] "Addo Elephant National Park" "Agulhas National Park" ...
    ## population :  Time-Series [1:5] from 1996 to 2016: 40583573 44819778 47390900 51770560 55908900

-   Find the parent environment of `rsa_env` and print its name.

``` r
# From previous steps
rsa_lst <- list(
  capitals = capitals,
  national_parks = national_parks,
  population = population
)
rsa_env <- list2env(rsa_lst)

# Find the parent environment of rsa_env
parent <- parent.env(rsa_env)

# Print its name
environmentName(parent)
```

    ## [1] "R_GlobalEnv"

### **`06-Do variables exist?`**

If R cannot find a variable in the current environment, it will look in
the parent environment, then the grandparent environment, and so on
until it finds it.

`rsa_env` has been modified so it includes `capitals` and
`national_parks`, but not `population`.

``` r
rsa_lst <- list(
  capitals = capitals,
  national_parks = national_parks
)
rsa_env <- list2env(rsa_lst)
```

-   Check if `population` exists in `rsa_env`, using default inheritance
    rules.

-   Check if `population` exists in `rsa_env`, ignoring inheritance.

``` r
# Compare the contents of the global environment and rsa_env
#ls.str(globalenv())
#ls.str(rsa_env)

# Does population exist in rsa_env?
exists("population", envir = rsa_env)
```

    ## [1] TRUE

``` r
# Does population exist in rsa_env, ignoring inheritance?
exists("population", envir = rsa_env, inherits = FALSE)
```

    ## [1] FALSE

Elegant existence checking! R searches for variables in all the parent
environments, unless you explicitly tell it not to.

### `The End`

  

  

  
