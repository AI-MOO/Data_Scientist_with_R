How to write a function
================
Mohamad Osman
2022-07-05

# **Section 01: How to write a function**

### **`01-Calling functions`**

*The final line calculates the median number of gold medals each country
won.*

Rewrite the call to
[**`median()`**](https://www.rdocumentation.org/packages/stats/topics/median),
following best practices.

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
names <- c("USA", "GBR", "CHN", "RUS", "GER", "JPN", "FRA", "KOR", "ITA", "AUS", "NED", "HUN", "BRA", "ESP", "KEN", "JAM", "CRO", "CUB", "NZL", "CAN", "UZB", "KAZ", "COL", "SUI", "IRI", "GRE", "ARG", "DEN", "SWE", "RSA", "UKR", "SRB", "POL", "PRK", "BEL", "THA", "SVK", "GEO", "AZE", "BLR", "TUR", "ARM", "CZE", "ETH", "SLO", "INA", "ROU", "BRN", "VIE", "TPE", "BAH", "IOA", "CIV", "FIJ", "JOR", "KOS", "PUR", "SIN", "TJK", "MAS", "MEX", "VEN", "ALG", "IRL", "LTU", "BUL", "IND", "MGL", "BDI", "GRN", "NIG", "PHI", "QAT", "NOR", "EGY", "TUN", "ISR", "AUT", "DOM", "EST", "FIN", "MAR", "NGR", "POR", "TTO", "UAE", "IOC")

gold_medals <- c(46, 27, 26, 19, 17, 12, 10, 9, 8, 8, 8, 8, 7, 7, 6, 6, 5, 5, 4, 4, 4, 3,3, 3, 3, 3, 3, 2, 2,  2, 2, 2, 2, 2, 2, 2, 2, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, NA)
#names(gold_medals) <- attr

names(gold_medals) <- names

typeof(gold_medals)
```

    ## [1] "double"

``` r
# Look at the gold medals data
gold_medals
```

    ## USA GBR CHN RUS GER JPN FRA KOR ITA AUS NED HUN BRA ESP KEN JAM CRO CUB NZL CAN 
    ##  46  27  26  19  17  12  10   9   8   8   8   8   7   7   6   6   5   5   4   4 
    ## UZB KAZ COL SUI IRI GRE ARG DEN SWE RSA UKR SRB POL PRK BEL THA SVK GEO AZE BLR 
    ##   4   3   3   3   3   3   3   2   2   2   2   2   2   2   2   2   2   2   1   1 
    ## TUR ARM CZE ETH SLO INA ROU BRN VIE TPE BAH IOA CIV FIJ JOR KOS PUR SIN TJK MAS 
    ##   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   1   0 
    ## MEX VEN ALG IRL LTU BUL IND MGL BDI GRN NIG PHI QAT NOR EGY TUN ISR AUT DOM EST 
    ##   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0 
    ## FIN MAR NGR POR TTO UAE IOC 
    ##   0   0   0   0   0   0  NA

``` r
# Note the arguments to median()
args(median)
```

    ## function (x, na.rm = FALSE, ...) 
    ## NULL

``` r
# Rewrite this function call, following best practices
median(gold_medals, na.rm = TRUE)
```

    ## [1] 1

*The final line calculates each country’s ranking by number of gold
medals. It uses negative `gold_medals` so that the country with the most
medals will have 1st place: the largest positive value in `gold_medals`
is the smallest (“most negative”) value in `-gold_medals`.*

Rewrite the call to
[**`rank()`**](https://www.rdocumentation.org/packages/base/topics/rank),
following best practices.

``` r
# Note the arguments to rank()
args(rank)
```

    ## function (x, na.last = TRUE, ties.method = c("average", "first", 
    ##     "last", "random", "max", "min")) 
    ## NULL

``` r
# Rewrite this function call, following best practices
rank(-gold_medals, na.last = "keep", ties.method = "min")
```

    ## USA GBR CHN RUS GER JPN FRA KOR ITA AUS NED HUN BRA ESP KEN JAM CRO CUB NZL CAN 
    ##   1   2   3   4   5   6   7   8   9   9   9   9  13  13  15  15  17  17  19  19 
    ## UZB KAZ COL SUI IRI GRE ARG DEN SWE RSA UKR SRB POL PRK BEL THA SVK GEO AZE BLR 
    ##  19  22  22  22  22  22  22  28  28  28  28  28  28  28  28  28  28  28  39  39 
    ## TUR ARM CZE ETH SLO INA ROU BRN VIE TPE BAH IOA CIV FIJ JOR KOS PUR SIN TJK MAS 
    ##  39  39  39  39  39  39  39  39  39  39  39  39  39  39  39  39  39  39  39  60 
    ## MEX VEN ALG IRL LTU BUL IND MGL BDI GRN NIG PHI QAT NOR EGY TUN ISR AUT DOM EST 
    ##  60  60  60  60  60  60  60  60  60  60  60  60  60  60  60  60  60  60  60  60 
    ## FIN MAR NGR POR TTO UAE IOC 
    ##  60  60  60  60  60  60  NA

### **`02-Your first function: tossing a coin`**

Your first function: tossing a coin

``` r
coin_sides <- c("head", "tail")

# Sample from coin_sides once
sample(coin_sides, size = 1)
```

    ## [1] "tail"

-   Write a template for your function, naming it `toss_coin`. The
    function should take no arguments. Don’t include the body of the
    function yet.

-   Copy your script, and paste it into the function body.

-   Call your function.

``` r
# Write a template for your function, toss_coin()
toss_coin <- function() {
  # (Leave the contents of the body for later)
# Add punctuation to finish the body
  sample(coin_sides, size = 1)
} 

toss_coin()
```

    ## [1] "tail"

### **`03-Inputs to functions`**

-   Sample from `coin_sides` `n_flips` times with replacement.

``` r
coin_sides <- c("head", "tail")
n_flips <- 10

# Sample from coin_sides n_flips times with replacement
sample(coin_sides, size = n_flips, replace =TRUE)
```

    ##  [1] "tail" "head" "head" "tail" "head" "head" "tail" "head" "head" "head"

-   Update the definition of `toss_coin()` to accept a single argument,
    `n_flips`. The function should sample `coin_sides` `n_flips` times
    with replacement. *Remember to change the signature and the body.*

-   Generate 10 coin flips.

``` r
# Update the function to return n coin tosses
toss_coin <- function(n_flips = 1) {
  coin_sides <- c("head", "tail")
  sample(coin_sides, n_flips, replace = TRUE)
}

# Generate 10 coin tosses
toss_coin(n_flips = 10)
```

    ##  [1] "head" "head" "head" "tail" "tail" "tail" "head" "tail" "head" "tail"

### **`04-Multiple inputs to functions`**

-   Bias the coin by weighting the sampling. Specify the `prob` argument
    so that heads are sampled with probability `p_head` (and tails are
    sampled with probability `1 - p_head`).

``` r
coin_sides <- c("head", "tail")
n_flips <- 10
p_head <- 0.8

# Define a vector of weights
weights <- c(p_head, 1 - p_head)

# Update so that heads are sampled with prob p_head
sample(coin_sides, n_flips, replace = TRUE, prob = weights)
```

    ##  [1] "head" "head" "head" "tail" "head" "head" "head" "head" "head" "head"

-   Update the definition of `toss_coin()` so it accepts an argument,
    `p_head`, and weights the samples using the code you wrote in the
    previous step.

-   Generate 10 coin tosses with an 80% chance of each head.

``` r
# Update the function so heads have probability p_head
toss_coin <- function(n_flips, p_head) {
  coin_sides <- c("head", "tail")
  # Define a vector of weights
  weights <- c(p_head, 1-p_head)
  # Modify the sampling to be weighted
  sample(coin_sides, n_flips, replace = TRUE, prob = weights)
}

# Generate 10 coin tosses
toss_coin(10, 0.8)
```

    ##  [1] "tail" "head" "head" "head" "head" "head" "head" "head" "head" "head"

### **`05-Renaming GLM`**

-   Run a generalized linear regression by calling `glm()`. Model
    `n_visits` vs. `gender`, `income`, and `travel` on the
    `snake_river_visits` dataset, setting the `family` to `poisson`.

``` r
# Load data
file_path <- file.path("..", "00_Datasets", "snake_river_visits.rds")
snake_river_visits <- readRDS(file_path)

# Run a generalized linear regression 
glm(
  # Model no. of visits vs. gender, income, travel
  n_visits ~ gender + income + travel, 
  # Use the snake_river_visits dataset
  data = snake_river_visits, 
  # Make it a Poisson regression
  family = poisson
)
```

    ## 
    ## Call:  glm(formula = n_visits ~ gender + income + travel, family = poisson, 
    ##     data = snake_river_visits)
    ## 
    ## Coefficients:
    ##       (Intercept)       genderfemale  income($25k,$55k]  income($55k,$95k]  
    ##            4.0864             0.3740            -0.0199            -0.5807  
    ## income($95k,$Inf)   travel(0.25h,4h]    travel(4h,Infh)  
    ##           -0.5782            -0.6271            -2.4230  
    ## 
    ## Degrees of Freedom: 345 Total (i.e. Null);  339 Residual
    ##   (64 observations deleted due to missingness)
    ## Null Deviance:       18850 
    ## Residual Deviance: 11530     AIC: 12860

-   Define a function, `run_poisson_regression()`, to run a Poisson
    regression. This should take two arguments: `data` and `formula`,
    and call `glm()`, passing those arguments and setting `family` to
    `poisson`.

``` r
# Write a function to run a Poisson regression
run_poisson_regression <- function(data, formula) {

    glm(formula, data, family = poisson)
}
```

-   Recreate the Poisson regression model from the first step, this time
    by calling your `run_poisson_regression()` function.

``` r
snake_river_explanatory <- snake_river_visits

# From previous step
run_poisson_regression <- function(data, formula) {
  glm(formula, data, family = poisson)
}

# Re-run the Poisson regression, using your function
model <- snake_river_visits %>%
  run_poisson_regression(n_visits ~ gender + income + travel)

# Run this to see the predictions
snake_river_explanatory %>%
  mutate(predicted_n_visits = predict(model, ., type = "response"))%>%
  arrange(desc(predicted_n_visits))
```

    ##     n_visits gender      income     travel predicted_n_visits
    ## 1         80 female   [$0,$25k] [0h,0.25h]          86.518598
    ## 2         35 female   [$0,$25k] [0h,0.25h]          86.518598
    ## 3         50 female   [$0,$25k] [0h,0.25h]          86.518598
    ## 4        125 female   [$0,$25k] [0h,0.25h]          86.518598
    ## 5         24 female   [$0,$25k] [0h,0.25h]          86.518598
    ## 6        100 female   [$0,$25k] [0h,0.25h]          86.518598
    ## 7         20 female   [$0,$25k] [0h,0.25h]          86.518598
    ## 8         50 female   [$0,$25k] [0h,0.25h]          86.518598
    ## 9         75 female   [$0,$25k] [0h,0.25h]          86.518598
    ## 10        15 female   [$0,$25k] [0h,0.25h]          86.518598
    ## 11        20 female   [$0,$25k] [0h,0.25h]          86.518598
    ## 12       350 female ($25k,$55k] [0h,0.25h]          84.813684
    ## 13        20 female ($25k,$55k] [0h,0.25h]          84.813684
    ## 14         9 female ($25k,$55k] [0h,0.25h]          84.813684
    ## 15        10 female ($25k,$55k] [0h,0.25h]          84.813684
    ## 16       120 female ($25k,$55k] [0h,0.25h]          84.813684
    ## 17       160 female ($25k,$55k] [0h,0.25h]          84.813684
    ## 18        96 female ($25k,$55k] [0h,0.25h]          84.813684
    ## 19        30 female ($25k,$55k] [0h,0.25h]          84.813684
    ## 20        10 female ($25k,$55k] [0h,0.25h]          84.813684
    ## 21        35 female ($25k,$55k] [0h,0.25h]          84.813684
    ## 22       300 female ($25k,$55k] [0h,0.25h]          84.813684
    ## 23       150 female ($25k,$55k] [0h,0.25h]          84.813684
    ## 24        50 female ($25k,$55k] [0h,0.25h]          84.813684
    ## 25        12 female ($25k,$55k] [0h,0.25h]          84.813684
    ## 26         1   male   [$0,$25k] [0h,0.25h]          59.524843
    ## 27       100   male   [$0,$25k] [0h,0.25h]          59.524843
    ## 28       200   male   [$0,$25k] [0h,0.25h]          59.524843
    ## 29        30   male   [$0,$25k] [0h,0.25h]          59.524843
    ## 30        20   male   [$0,$25k] [0h,0.25h]          59.524843
    ## 31       200   male ($25k,$55k] [0h,0.25h]          58.351861
    ## 32        15   male ($25k,$55k] [0h,0.25h]          58.351861
    ## 33        30   male ($25k,$55k] [0h,0.25h]          58.351861
    ## 34        75   male ($25k,$55k] [0h,0.25h]          58.351861
    ## 35         8   male ($25k,$55k] [0h,0.25h]          58.351861
    ## 36       200   male ($25k,$55k] [0h,0.25h]          58.351861
    ## 37         6   male ($25k,$55k] [0h,0.25h]          58.351861
    ## 38       100   male ($25k,$55k] [0h,0.25h]          58.351861
    ## 39        17   male ($25k,$55k] [0h,0.25h]          58.351861
    ## 40        60   male ($25k,$55k] [0h,0.25h]          58.351861
    ## 41        20   male ($25k,$55k] [0h,0.25h]          58.351861
    ## 42         2   male ($25k,$55k] [0h,0.25h]          58.351861
    ## 43        75   male ($25k,$55k] [0h,0.25h]          58.351861
    ## 44        25   male ($25k,$55k] [0h,0.25h]          58.351861
    ## 45        50   male ($25k,$55k] [0h,0.25h]          58.351861
    ## 46       104 female ($95k,$Inf) [0h,0.25h]          48.526883
    ## 47        60 female ($95k,$Inf) [0h,0.25h]          48.526883
    ## 48       250 female ($95k,$Inf) [0h,0.25h]          48.526883
    ## 49        50 female ($95k,$Inf) [0h,0.25h]          48.526883
    ## 50         6 female ($95k,$Inf) [0h,0.25h]          48.526883
    ## 51       120 female ($95k,$Inf) [0h,0.25h]          48.526883
    ## 52        25 female ($95k,$Inf) [0h,0.25h]          48.526883
    ## 53        24 female ($95k,$Inf) [0h,0.25h]          48.526883
    ## 54        20 female ($95k,$Inf) [0h,0.25h]          48.526883
    ## 55        30 female ($95k,$Inf) [0h,0.25h]          48.526883
    ## 56        50 female ($95k,$Inf) [0h,0.25h]          48.526883
    ## 57         2 female ($95k,$Inf) [0h,0.25h]          48.526883
    ## 58       150 female ($95k,$Inf) [0h,0.25h]          48.526883
    ## 59         2 female ($55k,$95k] [0h,0.25h]          48.408009
    ## 60        12 female ($55k,$95k] [0h,0.25h]          48.408009
    ## 61        52 female ($55k,$95k] [0h,0.25h]          48.408009
    ## 62       100 female ($55k,$95k] [0h,0.25h]          48.408009
    ## 63        30 female ($55k,$95k] [0h,0.25h]          48.408009
    ## 64        12 female ($55k,$95k] [0h,0.25h]          48.408009
    ## 65        70 female ($55k,$95k] [0h,0.25h]          48.408009
    ## 66       100 female ($55k,$95k] [0h,0.25h]          48.408009
    ## 67        20 female ($55k,$95k] [0h,0.25h]          48.408009
    ## 68        30 female ($55k,$95k] [0h,0.25h]          48.408009
    ## 69       260 female   [$0,$25k] (0.25h,4h]          46.212343
    ## 70         6 female   [$0,$25k] (0.25h,4h]          46.212343
    ## 71        12 female   [$0,$25k] (0.25h,4h]          46.212343
    ## 72        50 female   [$0,$25k] (0.25h,4h]          46.212343
    ## 73        30 female   [$0,$25k] (0.25h,4h]          46.212343
    ## 74        50 female   [$0,$25k] (0.25h,4h]          46.212343
    ## 75        50 female   [$0,$25k] (0.25h,4h]          46.212343
    ## 76       150 female   [$0,$25k] (0.25h,4h]          46.212343
    ## 77        25 female   [$0,$25k] (0.25h,4h]          46.212343
    ## 78       150 female   [$0,$25k] (0.25h,4h]          46.212343
    ## 79         4 female   [$0,$25k] (0.25h,4h]          46.212343
    ## 80        30 female   [$0,$25k] (0.25h,4h]          46.212343
    ## 81        50 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 82        25 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 83        15 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 84        50 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 85        50 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 86        12 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 87        50 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 88         1 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 89         1 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 90         3 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 91         5 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 92        60 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 93        10 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 94         1 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 95       120 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 96        15 female ($25k,$55k] (0.25h,4h]          45.301694
    ## 97        40   male ($95k,$Inf) [0h,0.25h]          33.386522
    ## 98        15   male ($95k,$Inf) [0h,0.25h]          33.386522
    ## 99         2   male ($95k,$Inf) [0h,0.25h]          33.386522
    ## 100       40   male ($95k,$Inf) [0h,0.25h]          33.386522
    ## 101       30   male ($95k,$Inf) [0h,0.25h]          33.386522
    ## 102        5   male ($95k,$Inf) [0h,0.25h]          33.386522
    ## 103        0   male ($95k,$Inf) [0h,0.25h]          33.386522
    ## 104        3   male ($95k,$Inf) [0h,0.25h]          33.386522
    ## 105       40   male ($95k,$Inf) [0h,0.25h]          33.386522
    ## 106        2   male ($95k,$Inf) [0h,0.25h]          33.386522
    ## 107        6   male ($95k,$Inf) [0h,0.25h]          33.386522
    ## 108        6   male ($95k,$Inf) [0h,0.25h]          33.386522
    ## 109       40   male ($95k,$Inf) [0h,0.25h]          33.386522
    ## 110      250   male ($55k,$95k] [0h,0.25h]          33.304737
    ## 111       15   male ($55k,$95k] [0h,0.25h]          33.304737
    ## 112       13   male ($55k,$95k] [0h,0.25h]          33.304737
    ## 113        1   male ($55k,$95k] [0h,0.25h]          33.304737
    ## 114       40   male ($55k,$95k] [0h,0.25h]          33.304737
    ## 115       15   male ($55k,$95k] [0h,0.25h]          33.304737
    ## 116       70   male ($55k,$95k] [0h,0.25h]          33.304737
    ## 117        6   male ($55k,$95k] [0h,0.25h]          33.304737
    ## 118       50   male ($55k,$95k] [0h,0.25h]          33.304737
    ## 119        6   male ($55k,$95k] [0h,0.25h]          33.304737
    ## 120      114   male   [$0,$25k] (0.25h,4h]          31.794117
    ## 121       72   male   [$0,$25k] (0.25h,4h]          31.794117
    ## 122       15   male   [$0,$25k] (0.25h,4h]          31.794117
    ## 123        2   male   [$0,$25k] (0.25h,4h]          31.794117
    ## 124        2   male   [$0,$25k] (0.25h,4h]          31.794117
    ## 125       30   male   [$0,$25k] (0.25h,4h]          31.794117
    ## 126       10   male   [$0,$25k] (0.25h,4h]          31.794117
    ## 127       90   male   [$0,$25k] (0.25h,4h]          31.794117
    ## 128        6   male   [$0,$25k] (0.25h,4h]          31.794117
    ## 129        4   male   [$0,$25k] (0.25h,4h]          31.794117
    ## 130       30   male   [$0,$25k] (0.25h,4h]          31.794117
    ## 131       10   male   [$0,$25k] (0.25h,4h]          31.794117
    ## 132       60   male   [$0,$25k] (0.25h,4h]          31.794117
    ## 133       76   male   [$0,$25k] (0.25h,4h]          31.794117
    ## 134       55   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 135        4   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 136       14   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 137       30   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 138        1   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 139        2   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 140        7   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 141       50   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 142       50   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 143      100   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 144       10   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 145       30   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 146       26   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 147       20   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 148       20   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 149        0   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 150        4   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 151       52   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 152       30   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 153        1   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 154       12   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 155       20   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 156        3   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 157       60   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 158        1   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 159        1   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 160      150   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 161       26   male ($25k,$55k] (0.25h,4h]          31.167590
    ## 162       20 female ($95k,$Inf) (0.25h,4h]          25.919756
    ## 163        4 female ($95k,$Inf) (0.25h,4h]          25.919756
    ## 164      100 female ($95k,$Inf) (0.25h,4h]          25.919756
    ## 165       20 female ($95k,$Inf) (0.25h,4h]          25.919756
    ## 166        3 female ($95k,$Inf) (0.25h,4h]          25.919756
    ## 167       14 female ($95k,$Inf) (0.25h,4h]          25.919756
    ## 168        3 female ($95k,$Inf) (0.25h,4h]          25.919756
    ## 169        2 female ($95k,$Inf) (0.25h,4h]          25.919756
    ## 170        6 female ($95k,$Inf) (0.25h,4h]          25.919756
    ## 171        5 female ($55k,$95k] (0.25h,4h]          25.856261
    ## 172        1 female ($55k,$95k] (0.25h,4h]          25.856261
    ## 173        1 female ($55k,$95k] (0.25h,4h]          25.856261
    ## 174       30 female ($55k,$95k] (0.25h,4h]          25.856261
    ## 175       25 female ($55k,$95k] (0.25h,4h]          25.856261
    ## 176       30 female ($55k,$95k] (0.25h,4h]          25.856261
    ## 177        1   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 178      100   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 179       25   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 180       15   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 181        3   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 182       25   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 183        1   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 184        3   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 185        6   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 186       10   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 187        6   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 188      100   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 189       10   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 190       12   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 191       70   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 192       20   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 193       20   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 194       50   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 195        1   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 196        1   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 197        5   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 198        2   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 199        3   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 200        4   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 201       50   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 202        1   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 203        1   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 204       30   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 205       30   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 206        2   male ($95k,$Inf) (0.25h,4h]          17.832806
    ## 207       30   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 208       30   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 209        0   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 210        1   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 211       52   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 212       17   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 213        1   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 214        1   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 215       60   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 216        7   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 217       24   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 218        6   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 219        0   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 220       25   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 221        3   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 222       75   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 223       35   male ($55k,$95k] (0.25h,4h]          17.789122
    ## 224        1 female   [$0,$25k]  (4h,Infh)           7.670599
    ## 225        6 female   [$0,$25k]  (4h,Infh)           7.670599
    ## 226        1 female ($25k,$55k]  (4h,Infh)           7.519444
    ## 227        1 female ($25k,$55k]  (4h,Infh)           7.519444
    ## 228        1 female ($25k,$55k]  (4h,Infh)           7.519444
    ## 229      208 female ($25k,$55k]  (4h,Infh)           7.519444
    ## 230        3 female ($25k,$55k]  (4h,Infh)           7.519444
    ## 231        1   male   [$0,$25k]  (4h,Infh)           5.277376
    ## 232        1   male   [$0,$25k]  (4h,Infh)           5.277376
    ## 233        1   male   [$0,$25k]  (4h,Infh)           5.277376
    ## 234        1   male   [$0,$25k]  (4h,Infh)           5.277376
    ## 235        1   male   [$0,$25k]  (4h,Infh)           5.277376
    ## 236        1   male   [$0,$25k]  (4h,Infh)           5.277376
    ## 237       25   male ($25k,$55k]  (4h,Infh)           5.173382
    ## 238        1   male ($25k,$55k]  (4h,Infh)           5.173382
    ## 239        1   male ($25k,$55k]  (4h,Infh)           5.173382
    ## 240        1   male ($25k,$55k]  (4h,Infh)           5.173382
    ## 241        1   male ($25k,$55k]  (4h,Infh)           5.173382
    ## 242        8   male ($25k,$55k]  (4h,Infh)           5.173382
    ## 243        1   male ($25k,$55k]  (4h,Infh)           5.173382
    ## 244        2 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 245        1 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 246        1 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 247        4 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 248       25 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 249        1 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 250        3 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 251        1 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 252        1 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 253        2 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 254        1 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 255        1 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 256        1 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 257        1 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 258        1 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 259        2 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 260        1 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 261        1 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 262        3 female ($95k,$Inf)  (4h,Infh)           4.302315
    ## 263        1 female ($55k,$95k]  (4h,Infh)           4.291776
    ## 264        7 female ($55k,$95k]  (4h,Infh)           4.291776
    ## 265        1 female ($55k,$95k]  (4h,Infh)           4.291776
    ## 266        1 female ($55k,$95k]  (4h,Infh)           4.291776
    ## 267        2 female ($55k,$95k]  (4h,Infh)           4.291776
    ## 268        2 female ($55k,$95k]  (4h,Infh)           4.291776
    ## 269        1 female ($55k,$95k]  (4h,Infh)           4.291776
    ## 270        1 female ($55k,$95k]  (4h,Infh)           4.291776
    ## 271        1 female ($55k,$95k]  (4h,Infh)           4.291776
    ## 272        1 female ($55k,$95k]  (4h,Infh)           4.291776
    ## 273        1 female ($55k,$95k]  (4h,Infh)           4.291776
    ## 274        1 female ($55k,$95k]  (4h,Infh)           4.291776
    ## 275        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 276        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 277        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 278        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 279        4   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 280        2   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 281        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 282        2   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 283        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 284        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 285        2   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 286        2   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 287       10   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 288        3   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 289        3   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 290        5   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 291        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 292        2   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 293        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 294        6   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 295        3   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 296        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 297        2   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 298        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 299        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 300        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 301        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 302        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 303        2   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 304        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 305        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 306        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 307        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 308        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 309        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 310        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 311        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 312        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 313        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 314        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 315        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 316        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 317        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 318        2   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 319       20   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 320        1   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 321        2   male ($95k,$Inf)  (4h,Infh)           2.959995
    ## 322        2   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 323        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 324        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 325        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 326        2   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 327        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 328        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 329        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 330        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 331        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 332        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 333        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 334        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 335        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 336        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 337        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 338        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 339        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 340        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 341        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 342        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 343        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 344        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 345        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 346        1   male ($55k,$95k]  (4h,Infh)           2.952744
    ## 347        0   male ($95k,$Inf)       <NA>                 NA
    ## 348        0   male ($25k,$55k]       <NA>                 NA
    ## 349        0   male ($95k,$Inf)       <NA>                 NA
    ## 350        0 female ($25k,$55k]       <NA>                 NA
    ## 351        0   male ($95k,$Inf)       <NA>                 NA
    ## 352        0 female ($25k,$55k]       <NA>                 NA
    ## 353        0 female ($95k,$Inf)       <NA>                 NA
    ## 354        0 female ($95k,$Inf)       <NA>                 NA
    ## 355        0   male ($95k,$Inf)       <NA>                 NA
    ## 356        0   male ($95k,$Inf)       <NA>                 NA
    ## 357        0   <NA>        <NA>       <NA>                 NA
    ## 358        0   <NA>        <NA>       <NA>                 NA
    ## 359        0   <NA>        <NA>       <NA>                 NA
    ## 360        0   <NA>        <NA>       <NA>                 NA
    ## 361        0   <NA>        <NA>       <NA>                 NA
    ## 362        0   <NA>        <NA>       <NA>                 NA
    ## 363        0   <NA>        <NA>       <NA>                 NA
    ## 364        0   male        <NA>       <NA>                 NA
    ## 365        0 female   [$0,$25k]       <NA>                 NA
    ## 366       12 female ($55k,$95k]       <NA>                 NA
    ## 367      100 female ($95k,$Inf)       <NA>                 NA
    ## 368       35   male   [$0,$25k]       <NA>                 NA
    ## 369        1 female ($55k,$95k]       <NA>                 NA
    ## 370        6   male ($95k,$Inf)       <NA>                 NA
    ## 371        1 female   [$0,$25k]       <NA>                 NA
    ## 372      100   male        <NA> (0.25h,4h]                 NA
    ## 373        8   male        <NA> [0h,0.25h]                 NA
    ## 374        9   male        <NA> (0.25h,4h]                 NA
    ## 375      100 female        <NA> (0.25h,4h]                 NA
    ## 376       10   male        <NA> (0.25h,4h]                 NA
    ## 377        2   male        <NA> (0.25h,4h]                 NA
    ## 378        1   male        <NA>  (4h,Infh)                 NA
    ## 379        1 female ($25k,$55k]       <NA>                 NA
    ## 380        1   male ($95k,$Inf)       <NA>                 NA
    ## 381        1   male ($25k,$55k]       <NA>                 NA
    ## 382        1   male ($55k,$95k]       <NA>                 NA
    ## 383        1   male        <NA>  (4h,Infh)                 NA
    ## 384        1   <NA>        <NA>  (4h,Infh)                 NA
    ## 385      150 female        <NA> [0h,0.25h]                 NA
    ## 386       35   <NA> ($55k,$95k] (0.25h,4h]                 NA
    ## 387        0   male ($95k,$Inf)       <NA>                 NA
    ## 388       22   <NA>        <NA> (0.25h,4h]                 NA
    ## 389        1   male        <NA>       <NA>                 NA
    ## 390        1 female ($55k,$95k]       <NA>                 NA
    ## 391        0   male ($25k,$55k]       <NA>                 NA
    ## 392        1 female ($95k,$Inf)       <NA>                 NA
    ## 393       15 female        <NA> (0.25h,4h]                 NA
    ## 394        1 female ($25k,$55k]       <NA>                 NA
    ## 395        1   male ($25k,$55k]       <NA>                 NA
    ## 396        1 female ($25k,$55k]       <NA>                 NA
    ## 397        1 female ($55k,$95k]       <NA>                 NA
    ## 398        1 female        <NA> (0.25h,4h]                 NA
    ## 399        1 female ($25k,$55k]       <NA>                 NA
    ## 400       50 female        <NA> [0h,0.25h]                 NA
    ## 401        1   male ($25k,$55k]       <NA>                 NA
    ## 402        1 female        <NA>  (4h,Infh)                 NA
    ## 403        2 female ($25k,$55k]       <NA>                 NA
    ## 404        1   male        <NA>  (4h,Infh)                 NA
    ## 405        1   male ($55k,$95k]       <NA>                 NA
    ## 406        1 female        <NA>  (4h,Infh)                 NA
    ## 407        1   male        <NA>  (4h,Infh)                 NA
    ## 408        0   male ($95k,$Inf)       <NA>                 NA
    ## 409       80 female        <NA> [0h,0.25h]                 NA
    ## 410        2 female        <NA> (0.25h,4h]                 NA

### `The End`

  

  

  

  

  

  

  
