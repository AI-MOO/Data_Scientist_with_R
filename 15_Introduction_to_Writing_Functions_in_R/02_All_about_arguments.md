All about arguments
================
Mohamad Osman
2022-07-05

# Section 02: All about arguments

### **`01-Numeric defaults`**

`cut_by_quantile()` converts a numeric vector into a categorical
variable where quantiles define the cut points. This is a useful
function, but at the moment you have to specify five arguments to make
it work. This is too much thinking and typing.

By specifying default arguments, you can make it easier to use. Let’s
start with `n`, which specifies how many categories to cut `x` into.

A numeric vector of the number of visits to Snake River is provided as
`n_visits`.

-   Update the definition of `cut_by_quantile()` so that the `n`
    argument defaults to `5`.

-   Remove the `n` argument from the call to `cut_by_quantile()`.

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
file_path <- file.path("..", "00_Datasets", "snake_river_visits.rds")
snake_river_visits <- readRDS(file_path)
n_visits <- snake_river_visits$n_visits
```

``` r
# Set the default for n to 5
cut_by_quantile <- function(x, n = 5, na.rm, labels, interval_type) {
  probs <- seq(0, 1, length.out = n + 1)
  qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE)
  right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE)
  cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE)
}

# Remove the n argument from the call
cut_by_quantile(
  n_visits, 
  na.rm = FALSE, 
  labels = c("very low", "low", "medium", "high", "very high"),
  interval_type = "(lo, hi]"
)
```

    ##   [1] very low  very low  very low  very low  very low  very low  very low 
    ##   [8] very low  very low  very low  very low  very low  very low  very low 
    ##  [15] very low  very low  very low  very low  very low  high      very high
    ##  [22] high      very low  medium    low       very low  very low  very low 
    ##  [29] very low  very low  very low  very high very high very high very high
    ##  [36] very high high      very high very high very high very high very high
    ##  [43] medium    very high very high very high medium    medium    low      
    ##  [50] high      high      high      very high very high high      high     
    ##  [57] very high medium    very high high      medium    high      very high
    ##  [64] very high very high very high high      high      very high high     
    ##  [71] very low  very high high      high      medium    high      high     
    ##  [78] high      medium    very high very high very high high      high     
    ##  [85] high      very low  very high medium    high      very high high     
    ##  [92] high      very high high      very low  very low  medium    very low 
    ##  [99] medium    medium    very high medium    medium    medium    high     
    ## [106] low       high      very high medium    very high medium    very high
    ## [113] low       very high low       very high high      very low  very low 
    ## [120] very low  very low  low       very low  very low  very low  very low 
    ## [127] very low  very low  medium    very low  very low  low       low      
    ## [134] very low  very low  low       very low  very low  very low  low      
    ## [141] low       medium    medium    medium    medium    medium    very low 
    ## [148] very low  low       very low  low       medium    very low  very low 
    ## [155] very low  very low  very high high      very high high      medium   
    ## [162] very high medium    very low  high      medium    high      high     
    ## [169] very high high      high      very high very high high      very high
    ## [176] high      high      medium    very high high      high      high     
    ## [183] very high very high very low  high      very high high      high     
    ## [190] medium    very high high      very high high      very high high     
    ## [197] very high high      very high very low  high      very high very high
    ## [204] very low  very low  medium    very high medium    low       medium   
    ## [211] high      medium    very low  medium    very high high      very high
    ## [218] high      very high high      low       high      medium    very high
    ## [225] medium    high      high      high      very low  high      high     
    ## [232] high      very high high      medium    medium    very low  very low 
    ## [239] very low  very low  medium    low       very low  very low  very low 
    ## [246] medium    high      very low  very low  medium    very low  very low 
    ## [253] very low  very low  very low  very low  very low  very low  very low 
    ## [260] very low  very high medium    very low  very high medium    very high
    ## [267] medium    low       very high medium    medium    medium    low      
    ## [274] high      medium    high      very high medium    very high very high
    ## [281] medium    medium    very high high      medium    very high high     
    ## [288] medium    low       very low  medium    very low  very low  very low 
    ## [295] very low  very low  low       very low  very low  very low  very low 
    ## [302] very low  very low  very low  very low  low       very low  very low 
    ## [309] very low  very low  low       very low  very low  low       very low 
    ## [316] very low  very low  very low  low       very low  very low  very low 
    ## [323] very low  very low  low       very low  very low  very low  very low 
    ## [330] very low  very low  very low  very low  very low  very low  very low 
    ## [337] very low  very low  very low  very low  very low  very low  very low 
    ## [344] very low  very low  medium    very low  very low  very low  very low 
    ## [351] very low  very low  very low  very low  very low  very low  very low 
    ## [358] very low  low       very low  very low  very low  very low  very low 
    ## [365] very low  very low  very low  very low  very low  very low  low      
    ## [372] very low  very low  very low  very high high      very high very high
    ## [379] very high high      very high very high very high very high medium   
    ## [386] medium    medium    high      very high high      high      high     
    ## [393] high      high      high      high      very high high      very high
    ## [400] medium    high      low       high      very high low       very low 
    ## [407] medium    very low  medium    low      
    ## Levels: very low low medium high very high

### **`03-Logical defaults`**

-   Update the definition of `cut_by_quantile()` so that the `na.rm`
    argument defaults to `FALSE`.

-   Remove the `na.rm` argument from the call to `cut_by_quantile()`.

``` r
# Set the default for na.rm to FALSE
cut_by_quantile <- function(x, n = 5, na.rm = FALSE, labels, interval_type) {
  probs <- seq(0, 1, length.out = n + 1)
  qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE)
  right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE)
  cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE)
}

# Remove the na.rm argument from the call
cut_by_quantile(
  n_visits, 
  labels = c("very low", "low", "medium", "high", "very high"),
  interval_type = "(lo, hi]"
)
```

    ##   [1] very low  very low  very low  very low  very low  very low  very low 
    ##   [8] very low  very low  very low  very low  very low  very low  very low 
    ##  [15] very low  very low  very low  very low  very low  high      very high
    ##  [22] high      very low  medium    low       very low  very low  very low 
    ##  [29] very low  very low  very low  very high very high very high very high
    ##  [36] very high high      very high very high very high very high very high
    ##  [43] medium    very high very high very high medium    medium    low      
    ##  [50] high      high      high      very high very high high      high     
    ##  [57] very high medium    very high high      medium    high      very high
    ##  [64] very high very high very high high      high      very high high     
    ##  [71] very low  very high high      high      medium    high      high     
    ##  [78] high      medium    very high very high very high high      high     
    ##  [85] high      very low  very high medium    high      very high high     
    ##  [92] high      very high high      very low  very low  medium    very low 
    ##  [99] medium    medium    very high medium    medium    medium    high     
    ## [106] low       high      very high medium    very high medium    very high
    ## [113] low       very high low       very high high      very low  very low 
    ## [120] very low  very low  low       very low  very low  very low  very low 
    ## [127] very low  very low  medium    very low  very low  low       low      
    ## [134] very low  very low  low       very low  very low  very low  low      
    ## [141] low       medium    medium    medium    medium    medium    very low 
    ## [148] very low  low       very low  low       medium    very low  very low 
    ## [155] very low  very low  very high high      very high high      medium   
    ## [162] very high medium    very low  high      medium    high      high     
    ## [169] very high high      high      very high very high high      very high
    ## [176] high      high      medium    very high high      high      high     
    ## [183] very high very high very low  high      very high high      high     
    ## [190] medium    very high high      very high high      very high high     
    ## [197] very high high      very high very low  high      very high very high
    ## [204] very low  very low  medium    very high medium    low       medium   
    ## [211] high      medium    very low  medium    very high high      very high
    ## [218] high      very high high      low       high      medium    very high
    ## [225] medium    high      high      high      very low  high      high     
    ## [232] high      very high high      medium    medium    very low  very low 
    ## [239] very low  very low  medium    low       very low  very low  very low 
    ## [246] medium    high      very low  very low  medium    very low  very low 
    ## [253] very low  very low  very low  very low  very low  very low  very low 
    ## [260] very low  very high medium    very low  very high medium    very high
    ## [267] medium    low       very high medium    medium    medium    low      
    ## [274] high      medium    high      very high medium    very high very high
    ## [281] medium    medium    very high high      medium    very high high     
    ## [288] medium    low       very low  medium    very low  very low  very low 
    ## [295] very low  very low  low       very low  very low  very low  very low 
    ## [302] very low  very low  very low  very low  low       very low  very low 
    ## [309] very low  very low  low       very low  very low  low       very low 
    ## [316] very low  very low  very low  low       very low  very low  very low 
    ## [323] very low  very low  low       very low  very low  very low  very low 
    ## [330] very low  very low  very low  very low  very low  very low  very low 
    ## [337] very low  very low  very low  very low  very low  very low  very low 
    ## [344] very low  very low  medium    very low  very low  very low  very low 
    ## [351] very low  very low  very low  very low  very low  very low  very low 
    ## [358] very low  low       very low  very low  very low  very low  very low 
    ## [365] very low  very low  very low  very low  very low  very low  low      
    ## [372] very low  very low  very low  very high high      very high very high
    ## [379] very high high      very high very high very high very high medium   
    ## [386] medium    medium    high      very high high      high      high     
    ## [393] high      high      high      high      very high high      very high
    ## [400] medium    high      low       high      very high low       very low 
    ## [407] medium    very low  medium    low      
    ## Levels: very low low medium high very high

### **`03-NULL defaults`**

The `cut()` function used by `cut_by_quantile()` can automatically
provide sensible labels for each category. The code to generate these
labels is [**pretty
complicated**](https://github.com/wch/r-source/blob/29a9e663a2352843a6ea26b259725b0b97d0e4bd/src/library/base/R/cut.R#L42-L60),
so rather than appearing in the function signature directly, its
`labels` argument defaults to `NULL`, and the calculation details are
shown on the
[**`?cut`**](https://www.rdocumentation.org/packages/base/topics/cut)
help page.

-   Update the definition of `cut_by_quantile()` so that the `labels`
    argument defaults to `NULL`.

-   Remove the `labels` argument from the call to `cut_by_quantile()`.

``` r
# Set the default for labels to NULL
cut_by_quantile <- function(x, n = 5, na.rm = FALSE, labels = NULL, interval_type) {
  probs <- seq(0, 1, length.out = n + 1)
  qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE)
  right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE)
  cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE)
}

# Remove the labels argument from the call
cut_by_quantile(
  n_visits,
  interval_type = "(lo, hi]"
)
```

    ##   [1] [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ##   [9] [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ##  [17] [0,1]    [0,1]    [0,1]    (10,35]  (35,350] (10,35]  [0,1]    (2,10]  
    ##  [25] (1,2]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    (35,350]
    ##  [33] (35,350] (35,350] (35,350] (35,350] (10,35]  (35,350] (35,350] (35,350]
    ##  [41] (35,350] (35,350] (2,10]   (35,350] (35,350] (35,350] (2,10]   (2,10]  
    ##  [49] (1,2]    (10,35]  (10,35]  (10,35]  (35,350] (35,350] (10,35]  (10,35] 
    ##  [57] (35,350] (2,10]   (35,350] (10,35]  (2,10]   (10,35]  (35,350] (35,350]
    ##  [65] (35,350] (35,350] (10,35]  (10,35]  (35,350] (10,35]  [0,1]    (35,350]
    ##  [73] (10,35]  (10,35]  (2,10]   (10,35]  (10,35]  (10,35]  (2,10]   (35,350]
    ##  [81] (35,350] (35,350] (10,35]  (10,35]  (10,35]  [0,1]    (35,350] (2,10]  
    ##  [89] (10,35]  (35,350] (10,35]  (10,35]  (35,350] (10,35]  [0,1]    [0,1]   
    ##  [97] (2,10]   [0,1]    (2,10]   (2,10]   (35,350] (2,10]   (2,10]   (2,10]  
    ## [105] (10,35]  (1,2]    (10,35]  (35,350] (2,10]   (35,350] (2,10]   (35,350]
    ## [113] (1,2]    (35,350] (1,2]    (35,350] (10,35]  [0,1]    [0,1]    [0,1]   
    ## [121] [0,1]    (1,2]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [129] (2,10]   [0,1]    [0,1]    (1,2]    (1,2]    [0,1]    [0,1]    (1,2]   
    ## [137] [0,1]    [0,1]    [0,1]    (1,2]    (1,2]    (2,10]   (2,10]   (2,10]  
    ## [145] (2,10]   (2,10]   [0,1]    [0,1]    (1,2]    [0,1]    (1,2]    (2,10]  
    ## [153] [0,1]    [0,1]    [0,1]    [0,1]    (35,350] (10,35]  (35,350] (10,35] 
    ## [161] (2,10]   (35,350] (2,10]   [0,1]    (10,35]  (2,10]   (10,35]  (10,35] 
    ## [169] (35,350] (10,35]  (10,35]  (35,350] (35,350] (10,35]  (35,350] (10,35] 
    ## [177] (10,35]  (2,10]   (35,350] (10,35]  (10,35]  (10,35]  (35,350] (35,350]
    ## [185] [0,1]    (10,35]  (35,350] (10,35]  (10,35]  (2,10]   (35,350] (10,35] 
    ## [193] (35,350] (10,35]  (35,350] (10,35]  (35,350] (10,35]  (35,350] [0,1]   
    ## [201] (10,35]  (35,350] (35,350] [0,1]    [0,1]    (2,10]   (35,350] (2,10]  
    ## [209] (1,2]    (2,10]   (10,35]  (2,10]   [0,1]    (2,10]   (35,350] (10,35] 
    ## [217] (35,350] (10,35]  (35,350] (10,35]  (1,2]    (10,35]  (2,10]   (35,350]
    ## [225] (2,10]   (10,35]  (10,35]  (10,35]  [0,1]    (10,35]  (10,35]  (10,35] 
    ## [233] (35,350] (10,35]  (2,10]   (2,10]   [0,1]    [0,1]    [0,1]    [0,1]   
    ## [241] (2,10]   (1,2]    [0,1]    [0,1]    [0,1]    (2,10]   (10,35]  [0,1]   
    ## [249] [0,1]    (2,10]   [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [257] [0,1]    [0,1]    [0,1]    [0,1]    (35,350] (2,10]   [0,1]    (35,350]
    ## [265] (2,10]   (35,350] (2,10]   (1,2]    (35,350] (2,10]   (2,10]   (2,10]  
    ## [273] (1,2]    (10,35]  (2,10]   (10,35]  (35,350] (2,10]   (35,350] (35,350]
    ## [281] (2,10]   (2,10]   (35,350] (10,35]  (2,10]   (35,350] (10,35]  (2,10]  
    ## [289] (1,2]    [0,1]    (2,10]   [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [297] (1,2]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [305] [0,1]    (1,2]    [0,1]    [0,1]    [0,1]    [0,1]    (1,2]    [0,1]   
    ## [313] [0,1]    (1,2]    [0,1]    [0,1]    [0,1]    [0,1]    (1,2]    [0,1]   
    ## [321] [0,1]    [0,1]    [0,1]    [0,1]    (1,2]    [0,1]    [0,1]    [0,1]   
    ## [329] [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [337] [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [345] [0,1]    (2,10]   [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [353] [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    (1,2]    [0,1]   
    ## [361] [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [369] [0,1]    [0,1]    (1,2]    [0,1]    [0,1]    [0,1]    (35,350] (10,35] 
    ## [377] (35,350] (35,350] (35,350] (10,35]  (35,350] (35,350] (35,350] (35,350]
    ## [385] (2,10]   (2,10]   (2,10]   (10,35]  (35,350] (10,35]  (10,35]  (10,35] 
    ## [393] (10,35]  (10,35]  (10,35]  (10,35]  (35,350] (10,35]  (35,350] (2,10]  
    ## [401] (10,35]  (1,2]    (10,35]  (35,350] (1,2]    [0,1]    (2,10]   [0,1]   
    ## [409] (2,10]   (1,2]   
    ## Levels: [0,1] (1,2] (2,10] (10,35] (35,350]

### **`04-Categorical defaults`**

When cutting up a numeric vector, you need to worry about what happens
if a value lands exactly on a boundary. You can either put this value
into a category of the lower interval or the higher interval. That is,
you can choose your intervals to include values at the top boundary but
not the bottom (in mathematical terminology, “open on the left, closed
on the right”, or `(lo, hi]`). Or you can choose the opposite (“closed
on the left, open on the right”, or `[lo, hi)`). `cut_by_quantile()`
should allow these two choices.

The pattern for categorical defaults is:

    function(cat_arg = c("choice1", "choice2")) {
      cat_arg <- match.arg(cat_arg)
    }

**Free hint**: In the console, type `head(rank)` to see the start of
`rank()`’s definition, and look at the `ties.method` argument.

-   Update the signature of `cut_by_quantile()` so that the
    `interval_type` argument can be `"(lo, hi]"` or `"[lo, hi)"`. *Note
    the space after each comma.*

-   Update the body of `cut_by_quantile()` to match the `interval_type`
    argument.

-   Remove the `interval_type` argument from the call to
    `cut_by_quantile()`.

``` r
# Set the categories for interval_type to "(lo, hi]" and "[lo, hi)"
cut_by_quantile <- function(x, n = 5, na.rm = FALSE, labels = NULL, 
                            interval_type = c("(lo, hi]", "[lo, hi)")) {
  # Match the interval_type argument
  interval_type <- match.arg(interval_type)
  probs <- seq(0, 1, length.out = n + 1)
  qtiles <- quantile(x, probs, na.rm = na.rm, names = FALSE)
  right <- switch(interval_type, "(lo, hi]" = TRUE, "[lo, hi)" = FALSE)
  cut(x, qtiles, labels = labels, right = right, include.lowest = TRUE)
}

# Remove the interval_type argument from the call
cut_by_quantile(n_visits)
```

    ##   [1] [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ##   [9] [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ##  [17] [0,1]    [0,1]    [0,1]    (10,35]  (35,350] (10,35]  [0,1]    (2,10]  
    ##  [25] (1,2]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    (35,350]
    ##  [33] (35,350] (35,350] (35,350] (35,350] (10,35]  (35,350] (35,350] (35,350]
    ##  [41] (35,350] (35,350] (2,10]   (35,350] (35,350] (35,350] (2,10]   (2,10]  
    ##  [49] (1,2]    (10,35]  (10,35]  (10,35]  (35,350] (35,350] (10,35]  (10,35] 
    ##  [57] (35,350] (2,10]   (35,350] (10,35]  (2,10]   (10,35]  (35,350] (35,350]
    ##  [65] (35,350] (35,350] (10,35]  (10,35]  (35,350] (10,35]  [0,1]    (35,350]
    ##  [73] (10,35]  (10,35]  (2,10]   (10,35]  (10,35]  (10,35]  (2,10]   (35,350]
    ##  [81] (35,350] (35,350] (10,35]  (10,35]  (10,35]  [0,1]    (35,350] (2,10]  
    ##  [89] (10,35]  (35,350] (10,35]  (10,35]  (35,350] (10,35]  [0,1]    [0,1]   
    ##  [97] (2,10]   [0,1]    (2,10]   (2,10]   (35,350] (2,10]   (2,10]   (2,10]  
    ## [105] (10,35]  (1,2]    (10,35]  (35,350] (2,10]   (35,350] (2,10]   (35,350]
    ## [113] (1,2]    (35,350] (1,2]    (35,350] (10,35]  [0,1]    [0,1]    [0,1]   
    ## [121] [0,1]    (1,2]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [129] (2,10]   [0,1]    [0,1]    (1,2]    (1,2]    [0,1]    [0,1]    (1,2]   
    ## [137] [0,1]    [0,1]    [0,1]    (1,2]    (1,2]    (2,10]   (2,10]   (2,10]  
    ## [145] (2,10]   (2,10]   [0,1]    [0,1]    (1,2]    [0,1]    (1,2]    (2,10]  
    ## [153] [0,1]    [0,1]    [0,1]    [0,1]    (35,350] (10,35]  (35,350] (10,35] 
    ## [161] (2,10]   (35,350] (2,10]   [0,1]    (10,35]  (2,10]   (10,35]  (10,35] 
    ## [169] (35,350] (10,35]  (10,35]  (35,350] (35,350] (10,35]  (35,350] (10,35] 
    ## [177] (10,35]  (2,10]   (35,350] (10,35]  (10,35]  (10,35]  (35,350] (35,350]
    ## [185] [0,1]    (10,35]  (35,350] (10,35]  (10,35]  (2,10]   (35,350] (10,35] 
    ## [193] (35,350] (10,35]  (35,350] (10,35]  (35,350] (10,35]  (35,350] [0,1]   
    ## [201] (10,35]  (35,350] (35,350] [0,1]    [0,1]    (2,10]   (35,350] (2,10]  
    ## [209] (1,2]    (2,10]   (10,35]  (2,10]   [0,1]    (2,10]   (35,350] (10,35] 
    ## [217] (35,350] (10,35]  (35,350] (10,35]  (1,2]    (10,35]  (2,10]   (35,350]
    ## [225] (2,10]   (10,35]  (10,35]  (10,35]  [0,1]    (10,35]  (10,35]  (10,35] 
    ## [233] (35,350] (10,35]  (2,10]   (2,10]   [0,1]    [0,1]    [0,1]    [0,1]   
    ## [241] (2,10]   (1,2]    [0,1]    [0,1]    [0,1]    (2,10]   (10,35]  [0,1]   
    ## [249] [0,1]    (2,10]   [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [257] [0,1]    [0,1]    [0,1]    [0,1]    (35,350] (2,10]   [0,1]    (35,350]
    ## [265] (2,10]   (35,350] (2,10]   (1,2]    (35,350] (2,10]   (2,10]   (2,10]  
    ## [273] (1,2]    (10,35]  (2,10]   (10,35]  (35,350] (2,10]   (35,350] (35,350]
    ## [281] (2,10]   (2,10]   (35,350] (10,35]  (2,10]   (35,350] (10,35]  (2,10]  
    ## [289] (1,2]    [0,1]    (2,10]   [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [297] (1,2]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [305] [0,1]    (1,2]    [0,1]    [0,1]    [0,1]    [0,1]    (1,2]    [0,1]   
    ## [313] [0,1]    (1,2]    [0,1]    [0,1]    [0,1]    [0,1]    (1,2]    [0,1]   
    ## [321] [0,1]    [0,1]    [0,1]    [0,1]    (1,2]    [0,1]    [0,1]    [0,1]   
    ## [329] [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [337] [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [345] [0,1]    (2,10]   [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [353] [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    (1,2]    [0,1]   
    ## [361] [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]    [0,1]   
    ## [369] [0,1]    [0,1]    (1,2]    [0,1]    [0,1]    [0,1]    (35,350] (10,35] 
    ## [377] (35,350] (35,350] (35,350] (10,35]  (35,350] (35,350] (35,350] (35,350]
    ## [385] (2,10]   (2,10]   (2,10]   (10,35]  (35,350] (10,35]  (10,35]  (10,35] 
    ## [393] (10,35]  (10,35]  (10,35]  (10,35]  (35,350] (10,35]  (35,350] (2,10]  
    ## [401] (10,35]  (1,2]    (10,35]  (35,350] (1,2]    [0,1]    (2,10]   [0,1]   
    ## [409] (2,10]   (1,2]   
    ## Levels: [0,1] (1,2] (2,10] (10,35] (35,350]

### **`05-Harmonic mean`**

The harmonic mean is the reciprocal of the arithmetic mean of the
reciprocal of the data. That is

harmonic_mean(x)=1/arithmetic_mean(1/x)

The harmonic mean is often used to average ratio data. You’ll be using
it on the price/earnings ratio of stocks in the Standard and Poor’s 500
index, provided as `std_and_poor500`. Price/earnings ratio is a measure
of how expensive a stock is.

The `dplyr` package is loaded.

``` r
file_path <- file.path("..", "00_Datasets", 
                       "std_and_poor500_with_pe_2019-06-21.rds")

std_and_poor500 <- readRDS(file_path)
```

-   *Look at `std_and_poor500` (you’ll need this later).* Write a
    function, `get_reciprocal`, to get the reciprocal of an input `x`.
    Its only argument should be `x`, and it should return one over `x`.

-   Write a function, `calc_harmonic_mean()`, that calculates the
    harmonic mean of its only input, `x`.

-   Using `std_and_poor500`, group by `sector`, and summarize to
    calculate the harmonic mean of the price/earning ratios in the
    `pe_ratio` column.

``` r
# From previous steps
get_reciprocal <- function(x) {
  1 / x
}
calc_harmonic_mean <- function(x) {
  x %>%
    get_reciprocal() %>%
    mean() %>%
    get_reciprocal()
}

std_and_poor500 %>% 
  # Group by sector
  group_by(sector) %>% 
  # Summarize, calculating harmonic mean of P/E ratio
  summarize(hmean_pe_ratio = calc_harmonic_mean(pe_ratio))
```

    ## # A tibble: 11 × 2
    ##    sector                 hmean_pe_ratio
    ##    <chr>                           <dbl>
    ##  1 Communication Services           NA  
    ##  2 Consumer Discretionary           NA  
    ##  3 Consumer Staples                 NA  
    ##  4 Energy                           NA  
    ##  5 Financials                       NA  
    ##  6 Health Care                      NA  
    ##  7 Industrials                      NA  
    ##  8 Information Technology           NA  
    ##  9 Materials                        NA  
    ## 10 Real Estate                      32.5
    ## 11 Utilities                        NA

### 06-**Dealing with missing values**

-   Modify the signature and body of `calc_harmonic_mean()` so it has an
    `na.rm` argument, defaulting to false, that gets passed to `mean()`.

``` r
# Add an na.rm arg with a default, and pass it to mean()
calc_harmonic_mean <- function(x, na.rm = FALSE) {
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}
```

-   Using `std_and_poor500`, group by `sector`, and summarize to
    calculate the harmonic mean of the price/earning ratios in the
    `pe_ratio` column, removing missing values.

``` r
# From previous step
calc_harmonic_mean <- function(x, na.rm = FALSE) {
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}

std_and_poor500 %>% 
  # Group by sector
  group_by(sector) %>% 
  # Summarize, calculating harmonic mean of P/E ratio
  summarize(hmean_pe_ratio = calc_harmonic_mean(pe_ratio, na.rm = TRUE))
```

    ## # A tibble: 11 × 2
    ##    sector                 hmean_pe_ratio
    ##    <chr>                           <dbl>
    ##  1 Communication Services           17.5
    ##  2 Consumer Discretionary           15.2
    ##  3 Consumer Staples                 19.8
    ##  4 Energy                           13.7
    ##  5 Financials                       12.9
    ##  6 Health Care                      26.6
    ##  7 Industrials                      18.2
    ##  8 Information Technology           21.6
    ##  9 Materials                        16.3
    ## 10 Real Estate                      32.5
    ## 11 Utilities                        23.9

### **`07-Passing arguments with ...`**

-   Replace the `na.rm` argument with `...` in the signature and body of
    `calc_harmonic_mean()`.

``` r
# Swap na.rm arg for ... in signature and body
calc_harmonic_mean <- function(x, ...) {
  x %>%
    get_reciprocal() %>%
    mean(...) %>%
    get_reciprocal()
}

calc_harmonic_mean(x = c(1, NA, 3, NA, 5), na.rm = TRUE)
```

    ## [1] 1.956522

-   Using `std_and_poor500`, group by `sector`, and summarize to
    calculate the harmonic mean of the price/earning ratios in the
    `pe_ratio` column, removing missing values.

``` r
calc_harmonic_mean <- function(x, ...) {
  x %>%
    get_reciprocal() %>%
    mean(...) %>%
    get_reciprocal()
}

std_and_poor500 %>% 
  # Group by sector
  group_by(sector) %>% 
  # Summarize, calculating harmonic mean of P/E ratio
  summarize(hmean_pe_ratio = calc_harmonic_mean(pe_ratio, , na.rm = TRUE))
```

    ## # A tibble: 11 × 2
    ##    sector                 hmean_pe_ratio
    ##    <chr>                           <dbl>
    ##  1 Communication Services           17.5
    ##  2 Consumer Discretionary           15.2
    ##  3 Consumer Staples                 19.8
    ##  4 Energy                           13.7
    ##  5 Financials                       12.9
    ##  6 Health Care                      26.6
    ##  7 Industrials                      18.2
    ##  8 Information Technology           21.6
    ##  9 Materials                        16.3
    ## 10 Real Estate                      32.5
    ## 11 Utilities                        23.9

### **`08-Throwing errors with bad arguments`**

-   Add a line to the body of `calc_harmonic_mean()` to assert that `x`
    is numeric.

-   *Look at what happens when you pass a character argument to
    `calc_harmonic_mean()`.*

``` r
library(assertive )
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
calc_harmonic_mean <- function(x, na.rm = FALSE) {
  # Assert that x is numeric
  assert_is_numeric(x)
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}

# See what happens when you pass it strings
#calc_harmonic_mean(std_and_poor500$sector)
```

### **`09-Custom error logic`**

-   If any values of `x` are non-positive (ignoring `NA`s) then throw an
    error.

-   *Look at what happens when you pass a character argument to
    `calc_harmonic_mean()`.*

``` r
calc_harmonic_mean <- function(x, na.rm = FALSE) {
  assert_is_numeric(x)
  # Check if any values of x are non-positive
  if(any(is_non_positive(x), na.rm = TRUE)) {
    # Throw an error
    stop("x contains non-positive values, so the harmonic mean makes no sense.")
  }
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}

# See what happens when you pass it negative numbers
# calc_harmonic_mean(std_and_poor500$pe_ratio - 20, na.rm = TRUE)
```

### **`10-Fixing function arguments`**

The harmonic mean function is almost complete. However, you still need
to provide some checks on the `na.rm` argument. This time, rather than
throwing errors when the input is in an incorrect form, you are going to
try to fix it.

`na.rm` should be a logical vector with one element (that is, `TRUE`, or
`FALSE`).

The `assertive` package is loaded for you.

-   Update `calc_harmonic_mean()` to fix the `na.rm` argument by using
    `use_first()` to select the first `na.rm` element, and `coerce_to()`
    to change it to logical.

``` r
# Update the function definition to fix the na.rm argument
calc_harmonic_mean <- function(x, na.rm = FALSE) {
  assert_is_numeric(x)
  if(any(is_non_positive(x), na.rm = TRUE)) {
    stop("x contains non-positive values, so the harmonic mean makes no sense.")
  }
  # Use the first value of na.rm, and coerce to logical
  na.rm <- coerce_to(use_first(na.rm), target_class = "logical")
  x %>%
    get_reciprocal() %>%
    mean(na.rm = na.rm) %>%
    get_reciprocal()
}

# See what happens when you pass it malformed na.rm
calc_harmonic_mean(std_and_poor500$pe_ratio, na.rm = 1:5)
```

    ## Warning: Only the first value of na.rm (= 1) will be used.

    ## Warning: Coercing use_first(na.rm) to class 'logical'.

    ## [1] 18.23871

``` r
calc_harmonic_mean(std_and_poor500$pe_ratio, na.rm = 0:5)
```

    ## Warning: Only the first value of na.rm (= 0) will be used.

    ## Warning: Coercing use_first(na.rm) to class 'logical'.

    ## [1] NA

### `The End`
