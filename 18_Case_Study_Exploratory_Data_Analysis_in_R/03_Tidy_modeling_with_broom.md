Tidy modeling with broom
================
Mohamad Osman
2022-07-15

# Section 03: **Tidy modeling with broom**

``` r
library(ggplot2)
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
library(countrycode)
```

    ## Warning: package 'countrycode' was built under R version 4.2.1

``` r
votes_file <- file.path("..", "00_Datasets", "votes.rds")
votes <- readRDS(votes_file)

# Add a country column within the mutate: votes_processed
votes_processed <- votes %>%
  filter(vote <= 3) %>%
  mutate(year = session + 1945, 
         country = countrycode(ccode, "cown", "country.name"))
```

    ## Warning in countrycode_convert(sourcevar = sourcevar, origin = origin, destination = dest, : Some values were not matched unambiguously: 260

``` r
# Group by year and country: by_year_country
by_year_country <- votes_processed %>%
  group_by(year, country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))
```

    ## `summarise()` has grouped output by 'year'. You can override using the
    ## `.groups` argument.

### 

**`01-Linear regression on the United States`**

A linear regression is a model that lets us examine how one variable
changes with respect to another by fitting a best fit line. It is done
with the `lm()` function in R.

Here, you’ll fit a linear regression to just the percentage of “yes”
votes from the United States.

-   Print the `US_by_year` data to the console.

-   Using just the US data in `US_by_year`, use `lm()` to run a linear
    regression predicting `percent_yes` from `year`. Save this to a
    variable `US_fit`.

-   Summarize `US_fit` using the `summary()` function.

``` r
# Percentage of yes votes from the US by year: US_by_year
US_by_year <- by_year_country %>%
  filter(country == "United States")

# Print the US_by_year data
head(US_by_year)
```

    ## # A tibble: 6 × 4
    ## # Groups:   year [6]
    ##    year country       total percent_yes
    ##   <dbl> <chr>         <int>       <dbl>
    ## 1  1947 United States    38       0.711
    ## 2  1949 United States    64       0.281
    ## 3  1951 United States    25       0.4  
    ## 4  1953 United States    26       0.5  
    ## 5  1955 United States    37       0.622
    ## 6  1957 United States    34       0.647

``` r
# Perform a linear regression of percent_yes by year: US_fit
US_fit <- lm(percent_yes ~ year, data = US_by_year)

# Perform summary() on the US_fit object
summary(US_fit)
```

    ## 
    ## Call:
    ## lm(formula = percent_yes ~ year, data = US_by_year)
    ## 
    ## Residuals:
    ##       Min        1Q    Median        3Q       Max 
    ## -0.222491 -0.080635 -0.008661  0.081948  0.194307 
    ## 
    ## Coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept) 12.6641455  1.8379743   6.890 8.48e-08 ***
    ## year        -0.0062393  0.0009282  -6.722 1.37e-07 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.1062 on 32 degrees of freedom
    ## Multiple R-squared:  0.5854, Adjusted R-squared:  0.5724 
    ## F-statistic: 45.18 on 1 and 32 DF,  p-value: 1.367e-07

### **`02-Finding the slope of a linear regression`**

The `US_fit` object you created in the previous exercise is available in
your workspace. Calling `summary()` on this gives you lots of useful
information about the linear model.

What is the estimated slope of this relationship? Said differently,
what’s the estimated change each year of the probability of the US
voting “yes”?

`Ans:  -0.006`

### 

**`03-Finding the p-value of a linear regression`**

Not all positive or negative slopes are necessarily real. A p-value is a
way of assessing whether a trend could be due to chance. Generally, data
scientists set a threshold by declaring that, for example, p-values
below .05 are significant.

`US_fit` is still available in your workspace. In this linear model,
what is the p-value of the relationship between `year` and `percent_yes`

`Ans: 1.367e-07`

Correct! It seems `year` is very significant when modeling
`percent_yes`.

### **`04-Tidying a linear regression model`**

In the last section, you fit a linear model. Now, you’ll use the
`tidy()` function in the `broom` package to turn that model into a tidy
data frame.

The `US_fit` linear model is available in your workspace.

-   Load the `broom` package.

-   Use the `tidy()` function from `broom` on the model object to turn
    it into a tidy data frame. Don’t store the result; just print the
    output to the console.

``` r
# Load the broom package
library(broom)

# Call the tidy() function on the US_fit object
tidy(US_fit)
```

    ## # A tibble: 2 × 5
    ##   term        estimate std.error statistic      p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>        <dbl>
    ## 1 (Intercept) 12.7      1.84          6.89 0.0000000848
    ## 2 year        -0.00624  0.000928     -6.72 0.000000137

### **`05-Combining models for multiple countries`**

One important advantage of changing models to tidied data frames is that
they can be combined.

In an earlier section, you fit a linear model to the percentage of “yes”
votes for each year in the United States. Now you’ll fit the same model
for the United Kingdom and combine the results from both countries.

-   Fit a model for the United Kingdom similar to the one you fit for
    the US and save it as `UK_fit`.

-   Tidy `US_fit` into a data frame called `US_tidied` and the UK model
    into `UK_tidied`.

-   Use `bind_rows()` from `dplyr` to combine the two tidied models,
    printing the result to the console.

``` r
# Linear regression of percent_yes by year for US
US_by_year <- by_year_country %>%
  filter(country == "United States")
US_fit <- lm(percent_yes ~ year, US_by_year)

# Fit model for the United Kingdom
UK_by_year <- by_year_country %>%
  filter(country == "United Kingdom")
UK_fit <- lm(percent_yes ~ year, UK_by_year)

# Create US_tidied and UK_tidied
US_tidied <- tidy(US_fit)
UK_tidied <- tidy(UK_fit)

# Combine the two tidied models
bind_rows(US_tidied, UK_tidied)
```

    ## # A tibble: 4 × 5
    ##   term        estimate std.error statistic      p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>        <dbl>
    ## 1 (Intercept) 12.7      1.84          6.89 0.0000000848
    ## 2 year        -0.00624  0.000928     -6.72 0.000000137 
    ## 3 (Intercept) -3.27     1.96         -1.67 0.105       
    ## 4 year         0.00187  0.000989      1.89 0.0677

### **`06-Nesting a data frame`**

Right now, the `by_year_country` data frame has one row per country-vote
pair. So that you can model each country individually, you’re going to
“nest” all columns besides `country`, which will result in a data frame
with one row per country. The data for each individual country will then
be stored in a **list column** called `data`.

-   Load the `tidyr` package.

-   Use the `nest()` function to nest all the columns in
    `by_year_country` except `country`.

``` r
# Load the tidyr package
library(tidyr)
```

    ## Warning: package 'tidyr' was built under R version 4.2.1

``` r
# Nest all columns besides country
by_year_country %>%
    nest(-country)
```

    ## Warning: All elements of `...` must be named.
    ## Did you want `data = -country`?

    ## # A tibble: 200 × 2
    ##    country     data                 
    ##    <chr>       <list>               
    ##  1 Afghanistan <grouped_df [34 × 3]>
    ##  2 Argentina   <grouped_df [34 × 3]>
    ##  3 Australia   <grouped_df [34 × 3]>
    ##  4 Belarus     <grouped_df [34 × 3]>
    ##  5 Belgium     <grouped_df [34 × 3]>
    ##  6 Bolivia     <grouped_df [34 × 3]>
    ##  7 Brazil      <grouped_df [34 × 3]>
    ##  8 Canada      <grouped_df [34 × 3]>
    ##  9 Chile       <grouped_df [34 × 3]>
    ## 10 Colombia    <grouped_df [34 × 3]>
    ## # … with 190 more rows

### **`07-List columns`**

This “nested” data has an interesting structure. The second column,
`data`, is a **list**, a type of R object that hasn’t yet come up in
this course that allows complicated objects to be stored within each
row. This is because each item of the `data` column is itself a data
frame.

    # A tibble: 200 × 2
                               country              data
                                 <chr>            <list>
    1                      Afghanistan <tibble [34 × 3]>
    2                        Argentina <tibble [34 × 3]>
    3                        Australia <tibble [34 × 3]>
    4                          Belarus <tibble [34 × 3]>
    5                          Belgium <tibble [34 × 3]>
    6  Bolivia, Plurinational State of <tibble [34 × 3]>
    7                           Brazil <tibble [34 × 3]>
    8                           Canada <tibble [34 × 3]>
    9                            Chile <tibble [34 × 3]>
    10                        Colombia <tibble [34 × 3]>

You can use `nested$data` to access this list column and double brackets
to access a particular element. For example, `nested$data[[1]]` would
give you the data frame with Afghanistan’s voting history (the
`percent_yes` per year), since Afghanistan is the first row of the
table.

-   Print the data frame from the `data` column that contains the data
    for Brazil.

``` r
# All countries are nested besides country
nested <- by_year_country %>%
  nest(-country)
```

    ## Warning: All elements of `...` must be named.
    ## Did you want `data = -country`?

``` r
# Print the nested data for Brazil
nested$data[[7]]
```

    ## # A tibble: 34 × 3
    ## # Groups:   year [34]
    ##     year total percent_yes
    ##    <dbl> <int>       <dbl>
    ##  1  1947    38       0.658
    ##  2  1949    64       0.469
    ##  3  1951    25       0.64 
    ##  4  1953    26       0.731
    ##  5  1955    37       0.730
    ##  6  1957    34       0.735
    ##  7  1959    54       0.537
    ##  8  1961    76       0.553
    ##  9  1963    32       0.781
    ## 10  1965    41       0.610
    ## # … with 24 more rows

### 

**`08-Unnesting`**

The opposite of the `nest()` operation is the `unnest()` operation. This
takes each of the data frames in the list column and brings those rows
back to the main data frame.

In this exercise, you are just undoing the `nest()` operation. In the
next section, you’ll learn how to fit a model in between these nesting
and unnesting steps that makes this process useful.

Unnest the `data` list column, so that the table again has one row for
each country-year pair, much like `by_year_country`.

``` r
# All countries are nested besides country
nested <- by_year_country %>%
  nest(-country)
```

    ## Warning: All elements of `...` must be named.
    ## Did you want `data = -country`?

``` r
# Unnest the data column to return it to its original form
unnest(nested)
```

    ## Warning: `cols` is now required when using unnest().
    ## Please use `cols = c(data)`

    ## # A tibble: 4,744 × 4
    ##    country      year total percent_yes
    ##    <chr>       <dbl> <int>       <dbl>
    ##  1 Afghanistan  1947    34       0.382
    ##  2 Afghanistan  1949    51       0.608
    ##  3 Afghanistan  1951    25       0.76 
    ##  4 Afghanistan  1953    26       0.769
    ##  5 Afghanistan  1955    37       0.730
    ##  6 Afghanistan  1957    34       0.529
    ##  7 Afghanistan  1959    54       0.611
    ##  8 Afghanistan  1961    76       0.605
    ##  9 Afghanistan  1963    32       0.781
    ## 10 Afghanistan  1965    40       0.85 
    ## # … with 4,734 more rows

### `09-Performing linear regression on each nested dataset`

Now that you’ve divided the data for each country into a separate
dataset in the `data` column, you need to fit a linear model to each of
these datasets.

The `map()` function from `purrr` works by applying a formula to each
item in a list, where `.` represents the individual item. For example,
you could add one to each of a list of numbers:

    map(numbers, ~ 1 + .)

This means that to fit a model to each dataset, you can do:

    map(data, ~ lm(percent_yes ~ year, data = .))

where `.` represents each individual item from the `data` column in
`by_year_country`. Recall that each item in the `data` column is a
dataset that pertains to a specific country.

-   Load the `tidyr` and `purrr` packages.

-   After nesting, use the `map()` function within a `mutate()` to
    perform a linear regression on each dataset (i.e. each item in the
    `data` column in `by_year_country`) modeling `percent_yes` as a
    function of `year`. Save the results to the `model` column.

``` r
# Load tidyr and purrr
library(tidyr)
library(purrr)


# Perform a linear regression on each item in the data column
by_year_country %>%
  nest(-country) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, .)))
```

    ## Warning: All elements of `...` must be named.
    ## Did you want `data = -country`?

    ## # A tibble: 200 × 3
    ##    country     data                  model 
    ##    <chr>       <list>                <list>
    ##  1 Afghanistan <grouped_df [34 × 3]> <lm>  
    ##  2 Argentina   <grouped_df [34 × 3]> <lm>  
    ##  3 Australia   <grouped_df [34 × 3]> <lm>  
    ##  4 Belarus     <grouped_df [34 × 3]> <lm>  
    ##  5 Belgium     <grouped_df [34 × 3]> <lm>  
    ##  6 Bolivia     <grouped_df [34 × 3]> <lm>  
    ##  7 Brazil      <grouped_df [34 × 3]> <lm>  
    ##  8 Canada      <grouped_df [34 × 3]> <lm>  
    ##  9 Chile       <grouped_df [34 × 3]> <lm>  
    ## 10 Colombia    <grouped_df [34 × 3]> <lm>  
    ## # … with 190 more rows

### **`10-Tidy each linear regression model`**

You’ve now performed a linear regression on each nested dataset and have
a linear model stored in the list column `model`. But you can’t
recombine the models until you’ve *tidied* each into a table of
coefficients. To do that, you’ll need to use `map()` one more time and
the `tidy()` function from the `broom` package.

Recall that you can simply give a function to `map()`
(e.g. `map(models, tidy)`) in order to apply that function to each item
of a list.

-   Load the `broom` package.

-   Use the `map()` function to apply the `tidy()` function to each
    linear model in the `model` column, creating a new column called
    `tidied`.

``` r
# Load the broom package
library(broom)

# Add another mutate that applies tidy() to each model
by_year_country %>%
  nest(-country) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .))) %>%
  mutate(tidied = map(model, tidy))
```

    ## Warning: All elements of `...` must be named.
    ## Did you want `data = -country`?

    ## # A tibble: 200 × 4
    ##    country     data                  model  tidied          
    ##    <chr>       <list>                <list> <list>          
    ##  1 Afghanistan <grouped_df [34 × 3]> <lm>   <tibble [2 × 5]>
    ##  2 Argentina   <grouped_df [34 × 3]> <lm>   <tibble [2 × 5]>
    ##  3 Australia   <grouped_df [34 × 3]> <lm>   <tibble [2 × 5]>
    ##  4 Belarus     <grouped_df [34 × 3]> <lm>   <tibble [2 × 5]>
    ##  5 Belgium     <grouped_df [34 × 3]> <lm>   <tibble [2 × 5]>
    ##  6 Bolivia     <grouped_df [34 × 3]> <lm>   <tibble [2 × 5]>
    ##  7 Brazil      <grouped_df [34 × 3]> <lm>   <tibble [2 × 5]>
    ##  8 Canada      <grouped_df [34 × 3]> <lm>   <tibble [2 × 5]>
    ##  9 Chile       <grouped_df [34 × 3]> <lm>   <tibble [2 × 5]>
    ## 10 Colombia    <grouped_df [34 × 3]> <lm>   <tibble [2 × 5]>
    ## # … with 190 more rows

-   Add an `unnest()` step to unnest the tidied models stored in the
    `tidied` column. Save the result as `country_coefficients`.

-   Print the resulting `country_coefficients` object to the console.

``` r
# Add one more step that unnests the tidied column
country_coefficients <- by_year_country %>%
  nest(-country) %>%
  mutate(model = map(data, ~ lm(percent_yes ~ year, data = .)),
         tidied = map(model, tidy)) %>%
  unnest(tidied)
```

    ## Warning: All elements of `...` must be named.
    ## Did you want `data = -country`?

``` r
# Print the resulting country_coefficients variable
country_coefficients
```

    ## # A tibble: 400 × 8
    ##    country     data         model  term     estimate std.error statistic p.value
    ##    <chr>       <list>       <list> <chr>       <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 Afghanistan <grouped_df> <lm>   (Interc… -1.11e+1  1.47         -7.52 1.44e-8
    ##  2 Afghanistan <grouped_df> <lm>   year      6.01e-3  0.000743      8.09 3.06e-9
    ##  3 Argentina   <grouped_df> <lm>   (Interc… -9.46e+0  2.10         -4.50 8.32e-5
    ##  4 Argentina   <grouped_df> <lm>   year      5.15e-3  0.00106       4.85 3.05e-5
    ##  5 Australia   <grouped_df> <lm>   (Interc… -4.55e+0  2.15         -2.12 4.22e-2
    ##  6 Australia   <grouped_df> <lm>   year      2.57e-3  0.00108       2.37 2.42e-2
    ##  7 Belarus     <grouped_df> <lm>   (Interc… -7.00e+0  1.50         -4.66 5.33e-5
    ##  8 Belarus     <grouped_df> <lm>   year      3.91e-3  0.000759      5.15 1.28e-5
    ##  9 Belgium     <grouped_df> <lm>   (Interc… -5.85e+0  1.52         -3.86 5.22e-4
    ## 10 Belgium     <grouped_df> <lm>   year      3.20e-3  0.000765      4.19 2.07e-4
    ## # … with 390 more rows

### **`11-Filtering model terms`**

You currently have both the intercept and slope terms for each
by-country model. You’re probably more interested in how each is
changing over time, so you want to focus on the slope terms.

-   Print the `country_coefficients` data frame to the console.

-   Perform a `filter()` step that extracts only the slope (not
    intercept) terms.

``` r
# Print the country_coefficients dataset
country_coefficients
```

    ## # A tibble: 400 × 8
    ##    country     data         model  term     estimate std.error statistic p.value
    ##    <chr>       <list>       <list> <chr>       <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 Afghanistan <grouped_df> <lm>   (Interc… -1.11e+1  1.47         -7.52 1.44e-8
    ##  2 Afghanistan <grouped_df> <lm>   year      6.01e-3  0.000743      8.09 3.06e-9
    ##  3 Argentina   <grouped_df> <lm>   (Interc… -9.46e+0  2.10         -4.50 8.32e-5
    ##  4 Argentina   <grouped_df> <lm>   year      5.15e-3  0.00106       4.85 3.05e-5
    ##  5 Australia   <grouped_df> <lm>   (Interc… -4.55e+0  2.15         -2.12 4.22e-2
    ##  6 Australia   <grouped_df> <lm>   year      2.57e-3  0.00108       2.37 2.42e-2
    ##  7 Belarus     <grouped_df> <lm>   (Interc… -7.00e+0  1.50         -4.66 5.33e-5
    ##  8 Belarus     <grouped_df> <lm>   year      3.91e-3  0.000759      5.15 1.28e-5
    ##  9 Belgium     <grouped_df> <lm>   (Interc… -5.85e+0  1.52         -3.86 5.22e-4
    ## 10 Belgium     <grouped_df> <lm>   year      3.20e-3  0.000765      4.19 2.07e-4
    ## # … with 390 more rows

``` r
# Filter for only the slope terms
country_coefficients %>%
    filter(term == "year")
```

    ## # A tibble: 200 × 8
    ##    country     data         model  term  estimate std.error statistic    p.value
    ##    <chr>       <list>       <list> <chr>    <dbl>     <dbl>     <dbl>      <dbl>
    ##  1 Afghanistan <grouped_df> <lm>   year   0.00601  0.000743      8.09    3.06e-9
    ##  2 Argentina   <grouped_df> <lm>   year   0.00515  0.00106       4.85    3.05e-5
    ##  3 Australia   <grouped_df> <lm>   year   0.00257  0.00108       2.37    2.42e-2
    ##  4 Belarus     <grouped_df> <lm>   year   0.00391  0.000759      5.15    1.28e-5
    ##  5 Belgium     <grouped_df> <lm>   year   0.00320  0.000765      4.19    2.07e-4
    ##  6 Bolivia     <grouped_df> <lm>   year   0.00580  0.000966      6.01    1.06e-6
    ##  7 Brazil      <grouped_df> <lm>   year   0.00611  0.000817      7.48    1.64e-8
    ##  8 Canada      <grouped_df> <lm>   year   0.00152  0.000955      1.59    1.22e-1
    ##  9 Chile       <grouped_df> <lm>   year   0.00678  0.000822      8.24    2.05e-9
    ## 10 Colombia    <grouped_df> <lm>   year   0.00616  0.000965      6.38    3.58e-7
    ## # … with 190 more rows

### **`12-Filtering for significant countries`**

Not all slopes are significant, and you can use the p-value to guess
which are and which are not.

However, when you have lots of p-values, like one for each country, you
run into the problem of multiple hypothesis testing, where you have to
set a stricter threshold. The
[**`p.adjust()`**](https://www.rdocumentation.org/packages/stats/topics/p.adjust)
function is a simple way to correct for this, where `p.adjust(p.value)`
on a vector of p-values returns a set that you can trust.

Here you’ll add two steps to process the `slope_terms` dataset: use a
`mutate` to create the new, adjusted p-value column, and `filter` to
filter for those below a .05 threshold.

-   Use the `p.adjust()` function to adjust the `p.value` column, saving
    the result into a new `p.adjusted` column. Then, filter for cases
    where `p.adjusted` is less than .05.

``` r
# Filter for only the slope terms
slope_terms <- country_coefficients %>%
  filter(term == "year")

# Add p.adjusted column, then filter
slope_terms %>%
  mutate(p.adjusted = p.adjust(p.value)) %>%
  filter(p.adjusted < 0.05)
```

    ## # A tibble: 61 × 9
    ##    country     data         model  term  estimate std.error statistic    p.value
    ##    <chr>       <list>       <list> <chr>    <dbl>     <dbl>     <dbl>      <dbl>
    ##  1 Afghanistan <grouped_df> <lm>   year   0.00601  0.000743      8.09    3.06e-9
    ##  2 Argentina   <grouped_df> <lm>   year   0.00515  0.00106       4.85    3.05e-5
    ##  3 Belarus     <grouped_df> <lm>   year   0.00391  0.000759      5.15    1.28e-5
    ##  4 Belgium     <grouped_df> <lm>   year   0.00320  0.000765      4.19    2.07e-4
    ##  5 Bolivia     <grouped_df> <lm>   year   0.00580  0.000966      6.01    1.06e-6
    ##  6 Brazil      <grouped_df> <lm>   year   0.00611  0.000817      7.48    1.64e-8
    ##  7 Chile       <grouped_df> <lm>   year   0.00678  0.000822      8.24    2.05e-9
    ##  8 Colombia    <grouped_df> <lm>   year   0.00616  0.000965      6.38    3.58e-7
    ##  9 Costa Rica  <grouped_df> <lm>   year   0.00654  0.000812      8.05    3.39e-9
    ## 10 Cuba        <grouped_df> <lm>   year   0.00461  0.000721      6.40    3.43e-7
    ## # … with 51 more rows, and 1 more variable: p.adjusted <dbl>

### **`13-Sorting by slope`**

Now that you’ve filtered for countries where the trend is probably not
due to chance, you may be interested in countries whose percentage of
“yes” votes is changing most quickly over time. Thus, you want to find
the countries with the highest and lowest slopes; that is, the
`estimate` column.

-   Using `arrange()` and `desc()`, sort the filtered countries to find
    the countries whose percentage “yes” is most quickly increasing over
    time.

-   Using `arrange()`, sort to find the countries whose percentage “yes”
    is most quickly decreasing.

``` r
# Filter by adjusted p-values
filtered_countries <- country_coefficients %>%
  filter(term == "year") %>%
  mutate(p.adjusted = p.adjust(p.value)) %>%
  filter(p.adjusted < .05)

# Sort for the countries increasing most quickly
filtered_countries %>%
  arrange(desc(estimate))
```

    ## # A tibble: 61 × 9
    ##    country        data         model term  estimate std.error statistic  p.value
    ##    <chr>          <list>       <lis> <chr>    <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 South Africa   <grouped_df> <lm>  year   0.0119   0.00140       8.47 1.60e- 8
    ##  2 Kazakhstan     <grouped_df> <lm>  year   0.0110   0.00195       5.62 3.24e- 4
    ##  3 Yemen Arab Re… <grouped_df> <lm>  year   0.0109   0.00159       6.84 1.20e- 6
    ##  4 Kyrgyzstan     <grouped_df> <lm>  year   0.00973  0.000988      9.84 2.38e- 5
    ##  5 Malawi         <grouped_df> <lm>  year   0.00908  0.00181       5.02 4.48e- 5
    ##  6 Dominican Rep… <grouped_df> <lm>  year   0.00806  0.000914      8.81 5.96e-10
    ##  7 Portugal       <grouped_df> <lm>  year   0.00802  0.00171       4.68 7.13e- 5
    ##  8 Honduras       <grouped_df> <lm>  year   0.00772  0.000921      8.38 1.43e- 9
    ##  9 Peru           <grouped_df> <lm>  year   0.00730  0.000976      7.48 1.65e- 8
    ## 10 Nicaragua      <grouped_df> <lm>  year   0.00708  0.00107       6.60 1.92e- 7
    ## # … with 51 more rows, and 1 more variable: p.adjusted <dbl>

``` r
# Sort for the countries decreasing most quickly
filtered_countries %>%
  arrange(estimate)
```

    ## # A tibble: 61 × 9
    ##    country         data         model term  estimate std.error statistic p.value
    ##    <chr>           <list>       <lis> <chr>    <dbl>     <dbl>     <dbl>   <dbl>
    ##  1 South Korea     <grouped_df> <lm>  year  -0.00921  0.00155      -5.96 1.39e-4
    ##  2 Israel          <grouped_df> <lm>  year  -0.00685  0.00117      -5.85 1.89e-6
    ##  3 United States   <grouped_df> <lm>  year  -0.00624  0.000928     -6.72 1.37e-7
    ##  4 Belgium         <grouped_df> <lm>  year   0.00320  0.000765      4.19 2.07e-4
    ##  5 Guinea          <grouped_df> <lm>  year   0.00362  0.000833      4.35 1.87e-4
    ##  6 Morocco         <grouped_df> <lm>  year   0.00380  0.000860      4.42 1.46e-4
    ##  7 Belarus         <grouped_df> <lm>  year   0.00391  0.000759      5.15 1.28e-5
    ##  8 Iran            <grouped_df> <lm>  year   0.00391  0.000856      4.57 6.91e-5
    ##  9 Congo - Brazza… <grouped_df> <lm>  year   0.00397  0.000922      4.30 2.27e-4
    ## 10 Sudan           <grouped_df> <lm>  year   0.00399  0.000961      4.15 2.98e-4
    ## # … with 51 more rows, and 1 more variable: p.adjusted <dbl>

### `The End`
