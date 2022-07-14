Data cleaning and summarizing with dplyr
================
Mohamad Osman
2022-07-14

# **`Section 01: Data cleaning and summarizing with dplyr`**

### **`01-Filtering rows`**

The `vote` column in the dataset has a number that represents that
country’s vote:

-   **1** = Yes

-   **2** = Abstain

-   **3** = No

-   **8** = Not present

-   **9** = Not a member

One step of data cleaning is removing observations (rows) that you’re
not interested in. In this case, you want to remove “Not present” and
“Not a member”.

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
votes_file <- file.path("..", "00_Datasets", "votes.rds")
votes <- readRDS(votes_file)
```

-   Load the `dplyr` package.

-   Print the `votes` table.

-   Filter out rows where the vote recorded is “not present” or “not a
    member”, leaving cases where it is “yes”, “abstain”, or “no”.

``` r
    # Load the dplyr package
library(dplyr)

# Print the votes dataset
head(votes)
```

    ## # A tibble: 6 × 4
    ##    rcid session  vote ccode
    ##   <dbl>   <dbl> <dbl> <int>
    ## 1    46       2     1     2
    ## 2    46       2     1    20
    ## 3    46       2     9    31
    ## 4    46       2     1    40
    ## 5    46       2     1    41
    ## 6    46       2     1    42

``` r
# Filter for votes that are "yes", "abstain", or "no"
votes %>%
    filter(vote <= 3) %>%
    head()
```

    ## # A tibble: 6 × 4
    ##    rcid session  vote ccode
    ##   <dbl>   <dbl> <dbl> <int>
    ## 1    46       2     1     2
    ## 2    46       2     1    20
    ## 3    46       2     1    40
    ## 4    46       2     1    41
    ## 5    46       2     1    42
    ## 6    46       2     1    70

### **`02-Adding a year column`**

The next step of data cleaning is manipulating your variables (columns)
to make them more informative.

In this case, you have a `session` column that is hard to interpret
intuitively. But since the UN started voting in 1946, and holds one
session per year, you can get the year of a UN resolution by adding 1945
to the `session` number.

``` r
# Add another %>% step to add a year column
votes %>%
  filter(vote <= 3) %>%
  mutate(year = session + 1945) %>%
  head()
```

    ## # A tibble: 6 × 5
    ##    rcid session  vote ccode  year
    ##   <dbl>   <dbl> <dbl> <int> <dbl>
    ## 1    46       2     1     2  1947
    ## 2    46       2     1    20  1947
    ## 3    46       2     1    40  1947
    ## 4    46       2     1    41  1947
    ## 5    46       2     1    42  1947
    ## 6    46       2     1    70  1947

### **`03-Adding a country column`**

The country codes in the `ccode` column are what’s called [**Correlates
of War
codes**](https://cow.dss.ucdavis.edu/data-sets/cow-country-codes). This
isn’t ideal for an analysis, since you’d like to work with recognizable
country names.

You can use the `countrycode` package to translate. For example:

    library(countrycode)

    # Translate the country code 2
    > countrycode(2, "cown", "country.name")
    [1] "United States"

    # Translate multiple country codes
    > countrycode(c(2, 20, 40), "cown", "country.name")
    [1] "United States" "Canada"        "Cuba"

-   Load the `countrycode` package.

-   Convert the country code 100 to its country name.

-   Add a new `country` column in your `mutate()` statement containing
    country names, using the `countrycode()` function to translate from
    the `ccode` column. Save the result to `votes_processed`.

``` r
# Load the countrycode package
library(countrycode)

# Convert country code 100
countrycode(100, "cown", "country.name")
```

    ## [1] "Colombia"

``` r
# Add a country column within the mutate: votes_processed
votes_processed <- votes %>%
  filter(vote <= 3) %>%
  mutate(year = session + 1945, 
         country = countrycode(ccode, "cown", "country.name")) 
```

    ## Warning in countrycode_convert(sourcevar = sourcevar, origin = origin, destination = dest, : Some values were not matched unambiguously: 260

``` r
head(votes_processed)
```

    ## # A tibble: 6 × 6
    ##    rcid session  vote ccode  year country           
    ##   <dbl>   <dbl> <dbl> <int> <dbl> <chr>             
    ## 1    46       2     1     2  1947 United States     
    ## 2    46       2     1    20  1947 Canada            
    ## 3    46       2     1    40  1947 Cuba              
    ## 4    46       2     1    41  1947 Haiti             
    ## 5    46       2     1    42  1947 Dominican Republic
    ## 6    46       2     1    70  1947 Mexico

### 

**`04-Summarizing the full dataset`**

In this analysis, you’re going to focus on “% of votes that are yes” as
a metric for the “agreeableness” of countries.

You’ll start by finding this summary for the entire dataset: the
fraction of all votes in their history that were “yes”. Note that within
your call to `summarize()`, you can use `n()` to find the total number
of votes and `mean(vote == 1)` to find the fraction of “yes” votes.

-   Print the `votes_processed` dataset that you created in the previous
    exercise.

-   Summarize the dataset using the `summarize()` function to create two
    columns:

    -   `total`: with the number of votes

    -   `percent_yes`: the percentage of “yes” votes

``` r
# Print votes_processed
head(votes_processed)
```

    ## # A tibble: 6 × 6
    ##    rcid session  vote ccode  year country           
    ##   <dbl>   <dbl> <dbl> <int> <dbl> <chr>             
    ## 1    46       2     1     2  1947 United States     
    ## 2    46       2     1    20  1947 Canada            
    ## 3    46       2     1    40  1947 Cuba              
    ## 4    46       2     1    41  1947 Haiti             
    ## 5    46       2     1    42  1947 Dominican Republic
    ## 6    46       2     1    70  1947 Mexico

``` r
# Find total and fraction of "yes" votes
votes_processed %>%
    summarise(total = n(),
              percent_yes = mean(vote == 1))
```

    ## # A tibble: 1 × 2
    ##    total percent_yes
    ##    <int>       <dbl>
    ## 1 353547       0.800

### `05-Summarizing by year`

The `summarize()` function is especially useful because it can be used
within *groups*.

For example, you might like to know how much the average “agreeableness”
of countries changed from year to year. To examine this, you can use
`group_by()` to perform your summary not for the entire dataset, but
within each year.

-   Add a `group_by()` to your code to `summarize()` within each year.

``` r
# Change this code to summarize by year
votes_processed %>%
  group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))
```

    ## # A tibble: 34 × 3
    ##     year total percent_yes
    ##    <dbl> <int>       <dbl>
    ##  1  1947  2039       0.569
    ##  2  1949  3469       0.438
    ##  3  1951  1434       0.585
    ##  4  1953  1537       0.632
    ##  5  1955  2169       0.695
    ##  6  1957  2708       0.609
    ##  7  1959  4326       0.588
    ##  8  1961  7482       0.573
    ##  9  1963  3308       0.729
    ## 10  1965  4382       0.708
    ## # … with 24 more rows

### **`06-Summarizing by country`**

In the last exercise, you performed a summary of the votes within each
year. You could instead `summarize()` within each country, which would
let you compare voting patterns between countries.

``` r
# Summarize by country: by_country
by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

head(by_country)
```

    ## # A tibble: 6 × 3
    ##   country           total percent_yes
    ##   <chr>             <int>       <dbl>
    ## 1 Afghanistan        2373       0.859
    ## 2 Albania            1695       0.717
    ## 3 Algeria            2213       0.899
    ## 4 Andorra             719       0.638
    ## 5 Angola             1431       0.924
    ## 6 Antigua & Barbuda  1302       0.912

### **`07-Sorting by percentage of "yes" votes`**

Now that you’ve summarized the dataset by country, you can start
examining it and answering interesting questions.

For example, you might be especially interested in the countries that
voted “yes” least often, or the ones that voted “yes” most often.

-   Print the `by_country` dataset created in the last exercise.

-   Use `arrange()` to sort the countries in ascending order of
    `percent_yes`.

-   Arrange the countries by the same variable, but in descending order.

``` r
# You have the votes summarized by country
by_country <- votes_processed %>%
  group_by(country) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

# Print the by_country dataset
by_country
```

    ## # A tibble: 200 × 3
    ##    country           total percent_yes
    ##    <chr>             <int>       <dbl>
    ##  1 Afghanistan        2373       0.859
    ##  2 Albania            1695       0.717
    ##  3 Algeria            2213       0.899
    ##  4 Andorra             719       0.638
    ##  5 Angola             1431       0.924
    ##  6 Antigua & Barbuda  1302       0.912
    ##  7 Argentina          2553       0.768
    ##  8 Armenia             758       0.747
    ##  9 Australia          2575       0.557
    ## 10 Austria            2389       0.622
    ## # … with 190 more rows

``` r
# Sort in ascending order of percent_yes
by_country %>% 
  arrange(percent_yes)
```

    ## # A tibble: 200 × 3
    ##    country                          total percent_yes
    ##    <chr>                            <int>       <dbl>
    ##  1 Zanzibar                             2       0    
    ##  2 United States                     2568       0.269
    ##  3 Palau                              369       0.339
    ##  4 Israel                            2380       0.341
    ##  5 <NA>                              1075       0.397
    ##  6 United Kingdom                    2558       0.417
    ##  7 France                            2527       0.427
    ##  8 Micronesia (Federated States of)   724       0.442
    ##  9 Marshall Islands                   757       0.491
    ## 10 Belgium                           2568       0.492
    ## # … with 190 more rows

``` r
# Now sort in descending order
by_country %>% 
  arrange(desc(percent_yes))
```

    ## # A tibble: 200 × 3
    ##    country              total percent_yes
    ##    <chr>                <int>       <dbl>
    ##  1 São Tomé & Príncipe   1091       0.976
    ##  2 Seychelles             881       0.975
    ##  3 Djibouti              1598       0.961
    ##  4 Guinea-Bissau         1538       0.960
    ##  5 Timor-Leste            326       0.957
    ##  6 Mauritius             1831       0.950
    ##  7 Zimbabwe              1361       0.949
    ##  8 Comoros               1133       0.947
    ##  9 United Arab Emirates  1934       0.947
    ## 10 Mozambique            1701       0.947
    ## # … with 190 more rows

### **`08-Filtering summarized output`**

In the last exercise, you may have noticed that the country that voted
least frequently, Zanzibar, had only 2 votes in the entire dataset. You
certainly can’t make any substantial conclusions based on that data!

Typically in a progressive analysis, when you find that a few of your
observations have very little data while others have plenty, you set
some threshold to filter them out

-   Use `filter()` to remove from the sorted data countries that have
    fewer than 100 votes.

``` r
# Filter out countries with fewer than 100 votes
by_country %>%
  arrange(percent_yes) %>%
  filter(total >= 100)
```

    ## # A tibble: 197 × 3
    ##    country                          total percent_yes
    ##    <chr>                            <int>       <dbl>
    ##  1 United States                     2568       0.269
    ##  2 Palau                              369       0.339
    ##  3 Israel                            2380       0.341
    ##  4 <NA>                              1075       0.397
    ##  5 United Kingdom                    2558       0.417
    ##  6 France                            2527       0.427
    ##  7 Micronesia (Federated States of)   724       0.442
    ##  8 Marshall Islands                   757       0.491
    ##  9 Belgium                           2568       0.492
    ## 10 Canada                            2576       0.508
    ## # … with 187 more rows

### `THE END`
