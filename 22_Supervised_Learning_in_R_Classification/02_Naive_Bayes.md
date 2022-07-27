Naive Bayes
================
Mohamad Osman
2022-07-26

# Section 02: **Naive Bayes**

### **`01-Computing probabilities`**

The `where9am` data frame contains 91 days (thirteen weeks) worth of
data in which Brett recorded his `location` at 9am each day as well as
whether the `daytype` was a weekend or weekday.

Using the conditional probability formula below, you can compute the
probability that Brett is working in the office, given that it is a
weekday.

P(A\|B)=P(A and B)P(B)

<img src="images/chrome_CsCL448Yge.png" width="238" />

Calculations like these are the basis of the Naive Bayes destination
prediction model you’ll develop in later exercises.

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
library(readr)
```

``` r
file_path <- file.path("..", "00_Datasets", "where9am.txt")
where9am <- read.delim(file_path)

locations_data_URL <- "https://assets.datacamp.com/production/repositories/718/datasets/571628c39048df59c40c9dcfba146a2cf7a4a0e3/locations.csv"
locations <- read_csv(locations_data_URL)
```

    ## Rows: 2184 Columns: 7
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): weekday, daytype, hourtype, location
    ## dbl (3): month, day, hour
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(where9am)
```

    ##   daytype location
    ## 1 weekday   office
    ## 2 weekday   office
    ## 3 weekday   office
    ## 4 weekend     home
    ## 5 weekend     home
    ## 6 weekday   campus

``` r
head(locations)
```

    ## # A tibble: 6 × 7
    ##   month   day weekday   daytype  hour hourtype location
    ##   <dbl> <dbl> <chr>     <chr>   <dbl> <chr>    <chr>   
    ## 1     1     4 wednesday weekday     0 night    home    
    ## 2     1     4 wednesday weekday     1 night    home    
    ## 3     1     4 wednesday weekday     2 night    home    
    ## 4     1     4 wednesday weekday     3 night    home    
    ## 5     1     4 wednesday weekday     4 night    home    
    ## 6     1     4 wednesday weekday     5 night    home

-   Find P(office) using `nrow()` and `subset()` to count rows in the
    dataset and save the result as `p_A`.

-   Find P(weekday), using `nrow()` and `subset()` again, and save the
    result as `p_B`.

-   Use `nrow()` and `subset()` a final time to find P(office and
    weekday). Save the result as `p_AB`.

-   Compute P(office \| weekday) and save the result as `p_A_given_B`.

-   Print the value of `p_A_given_B`.

``` r
# Compute P(A) 
p_A <- nrow(subset(where9am, location == "office")) / nrow(where9am)

# Compute P(B)
p_B <- nrow(subset(where9am, daytype == "weekday")) / nrow(where9am)

# Compute the observed P(A and B)
p_AB <- nrow(subset(where9am, (location == "office") & (daytype == "weekday"))) / nrow(where9am)

# Compute P(A | B) and print its value
p_A_given_B <- p_AB / p_B
print(p_A_given_B)
```

    ## [1] 0.6

### **`02-Understanding dependent events`**

In the previous exercise, you found that there is a 60% chance Brett is
in the office at 9am given that it is a weekday. On the other hand, if
Brett is never in the office on a weekend, which of the following is/are
true?

-   P(office and weekend) = 0.

-   P(office \| weekend) = 0.

-   Brett’s location is dependent on the day of the week.

-   All of the above. `(the answer)`

### 

### **`03-A simple Naive Bayes location model`**

The previous exercises showed that the probability that Brett is at work
or at home at 9am is highly dependent on whether it is the weekend or a
weekday.

To see this finding in action, use the `where9am` data frame to build a
Naive Bayes model on the same data.

You can then use this model to predict the future: where does the model
think that Brett will be at 9am on Thursday and at 9am on Saturday?

The data frame `where9am` is available in your workspace. This dataset
contains information about Brett’s location at 9am on different days.

``` r
# Load the naivebayes package
library(naivebayes)
```

    ## naivebayes 0.9.7 loaded

``` r
# Build the location prediction model
locmodel <- naive_bayes(location ~ daytype, data = where9am)
```

    ## Warning: naive_bayes(): Feature daytype - zero probabilities are present.
    ## Consider Laplace smoothing.

``` r
thursday9am <- data.frame(daytype = "weekday")
saturday9am <- data.frame(daytype = "weekend")

# Predict Thursday's 9am location
predict(locmodel, thursday9am)
```

    ## [1] office
    ## Levels: appointment campus home office

``` r
# Predict Saturdays's 9am location
predict(locmodel, saturday9am)
```

    ## [1] home
    ## Levels: appointment campus home office

Awesome job! Not surprisingly, Brett is most likely at the office at 9am
on a Thursday, but at home at the same time on a Saturday!

### **`04-Examining "raw" probabilities`**

The `naivebayes` package offers several ways to peek inside a Naive
Bayes model.

Typing the name of the model object provides the *a priori* (overall)
and conditional probabilities of each of the model’s predictors. If one
were so inclined, you might use these for calculating *posterior*
(predicted) probabilities by hand.

Alternatively, R will compute the posterior probabilities for you if the
`type = "prob"` parameter is supplied to the `predict()` function.

Using these methods, examine how the model’s predicted 9am location
probability varies from day-to-day. The model `locmodel` that you fit in
the previous exercise is available for you to use, and the `naivebayes`
package has been pre-loaded.

-   Print the `locmodel` object to the console to view the computed *a
    priori* and conditional probabilities.

-   Use the `predict()` function similarly to the previous exercise, but
    with `type = "prob"` to see the predicted probabilities for Thursday
    at 9am.

-   Compare these to the predicted probabilities for Saturday at 9am.

``` r
# Examine the location prediction model
locmodel
```

    ## 
    ## ================================== Naive Bayes ================================== 
    ##  
    ##  Call: 
    ## naive_bayes.formula(formula = location ~ daytype, data = where9am)
    ## 
    ## --------------------------------------------------------------------------------- 
    ##  
    ## Laplace smoothing: 0
    ## 
    ## --------------------------------------------------------------------------------- 
    ##  
    ##  A priori probabilities: 
    ## 
    ## appointment      campus        home      office 
    ##  0.01098901  0.10989011  0.45054945  0.42857143 
    ## 
    ## --------------------------------------------------------------------------------- 
    ##  
    ##  Tables: 
    ## 
    ## --------------------------------------------------------------------------------- 
    ##  ::: daytype (Bernoulli) 
    ## --------------------------------------------------------------------------------- 
    ##          
    ## daytype   appointment    campus      home    office
    ##   weekday   1.0000000 1.0000000 0.3658537 1.0000000
    ##   weekend   0.0000000 0.0000000 0.6341463 0.0000000
    ## 
    ## ---------------------------------------------------------------------------------

``` r
# Obtain the predicted probabilities for Thursday at 9am
predict(locmodel, thursday9am, type = "prob")
```

    ##      appointment    campus      home office
    ## [1,]  0.01538462 0.1538462 0.2307692    0.6

``` r
# Obtain the predicted probabilities for Saturday at 9am
predict(locmodel, saturday9am, type = "prob")
```

    ##       appointment       campus      home      office
    ## [1,] 3.838772e-05 0.0003838772 0.9980806 0.001497121

### **`05-Understanding independence`**

Understanding the idea of event independence will become important as
you learn more about how “naive” Bayes got its name. Which of the
following is true about independent events?

**`Ans.`** Knowing the outcome of one event does not help predict the
other.

Yes! One event is independent of another if knowing one doesn’t give you
information about how likely the other is. For example, knowing if it’s
raining in New York doesn’t help you predict the weather in San
Francisco. The weather events in the two cities are independent of each
other.

### **`06-Who are you calling naive?`**

The Naive Bayes algorithm got its name because it makes a “naive”
assumption about event independence.

What is the purpose of making this assumption?

**`Answer:`** The joint probability calculation is simpler for
independent events.

Yes! The joint probability of independent events can be computed much
more simply by multiplying their individual probabilities.

### 

**`07-A more sophisticated location model`**

The `locations` dataset records Brett’s location every hour for 13
weeks. Each hour, the tracking information includes the `daytype`
(weekend or weekday) as well as the `hourtype` (morning, afternoon,
evening, or night).

Using this data, build a more sophisticated model to see how Brett’s
predicted location not only varies by the day of week but also by the
time of day. The dataset `locations` is already loaded in your
workspace.

You can specify additional independent variables in your formula using
the `+` sign (e.g. `y ~ x + b`).

The `naivebayes` package has been pre-loaded.

-   Use the R formula interface to build a model where location depends
    on both `daytype` and `hourtype`. Recall that the function
    `naive_bayes()` takes 2 arguments: `formula` and `data`.

-   Predict Brett’s location on a weekday afternoon using the data frame
    `weekday_afternoon` and the `predict()` function.

-   Do the same for a `weekday_evening`.

``` r
file_path <- file.path("..", "00_Datasets", "locations_co22.txt")
locations <- read.delim(file_path)

weekday_afternoon <- data.frame(daytype = "weekday",
                                hourtype = "afternoon",
                                location = "office" )

weekday_evening <- data.frame(daytype = "weekday",
                                hourtype = "evening",
                                location = "home" )
```

``` r
# Build a NB model of location
locmodel <- naive_bayes(location ~ daytype  + hourtype, data = locations)
```

    ## Warning: naive_bayes(): Feature daytype - zero probabilities are present.
    ## Consider Laplace smoothing.

    ## Warning: naive_bayes(): Feature hourtype - zero probabilities are present.
    ## Consider Laplace smoothing.

``` r
# Predict Brett's location on a weekday afternoon
predict(locmodel, weekday_afternoon)
```

    ## Warning: predict.naive_bayes(): more features in the newdata are provided as
    ## there are probability tables in the object. Calculation is performed based on
    ## features to be found in the tables.

    ## [1] office
    ## Levels:  appointment campus home office restaurant store theater

``` r
# Predict Brett's location on a weekday evening
predict(locmodel, weekday_evening)
```

    ## Warning: predict.naive_bayes(): more features in the newdata are provided as
    ## there are probability tables in the object. Calculation is performed based on
    ## features to be found in the tables.

    ## [1] home
    ## Levels:  appointment campus home office restaurant store theater

### **`08-Preparing for unforeseen circumstances`**

While Brett was tracking his location over 13 weeks, he never went into
the office during the weekend. Consequently, the joint probability of
P(office and weekend) = 0.

Explore how this impacts the predicted probability that Brett may go to
work on the weekend in the future. Additionally, you can see how using
the Laplace correction will allow a small chance for these types of
unforeseen circumstances.

The model `locmodel` is available for you to use, along with the data
frame `weekend_afternoon`. The `naivebayes` package has also been
pre-loaded.

-   Use the `locmodel` to output predicted probabilities for a weekend
    afternoon by using the `predict()` function. Remember to set the
    `type` argument.

-   Create a new naive Bayes model with the Laplace smoothing parameter
    set to `1`. You can do this by setting the `laplace` argument in
    your call to `naive_bayes()`. Save this as `locmodel2`.

-   See how the new predicted probabilities compare by using the
    `predict()` function on your new model.

``` r
weekend_afternoon <- locations[85,]

# Observe the predicted probabilities for a weekend afternoon
predict(locmodel, weekend_afternoon, type = "prob")
```

    ## Warning: predict.naive_bayes(): more features in the newdata are provided as
    ## there are probability tables in the object. Calculation is performed based on
    ## features to be found in the tables.

    ##                  appointment       campus      home      office restaurant
    ## [1,] 1.34862e-08  0.02697241 0.0004989896 0.8458048 0.003358065  0.1053032
    ##           store      theater
    ## [1,] 0.01798161 8.091723e-05

``` r
# Build a new model using the Laplace correction
locmodel2 <- naive_bayes(location ~ daytype  + hourtype, data = locations, laplace = 1)

# Observe the new predicted probabilities for a weekend afternoon
predict(locmodel2, weekend_afternoon, type = "prob")
```

    ## Warning: predict.naive_bayes(): more features in the newdata are provided as
    ## there are probability tables in the object. Calculation is performed based on
    ## features to be found in the tables.

    ##                   appointment      campus      home      office restaurant
    ## [1,] 0.0008794742  0.02198685 0.006586152 0.8303895 0.008747585  0.1040566
    ##           store     theater
    ## [1,] 0.02042795 0.006925859

### **`09-Understanding the Laplace correction`**

By default, the `naive_bayes()` function in the `naivebayes` package
does not use the Laplace correction. What is the risk of leaving this
parameter unset?

-   Some potential outcomes may be predicted to be impossible.
    [✅](https://emojipedia.org/check-mark-button/)

-   The algorithm may have a divide by zero error.

-   Naive Bayes will ignore features with zero values.

-   The model may not estimate probabilities for some cases.

Correct! The small probability added to every outcome ensures that they
are all possible even if never previously observed.

### **`10-Handling numeric predictors`**

Numeric data is often **binned** before it is used with Naive Bayes.
Which of these is not an example of binning?

-   Age values recoded as ‘child’ or ‘adult’ categories

-   Geographic coordinates recoded into geographic regions (West, East,
    etc.)

-   Test scores divided into four groups by percentile

-   Income values standardized to follow a normal bell curve
    [✅](https://emojipedia.org/check-mark-button/)

### **`The End`**
