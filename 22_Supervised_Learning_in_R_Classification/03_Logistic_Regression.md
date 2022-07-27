Logistic Regression
================
Mohamad Osman
2022-07-27

# Section 03: Logistic Regression

### **`01-Building simple logistic regression models`**

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

The `donors` dataset contains 93,462 examples of people mailed in a
fundraising solicitation for paralyzed military veterans. The `donated`
column is `1` if the person made a donation in response to the mailing
and `0` otherwise. This binary outcome will be the *dependent* variable
for the logistic regression model.

The remaining columns are features of the prospective donors that may
influence their donation behavior. These are the model’s *independent
variables*.

When building a regression model, it is often helpful to form a
hypothesis about which independent variables will be predictive of the
dependent variable. The `bad_address` column, which is set to `1` for an
invalid mailing address and `0` otherwise, seems like it might reduce
the chances of a donation. Similarly, one might suspect that religious
interest (`interest_religion`) and interest in veterans affairs
(`interest_veterans`) would be associated with greater charitable
giving.

In this exercise, you will use these three factors to create a simple
model of donation behavior. The dataset `donors` is available for you to
use.

``` r
donors_URL <- "https://assets.datacamp.com/production/repositories/718/datasets/9055dac929e4515286728a2a5dae9f25f0e4eff6/donors.csv"

donors <- read_csv(donors_URL)
```

    ## Rows: 93462 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): recency, frequency, money
    ## dbl (10): donated, veteran, bad_address, age, has_children, wealth_rating, i...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
head(donors)
```

    ## # A tibble: 6 × 13
    ##   donated veteran bad_address   age has_children wealth_rating interest_veterans
    ##     <dbl>   <dbl>       <dbl> <dbl>        <dbl>         <dbl>             <dbl>
    ## 1       0       0           0    60            0             0                 0
    ## 2       0       0           0    46            1             3                 0
    ## 3       0       0           0    NA            0             1                 0
    ## 4       0       0           0    70            0             2                 0
    ## 5       0       0           0    78            1             1                 0
    ## 6       0       0           0    NA            0             0                 0
    ## # … with 6 more variables: interest_religion <dbl>, pet_owner <dbl>,
    ## #   catalog_shopper <dbl>, recency <chr>, frequency <chr>, money <chr>

-   Examine `donors` using the `str()` function.

-   Count the number of occurrences of each level of the `donated`
    variable using the `table()` function.

-   Fit a logistic regression model using the formula interface and the
    three independent variables described above.

    -   Call `glm()` with the formula as its first argument and the data
        frame as the `data` argument.

    -   Save the result as `donation_model`.

-   Summarize the model object with `summary()`.

``` r
# Examine the dataset to identify potential independent variables
str(donors)
```

    ## spec_tbl_df [93,462 × 13] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ donated          : num [1:93462] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ veteran          : num [1:93462] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ bad_address      : num [1:93462] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ age              : num [1:93462] 60 46 NA 70 78 NA 38 NA NA 65 ...
    ##  $ has_children     : num [1:93462] 0 1 0 0 1 0 1 0 0 0 ...
    ##  $ wealth_rating    : num [1:93462] 0 3 1 2 1 0 2 3 1 0 ...
    ##  $ interest_veterans: num [1:93462] 0 0 0 0 0 0 0 0 0 0 ...
    ##  $ interest_religion: num [1:93462] 0 0 0 0 1 0 0 0 0 0 ...
    ##  $ pet_owner        : num [1:93462] 0 0 0 0 0 0 1 0 0 0 ...
    ##  $ catalog_shopper  : num [1:93462] 0 0 0 0 1 0 0 0 0 0 ...
    ##  $ recency          : chr [1:93462] "CURRENT" "CURRENT" "CURRENT" "CURRENT" ...
    ##  $ frequency        : chr [1:93462] "FREQUENT" "FREQUENT" "FREQUENT" "FREQUENT" ...
    ##  $ money            : chr [1:93462] "MEDIUM" "HIGH" "MEDIUM" "MEDIUM" ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   donated = col_double(),
    ##   ..   veteran = col_double(),
    ##   ..   bad_address = col_double(),
    ##   ..   age = col_double(),
    ##   ..   has_children = col_double(),
    ##   ..   wealth_rating = col_double(),
    ##   ..   interest_veterans = col_double(),
    ##   ..   interest_religion = col_double(),
    ##   ..   pet_owner = col_double(),
    ##   ..   catalog_shopper = col_double(),
    ##   ..   recency = col_character(),
    ##   ..   frequency = col_character(),
    ##   ..   money = col_character()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

``` r
# Explore the dependent variable
table(donors$donated)
```

    ## 
    ##     0     1 
    ## 88751  4711

``` r
# Build the donation model
donation_model <- glm(donated ~ bad_address + interest_religion + interest_veterans, 
                      data = donors, family = "binomial")

# Summarize the model results
summary(donation_model)
```

    ## 
    ## Call:
    ## glm(formula = donated ~ bad_address + interest_religion + interest_veterans, 
    ##     family = "binomial", data = donors)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.3480  -0.3192  -0.3192  -0.3192   2.5678  
    ## 
    ## Coefficients:
    ##                   Estimate Std. Error  z value Pr(>|z|)    
    ## (Intercept)       -2.95139    0.01652 -178.664   <2e-16 ***
    ## bad_address       -0.30780    0.14348   -2.145   0.0319 *  
    ## interest_religion  0.06724    0.05069    1.327   0.1847    
    ## interest_veterans  0.11009    0.04676    2.354   0.0186 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 37330  on 93461  degrees of freedom
    ## Residual deviance: 37316  on 93458  degrees of freedom
    ## AIC: 37324
    ## 
    ## Number of Fisher Scoring iterations: 5

### **`02-Making a binary prediction`**

In the previous exercise, you used the `glm()` function to build a
logistic regression model of donor behavior. As with many of R’s machine
learning methods, you can apply the `predict()` function to the model
object to forecast future behavior. By default, `predict()` outputs
predictions in terms of *log odds* unless `type = "response"` is
specified. This converts the log odds to *probabilities*.

Because a logistic regression model estimates the *probability* of the
outcome, it is up to you to determine the threshold at which the
probability implies action. One must balance the extremes of being too
cautious versus being too aggressive. For example, if you were to
solicit only the people with a 99% or greater donation probability, you
may miss out on many people with lower estimated probabilities that
still choose to donate. This balance is particularly important to
consider for severely imbalanced outcomes, such as in this dataset where
donations are relatively rare.

The dataset `donors` and the model `donation_model` are available for
you to use.

-   Use the `predict()` function to estimate each person’s donation
    probability. Use the `type` argument to get probabilities. Assign
    the predictions to a new column called `donation_prob`.

-   Find the actual probability that an average person would donate by
    passing the `mean()` function the appropriate column of the `donors`
    data frame.

-   Use `ifelse()` to predict a donation if their predicted donation
    probability is greater than average. Assign the predictions to a new
    column called `donation_pred`.

-   Use the `mean()` function to calculate the model’s accuracy.

``` r
# Estimate the donation probability
donors$donation_prob <- predict(donation_model, donors, type = "response")

# Find the donation probability of the average prospect
mean(donors$donated)
```

    ## [1] 0.05040551

``` r
# Predict a donation if probability of donation is greater than average (0.0504)
donors$donation_pred <- ifelse(donors$donation_prob  > 0.0504, 1, 0)

# Calculate the model's accuracy
mean(donors$donated == donors$donation_pred)
```

    ## [1] 0.794815

Nice work! With an accuracy of nearly `80%`, the model seems to be doing
its job. But is it too good to be true?

### **`03-The limitations of accuracy`**

In the previous exercise, you found that the logistic regression model
made a correct prediction nearly 80% of the time. Despite this
relatively high accuracy, the result is misleading due to the rarity of
outcome being predicted.

The `donors` dataset is available to use. What would the accuracy have
been if a model had simply predicted “no donation” for each person?

-   80%

-   85%

-   90%

-   95% [✅](https://emojipedia.org/check-mark-button/)

    Correct! With an accuracy of only 80%, the model is actually
    performing WORSE than if it were to predict non-donor for every
    record.

### **`04-Calculating ROC Curves and AUC`**

The previous exercises have demonstrated that accuracy is a very
misleading measure of model performance on imbalanced datasets. Graphing
the model’s performance better illustrates the tradeoff between a model
that is overly aggressive and one that is overly passive.

In this exercise you will create a ROC curve and compute the area under
the curve (AUC) to evaluate the logistic regression model of donations
you built earlier.

The dataset `donors` with the column of predicted probabilities,
`donation_prob`, has been loaded for you.

-   Load the `pROC` package.

-   Create a ROC curve with `roc()` and the columns of actual and
    predicted donations. Store the result as `ROC`.

-   Use `plot()` to draw the `ROC` object. Specify `col = "blue"` to
    color the curve blue.

-   Compute the area under the curve with `auc()`.

``` r
# Load the pROC package
library(pROC)
```

    ## Type 'citation("pROC")' for a citation.

    ## 
    ## Attaching package: 'pROC'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     cov, smooth, var

``` r
# Create a ROC curve
ROC <- roc(donors$donated, donors$donation_prob)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
# Plot the ROC curve
plot(ROC, col =  "blue")
```

![](03_Logistic_Regression_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# Calculate the area under the curve (AUC)
auc(ROC)
```

    ## Area under the curve: 0.5102

### **`05-Comparing ROC curves`**

Which of the following ROC curves illustrates the best model?

<img src="images/chrome_li0S9csMP7.png" width="442" />

-   AUC 0.55

-   AUC 0.59

-   AUC 0.62

-   I need more information!
    [✅](https://emojipedia.org/check-mark-button/)

### **`06-Coding categorical features`**

Sometimes a dataset contains numeric values that represent a categorical
feature.

In the `donors` dataset, `wealth_rating` uses numbers to indicate the
donor’s wealth level:

-   **0** = Unknown

-   **1** = Low

-   **2** = Medium

-   **3** = High

This exercise illustrates how to prepare this type of categorical
feature and examines its impact on a logistic regression model. The
`donors` data frame is available for you to use.

-   Create a factor `wealth_levels` from the numeric `wealth_rating`
    with labels as shown above by passing the `factor()` function the
    column you want to convert, the individual levels, and the labels.

-   Use `relevel()` to change the reference category to `Medium`. The
    first argument should be your new `factor` column.

-   Build a logistic regression model using the column `wealth_levels`
    to predict `donated` and display the result with `summary()`.

``` r
# Convert the wealth rating to a factor
donors$wealth_levels <- factor(donors$wealth_rating, levels = c(0,1,2,3),labels = c("Unknown", "Low", "Medium", "High"))

# Use relevel() to change reference category
donors$wealth_levels <- relevel(donors$wealth_levels, ref = "Medium")

# See how our factor coding impacts the model
summary(glm(donated ~ wealth_levels, data = donors, family = "binomial"))
```

    ## 
    ## Call:
    ## glm(formula = donated ~ wealth_levels, family = "binomial", data = donors)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.3320  -0.3243  -0.3175  -0.3175   2.4582  
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)          -2.91894    0.03614 -80.772   <2e-16 ***
    ## wealth_levelsUnknown -0.04373    0.04243  -1.031    0.303    
    ## wealth_levelsLow     -0.05245    0.05332  -0.984    0.325    
    ## wealth_levelsHigh     0.04804    0.04768   1.008    0.314    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 37330  on 93461  degrees of freedom
    ## Residual deviance: 37323  on 93458  degrees of freedom
    ## AIC: 37331
    ## 
    ## Number of Fisher Scoring iterations: 5

### `07-Handling missing data`

Some of the prospective donors have missing `age` data. Unfortunately, R
will exclude any cases with `NA` values when building a regression
model.

One workaround is to replace, or **impute**, the missing values with an
estimated value. After doing so, you may also create a missing data
indicator to model the possibility that cases with missing data are
different in some way from those without.

The data frame `donors` is loaded in your workspace.

-   Use `summary()` on `donors$age` to find the average age of prospects
    with non-missing data.

-   Use `ifelse()` and the test `is.na(donors$age)` to impute the
    average (rounded to 2 decimal places) for cases with missing `age`.
    Be sure to also ignore `NA`s.

-   Create a binary dummy variable named `missing_age` indicating the
    presence of missing data using another `ifelse()` call and the same
    test.

``` r
# Find the average age among non-missing values
summary(donors$age)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
    ##    1.00   48.00   62.00   61.65   75.00   98.00   22546

``` r
# Impute missing age values with the mean age
donors$imputed_age <- ifelse(is.na(donors$age) == TRUE,
round(mean(donors$age, na.rm = TRUE),2), donors$age)
head(donors$imputed_age)
```

    ## [1] 60.00 46.00 61.65 70.00 78.00 61.65

``` r
# Create missing value indicator for age
donors$missing_age <- ifelse(is.na(donors$age) == TRUE, 1, 0)
head(donors$missing_age)
```

    ## [1] 0 0 1 0 0 1

### `08-Understanding missing value indicators`

A missing value indicator provides a reminder that, before imputation,
there was a missing value present on the record.

Why is it often useful to include this indicator as a predictor in the
model?

-   A missing value may represent a unique category by itself

-   There may be an important difference between records with and
    without missing data

-   Whatever caused the missing value may also be related to the outcome

-   All of the above [✅](https://emojipedia.org/check-mark-button/)

Yes! Sometimes a missing value says a great deal about the record it
appeared on!

### **`09-Building a more sophisticated model`**

One of the best predictors of future giving is a history of recent,
frequent, and large gifts. In marketing terms, this is known as R/F/M:

-   Recency

-   Frequency

-   Money

Donors that haven’t given both recently and frequently may be especially
likely to give again; in other words, the *combined* impact of recency
and frequency may be greater than the sum of the separate effects.

Because these predictors together have a greater impact on the dependent
variable, their joint effect must be modeled as an interaction. The
`donors` dataset has been loaded for you.

-   Create a logistic regression model of `donated` as a function of
    `money` plus the interaction of `recency` and `frequency`. Use `*`
    to add the interaction term.

-   Examine the model’s `summary()` to confirm the interaction effect
    was added.

-   Save the model’s predicted probabilities as `rfm_prob`. Use the
    `predict()` function, and remember to set the `type` argument.

-   Plot a ROC curve by using the function `roc()`. Remember, this
    function takes the column of outcomes and the vector of predictions.

-   Compute the AUC for the new model with the function `auc()` and
    compare performance to the simpler model.

``` r
# Build a recency, frequency, and money (RFM) model
rfm_model <- glm(donated ~ money + recency * frequency, data = donors, family = "binomial")

# Summarize the RFM model to see how the parameters were coded
summary(rfm_model)
```

    ## 
    ## Call:
    ## glm(formula = donated ~ money + recency * frequency, family = "binomial", 
    ##     data = donors)
    ## 
    ## Deviance Residuals: 
    ##     Min       1Q   Median       3Q      Max  
    ## -0.3696  -0.3696  -0.2895  -0.2895   2.7924  
    ## 
    ## Coefficients:
    ##                                   Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)                       -3.01142    0.04279 -70.375   <2e-16 ***
    ## moneyMEDIUM                        0.36186    0.04300   8.415   <2e-16 ***
    ## recencyLAPSED                     -0.86677    0.41434  -2.092   0.0364 *  
    ## frequencyINFREQUENT               -0.50148    0.03107 -16.143   <2e-16 ***
    ## recencyLAPSED:frequencyINFREQUENT  1.01787    0.51713   1.968   0.0490 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 37330  on 93461  degrees of freedom
    ## Residual deviance: 36938  on 93457  degrees of freedom
    ## AIC: 36948
    ## 
    ## Number of Fisher Scoring iterations: 6

``` r
# Compute predicted probabilities for the RFM model
rfm_prob <- predict(rfm_model, donors, type = "response")

# Plot the ROC curve and find AUC for the new model
library(pROC)
ROC <- roc(donors$donated, rfm_prob)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
plot(ROC, col = "red")
```

![](03_Logistic_Regression_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
auc(ROC)
```

    ## Area under the curve: 0.5785

Great work! Based on the ROC curve, you’ve confirmed that past giving
patterns are certainly predictive of future giving.

### **`10-The dangers of stepwise regression`**

In spite of its utility for feature selection, stepwise regression is
not frequently used in disciplines outside of machine learning due to
some important caveats. Which of these is NOT one of these concerns?

-   It is not guaranteed to find the best possible model

<!-- -->

-   A stepwise model’s predictions can not be trusted
    [✅](https://emojipedia.org/check-mark-button/)

-   The stepwise regression procedure violates some statistical
    assumptions

-   It can result in a model that makes little sense in the real world

Correct! Though stepwise regression is frowned upon, it may still be
useful for building predictive models in the absence of another starting
place.

### **`11-Building a stepwise regression model`**

In the absence of subject-matter expertise, **stepwise regression** can
assist with the search for the most important predictors of the outcome
of interest.

In this exercise, you will use a forward stepwise approach to add
predictors to the model one-by-one until no additional benefit is seen.
The `donors` dataset has been loaded for you.

-   Use the R formula interface with `glm()` to specify the base model
    with no predictors. Set the explanatory variable equal to `1`.

-   Use the R formula interface again with `glm()` to specify the model
    with all predictors.

-   Apply `step()` to these models to perform forward stepwise
    regression. Set the first argument to `null_model` and set
    `direction = "forward"`. This might take a while (up to 10 or 15
    seconds) as your computer has to fit quite a few different models to
    perform stepwise selection.

-   Create a vector of predicted probabilities using the `predict()`
    function.

-   Plot the ROC curve with `roc()` and `plot()` and compute the AUC of
    the stepwise model with `auc()`.

``` r
# Specify a null model with no predictors
null_model <- glm(donated ~ 1, data = donors, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(donated ~ ., data = donors, family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
```

    ## Start:  AIC=37332.13
    ## donated ~ 1

    ## Warning in add1.glm(fit, scope$add, scale = scale, trace = trace, k = k, : using
    ## the 70916/93462 rows from a combined fit

    ##                     Df Deviance   AIC
    ## + frequency          1    28502 37122
    ## + money              1    28621 37241
    ## + wealth_rating      1    28705 37326
    ## + has_children       1    28705 37326
    ## + age                1    28707 37328
    ## + imputed_age        1    28707 37328
    ## + wealth_levels      3    28704 37328
    ## + interest_veterans  1    28709 37330
    ## + donation_prob      1    28710 37330
    ## + donation_pred      1    28710 37330
    ## + catalog_shopper    1    28710 37330
    ## + pet_owner          1    28711 37331
    ## <none>                    28714 37332
    ## + interest_religion  1    28712 37333
    ## + recency            1    28713 37333
    ## + bad_address        1    28714 37334
    ## + veteran            1    28714 37334
    ## 
    ## Step:  AIC=37024.77
    ## donated ~ frequency

    ## Warning in add1.glm(fit, scope$add, scale = scale, trace = trace, k = k, : using
    ## the 70916/93462 rows from a combined fit

    ##                     Df Deviance   AIC
    ## + money              1    28441 36966
    ## + wealth_rating      1    28493 37018
    ## + wealth_levels      3    28490 37019
    ## + has_children       1    28494 37019
    ## + donation_prob      1    28498 37023
    ## + interest_veterans  1    28498 37023
    ## + catalog_shopper    1    28499 37024
    ## + donation_pred      1    28499 37024
    ## + age                1    28499 37024
    ## + imputed_age        1    28499 37024
    ## + pet_owner          1    28499 37024
    ## <none>                    28502 37025
    ## + interest_religion  1    28501 37026
    ## + recency            1    28501 37026
    ## + bad_address        1    28502 37026
    ## + veteran            1    28502 37027
    ## 
    ## Step:  AIC=36949.71
    ## donated ~ frequency + money

    ## Warning in add1.glm(fit, scope$add, scale = scale, trace = trace, k = k, : using
    ## the 70916/93462 rows from a combined fit

    ##                     Df Deviance   AIC
    ## + wealth_levels      3    28427 36942
    ## + wealth_rating      1    28431 36942
    ## + has_children       1    28432 36943
    ## + interest_veterans  1    28438 36948
    ## + donation_prob      1    28438 36949
    ## + catalog_shopper    1    28438 36949
    ## + donation_pred      1    28439 36949
    ## + age                1    28439 36949
    ## + imputed_age        1    28439 36949
    ## + pet_owner          1    28439 36949
    ## <none>                    28441 36950
    ## + interest_religion  1    28440 36951
    ## + recency            1    28441 36951
    ## + bad_address        1    28441 36951
    ## + veteran            1    28441 36952
    ## 
    ## Step:  AIC=36945.48
    ## donated ~ frequency + money + wealth_levels

    ## Warning in add1.glm(fit, scope$add, scale = scale, trace = trace, k = k, : using
    ## the 70916/93462 rows from a combined fit

    ##                     Df Deviance   AIC
    ## + has_children       1    28416 36937
    ## + age                1    28424 36944
    ## + imputed_age        1    28424 36944
    ## + interest_veterans  1    28424 36945
    ## + donation_prob      1    28424 36945
    ## + catalog_shopper    1    28425 36945
    ## + donation_pred      1    28425 36945
    ## <none>                    28427 36945
    ## + pet_owner          1    28425 36946
    ## + interest_religion  1    28426 36947
    ## + recency            1    28427 36947
    ## + bad_address        1    28427 36947
    ## + veteran            1    28427 36947
    ## 
    ## Step:  AIC=36938.4
    ## donated ~ frequency + money + wealth_levels + has_children

    ## Warning in add1.glm(fit, scope$add, scale = scale, trace = trace, k = k, : using
    ## the 70916/93462 rows from a combined fit

    ##                     Df Deviance   AIC
    ## + pet_owner          1    28413 36937
    ## + donation_prob      1    28413 36937
    ## + catalog_shopper    1    28413 36937
    ## + interest_veterans  1    28413 36937
    ## + donation_pred      1    28414 36938
    ## <none>                    28416 36938
    ## + interest_religion  1    28415 36939
    ## + age                1    28416 36940
    ## + imputed_age        1    28416 36940
    ## + recency            1    28416 36940
    ## + bad_address        1    28416 36940
    ## + veteran            1    28416 36940
    ## 
    ## Step:  AIC=36932.25
    ## donated ~ frequency + money + wealth_levels + has_children + 
    ##     pet_owner

    ## Warning in add1.glm(fit, scope$add, scale = scale, trace = trace, k = k, : using
    ## the 70916/93462 rows from a combined fit

    ##                     Df Deviance   AIC
    ## <none>                    28413 36932
    ## + donation_prob      1    28411 36932
    ## + interest_veterans  1    28411 36932
    ## + catalog_shopper    1    28412 36933
    ## + donation_pred      1    28412 36933
    ## + age                1    28412 36933
    ## + imputed_age        1    28412 36933
    ## + recency            1    28413 36934
    ## + interest_religion  1    28413 36934
    ## + bad_address        1    28413 36934
    ## + veteran            1    28413 36934

``` r
# Estimate the stepwise donation probability
step_prob <- predict(step_model, donors, type = "response")

# Plot the ROC of the stepwise model
library(pROC)
ROC <- roc(donors$donated, step_prob)
```

    ## Setting levels: control = 0, case = 1

    ## Setting direction: controls < cases

``` r
plot(ROC, col = "red")
```

![](03_Logistic_Regression_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
auc(ROC)
```

    ## Area under the curve: 0.5849

Fantastic work! Despite the caveats of stepwise regression, it seems to
have resulted in a relatively strong model!

### **`The End`**
