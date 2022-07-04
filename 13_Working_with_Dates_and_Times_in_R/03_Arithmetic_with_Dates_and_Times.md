Arithmetic with Dates and Times
================
Mohamad Osman
2022-06-29

# Section 03: Arithmetic with Dates and Times

### `01-How long has it been?`

To get finer control over a difference between datetimes use the `base`
function
[**`difftime()`**](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/difftime).
For example instead of `time1 - time2`, you use
`difftime(time1, time2)`.

[**`difftime()`**](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/difftime)
takes an argument `units` which specifies the units for the difference.
Your options are `"secs"`, `"mins"`, `"hours"`, `"days"`, or `"weeks"`.

To practice you’ll find the time since the first man stepped on the
moon. You’ll also see the `lubridate` functions `today()` and
[**`now()`**](https://www.rdocumentation.org/packages/lubridate/versions/1.8.0/topics/now)
which when called with no arguments return the current date and time in
your system’s timezone.

-   Apollo 11 landed on July 20, 1969. Use `difftime()` to find the
    number of days between `today()` and `date_landing`.

-   Neil Armstrong stepped onto the surface at 02:56:15 UTC. Use
    `difftime()` to find the number of seconds between `now()` and
    `moment_step`.

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
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

``` r
# The date of landing and moment of step
date_landing <- mdy("July 20, 1969")
moment_step <- mdy_hms("July 20, 1969, 02:56:15", tz = "UTC")

# How many days since the first man on the moon?
difftime(today(), date_landing, units = "days")
```

    ## Time difference of 19342 days

``` r
# How many seconds since the first man on the moon?
difftime( now(), moment_step, units = "secs")
```

    ## Time difference of 1671186857 secs

### **`02-How many seconds are in a day?`**

How many seconds are in a day? There are 24 hours in a day, 60 minutes
in an hour, and 60 seconds in a minute, so there should be `24*60*60` =
`86400` seconds, right?

Not always! In this exercise you’ll see a counter example, can you
figure out what is going on?

We’ve put code to define three times in your script - noon on March
11th, March 12th, and March 13th in 2017 in the US Pacific timezone.

-   Find the difference in time between `mar_13` and `mar_12` in
    seconds. *This should match your intuition*.

-   Now, find the difference in time between `mar_12` and `mar_11` in
    seconds. *Surprised?*

``` r
# Three dates
mar_11 <- ymd_hms("2017-03-11 12:00:00", 
  tz = "America/Los_Angeles")
mar_12 <- ymd_hms("2017-03-12 12:00:00", 
  tz = "America/Los_Angeles")
mar_13 <- ymd_hms("2017-03-13 12:00:00", 
  tz = "America/Los_Angeles")

# Difference between mar_13 and mar_12 in seconds
difftime(mar_13, mar_12, units = "secs")
```

    ## Time difference of 86400 secs

``` r
# Difference between mar_12 and mar_11 in seconds
difftime(mar_12, mar_11, units = "secs")
```

    ## Time difference of 82800 secs

Good work. Why would a day only have 82800 seconds? At 2am on Mar 12th
2017, Daylight Savings started in the Pacific timezone. That means a
whole hour of seconds gets skipped between noon on the 11th and noon on
the 12th.

### **`03-Adding or subtracting a time span to a datetime`**

-   It’s Monday Aug 27th 2018 at 2pm and you want to remind yourself
    this time next week to send an email. Add a period of one week to
    `mon_2pm`.

-   It’s Tuesday Aug 28th 2018 at 9am and you are starting some code
    that usually takes about 81 hours to run. When will it finish? Add a
    duration of 81 hours to `tue_9am`.

-   What were you doing five years ago? Subtract a period of 5 years
    from `today()`.

-   Subtract a duration of 5 years from `today()`. *Will this give a
    different date?*

``` r
# Add a period of one week to mon_2pm
mon_2pm <- dmy_hm("27 Aug 2018 14:00")
mon_2pm + ddays(7)
```

    ## [1] "2018-09-03 14:00:00 UTC"

``` r
# Add a duration of 81 hours to tue_9am
tue_9am <- dmy_hm("28 Aug 2018 9:00")
tue_9am + dhours(81)
```

    ## [1] "2018-08-31 18:00:00 UTC"

``` r
# Subtract a period of five years from today()
today() - years(x = 5)
```

    ## [1] "2017-07-04"

``` r
# Subtract a duration of five years from today()
today() - dyears(5)
```

    ## [1] "2017-07-03 18:00:00 UTC"

### **`04-Arithmetic with timespans`**

You can add and subtract timespans to create different length timespans,
and even multiply them by numbers. For example, to create a duration of
three days and three hours you could do: `ddays(3) + dhours(3)`, or
`3*ddays(1) + 3*dhours(1)` or even `3*(ddays(1) + dhours(1))`.

There was an eclipse over North America on 2017-08-21 at 18:26:40. It’s
possible to predict the next eclipse with similar geometry by
calculating the time and date one
[**Saros**](https://eclipse.gsfc.nasa.gov/SEsaros/SEsaros.html) in the
future. A Saros is a length of time that corresponds to 223 Synodic
months, a Synodic month being the period of the Moon’s phases, a
duration of 29 days, 12 hours, 44 minutes and 3 seconds.

Do just that in this exercise

-   Create a duration corresponding to one Synodic Month: 29 days, 12
    hours, 44 minutes and 3 seconds.

-   Create a duration corresponding to one Saros by multiplying
    `synodic` by 223.

-   Add `saros` to `eclipse_2017` to predict the next eclipse.

``` r
# Time of North American Eclipse 2017
eclipse_2017 <- ymd_hms("2017-08-21 18:26:40")

# Duration of 29 days, 12 hours, 44 mins and 3 secs
synodic <- ddays(29) + dhours(12) + dminutes(44) + dseconds(3)

# 223 synodic months
saros <- 223 * synodic

# Add saros to eclipse_2017
saros + eclipse_2017
```

    ## [1] "2035-09-02 02:09:49 UTC"

### **`05-Generating sequences of datetimes`**

By combining addition and multiplication with sequences you can generate
sequences of datetimes. For example, you can generate a sequence of
periods from 1 day up to 10 days with,

    1:10 * days(1)

Then by adding this sequence to a specific datetime, you can construct a
sequence of datetimes from 1 day up to 10 days into the future

    today() + 1:10 * days(1)

-   Create `today_8am()` by adding a period of 8 hours to `today()`

-   Create a sequence of periods from one period of two weeks, up to 26
    periods of two weeks.

-   Add `every_two_weeks` to `today_8am`.

``` r
# Add a period of 8 hours to today
today_8am <- today() + hours(8)

# Sequence of two weeks from 1 to 26
every_two_weeks <- 1:26 * weeks(1)

# Create datetime for every two weeks for a year
today_8am + every_two_weeks
```

    ##  [1] "2022-07-11 08:00:00 UTC" "2022-07-18 08:00:00 UTC"
    ##  [3] "2022-07-25 08:00:00 UTC" "2022-08-01 08:00:00 UTC"
    ##  [5] "2022-08-08 08:00:00 UTC" "2022-08-15 08:00:00 UTC"
    ##  [7] "2022-08-22 08:00:00 UTC" "2022-08-29 08:00:00 UTC"
    ##  [9] "2022-09-05 08:00:00 UTC" "2022-09-12 08:00:00 UTC"
    ## [11] "2022-09-19 08:00:00 UTC" "2022-09-26 08:00:00 UTC"
    ## [13] "2022-10-03 08:00:00 UTC" "2022-10-10 08:00:00 UTC"
    ## [15] "2022-10-17 08:00:00 UTC" "2022-10-24 08:00:00 UTC"
    ## [17] "2022-10-31 08:00:00 UTC" "2022-11-07 08:00:00 UTC"
    ## [19] "2022-11-14 08:00:00 UTC" "2022-11-21 08:00:00 UTC"
    ## [21] "2022-11-28 08:00:00 UTC" "2022-12-05 08:00:00 UTC"
    ## [23] "2022-12-12 08:00:00 UTC" "2022-12-19 08:00:00 UTC"
    ## [25] "2022-12-26 08:00:00 UTC" "2023-01-02 08:00:00 UTC"

### **`06-The tricky thing about months`**

What should `ymd("2018-01-31") + months(1)` return? Should it be 30, 31
or 28 days in the future? Try it. In general `lubridate` returns the
same *day of the month* in the next month, but since the 31st of
February doesn’t exist `lubridate` returns a missing value, `NA`.

There are alternative addition and subtraction operators: `%m+%` and
`%m-%` that have different behavior. Rather than returning an `NA` for a
non-existent date, they roll back to the last existing date.

You’ll explore their behavior by trying to generate a sequence for the
last day in every month this year.

We’ve put `jan_31`, the date for January 31st this year in your
workspace.

-   Start by creating a sequence of 1 to 12 periods of 1 month.

-   Add `month_seq` to `jan_31`. *Notice what happens to any month where
    the 31st doesn’t exist*

-   Now add `month_seq` to `jan_31` using the `%m+%` operator.

-   Try subtracting `month_seq` from `jan_31` using the `%m-%` operator.

``` r
# A sequence of 1 to 12 periods of 1 month
month_seq <- 1:12 * months(1)
jan_31 <- as.Date("2022-01-31")
# Add 1 to 12 months to jan_31
month_seq + jan_31
```

    ##  [1] NA           "2022-03-31" NA           "2022-05-31" NA          
    ##  [6] "2022-07-31" "2022-08-31" NA           "2022-10-31" NA          
    ## [11] "2022-12-31" "2023-01-31"

``` r
print("--------------------------------------")
```

    ## [1] "--------------------------------------"

``` r
# Replace + with %m+%
month_seq %m+% jan_31
```

    ##  [1] "2022-02-28" "2022-03-31" "2022-04-30" "2022-05-31" "2022-06-30"
    ##  [6] "2022-07-31" "2022-08-31" "2022-09-30" "2022-10-31" "2022-11-30"
    ## [11] "2022-12-31" "2023-01-31"

``` r
print("--------------------------------------")
```

    ## [1] "--------------------------------------"

``` r
# Replace + with %m-%
month_seq %m-% jan_31
```

    ##  [1] "2021-12-31" "2021-11-30" "2021-10-31" "2021-09-30" "2021-08-31"
    ##  [6] "2021-07-31" "2021-06-30" "2021-05-31" "2021-04-30" "2021-03-31"
    ## [11] "2021-02-28" "2021-01-31"

### **`07-Examining intervals. Reigns of kings and queens`**

You can create an interval by using the operator `%--%` with two
datetimes. For example `ymd("2001-01-01") %--% ymd("2001-12-31")`
creates an interval for the year of 2001.

Once you have an interval you can find out certain properties like its
start, end and length with `int_start()`, `int_end()` and `int_length()`
respectively.

Practice by exploring the reigns of kings and queens of Britain (and its
historical dominions).

We’ve put the data `monarchs` in your workspace.

-   Print `monarchs` to take a look at the data

-   Create a new column called `reign` that is an interval between
    `from` and `to`.

-   Create another new column, `length`, that is the interval length of
    `reign`. The rest of the pipeline we’ve filled in for you, it
    arranges by decreasing `length` and selects the `name`, `length` and
    `dominion` columns.

``` r
library(readxl)


monarchs_file = file.path("..", "00_Datasets", "monarchs.xlsx")

monarchs <- read_excel(monarchs_file, sheet = 1, col_names = TRUE)
monarchs$from <- as.POSIXct(monarchs$from, format = "%Y-%m-%d %H:%M:%S")
monarchs$to <- as.POSIXct(monarchs$to, format = "%Y-%m-%d %H:%M:%S")
```

``` r
# Print monarchs
 print(monarchs)
```

    ## # A tibble: 131 × 4
    ##    name                     from                to                  dominion 
    ##    <chr>                    <dttm>              <dttm>              <chr>    
    ##  1 Elizabeth II             1952-02-06 00:00:00 2022-07-04 00:00:00 United Ki
    ##  2 Victoria                 1837-06-20 00:00:00 1901-01-22 00:00:00 United Ki
    ##  3 George V                 1910-05-06 00:00:00 1936-01-20 00:00:00 United Ki
    ##  4 George III               1801-01-01 00:00:00 1820-01-29 00:00:00 United Ki
    ##  5 George VI                1936-12-11 00:00:00 1952-02-06 00:00:00 United Ki
    ##  6 George IV                1820-01-29 00:00:00 1830-06-26 00:00:00 United Ki
    ##  7 Edward VII               1901-01-22 00:00:00 1910-05-06 00:00:00 United Ki
    ##  8 William IV               1830-06-26 00:00:00 1837-06-20 00:00:00 United Ki
    ##  9 Edward VIII              1936-01-20 00:00:00 1936-12-11 00:00:00 United Ki
    ## 10 George III(also United ~ 1760-10-25 00:00:00 1801-01-01 00:00:00 Great Bri
    ## # … with 121 more rows

``` r
# Create an interval for reign
monarchs <- monarchs %>%
  mutate(reign = from %--% to) 

# Find the length of reign, and arrange
monarchs %>%
  mutate(length = int_length(reign)) %>% 
  arrange(desc(length)) %>%
  select(name, length, dominion)
```

    ## # A tibble: 131 × 3
    ##    name                   length dominion 
    ##    <chr>                   <dbl> <chr>    
    ##  1 Elizabeth II       2221862400 United Ki
    ##  2 Victoria           2006726400 United Ki
    ##  3 James VI           1820102400 Scotland 
    ##  4 Gruffudd ap Cynan  1767139200 Gwynedd  
    ##  5 Edward III         1590624000 England  
    ##  6 William I          1545868800 Scotland 
    ##  7 Llywelyn the Great 1428796800 Gwynedd  
    ##  8 Elizabeth I        1399507200 England  
    ##  9 Constantine II     1356912000 Scotland 
    ## 10 David II           1316304000 Scotland 
    ## # … with 121 more rows

### **`08-Comparing intervals and datetimes`**

The operator `%within%` tests if the datetime (or interval) on the left
hand side is within the interval of the right hand side. For example, if
`y2001` is the interval covering the year 2001,

    y2001 <- ymd("2001-01-01") %--% ymd("2001-12-31")

Then `ymd("2001-03-30") %within% y2001` will return `TRUE` and
`ymd("2002-03-30") %within% y2001` will return `FALSE`.

`int_overlaps()` performs a similar test, but will return true if two
intervals overlap at all.

We’ve put `halleys` a data set describing appearances of Halley’s comet
in your workspace.

-   Print `halleys` to examine the date. `perihelion_date` is the date
    the Comet is closest to the Sun. `start_date` and `end_date` are the
    range of dates the comet is visible from Earth.

-   Create a new column, `visible`, that is an interval from
    `start_date` to `end_date`.

-   You’ll work with one appearance, extract the 14th row of `halleys.`

-   Filter `monarchs` to those where `halleys_1066$perihelion_date` is
    within `reign`.

-   Filter `monarchs` to those where `halleys_1066$visible` overlaps
    `reign`.

``` r
halleys_file = file.path("..", "00_Datasets", "halleys.xlsx")

halleys <- read_excel(halleys_file, sheet = 1, col_names = TRUE)
halleys$perihelion_date <- as.Date(halleys$perihelion_date, format = "%Y-%m-%d")
halleys$start_date <- as.Date(halleys$start_date, format = "%Y-%m-%d")
halleys$end_date <- as.Date(halleys$end_date, format = "%Y-%m-%d")
halleys
```

    ## # A tibble: 27 × 6
    ##    designation     year perihelion_date start_date end_date   distance
    ##    <chr>          <dbl> <date>          <date>     <date>     <chr>   
    ##  1 1P/66 B1, 66      66 1966-01-26      1966-01-26 1966-01-26 NA      
    ##  2 1P/141 F1, 141   141 0141-03-25      0141-03-22 0141-03-25 NA      
    ##  3 1P/218 H1, 218   218 0218-04-06      0218-04-06 0218-05-17 NA      
    ##  4 1P/295 J1, 295   295 0295-04-07      0295-04-07 0295-04-20 NA      
    ##  5 1P/374 E1, 374   374 0374-02-13      0374-02-13 0374-02-16 0.09 AU 
    ##  6 1P/451 L1, 451   451 0451-07-03      0451-06-28 0451-07-03 NA      
    ##  7 1P/530 Q1, 530   530 0530-11-15      0530-09-27 0530-11-15 NA      
    ##  8 1P/607 H1, 607   607 0607-03-26      0607-03-15 0607-03-26 0.09 AU 
    ##  9 1P/684 R1, 684   684 0684-11-26      0684-10-02 0684-11-26 NA      
    ## 10 1P/760 K1, 760   760 0760-06-10      0760-05-20 0760-06-10 NA      
    ## # … with 17 more rows

``` r
# Print halleys
print(halleys)
```

    ## # A tibble: 27 × 6
    ##    designation     year perihelion_date start_date end_date   distance
    ##    <chr>          <dbl> <date>          <date>     <date>     <chr>   
    ##  1 1P/66 B1, 66      66 1966-01-26      1966-01-26 1966-01-26 NA      
    ##  2 1P/141 F1, 141   141 0141-03-25      0141-03-22 0141-03-25 NA      
    ##  3 1P/218 H1, 218   218 0218-04-06      0218-04-06 0218-05-17 NA      
    ##  4 1P/295 J1, 295   295 0295-04-07      0295-04-07 0295-04-20 NA      
    ##  5 1P/374 E1, 374   374 0374-02-13      0374-02-13 0374-02-16 0.09 AU 
    ##  6 1P/451 L1, 451   451 0451-07-03      0451-06-28 0451-07-03 NA      
    ##  7 1P/530 Q1, 530   530 0530-11-15      0530-09-27 0530-11-15 NA      
    ##  8 1P/607 H1, 607   607 0607-03-26      0607-03-15 0607-03-26 0.09 AU 
    ##  9 1P/684 R1, 684   684 0684-11-26      0684-10-02 0684-11-26 NA      
    ## 10 1P/760 K1, 760   760 0760-06-10      0760-05-20 0760-06-10 NA      
    ## # … with 17 more rows

``` r
# New column for interval from start to end date
halleys <- halleys %>% 
  mutate(visible = start_date  %--% end_date)

# The visitation of 1066
halleys_1066 <- halleys[14, ] 

# Monarchs in power on perihelion date
monarchs %>% 
  filter(halleys_1066$perihelion_date %within% reign) %>%
  select(name, from, to, dominion)
```

    ## # A tibble: 2 × 4
    ##   name        from                to                  dominion
    ##   <chr>       <dttm>              <dttm>              <chr>   
    ## 1 Harold II   1066-01-05 00:00:00 1066-10-14 00:00:00 England 
    ## 2 Malcolm III 1058-03-17 00:00:00 1093-11-13 00:00:00 Scotland

``` r
# Monarchs whose reign overlaps visible time
monarchs %>% 
  filter(int_overlaps(halleys_1066$visible, reign)) %>%
  select(name, from, to, dominion)
```

    ## # A tibble: 3 × 4
    ##   name                 from                to                  dominion
    ##   <chr>                <dttm>              <dttm>              <chr>   
    ## 1 Edward the Confessor 1042-06-08 00:00:00 1066-01-05 00:00:00 England 
    ## 2 Harold II            1066-01-05 00:00:00 1066-10-14 00:00:00 England 
    ## 3 Malcolm III          1058-03-17 00:00:00 1093-11-13 00:00:00 Scotland

**09-Converting to durations and periods**

Intervals are the most specific way to represent a span of time since
they retain information about the exact start and end moments. They can
be converted to periods and durations exactly: it’s possible to
calculate both the exact number of seconds elapsed between the start and
end date, as well as the perceived change in clock time.

To do so you use the
[**`as.period()`**](https://www.rdocumentation.org/packages/lubridate/versions/1.8.0/topics/as.period),
and
[**`as.duration()`**](https://www.rdocumentation.org/packages/lubridate/versions/1.8.0/topics/as.duration)
functions, parsing in an interval as the only argument.

Try them out to get better representations of the length of the monarchs
reigns.

-   Create new columns for `duration` and `period` that convert `reign`
    into the appropriate object.

-   Examine the `name`, `duration` and `period` columns.

``` r
# New columns for duration and period
monarchs <- monarchs %>%
  mutate(
    duration = as.duration(reign),
    period = as.period(reign)) 
    
# Examine results    
monarchs %>%
  select(name, duration, period)
```

    ## # A tibble: 131 × 3
    ##    name                     duration                   period             
    ##    <chr>                    <Duration>                 <Period>           
    ##  1 Elizabeth II             2221862400s (~70.41 years) 70y 4m 28d 0H 0M 0S
    ##  2 Victoria                 2006726400s (~63.59 years) 63y 7m 2d 0H 0M 0S 
    ##  3 George V                 811296000s (~25.71 years)  25y 8m 14d 0H 0M 0S
    ##  4 George III               601948800s (~19.07 years)  19y 0m 28d 0H 0M 0S
    ##  5 George VI                478224412s (~15.15 years)  15y 1m 26d 0H 0M 0S
    ##  6 George IV                328406400s (~10.41 years)  10y 4m 28d 0H 0M 0S
    ##  7 Edward VII               292982400s (~9.28 years)   9y 3m 14d 0H 0M 0S 
    ##  8 William IV               220406400s (~6.98 years)   6y 11m 25d 0H 0M 0S
    ##  9 Edward VIII              28166400s (~46.57 weeks)   10m 21d 0H 0M 0S   
    ## 10 George III(also United ~ 1268092800s (~40.18 years) 40y 2m 7d 0H 0M 0S 
    ## # … with 121 more rows

### `The End`

  

  

  

  

  

  

  
