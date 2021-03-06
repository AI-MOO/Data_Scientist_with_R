Problems in practice
================
Mohamad Osman
2022-07-04

# **Section 04: Problems in practice**

### **`01-Setting the timezone`**

If you import a datetime and it has the wrong timezone, you can set it
with
[**`force_tz()`**](https://www.rdocumentation.org/packages/lubridate/versions/1.8.0/topics/force_tz).
Pass in the datetime as the first argument and the appropriate timezone
to the `tzone` argument. Remember the timezone needs to be one from
`OlsonNames()`.

I’ve put the times as listed on the FIFA website for games 2 and 3 in
the group stage for New Zealand in your code.

-   Game 2 was played in Edmonton. Use `force_tz()` to set the timezone
    of game 2 to `"America/Edmonton"`.

-   Game 3 was played in Winnipeg. Use `force_tz()` to set the timezone
    of game 3 to `"America/Winnipeg"`.

-   Find out how long the team had to rest between the two games, by
    using `as.period()` on the interval between `game2_local` and
    `game3_local`.

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following objects are masked from 'package:base':
    ## 
    ##     date, intersect, setdiff, union

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
# Game2: CAN vs NZL in Edmonton
game2 <- mdy_hm("June 11 2015 19:00")

# Game3: CHN vs NZL in Winnipeg
game3 <- mdy_hm("June 15 2015 18:30")

# Set the timezone to "America/Edmonton"
game2_local <- force_tz(game2, tzone = "America/Edmonton")
game2_local
```

    ## [1] "2015-06-11 19:00:00 MDT"

``` r
# Set the timezone to "America/Winnipeg"
game3_local <- force_tz(game3, tzone = "America/Winnipeg")
game3_local
```

    ## [1] "2015-06-15 18:30:00 CDT"

``` r
# How long does the team have to rest?
as.period(game2_local %--% game3_local)
```

    ## [1] "3d 22H 30M 0S"

Great work! Edmonton and Winnipeg are in different timezones, so even
though the start times of the games only look 30 minutes apart, they are
in fact 1 hour and 30 minutes apart, and the team only has 3 days, 22
hours and 30 minutes to prepare.

### **`02-Viewing in a timezone`**

To view a datetime in another timezone use
[**`with_tz()`**](https://www.rdocumentation.org/packages/lubridate/versions/1.8.0/topics/with_tz).
The syntax of
[**`with_tz()`**](https://www.rdocumentation.org/packages/lubridate/versions/1.8.0/topics/with_tz)
is the same as
[**`force_tz()`**](https://www.rdocumentation.org/packages/lubridate/versions/1.8.0/topics/force_tz),
passing a datetime and set the `tzone` argument to the desired timezone.
Unlike
[**`force_tz()`**](https://www.rdocumentation.org/packages/lubridate/versions/1.8.0/topics/force_tz),
[**`with_tz()`**](https://www.rdocumentation.org/packages/lubridate/versions/1.8.0/topics/with_tz)
isn’t changing the underlying moment of time, just how it is displayed.

For example, the difference between `now()` displayed in the
“America/Los_Angeles” timezone and “Pacific/Auckland” timezone is 0:

    now <- now()
    with_tz(now, "America/Los_Angeles") - 
      with_tz(now,  "Pacific/Auckland")

Help me figure out when to tune into the games from the previous
exercise.

``` r
# What time is game2_local in NZ?
with_tz(game2_local, tzone = "Pacific/Auckland")
```

    ## [1] "2015-06-12 13:00:00 NZST"

``` r
# What time is game2_local in Corvallis, Oregon?
with_tz(game2_local, tzone = "America/Los_Angeles")
```

    ## [1] "2015-06-11 18:00:00 PDT"

``` r
# What time is game3_local in NZ?
with_tz(game3_local, tzone = "Pacific/Auckland")
```

    ## [1] "2015-06-16 11:30:00 NZST"

### **`03-Timezones in the weather data`**

``` r
# Import "akl_weather_hourly_2016.csv"
akl_weather_h_path <- file.path("..", "00_Datasets", "akl_weather_hourly_2016.csv")
akl_hourly_raw <- read_csv(akl_weather_h_path)
```

    ## Rows: 17454 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): weather, conditions, events
    ## dbl  (5): year, month, mday, temperature, humidity
    ## dttm (1): date_utc
    ## time (1): time
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Use make_date() to combine year, month and mday 
akl_hourly  <- akl_hourly_raw  %>% 
  mutate(date = make_date(year = year, month = month, day = mday))

# Parse datetime_string 
akl_hourly <- akl_hourly  %>% 
  mutate(
    datetime_string = paste(date, time, sep = "T"),
    datetime = parse_date_time(datetime_string, orders = "ymdHMS")
  )

head(akl_hourly, 5)
```

    ## # A tibble: 5 × 13
    ##    year month  mday time   temperature weather conditions events humidity
    ##   <dbl> <dbl> <dbl> <time>       <dbl> <chr>   <chr>      <chr>     <dbl>
    ## 1  2016     1     1 00:00           68 Clear   Clear      <NA>         68
    ## 2  2016     1     1 00:30           68 Clear   Clear      <NA>         68
    ## 3  2016     1     1 01:00           68 Clear   Clear      <NA>         73
    ## 4  2016     1     1 01:30           68 Clear   Clear      <NA>         68
    ## 5  2016     1     1 02:00           68 Clear   Clear      <NA>         68
    ## # … with 4 more variables: date_utc <dttm>, date <date>, datetime_string <chr>,
    ## #   datetime <dttm>

The data is available in the `akl_hourly` data frame.

-   What timezone are `datetime` and `date_utc` currently in? Examine
    the head of the `datetime` and `date_utc` columns to find out.

-   Fix `datetime` to have the timezone for `"Pacific/Auckland"`.

-   Reexamine the head of the `datetime` column to check the times have
    the same clocktime, but are now in the right timezone.

-   Now tabulate up the difference between the `datetime` and `date_utc`
    columns. *It should be zero if our hypothesis was correct*.

``` r
# Examine datetime and date_utc columns
head(akl_hourly$datetime)
```

    ## [1] "2016-01-01 00:00:00 UTC" "2016-01-01 00:30:00 UTC"
    ## [3] "2016-01-01 01:00:00 UTC" "2016-01-01 01:30:00 UTC"
    ## [5] "2016-01-01 02:00:00 UTC" "2016-01-01 02:30:00 UTC"

``` r
head(akl_hourly$date_utc)
```

    ## [1] "2015-12-31 11:00:00 UTC" "2015-12-31 11:30:00 UTC"
    ## [3] "2015-12-31 12:00:00 UTC" "2015-12-31 12:30:00 UTC"
    ## [5] "2015-12-31 13:00:00 UTC" "2015-12-31 13:30:00 UTC"

``` r
# Force datetime to Pacific/Auckland
akl_hourly <- akl_hourly %>%
  mutate(
    datetime = force_tz(datetime, tzone = "Pacific/Auckland"))

# Reexamine datetime
head(akl_hourly$datetime)
```

    ## [1] "2016-01-01 00:00:00 NZDT" "2016-01-01 00:30:00 NZDT"
    ## [3] "2016-01-01 01:00:00 NZDT" "2016-01-01 01:30:00 NZDT"
    ## [5] "2016-01-01 02:00:00 NZDT" "2016-01-01 02:30:00 NZDT"

``` r
# Are datetime and date_utc the same moments
table(akl_hourly$datetime - akl_hourly$date_utc)
```

    ## 
    ## -82800      0   3600 
    ##      2  17450      2

### **`04-Times without dates`**

-   Use `read_csv()` to read in `"akl_weather_hourly_2016.csv"`.
    *`readr` knows about the `hms` class, so if it comes across
    something that looks like a time it will use it.*

-   In this case the `time` column has been parsed as a time without a
    date. Take a look at the structure of the `time` column to verify it
    has the class `hms`.

-   `hms` objects print like times should. Take a look by examining the
    head of the `time` column.

-   You can use `hms` objects in plots too. Create a plot with `time` on
    the x-axis, `temperature` on the y-axis, with lines grouped by
    `date`.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ stringr 1.4.0
    ## ✔ tidyr   1.2.0     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ lubridate::as.difftime() masks base::as.difftime()
    ## ✖ lubridate::date()        masks base::date()
    ## ✖ dplyr::filter()          masks stats::filter()
    ## ✖ lubridate::intersect()   masks base::intersect()
    ## ✖ dplyr::lag()             masks stats::lag()
    ## ✖ lubridate::setdiff()     masks base::setdiff()
    ## ✖ lubridate::union()       masks base::union()

``` r
# Import auckland hourly data 
akl_hourly <- read_csv(akl_weather_h_path)
```

    ## Rows: 17454 Columns: 10

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (3): weather, conditions, events
    ## dbl  (5): year, month, mday, temperature, humidity
    ## dttm (1): date_utc
    ## time (1): time
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Examine structure of time column
str(akl_hourly$time)
```

    ##  'hms' num [1:17454] 00:00:00 00:30:00 01:00:00 01:30:00 ...
    ##  - attr(*, "units")= chr "secs"

``` r
# Examine head of time column
head(akl_hourly$time)
```

    ## 00:00:00
    ## 00:30:00
    ## 01:00:00
    ## 01:30:00
    ## 02:00:00
    ## 02:30:00

``` r
# A plot using just time
ggplot(akl_hourly, aes(x = time, y = temperature)) +
  geom_line(aes(group = make_date(year, month, mday)), alpha = 0.2)
```

![](04_Problems_in_practice_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### `05-Fast parsing with fasttime`

The `fasttime` package provides a single function
[**`fastPOSIXct()`**](https://www.rdocumentation.org/packages/fasttime/versions/1.0-2/topics/fastPOSIXct),
designed to read in datetimes formatted according to ISO 8601. Because
it only reads in one format, and doesn’t have to guess a format, it is
really fast!

You’ll see how fast in this exercise by comparing how fast it reads in
the dates from the Auckland hourly weather data (over 17,000 dates) to
`lubridate`s
[**`ymd_hms()`**](https://www.rdocumentation.org/packages/lubridate/versions/1.8.0/topics/ymd_hms).

To compare run times you’ll use the
[**`microbenchmark()`**](https://www.rdocumentation.org/packages/microbenchmark/versions/1.4.9/topics/microbenchmark)
function from the package of the same name. You pass in as many
arguments as you want each being an expression to time.

We’ve loaded the datetimes from the Auckland hourly data as strings into
the vector `dates`.

-   Examine the structure of `dates` to verify it is a string and in the
    ISO 8601 format.

-   Parse `dates` with `fasttime` and pipe to `str()` to verify
    `fastPOSIXct` parses them correctly.

-   Now to compare timing, call `microbenchmark` where the first
    argument uses `ymd_hms()` to parse `dates` and the second uses
    `fastPOSIXct()`.

``` r
dates <- c("2015-12-31T11:00:00Z", "2015-12-31T11:30:00Z", "2015-12-31T12:00:00Z",
"2015-12-31T12:30:00Z", "2015-12-31T13:00:00Z", "2015-12-31T13:30:00Z",
"2015-12-31T14:00:00Z", "2015-12-31T14:30:00Z", "2015-12-31T15:00:00Z",
"2015-12-31T15:30:00Z", "2015-12-31T16:00:00Z", "2015-12-31T16:30:00Z",
"2015-12-31T17:00:00Z", "2015-12-31T17:30:00Z", "2015-12-31T18:00:00Z",
"2015-12-31T18:30:00Z", "2015-12-31T19:00:00Z", "2015-12-31T19:30:00Z",
"2015-12-31T20:00:00Z", "2015-12-31T20:30:00Z", "2015-12-31T21:00:00Z",
"2015-12-31T21:30:00Z", "2015-12-31T22:00:00Z", "2015-12-31T22:30:00Z",
"2015-12-31T23:00:00Z", "2015-12-31T23:30:00Z", "2016-01-01T00:00:00Z",
"2016-01-01T00:30:00Z", "2016-01-01T01:00:00Z", "2016-01-01T01:30:00Z",
"2016-01-01T02:00:00Z", "2016-01-01T02:30:00Z", "2016-01-01T03:00:00Z",
"2016-01-01T03:30:00Z", "2016-01-01T04:00:00Z", "2016-01-01T04:30:00Z",
"2016-01-01T05:00:00Z", "2016-01-01T05:30:00Z", "2016-01-01T06:00:00Z",
"2016-01-01T06:30:00Z", "2016-01-01T07:00:00Z", "2016-01-01T07:30:00Z",
"2016-01-01T08:00:00Z", "2016-01-01T08:30:00Z", "2016-01-01T09:00:00Z",
"2016-01-01T09:30:00Z", "2016-01-01T10:00:00Z", "2016-01-01T10:30:00Z", 
"2016-01-01T11:00:00Z", "2016-01-01T11:30:00Z")
```

``` r
library(microbenchmark)
library(fasttime)

# Examine structure of dates
str(dates)
```

    ##  chr [1:50] "2015-12-31T11:00:00Z" "2015-12-31T11:30:00Z" ...

``` r
# Use fastPOSIXct() to parse dates
fastPOSIXct(dates) %>% str(dates)
```

    ##  POSIXct[1:50], format: "2015-12-31 14:00:00" "2015-12-31 14:30:00" "2015-12-31 15:00:00" ...

``` r
# Compare speed of fastPOSIXct() to ymd_hms()
microbenchmark(
  ymd_hms = ymd_hms(dates),
  fasttime = fastPOSIXct(dates),
  times = 20)
```

    ## Unit: microseconds
    ##      expr      min       lq       mean    median       uq      max neval
    ##   ymd_hms 6048.002 6294.700 6961.71115 6527.7510 7544.351 9694.102    20
    ##  fasttime    3.300    3.601    7.52095    7.7005    9.151   22.201    20

Original Results:

`Unit: microseconds      expr       min         lq      mean    median        uq        max neval   ymd_hms 28883.894 39871.5555 48564.648 43363.405 46950.459 128872.221    20  fasttime   708.559   925.7805  1152.037  1230.428  1329.998   1663.331    20`{=}

Great job! To compare speed, you can compare the average run time in the
mean column. You should see `fasttime` is about 20 times faster than
`ymd_hms()`

### 

**06-Fast parsing with lubridate::fast_strptime**

`dates` is in your workspace again.

-   Examine the head of `dates`. *What components are present? What
    separators are used?*

-   Parse `dates` with `fast_strptime()` by specifying the appropriate
    format string.

-   Compare the timing of `fast_strptime()` to `fasttime` and
    `ymd_hms()`.

``` r
# Head of dates
head(dates)
```

    ## [1] "2015-12-31T11:00:00Z" "2015-12-31T11:30:00Z" "2015-12-31T12:00:00Z"
    ## [4] "2015-12-31T12:30:00Z" "2015-12-31T13:00:00Z" "2015-12-31T13:30:00Z"

``` r
# Parse dates with fast_strptime
fast_strptime(dates, 
    format = "%Y-%m-%dT%H:%M:%SZ") %>% str()
```

    ##  POSIXlt[1:50], format: "2015-12-31 11:00:00" "2015-12-31 11:30:00" "2015-12-31 12:00:00" ...

``` r
# Comparse speed to ymd_hms() and fasttime
microbenchmark(
  ymd_hms = ymd_hms(dates),
  fasttime = fastPOSIXct(dates),
  fast_strptime = fast_strptime(dates, 
    format = "%Y-%m-%dT%H:%M:%SZ"),
  times = 20)
```

    ## Unit: microseconds
    ##           expr      min        lq       mean   median        uq      max neval
    ##        ymd_hms 6445.001 7312.1510 7946.60105 8048.252 8836.0010 9049.701    20
    ##       fasttime    3.701    4.3005   14.02595    8.051   10.6010   81.100    20
    ##  fast_strptime    9.102   11.7515   27.34095   20.901   24.6505  117.101    20

### **07-Outputting pretty dates and times**

-   Create a `stamp()` based on the string `"Saturday, Jan 1, 2000"`.

-   Print `date_stamp`. *Notice it is a function.*

-   Pass `today()` to `date_stamp` to format today’s date.

-   Now output today’s date in American style `MM/DD/YYYY`.

-   Finally, use stamp based on the `finished` string I’ve put in your
    workspace to format `today()`.

``` r
finished <- "I finished 'Dates and Times in R' on Thursday, September 4, 2017!"

# Create a stamp based on "Saturday, Jan 1, 2000"
date_stamp <- stamp("Saturday, Jan 1, 2000")
```

    ## Multiple formats matched: "%A, %b %d, %Y"(1), "Saturday, Jan %Om, %Y"(1), "Saturday, %Om %d, %Y"(1), "Saturday, %b %d, %Y"(1), "Saturday, Jan %m, %Y"(1), "%A, Jan %Om, %Y"(0), "%A, %Om %d, %Y"(0), "%A, Jan %m, %Y"(0)

    ## Using: "%A, %b %d, %Y"

``` r
# Print date_stamp
print(date_stamp)
```

    ## function (x, locale = "English_United States.utf8") 
    ## {
    ##     {
    ##         old_lc_time <- Sys.getlocale("LC_TIME")
    ##         if (old_lc_time != locale) {
    ##             on.exit(Sys.setlocale("LC_TIME", old_lc_time))
    ##             Sys.setlocale("LC_TIME", locale)
    ##         }
    ##     }
    ##     format(x, format = "%A, %b %d, %Y")
    ## }
    ## <environment: 0x0000019edd28dd78>

``` r
# Call date_stamp on today()
date_stamp(today())
```

    ## [1] "Tuesday, Jul 05, 2022"

``` r
# Create and call a stamp based on "12/31/1999"
stamp("12/31/1999")(today())
```

    ## Multiple formats matched: "%Om/%d/%Y"(1), "%m/%d/%Y"(1)

    ## Using: "%Om/%d/%Y"

    ## [1] "07/05/2022"

``` r
# Use string finished for stamp()
stamp(finished)(today())
```

    ## Multiple formats matched: "I finished 'Dates and Times in R' on %A, %B %d, %Y!"(1), "I finished 'Dates and Times in R' on Thursday, September %Om, %Y!"(1), "I finished 'Dates and Times in R' on Thursday, %Om %d, %Y!"(1), "I finished 'Dates and Times in R' on Thursday, %B %d, %Y!"(1), "I finished 'Dates and Times in R' on Thursday, September %m, %Y!"(1), "I finished 'Dates and Times in R' on %A, September %Om, %Y!"(0), "I finished 'Dates and Times in R' on %A, %Om %d, %Y!"(0), "I finished 'Dates and Times in R' on %A, September %m, %Y!"(0)

    ## Using: "I finished 'Dates and Times in R' on %A, %B %d, %Y!"

    ## [1] "I finished 'Dates and Times in R' on Tuesday, July 05, 2022!"

### `The End`

  

  
