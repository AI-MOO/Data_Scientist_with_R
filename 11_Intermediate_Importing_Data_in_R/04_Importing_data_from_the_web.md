Importing data from the web (Part 2)
================
Mohamad Osman
2022-06-22

# **Section 04: Importing data from the web (Part 2)**

### **`01-From JSON to R`**

In the simplest setting,
[**`fromJSON()`**](https://www.rdocumentation.org/packages/jsonlite/functions/fromJSON)
can convert character strings that represent JSON data into a nicely
structured R list. Give it a try!

-   Load the `jsonlite` package. It’s already installed on DataCamp’s
    servers.

-   `wine_json` represents a JSON. Use `fromJSON()` to convert it to a
    list, named `wine`.

-   Display the structure of `wine`

``` r
# Load the jsonlite package
library(jsonlite)

# wine_json is a JSON
wine_json <- '{"name":"Chateau Migraine", "year":1997, "alcohol_pct":12.4, "color":"red", "awarded":false}'

# Convert wine_json into a list: wine
wine <- fromJSON(wine_json)

# Print structure of wine
str(wine)
```

    ## List of 5
    ##  $ name       : chr "Chateau Migraine"
    ##  $ year       : int 1997
    ##  $ alcohol_pct: num 12.4
    ##  $ color      : chr "red"
    ##  $ awarded    : logi FALSE

### **`02-Quandl API`**

-   `quandl_url` represents a URL. Use `fromJSON()` directly on this URL
    and store the result in `quandl_data`.

-   Display the structure of `quandl_data`.

``` r
# jsonlite is preloaded

# Definition of quandl_url
quandl_url <- "https://www.quandl.com/api/v3/datasets/WIKI/FB/data.json?auth_token=i83asDsiWUUyfoypkgMz"

# Import Quandl data: quandl_data
quandl_data <- fromJSON(quandl_url)

# Print structure of quandl_data
str(quandl_data)
```

    ## List of 1
    ##  $ dataset_data:List of 10
    ##   ..$ limit       : NULL
    ##   ..$ transform   : NULL
    ##   ..$ column_index: NULL
    ##   ..$ column_names: chr [1:13] "Date" "Open" "High" "Low" ...
    ##   ..$ start_date  : chr "2012-05-18"
    ##   ..$ end_date    : chr "2018-03-27"
    ##   ..$ frequency   : chr "daily"
    ##   ..$ data        : chr [1:1472, 1:13] "2018-03-27" "2018-03-26" "2018-03-23" "2018-03-22" ...
    ##   ..$ collapse    : NULL
    ##   ..$ order       : NULL

### **`03-OMDb API`**

-   Two URLs are included in the sample code, as well as a `fromJSON()`
    call to build `sw4`. Add a similar call to build `sw3`.

-   Print out the element named `Title` of both `sw4` and `sw3`. You can
    use the `$` operator. What movies are we dealing with here?

-   Write an expression that evaluates to `TRUE` if `sw4` was released
    later than `sw3`. This information is stored in the `Year` element
    of the named lists.

``` r
# The package jsonlite is already loaded

# Definition of the URLs
url_sw4 <- "http://www.omdbapi.com/?apikey=72bc447a&i=tt0076759&r=json"
url_sw3 <- "http://www.omdbapi.com/?apikey=72bc447a&i=tt0121766&r=json"

# Import two URLs with fromJSON(): sw4 and sw3
sw4 <- fromJSON(url_sw4)
sw3 <- fromJSON(url_sw3)

# Print out the Title element of both lists
print(sw4$Title)
```

    ## [1] "Star Wars"

``` r
print(sw3$Title)
```

    ## [1] "Star Wars: Episode III - Revenge of the Sith"

``` r
# Is the release year of sw4 later than sw3?
sw4$Year > sw3$Year
```

    ## [1] FALSE

### **`04-JSON practice (1)`**

-   Change the assignment of `json1` such that the R vector after
    conversion contains the numbers 1 up to 6, in ascending order. Next,
    call
    [**`fromJSON()`**](https://www.rdocumentation.org/packages/jsonlite/functions/fromJSON)
    on `json1`.

-   Adapt the code for `json2` such that it’s converted to a named list
    with two elements: `a`, containing the numbers 1, 2 and 3 and `b`,
    containing the numbers 4, 5 and 6. Next, call
    [**`fromJSON()`**](https://www.rdocumentation.org/packages/jsonlite/functions/fromJSON)
    on `json2`.

``` r
# jsonlite is already loaded

# Challenge 1
json1 <- '[1, 2, 3, 4, 5, 6]'
fromJSON(json1)
```

    ## [1] 1 2 3 4 5 6

``` r
# Challenge 2
json2 <- '{"a": [1, 2, 3], "b": [4, 5, 6]}'
fromJSON(json2)
```

    ## $a
    ## [1] 1 2 3
    ## 
    ## $b
    ## [1] 4 5 6

### **`05-JSON practice (2)`**

-   Remove characters from `json1` to build a 2 by 2 matrix containing
    only 1, 2, 3 and 4. Call
    [**`fromJSON()`**](https://www.rdocumentation.org/packages/jsonlite/functions/fromJSON)
    on `json1`.

-   Add characters to `json2` such that the data frame in which the json
    is converted contains an additional observation in the last row. For
    this observations, `a` equals 5 and `b` equals 6. Call
    [**`fromJSON()`**](https://www.rdocumentation.org/packages/jsonlite/functions/fromJSON)
    one last time, on `json2`.

``` r
# jsonlite is already loaded

# Challenge 1
json1 <- '[[1, 2], [3, 4]]'
fromJSON(json1)
```

    ##      [,1] [,2]
    ## [1,]    1    2
    ## [2,]    3    4

``` r
# Challenge 2
json2 <- '[{"a": 1, "b": 2}, {"a": 3, "b": 4}, {"a": 5, "b": 6}]'
fromJSON(json2)
```

    ##   a b
    ## 1 1 2
    ## 2 3 4
    ## 3 5 6

### **`06-toJSON()`**

-   Use a function of the `utils` package to import the `.csv` file
    directly from the URL specified in `url_csv`. Save the resulting
    data frame as `water`. Make sure that strings are *not* imported as
    factors.

-   Convert the data frame `water` to a JSON. Call the resulting object
    `water_json`.

-   Print out `water_json`.

``` r
# jsonlite is already loaded

# URL pointing to the .csv file
url_csv <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/water.csv"

# Import the .csv file located at url_csv
water <- read.csv(url_csv)
water
```

    ##                     water       X1992       X1997       X2002  X2007
    ## 1                 Algeria 0.064000000          NA 0.017000000     NA
    ## 2          American Samoa          NA          NA          NA     NA
    ## 3                  Angola 0.000138000          NA 0.000138000     NA
    ## 4     Antigua and Barbuda 0.003300000          NA          NA     NA
    ## 5               Argentina 0.000749345 0.000749345 0.000749345     NA
    ## 6               Australia 0.029755895          NA 0.029755895     NA
    ## 7                 Austria 0.002153135          NA 0.002153135     NA
    ## 8                 Bahamas 0.001278960          NA 0.007400000     NA
    ## 9                 Bahrain 0.044100000          NA 0.044100000 0.1024
    ## 10               Barbados          NA          NA          NA 0.0146
    ## 11 British Virgin Islands          NA          NA          NA 0.0042
    ## 12                 Canada 0.002700000          NA 0.002700000     NA
    ## 13             Cape Verde 0.002000000 0.001681000          NA     NA
    ## 14         Cayman Islands 0.003318945          NA          NA     NA
    ## 15   Central African Rep.          NA          NA          NA     NA
    ## 16                  Chile 0.004821650          NA 0.004821650     NA
    ## 17               Colombia 0.002724725          NA 0.002724725     NA
    ## 18                   Cuba 0.006907990 0.006907990 0.006907990     NA
    ## 19                 Cyprus 0.003000000 0.003000000 0.033500000     NA
    ## 20             Czech Rep. 0.000150380          NA 0.000150380     NA
    ## 21                Denmark 0.015000000          NA 0.015000000     NA
    ## 22               Djibouti 0.000100000          NA 0.000100000     NA
    ## 23                Ecuador 0.002154960 0.002154960 0.002154960     NA
    ## 24                  Egypt 0.025000000 0.025000000 0.100000000     NA
    ## 25            El Salvador 0.000137970          NA 0.000137970     NA
    ## 26                Finland 0.000080665          NA 0.000080665     NA
    ## 27                 France 0.011732560          NA 0.011732560     NA
    ## 28              Gibraltar 0.007716100          NA          NA     NA
    ## 29                 Greece 0.010037135          NA 0.010037135     NA
    ## 30               Honduras 0.000237615          NA 0.000237615     NA
    ## 31                Hungary 0.000224475          NA 0.000224475     NA
    ## 32                  India          NA 0.000500000 0.000500000     NA
    ## 33              Indonesia 0.018727420          NA 0.018727420     NA
    ## 34                   Iran 0.003000000 0.003000000 0.003000000 0.2000
    ## 35                   Iraq          NA 0.007400000 0.007400000     NA
    ## 36                Ireland 0.000198925          NA 0.000198925     NA
    ## 37                 Israel 0.025572630          NA 0.025572630 0.1400
    ## 38                  Italy 0.097331995          NA 0.097331995     NA
    ## 39                Jamaica 0.000497495 0.000497495 0.000497495     NA
    ## 40                  Japan          NA 0.040000000 0.040000000     NA
    ## 41                 Jordan          NA 0.002000000          NA 0.0098
    ## 42             Kazakhstan          NA 1.328000000 1.328000000     NA
    ## 43                 Kuwait 0.507000000 0.231000000 0.420200000     NA
    ## 44                Lebanon          NA          NA          NA 0.0473
    ## 45                  Libya          NA          NA 0.018000000     NA
    ## 46               Malaysia 0.004333645          NA 0.004333645     NA
    ## 47               Maldives 0.000370000          NA          NA     NA
    ## 48                  Malta 0.024000000 0.031000000 0.031000000     NA
    ## 49       Marshall Islands 0.000748250          NA          NA     NA
    ## 50             Mauritania 0.002000000          NA 0.002000000     NA
    ## 51                 Mexico 0.030694675          NA 0.030694675     NA
    ## 52                Morocco 0.003400000 0.003400000 0.007000000     NA
    ## 53                Namibia 0.000300000          NA 0.000300000     NA
    ## 54   Netherlands Antilles 0.063008490          NA          NA     NA
    ## 55              Nicaragua 0.000219000          NA 0.000219000     NA
    ## 56                Nigeria 0.003000000          NA 0.003000000     NA
    ## 57                 Norway 0.000109500          NA 0.000109500     NA
    ## 58                   Oman          NA 0.034000000 0.034000000 0.1090
    ## 59                   Peru 0.005420980          NA 0.005420980     NA
    ## 60                 Poland 0.006986830          NA 0.006986830     NA
    ## 61               Portugal 0.001613300          NA 0.001613300     NA
    ## 62                  Qatar 0.065000000 0.099000000 0.099000000 0.1800
    ## 63           Saudi Arabia 0.683000000 0.727000000 0.863000000 1.0330
    ## 64                Senegal 0.000050000          NA 0.000050000     NA
    ## 65                Somalia 0.000100000          NA 0.000100000     NA
    ## 66           South Africa 0.018000000          NA 0.018000000     NA
    ## 67                  Spain 0.100184105          NA 0.100184105     NA
    ## 68                  Sudan 0.000400000 0.000400000 0.000400000     NA
    ## 69                 Sweden 0.000197830          NA 0.000197830     NA
    ## 70    Trinidad and Tobago          NA          NA          NA 0.0360
    ## 71                Tunisia 0.008000000          NA 0.013000000     NA
    ## 72                 Turkey 0.000500000          NA 0.000500000 0.0005
    ## 73   United Arab Emirates 0.163000000 0.385000000          NA 0.9500
    ## 74         United Kingdom 0.033313185          NA 0.033313185     NA
    ## 75          United States 0.579974050          NA 0.579974050     NA
    ## 76              Venezuela 0.005173875          NA 0.005173875     NA
    ## 77            Yemen, Rep. 0.010000000          NA 0.010000000     NA

``` r
# Convert the data file according to the requirements
water_json <- toJSON(water)

# Print out water_json
water_json
```

    ## [{"water":"Algeria","X1992":0.064,"X2002":0.017},{"water":"American Samoa"},{"water":"Angola","X1992":0.0001,"X2002":0.0001},{"water":"Antigua and Barbuda","X1992":0.0033},{"water":"Argentina","X1992":0.0007,"X1997":0.0007,"X2002":0.0007},{"water":"Australia","X1992":0.0298,"X2002":0.0298},{"water":"Austria","X1992":0.0022,"X2002":0.0022},{"water":"Bahamas","X1992":0.0013,"X2002":0.0074},{"water":"Bahrain","X1992":0.0441,"X2002":0.0441,"X2007":0.1024},{"water":"Barbados","X2007":0.0146},{"water":"British Virgin Islands","X2007":0.0042},{"water":"Canada","X1992":0.0027,"X2002":0.0027},{"water":"Cape Verde","X1992":0.002,"X1997":0.0017},{"water":"Cayman Islands","X1992":0.0033},{"water":"Central African Rep."},{"water":"Chile","X1992":0.0048,"X2002":0.0048},{"water":"Colombia","X1992":0.0027,"X2002":0.0027},{"water":"Cuba","X1992":0.0069,"X1997":0.0069,"X2002":0.0069},{"water":"Cyprus","X1992":0.003,"X1997":0.003,"X2002":0.0335},{"water":"Czech Rep.","X1992":0.0002,"X2002":0.0002},{"water":"Denmark","X1992":0.015,"X2002":0.015},{"water":"Djibouti","X1992":0.0001,"X2002":0.0001},{"water":"Ecuador","X1992":0.0022,"X1997":0.0022,"X2002":0.0022},{"water":"Egypt","X1992":0.025,"X1997":0.025,"X2002":0.1},{"water":"El Salvador","X1992":0.0001,"X2002":0.0001},{"water":"Finland","X1992":0.0001,"X2002":0.0001},{"water":"France","X1992":0.0117,"X2002":0.0117},{"water":"Gibraltar","X1992":0.0077},{"water":"Greece","X1992":0.01,"X2002":0.01},{"water":"Honduras","X1992":0.0002,"X2002":0.0002},{"water":"Hungary","X1992":0.0002,"X2002":0.0002},{"water":"India","X1997":0.0005,"X2002":0.0005},{"water":"Indonesia","X1992":0.0187,"X2002":0.0187},{"water":"Iran","X1992":0.003,"X1997":0.003,"X2002":0.003,"X2007":0.2},{"water":"Iraq","X1997":0.0074,"X2002":0.0074},{"water":"Ireland","X1992":0.0002,"X2002":0.0002},{"water":"Israel","X1992":0.0256,"X2002":0.0256,"X2007":0.14},{"water":"Italy","X1992":0.0973,"X2002":0.0973},{"water":"Jamaica","X1992":0.0005,"X1997":0.0005,"X2002":0.0005},{"water":"Japan","X1997":0.04,"X2002":0.04},{"water":"Jordan","X1997":0.002,"X2007":0.0098},{"water":"Kazakhstan","X1997":1.328,"X2002":1.328},{"water":"Kuwait","X1992":0.507,"X1997":0.231,"X2002":0.4202},{"water":"Lebanon","X2007":0.0473},{"water":"Libya","X2002":0.018},{"water":"Malaysia","X1992":0.0043,"X2002":0.0043},{"water":"Maldives","X1992":0.0004},{"water":"Malta","X1992":0.024,"X1997":0.031,"X2002":0.031},{"water":"Marshall Islands","X1992":0.0007},{"water":"Mauritania","X1992":0.002,"X2002":0.002},{"water":"Mexico","X1992":0.0307,"X2002":0.0307},{"water":"Morocco","X1992":0.0034,"X1997":0.0034,"X2002":0.007},{"water":"Namibia","X1992":0.0003,"X2002":0.0003},{"water":"Netherlands Antilles","X1992":0.063},{"water":"Nicaragua","X1992":0.0002,"X2002":0.0002},{"water":"Nigeria","X1992":0.003,"X2002":0.003},{"water":"Norway","X1992":0.0001,"X2002":0.0001},{"water":"Oman","X1997":0.034,"X2002":0.034,"X2007":0.109},{"water":"Peru","X1992":0.0054,"X2002":0.0054},{"water":"Poland","X1992":0.007,"X2002":0.007},{"water":"Portugal","X1992":0.0016,"X2002":0.0016},{"water":"Qatar","X1992":0.065,"X1997":0.099,"X2002":0.099,"X2007":0.18},{"water":"Saudi Arabia","X1992":0.683,"X1997":0.727,"X2002":0.863,"X2007":1.033},{"water":"Senegal","X1992":0,"X2002":0},{"water":"Somalia","X1992":0.0001,"X2002":0.0001},{"water":"South Africa","X1992":0.018,"X2002":0.018},{"water":"Spain","X1992":0.1002,"X2002":0.1002},{"water":"Sudan","X1992":0.0004,"X1997":0.0004,"X2002":0.0004},{"water":"Sweden","X1992":0.0002,"X2002":0.0002},{"water":"Trinidad and Tobago","X2007":0.036},{"water":"Tunisia","X1992":0.008,"X2002":0.013},{"water":"Turkey","X1992":0.0005,"X2002":0.0005,"X2007":0.0005},{"water":"United Arab Emirates","X1992":0.163,"X1997":0.385,"X2007":0.95},{"water":"United Kingdom","X1992":0.0333,"X2002":0.0333},{"water":"United States","X1992":0.58,"X2002":0.58},{"water":"Venezuela","X1992":0.0052,"X2002":0.0052},{"water":"Yemen, Rep.","X1992":0.01,"X2002":0.01}]

### **`07-Minify and prettify`**

JSONs can come in different formats. Take these two JSONs, that are in
fact exactly the same: the first one is in a minified format, the second
one is in a pretty format with indentation, whitespace and new lines:

    # Mini
    {"a":1,"b":2,"c":{"x":5,"y":6}}

    # Pretty
    {
      "a": 1,
      "b": 2,
      "c": {
        "x": 5,
        "y": 6
      }
    }

Unless you’re a computer, you surely prefer the second version. However,
the standard form that
[**`toJSON()`**](https://www.rdocumentation.org/packages/jsonlite/functions/fromJSON)
returns, is the minified version, as it is more concise. You can adapt
this behavior by setting the `pretty` argument inside
[**`toJSON()`**](https://www.rdocumentation.org/packages/jsonlite/functions/fromJSON)
to `TRUE`. If you already have a JSON string, you can use
[**`prettify()`**](https://www.rdocumentation.org/packages/jsonlite/functions/prettify)
or
[**`minify()`**](https://www.rdocumentation.org/packages/jsonlite/functions/prettify)
to make the JSON pretty or as concise as possible.

-   Convert the `mtcars` dataset, which is available in R by default, to
    a *pretty* `JSON`. Call the resulting JSON `pretty_json`.

-   Print out `pretty_json`. Can you understand the output easily?

-   Convert `pretty_json` to a minimal version using
    [**`minify()`**](https://www.rdocumentation.org/packages/jsonlite/functions/prettify).
    Store this version under a new variable, `mini_json`.

-   Print out `mini_json`. Which version do you prefer, the pretty one
    or the minified one?

``` r
# jsonlite is already loaded

# Convert mtcars to a pretty JSON: pretty_json
mtcars_sliced <- mtcars[20:30,]

pretty_json <- toJSON(mtcars_sliced, pretty = TRUE)

# Print pretty_json
pretty_json
```

    ## [
    ##   {
    ##     "mpg": 33.9,
    ##     "cyl": 4,
    ##     "disp": 71.1,
    ##     "hp": 65,
    ##     "drat": 4.22,
    ##     "wt": 1.835,
    ##     "qsec": 19.9,
    ##     "vs": 1,
    ##     "am": 1,
    ##     "gear": 4,
    ##     "carb": 1,
    ##     "_row": "Toyota Corolla"
    ##   },
    ##   {
    ##     "mpg": 21.5,
    ##     "cyl": 4,
    ##     "disp": 120.1,
    ##     "hp": 97,
    ##     "drat": 3.7,
    ##     "wt": 2.465,
    ##     "qsec": 20.01,
    ##     "vs": 1,
    ##     "am": 0,
    ##     "gear": 3,
    ##     "carb": 1,
    ##     "_row": "Toyota Corona"
    ##   },
    ##   {
    ##     "mpg": 15.5,
    ##     "cyl": 8,
    ##     "disp": 318,
    ##     "hp": 150,
    ##     "drat": 2.76,
    ##     "wt": 3.52,
    ##     "qsec": 16.87,
    ##     "vs": 0,
    ##     "am": 0,
    ##     "gear": 3,
    ##     "carb": 2,
    ##     "_row": "Dodge Challenger"
    ##   },
    ##   {
    ##     "mpg": 15.2,
    ##     "cyl": 8,
    ##     "disp": 304,
    ##     "hp": 150,
    ##     "drat": 3.15,
    ##     "wt": 3.435,
    ##     "qsec": 17.3,
    ##     "vs": 0,
    ##     "am": 0,
    ##     "gear": 3,
    ##     "carb": 2,
    ##     "_row": "AMC Javelin"
    ##   },
    ##   {
    ##     "mpg": 13.3,
    ##     "cyl": 8,
    ##     "disp": 350,
    ##     "hp": 245,
    ##     "drat": 3.73,
    ##     "wt": 3.84,
    ##     "qsec": 15.41,
    ##     "vs": 0,
    ##     "am": 0,
    ##     "gear": 3,
    ##     "carb": 4,
    ##     "_row": "Camaro Z28"
    ##   },
    ##   {
    ##     "mpg": 19.2,
    ##     "cyl": 8,
    ##     "disp": 400,
    ##     "hp": 175,
    ##     "drat": 3.08,
    ##     "wt": 3.845,
    ##     "qsec": 17.05,
    ##     "vs": 0,
    ##     "am": 0,
    ##     "gear": 3,
    ##     "carb": 2,
    ##     "_row": "Pontiac Firebird"
    ##   },
    ##   {
    ##     "mpg": 27.3,
    ##     "cyl": 4,
    ##     "disp": 79,
    ##     "hp": 66,
    ##     "drat": 4.08,
    ##     "wt": 1.935,
    ##     "qsec": 18.9,
    ##     "vs": 1,
    ##     "am": 1,
    ##     "gear": 4,
    ##     "carb": 1,
    ##     "_row": "Fiat X1-9"
    ##   },
    ##   {
    ##     "mpg": 26,
    ##     "cyl": 4,
    ##     "disp": 120.3,
    ##     "hp": 91,
    ##     "drat": 4.43,
    ##     "wt": 2.14,
    ##     "qsec": 16.7,
    ##     "vs": 0,
    ##     "am": 1,
    ##     "gear": 5,
    ##     "carb": 2,
    ##     "_row": "Porsche 914-2"
    ##   },
    ##   {
    ##     "mpg": 30.4,
    ##     "cyl": 4,
    ##     "disp": 95.1,
    ##     "hp": 113,
    ##     "drat": 3.77,
    ##     "wt": 1.513,
    ##     "qsec": 16.9,
    ##     "vs": 1,
    ##     "am": 1,
    ##     "gear": 5,
    ##     "carb": 2,
    ##     "_row": "Lotus Europa"
    ##   },
    ##   {
    ##     "mpg": 15.8,
    ##     "cyl": 8,
    ##     "disp": 351,
    ##     "hp": 264,
    ##     "drat": 4.22,
    ##     "wt": 3.17,
    ##     "qsec": 14.5,
    ##     "vs": 0,
    ##     "am": 1,
    ##     "gear": 5,
    ##     "carb": 4,
    ##     "_row": "Ford Pantera L"
    ##   },
    ##   {
    ##     "mpg": 19.7,
    ##     "cyl": 6,
    ##     "disp": 145,
    ##     "hp": 175,
    ##     "drat": 3.62,
    ##     "wt": 2.77,
    ##     "qsec": 15.5,
    ##     "vs": 0,
    ##     "am": 1,
    ##     "gear": 5,
    ##     "carb": 6,
    ##     "_row": "Ferrari Dino"
    ##   }
    ## ]

``` r
# Minify pretty_json: mini_json
mini_json <- minify(pretty_json)

# Print mini_json
mini_json
```

    ## [{"mpg":33.9,"cyl":4,"disp":71.1,"hp":65,"drat":4.22,"wt":1.835,"qsec":19.9,"vs":1,"am":1,"gear":4,"carb":1,"_row":"Toyota Corolla"},{"mpg":21.5,"cyl":4,"disp":120.1,"hp":97,"drat":3.7,"wt":2.465,"qsec":20.01,"vs":1,"am":0,"gear":3,"carb":1,"_row":"Toyota Corona"},{"mpg":15.5,"cyl":8,"disp":318,"hp":150,"drat":2.76,"wt":3.52,"qsec":16.87,"vs":0,"am":0,"gear":3,"carb":2,"_row":"Dodge Challenger"},{"mpg":15.2,"cyl":8,"disp":304,"hp":150,"drat":3.15,"wt":3.435,"qsec":17.3,"vs":0,"am":0,"gear":3,"carb":2,"_row":"AMC Javelin"},{"mpg":13.3,"cyl":8,"disp":350,"hp":245,"drat":3.73,"wt":3.84,"qsec":15.41,"vs":0,"am":0,"gear":3,"carb":4,"_row":"Camaro Z28"},{"mpg":19.2,"cyl":8,"disp":400,"hp":175,"drat":3.08,"wt":3.845,"qsec":17.05,"vs":0,"am":0,"gear":3,"carb":2,"_row":"Pontiac Firebird"},{"mpg":27.3,"cyl":4,"disp":79,"hp":66,"drat":4.08,"wt":1.935,"qsec":18.9,"vs":1,"am":1,"gear":4,"carb":1,"_row":"Fiat X1-9"},{"mpg":26,"cyl":4,"disp":120.3,"hp":91,"drat":4.43,"wt":2.14,"qsec":16.7,"vs":0,"am":1,"gear":5,"carb":2,"_row":"Porsche 914-2"},{"mpg":30.4,"cyl":4,"disp":95.1,"hp":113,"drat":3.77,"wt":1.513,"qsec":16.9,"vs":1,"am":1,"gear":5,"carb":2,"_row":"Lotus Europa"},{"mpg":15.8,"cyl":8,"disp":351,"hp":264,"drat":4.22,"wt":3.17,"qsec":14.5,"vs":0,"am":1,"gear":5,"carb":4,"_row":"Ford Pantera L"},{"mpg":19.7,"cyl":6,"disp":145,"hp":175,"drat":3.62,"wt":2.77,"qsec":15.5,"vs":0,"am":1,"gear":5,"carb":6,"_row":"Ferrari Dino"}]

### **`The End`**
