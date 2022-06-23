Importing data from the web (Part 1)
================
Mohamad Osman
2022-06-22

# Section 03: **Importing data from the web (Part 1)**

### **`01-Import flat files from the web`**

-   Load the `readr` package. It’s already installed on DataCamp’s
    servers.

-   Use `url_csv` to read in the `.csv` file it is pointing to. Use the
    [**`read_csv()`**](https://cran.r-project.org/web/packages/readr/readr.pdf)
    function. The `.csv` contains column names in the first row. Save
    the resulting data frame as `pools`.

-   Similarly, use `url_delim` to read in the online `.txt` file. Use
    the
    [**`read_tsv()`**](https://cran.r-project.org/web/packages/readr/readr.pdf)
    function and store the result as `potatoes`.

-   Print `pools` and `potatoes`. Looks correct?

``` r
# Load the readr package
library(readr)

# Import the csv file: pools
url_csv <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/swimming_pools.csv"
pools <- read_csv(url_csv) 
```

    ## Rows: 20 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Name, Address
    ## dbl (2): Latitude, Longitude
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Import the txt file: potatoes
url_delim <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/potatoes.txt"
potatoes <- read_tsv(url_delim)
```

    ## Rows: 160 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: "\t"
    ## dbl (8): area, temp, size, storage, method, texture, flavor, moistness
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Print pools and potatoes
pools
```

    ## # A tibble: 20 × 4
    ##    Name                                      Address          Latitude Longitude
    ##    <chr>                                     <chr>               <dbl>     <dbl>
    ##  1 Acacia Ridge Leisure Centre               1391 Beaudesert…    -27.6      153.
    ##  2 Bellbowrie Pool                           Sugarwood Stree…    -27.6      153.
    ##  3 Carole Park                               Cnr Boundary Ro…    -27.6      153.
    ##  4 Centenary Pool (inner City)               400 Gregory Ter…    -27.5      153.
    ##  5 Chermside Pool                            375 Hamilton Ro…    -27.4      153.
    ##  6 Colmslie Pool (Morningside)               400 Lytton Road…    -27.5      153.
    ##  7 Spring Hill Baths (inner City)            14 Torrington S…    -27.5      153.
    ##  8 Dunlop Park Pool (Corinda)                794 Oxley Road,…    -27.5      153.
    ##  9 Fortitude Valley Pool                     432 Wickham Str…    -27.5      153.
    ## 10 Hibiscus Sports Complex (upper MtGravatt) 90 Klumpp Road,…    -27.6      153.
    ## 11 Ithaca Pool ( Paddington)                 131 Caxton Stre…    -27.5      153.
    ## 12 Jindalee Pool                             11 Yallambee Ro…    -27.5      153.
    ## 13 Manly Pool                                1 Fairlead Cres…    -27.5      153.
    ## 14 Mt Gravatt East Aquatic Centre            Cnr wecker Road…    -27.5      153.
    ## 15 Musgrave Park Pool (South Brisbane)       100 Edmonstone …    -27.5      153.
    ## 16 Newmarket Pool                            71 Alderson Str…    -27.4      153.
    ## 17 Runcorn Pool                              37 Bonemill Roa…    -27.6      153.
    ## 18 Sandgate Pool                             231 Flinders Pa…    -27.3      153.
    ## 19 Langlands Parks Pool (Stones Corner)      5 Panitya Stree…    -27.5      153.
    ## 20 Yeronga Park Pool                         81 School Road,…    -27.5      153.

``` r
potatoes
```

    ## # A tibble: 160 × 8
    ##     area  temp  size storage method texture flavor moistness
    ##    <dbl> <dbl> <dbl>   <dbl>  <dbl>   <dbl>  <dbl>     <dbl>
    ##  1     1     1     1       1      1     2.9    3.2       3  
    ##  2     1     1     1       1      2     2.3    2.5       2.6
    ##  3     1     1     1       1      3     2.5    2.8       2.8
    ##  4     1     1     1       1      4     2.1    2.9       2.4
    ##  5     1     1     1       1      5     1.9    2.8       2.2
    ##  6     1     1     1       2      1     1.8    3         1.7
    ##  7     1     1     1       2      2     2.6    3.1       2.4
    ##  8     1     1     1       2      3     3      3         2.9
    ##  9     1     1     1       2      4     2.2    3.2       2.5
    ## 10     1     1     1       2      5     2      2.8       1.9
    ## # … with 150 more rows

### **`02-Secure importing`**

In the previous exercises, you have been working with URLs that all
start with `http://`. There is, however, a safer alternative to HTTP,
namely HTTPS, which stands for HyperText Transfer Protocol Secure. Just
remember this: HTTPS is relatively safe, HTTP is not.

Luckily for us, you can use the standard importing functions with
`https://` connections since R version 3.2.2.

-   Take a look at the URL in `url_csv`. It uses a secure connection,
    `https://`.

-   Use
    [**`read.csv()`**](https://www.rdocumentation.org/packages/utils/functions/read.table)
    to import the file at `url_csv`. The `.csv` file it is referring to
    contains column names in the first row. Call it `pools1`.

-   Load the `readr` package. It’s already installed on DataCamp’s
    servers.

-   Use
    [**`read_csv()`**](https://cran.r-project.org/web/packages/readr/readr.pdf)
    to read in the same `.csv` file in `url_csv`. Call it `pools2`.

-   Print out the structure of `pools1` and `pools2`. Looks like the
    importing went equally well as with a normal `http` connection!

``` r
# https URL to the swimming_pools csv file.
url_csv <- "https://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/swimming_pools.csv"

# Import the file using read.csv(): pools1
pools1 <- read.csv(url_csv)

# Load the readr package
library(readr)

# Import the file using read_csv(): pools2
pools2 <- read_csv(url_csv)
```

    ## Rows: 20 Columns: 4
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Name, Address
    ## dbl (2): Latitude, Longitude
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
# Print the structure of pools1 and pools2
str(pools1)
```

    ## 'data.frame':    20 obs. of  4 variables:
    ##  $ Name     : chr  "Acacia Ridge Leisure Centre" "Bellbowrie Pool" "Carole Park" "Centenary Pool (inner City)" ...
    ##  $ Address  : chr  "1391 Beaudesert Road, Acacia Ridge" "Sugarwood Street, Bellbowrie" "Cnr Boundary Road and Waterford Road Wacol" "400 Gregory Terrace, Spring Hill" ...
    ##  $ Latitude : num  -27.6 -27.6 -27.6 -27.5 -27.4 ...
    ##  $ Longitude: num  153 153 153 153 153 ...

``` r
str(pools2)
```

    ## spec_tbl_df [20 × 4] (S3: spec_tbl_df/tbl_df/tbl/data.frame)
    ##  $ Name     : chr [1:20] "Acacia Ridge Leisure Centre" "Bellbowrie Pool" "Carole Park" "Centenary Pool (inner City)" ...
    ##  $ Address  : chr [1:20] "1391 Beaudesert Road, Acacia Ridge" "Sugarwood Street, Bellbowrie" "Cnr Boundary Road and Waterford Road Wacol" "400 Gregory Terrace, Spring Hill" ...
    ##  $ Latitude : num [1:20] -27.6 -27.6 -27.6 -27.5 -27.4 ...
    ##  $ Longitude: num [1:20] 153 153 153 153 153 ...
    ##  - attr(*, "spec")=
    ##   .. cols(
    ##   ..   Name = col_character(),
    ##   ..   Address = col_character(),
    ##   ..   Latitude = col_double(),
    ##   ..   Longitude = col_double()
    ##   .. )
    ##  - attr(*, "problems")=<externalptr>

### **`03-Import Excel files from the web`**

-   Load the `readxl` and `gdata` packages. They are already installed
    on DataCamp’s servers.

-   Import the `.xls` file located at the URL `url_xls` using
    [**`read.xls()`**](https://www.rdocumentation.org/packages/gdata/functions/read.xls)
    from `gdata`. Store the resulting data frame as `excel_gdata`.

-   You can not use
    [**`read_excel()`**](https://cran.r-project.org/web/packages/readxl/readxl.pdf)
    directly with a URL. Complete the following instructions to work
    around this problem:

-   Use
    [**`download.file()`**](https://www.rdocumentation.org/packages/utils/functions/download.file)
    to download the `.xls` file behind the URL and store it locally as
    `"local_latitude.xls"`.

-   Call
    [**`read_excel()`**](https://cran.r-project.org/web/packages/readxl/readxl.pdf)
    to import the local file, `"local_latitude.xls"`. Name the resulting
    data frame `excel_readxl`.

``` r
# Load the readxl and gdata package
library(readxl)
library(gdata)
```

    ## gdata: read.xls support for 'XLS' (Excel 97-2004) files ENABLED.

    ## 

    ## gdata: read.xls support for 'XLSX' (Excel 2007+) files ENABLED.

    ## 
    ## Attaching package: 'gdata'

    ## The following object is masked from 'package:stats':
    ## 
    ##     nobs

    ## The following object is masked from 'package:utils':
    ## 
    ##     object.size

    ## The following object is masked from 'package:base':
    ## 
    ##     startsWith

``` r
# Specification of url: url_xls
url_xls <- "http://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/latitude.xls"

# Import the .xls file with gdata: excel_gdata
excel_gdata <- gdata::read.xls(url_xls)

# Download file behind URL, name it local_latitude.xls
dest_path <- file.path("..", "00_Datasets", "local_latitude.xls")
download.file(url_xls, dest_path, mode="wb")


# Import the local .xls file with readxl: excel_readxl
excel_readxl <- readxl::read_excel(dest_path)
excel_readxl
```

    ## # A tibble: 246 × 2
    ##    country               `1700`
    ##    <chr>                  <dbl>
    ##  1 Afghanistan            34.6 
    ##  2 Akrotiri and Dhekelia  34.6 
    ##  3 Albania                41.3 
    ##  4 Algeria                36.7 
    ##  5 American Samoa        -14.3 
    ##  6 Andorra                42.5 
    ##  7 Angola                 -8.84
    ##  8 Anguilla               18.2 
    ##  9 Antigua and Barbuda    17.1 
    ## 10 Argentina             -36.7 
    ## # … with 236 more rows

### **`04-Downloading any file, secure or not`**

In the previous exercise you’ve seen how you can read excel files on the
web using the `read_excel` package by first downloading the file with
the
[**`download.file()`**](https://www.rdocumentation.org/packages/utils/functions/download.file)
function.

There’s more: with
[**`download.file()`**](https://www.rdocumentation.org/packages/utils/functions/download.file)
you can download any kind of file from the web, using HTTP and HTTPS:
images, executable files, but also `.RData` files. An `RData` file is
very efficient format to store R data.

You can load data from an `RData` file using the
[**`load()`**](https://www.rdocumentation.org/packages/base/functions/load)
function, but this function does not accept a URL string as an argument.
In this exercise, you’ll first download the `RData` file securely, and
then import the local data file.

-   Take a look at the URL in `url_rdata`. It uses a secure connection,
    `https://`. This URL points to an `RData` file containing a data
    frame with some metrics on different kinds of wine.

-   Download the file at `url_rdata` using
    [**`download.file()`**](https://www.rdocumentation.org/packages/utils/functions/download.file).
    Call the file `"wine_local.RData"` in your working directory.

-   Load the file you created, `wine_local.RData`, using the
    [**`load()`**](https://www.rdocumentation.org/packages/base/functions/load)
    function. It takes one argument, the path to the file, which is just
    the filename in our case. After running this command, the variable
    `wine` will automatically be available in your workspace.

Print out the
[**`summary()`**](https://www.rdocumentation.org/packages/base/functions/summary)
of the `wine` dataset.

``` r
# https URL to the wine RData file.
url_rdata <- "https://s3.amazonaws.com/assets.datacamp.com/production/course_1478/datasets/wine.RData"

# Download the wine file to your working directory
dest_path <- file.path("..", "00_Datasets", "wine_local.RData")

download.file(url_rdata, dest_path)

# Load the wine data into your workspace using load()
load(dest_path)

# Print out the summary of the wine data
summary(wine)
```

    ##     Alcohol        Malic acid        Ash        Alcalinity of ash
    ##  Min.   :11.03   Min.   :0.74   Min.   :1.360   Min.   :10.60    
    ##  1st Qu.:12.36   1st Qu.:1.60   1st Qu.:2.210   1st Qu.:17.20    
    ##  Median :13.05   Median :1.87   Median :2.360   Median :19.50    
    ##  Mean   :12.99   Mean   :2.34   Mean   :2.366   Mean   :19.52    
    ##  3rd Qu.:13.67   3rd Qu.:3.10   3rd Qu.:2.560   3rd Qu.:21.50    
    ##  Max.   :14.83   Max.   :5.80   Max.   :3.230   Max.   :30.00    
    ##    Magnesium      Total phenols     Flavanoids    Nonflavanoid phenols
    ##  Min.   : 70.00   Min.   :0.980   Min.   :0.340   Min.   :0.1300      
    ##  1st Qu.: 88.00   1st Qu.:1.740   1st Qu.:1.200   1st Qu.:0.2700      
    ##  Median : 98.00   Median :2.350   Median :2.130   Median :0.3400      
    ##  Mean   : 99.59   Mean   :2.292   Mean   :2.023   Mean   :0.3623      
    ##  3rd Qu.:107.00   3rd Qu.:2.800   3rd Qu.:2.860   3rd Qu.:0.4400      
    ##  Max.   :162.00   Max.   :3.880   Max.   :5.080   Max.   :0.6600      
    ##  Proanthocyanins Color intensity       Hue           Proline      
    ##  Min.   :0.410   Min.   : 1.280   Min.   :1.270   Min.   : 278.0  
    ##  1st Qu.:1.250   1st Qu.: 3.210   1st Qu.:1.930   1st Qu.: 500.0  
    ##  Median :1.550   Median : 4.680   Median :2.780   Median : 672.0  
    ##  Mean   :1.587   Mean   : 5.055   Mean   :2.604   Mean   : 745.1  
    ##  3rd Qu.:1.950   3rd Qu.: 6.200   3rd Qu.:3.170   3rd Qu.: 985.0  
    ##  Max.   :3.580   Max.   :13.000   Max.   :4.000   Max.   :1680.0

### **`05-HTTP? httr! (1)`**

`httr` provides a convenient function,
[**`GET()`**](https://www.rdocumentation.org/packages/httr/functions/GET)
to execute this GET request. The result is a `response` object, that
provides easy access to the status code, content-type and, of course,
the actual content.

-   Load the `httr` package. It’s already installed on DataCamp’s
    servers.

-   Use
    [**`GET()`**](https://www.rdocumentation.org/packages/httr/functions/GET)
    to get the URL stored in `url`. Store the result of this `GET()`
    call as `resp`.

-   Print the `resp` object. What information does it contain?

-   Get the content of `resp` using
    [**`content()`**](https://www.rdocumentation.org/packages/httr/functions/content)
    and set the `as` argument to `"raw"`. Assign the resulting vector to
    `raw_content`.

-   Print the first values in `raw_content` with
    [**`head()`**](https://www.rdocumentation.org/packages/utils/functions/head).

``` r
# Load the httr package
library(httr)

# Get the url, save response to resp
url <- "http://www.example.com/"
resp <- GET(url)

# Print resp
print(resp)
```

    ## Response [http://www.example.com/]
    ##   Date: 2022-06-23 09:10
    ##   Status: 200
    ##   Content-Type: text/html; charset=UTF-8
    ##   Size: 1.26 kB
    ## <!doctype html>
    ## <html>
    ## <head>
    ##     <title>Example Domain</title>
    ## 
    ##     <meta charset="utf-8" />
    ##     <meta http-equiv="Content-type" content="text/html; charset=utf-8" />
    ##     <meta name="viewport" content="width=device-width, initial-scale=1" />
    ##     <style type="text/css">
    ##     body {
    ## ...

``` r
# Get the raw content of resp: raw_content
raw_content <- content(resp, as = "raw")

# Print the head of raw_content
print(raw_content)
```

    ##    [1] 3c 21 64 6f 63 74 79 70 65 20 68 74 6d 6c 3e 0a 3c 68 74 6d 6c 3e 0a 3c
    ##   [25] 68 65 61 64 3e 0a 20 20 20 20 3c 74 69 74 6c 65 3e 45 78 61 6d 70 6c 65
    ##   [49] 20 44 6f 6d 61 69 6e 3c 2f 74 69 74 6c 65 3e 0a 0a 20 20 20 20 3c 6d 65
    ##   [73] 74 61 20 63 68 61 72 73 65 74 3d 22 75 74 66 2d 38 22 20 2f 3e 0a 20 20
    ##   [97] 20 20 3c 6d 65 74 61 20 68 74 74 70 2d 65 71 75 69 76 3d 22 43 6f 6e 74
    ##  [121] 65 6e 74 2d 74 79 70 65 22 20 63 6f 6e 74 65 6e 74 3d 22 74 65 78 74 2f
    ##  [145] 68 74 6d 6c 3b 20 63 68 61 72 73 65 74 3d 75 74 66 2d 38 22 20 2f 3e 0a
    ##  [169] 20 20 20 20 3c 6d 65 74 61 20 6e 61 6d 65 3d 22 76 69 65 77 70 6f 72 74
    ##  [193] 22 20 63 6f 6e 74 65 6e 74 3d 22 77 69 64 74 68 3d 64 65 76 69 63 65 2d
    ##  [217] 77 69 64 74 68 2c 20 69 6e 69 74 69 61 6c 2d 73 63 61 6c 65 3d 31 22 20
    ##  [241] 2f 3e 0a 20 20 20 20 3c 73 74 79 6c 65 20 74 79 70 65 3d 22 74 65 78 74
    ##  [265] 2f 63 73 73 22 3e 0a 20 20 20 20 62 6f 64 79 20 7b 0a 20 20 20 20 20 20
    ##  [289] 20 20 62 61 63 6b 67 72 6f 75 6e 64 2d 63 6f 6c 6f 72 3a 20 23 66 30 66
    ##  [313] 30 66 32 3b 0a 20 20 20 20 20 20 20 20 6d 61 72 67 69 6e 3a 20 30 3b 0a
    ##  [337] 20 20 20 20 20 20 20 20 70 61 64 64 69 6e 67 3a 20 30 3b 0a 20 20 20 20
    ##  [361] 20 20 20 20 66 6f 6e 74 2d 66 61 6d 69 6c 79 3a 20 2d 61 70 70 6c 65 2d
    ##  [385] 73 79 73 74 65 6d 2c 20 73 79 73 74 65 6d 2d 75 69 2c 20 42 6c 69 6e 6b
    ##  [409] 4d 61 63 53 79 73 74 65 6d 46 6f 6e 74 2c 20 22 53 65 67 6f 65 20 55 49
    ##  [433] 22 2c 20 22 4f 70 65 6e 20 53 61 6e 73 22 2c 20 22 48 65 6c 76 65 74 69
    ##  [457] 63 61 20 4e 65 75 65 22 2c 20 48 65 6c 76 65 74 69 63 61 2c 20 41 72 69
    ##  [481] 61 6c 2c 20 73 61 6e 73 2d 73 65 72 69 66 3b 0a 20 20 20 20 20 20 20 20
    ##  [505] 0a 20 20 20 20 7d 0a 20 20 20 20 64 69 76 20 7b 0a 20 20 20 20 20 20 20
    ##  [529] 20 77 69 64 74 68 3a 20 36 30 30 70 78 3b 0a 20 20 20 20 20 20 20 20 6d
    ##  [553] 61 72 67 69 6e 3a 20 35 65 6d 20 61 75 74 6f 3b 0a 20 20 20 20 20 20 20
    ##  [577] 20 70 61 64 64 69 6e 67 3a 20 32 65 6d 3b 0a 20 20 20 20 20 20 20 20 62
    ##  [601] 61 63 6b 67 72 6f 75 6e 64 2d 63 6f 6c 6f 72 3a 20 23 66 64 66 64 66 66
    ##  [625] 3b 0a 20 20 20 20 20 20 20 20 62 6f 72 64 65 72 2d 72 61 64 69 75 73 3a
    ##  [649] 20 30 2e 35 65 6d 3b 0a 20 20 20 20 20 20 20 20 62 6f 78 2d 73 68 61 64
    ##  [673] 6f 77 3a 20 32 70 78 20 33 70 78 20 37 70 78 20 32 70 78 20 72 67 62 61
    ##  [697] 28 30 2c 30 2c 30 2c 30 2e 30 32 29 3b 0a 20 20 20 20 7d 0a 20 20 20 20
    ##  [721] 61 3a 6c 69 6e 6b 2c 20 61 3a 76 69 73 69 74 65 64 20 7b 0a 20 20 20 20
    ##  [745] 20 20 20 20 63 6f 6c 6f 72 3a 20 23 33 38 34 38 38 66 3b 0a 20 20 20 20
    ##  [769] 20 20 20 20 74 65 78 74 2d 64 65 63 6f 72 61 74 69 6f 6e 3a 20 6e 6f 6e
    ##  [793] 65 3b 0a 20 20 20 20 7d 0a 20 20 20 20 40 6d 65 64 69 61 20 28 6d 61 78
    ##  [817] 2d 77 69 64 74 68 3a 20 37 30 30 70 78 29 20 7b 0a 20 20 20 20 20 20 20
    ##  [841] 20 64 69 76 20 7b 0a 20 20 20 20 20 20 20 20 20 20 20 20 6d 61 72 67 69
    ##  [865] 6e 3a 20 30 20 61 75 74 6f 3b 0a 20 20 20 20 20 20 20 20 20 20 20 20 77
    ##  [889] 69 64 74 68 3a 20 61 75 74 6f 3b 0a 20 20 20 20 20 20 20 20 7d 0a 20 20
    ##  [913] 20 20 7d 0a 20 20 20 20 3c 2f 73 74 79 6c 65 3e 20 20 20 20 0a 3c 2f 68
    ##  [937] 65 61 64 3e 0a 0a 3c 62 6f 64 79 3e 0a 3c 64 69 76 3e 0a 20 20 20 20 3c
    ##  [961] 68 31 3e 45 78 61 6d 70 6c 65 20 44 6f 6d 61 69 6e 3c 2f 68 31 3e 0a 20
    ##  [985] 20 20 20 3c 70 3e 54 68 69 73 20 64 6f 6d 61 69 6e 20 69 73 20 66 6f 72
    ## [1009] 20 75 73 65 20 69 6e 20 69 6c 6c 75 73 74 72 61 74 69 76 65 20 65 78 61
    ## [1033] 6d 70 6c 65 73 20 69 6e 20 64 6f 63 75 6d 65 6e 74 73 2e 20 59 6f 75 20
    ## [1057] 6d 61 79 20 75 73 65 20 74 68 69 73 0a 20 20 20 20 64 6f 6d 61 69 6e 20
    ## [1081] 69 6e 20 6c 69 74 65 72 61 74 75 72 65 20 77 69 74 68 6f 75 74 20 70 72
    ## [1105] 69 6f 72 20 63 6f 6f 72 64 69 6e 61 74 69 6f 6e 20 6f 72 20 61 73 6b 69
    ## [1129] 6e 67 20 66 6f 72 20 70 65 72 6d 69 73 73 69 6f 6e 2e 3c 2f 70 3e 0a 20
    ## [1153] 20 20 20 3c 70 3e 3c 61 20 68 72 65 66 3d 22 68 74 74 70 73 3a 2f 2f 77
    ## [1177] 77 77 2e 69 61 6e 61 2e 6f 72 67 2f 64 6f 6d 61 69 6e 73 2f 65 78 61 6d
    ## [1201] 70 6c 65 22 3e 4d 6f 72 65 20 69 6e 66 6f 72 6d 61 74 69 6f 6e 2e 2e 2e
    ## [1225] 3c 2f 61 3e 3c 2f 70 3e 0a 3c 2f 64 69 76 3e 0a 3c 2f 62 6f 64 79 3e 0a
    ## [1249] 3c 2f 68 74 6d 6c 3e 0a

### **`06-HTTP? httr! (2)`**

-   Use
    [**`GET()`**](https://www.rdocumentation.org/packages/httr/functions/GET)
    to get the `url` that has already been specified in the sample code.
    Store the response as `resp`.

-   Print `resp`. What is the content-type?

-   Use
    [**`content()`**](https://www.rdocumentation.org/packages/httr/functions/content)
    to get the content of `resp`. Set the `as` argument to `"text"`.
    Simply print out the result. What do you see?

-   Use
    [**`content()`**](https://www.rdocumentation.org/packages/httr/functions/content)
    to get the content of `resp`, but this time do not specify a second
    argument. R figures out automatically that you’re dealing with a
    JSON, and converts the JSON to a named R list.

``` r
# httr is already loaded

# Get the url
url <- "http://www.omdbapi.com/?apikey=72bc447a&t=Annie+Hall&y=&plot=short&r=json"
resp <- GET(url)

# Print resp
print(resp)
```

    ## Response [http://www.omdbapi.com/?apikey=72bc447a&t=Annie+Hall&y=&plot=short&r=json]
    ##   Date: 2022-06-23 09:10
    ##   Status: 200
    ##   Content-Type: application/json; charset=utf-8
    ##   Size: 1.02 kB

``` r
# Print content of resp as text
print(content(resp, as = "text"))
```

    ## [1] "{\"Title\":\"Annie Hall\",\"Year\":\"1977\",\"Rated\":\"PG\",\"Released\":\"20 Apr 1977\",\"Runtime\":\"93 min\",\"Genre\":\"Comedy, Romance\",\"Director\":\"Woody Allen\",\"Writer\":\"Woody Allen, Marshall Brickman\",\"Actors\":\"Woody Allen, Diane Keaton, Tony Roberts\",\"Plot\":\"Alvy Singer, a divorced Jewish comedian, reflects on his relationship with ex-lover Annie Hall, an aspiring nightclub singer, which ended abruptly just like his previous marriages.\",\"Language\":\"English, German\",\"Country\":\"United States\",\"Awards\":\"Won 4 Oscars. 31 wins & 8 nominations total\",\"Poster\":\"https://m.media-amazon.com/images/M/MV5BZDg1OGQ4YzgtM2Y2NS00NjA3LWFjYTctMDRlMDI3NWE1OTUyXkEyXkFqcGdeQXVyMjUzOTY1NTc@._V1_SX300.jpg\",\"Ratings\":[{\"Source\":\"Internet Movie Database\",\"Value\":\"8.0/10\"},{\"Source\":\"Rotten Tomatoes\",\"Value\":\"97%\"},{\"Source\":\"Metacritic\",\"Value\":\"92/100\"}],\"Metascore\":\"92\",\"imdbRating\":\"8.0\",\"imdbVotes\":\"262,867\",\"imdbID\":\"tt0075686\",\"Type\":\"movie\",\"DVD\":\"05 Jul 2000\",\"BoxOffice\":\"$38,251,425\",\"Production\":\"N/A\",\"Website\":\"N/A\",\"Response\":\"True\"}"

``` r
# Print content of resp
print(content(resp))
```

    ## $Title
    ## [1] "Annie Hall"
    ## 
    ## $Year
    ## [1] "1977"
    ## 
    ## $Rated
    ## [1] "PG"
    ## 
    ## $Released
    ## [1] "20 Apr 1977"
    ## 
    ## $Runtime
    ## [1] "93 min"
    ## 
    ## $Genre
    ## [1] "Comedy, Romance"
    ## 
    ## $Director
    ## [1] "Woody Allen"
    ## 
    ## $Writer
    ## [1] "Woody Allen, Marshall Brickman"
    ## 
    ## $Actors
    ## [1] "Woody Allen, Diane Keaton, Tony Roberts"
    ## 
    ## $Plot
    ## [1] "Alvy Singer, a divorced Jewish comedian, reflects on his relationship with ex-lover Annie Hall, an aspiring nightclub singer, which ended abruptly just like his previous marriages."
    ## 
    ## $Language
    ## [1] "English, German"
    ## 
    ## $Country
    ## [1] "United States"
    ## 
    ## $Awards
    ## [1] "Won 4 Oscars. 31 wins & 8 nominations total"
    ## 
    ## $Poster
    ## [1] "https://m.media-amazon.com/images/M/MV5BZDg1OGQ4YzgtM2Y2NS00NjA3LWFjYTctMDRlMDI3NWE1OTUyXkEyXkFqcGdeQXVyMjUzOTY1NTc@._V1_SX300.jpg"
    ## 
    ## $Ratings
    ## $Ratings[[1]]
    ## $Ratings[[1]]$Source
    ## [1] "Internet Movie Database"
    ## 
    ## $Ratings[[1]]$Value
    ## [1] "8.0/10"
    ## 
    ## 
    ## $Ratings[[2]]
    ## $Ratings[[2]]$Source
    ## [1] "Rotten Tomatoes"
    ## 
    ## $Ratings[[2]]$Value
    ## [1] "97%"
    ## 
    ## 
    ## $Ratings[[3]]
    ## $Ratings[[3]]$Source
    ## [1] "Metacritic"
    ## 
    ## $Ratings[[3]]$Value
    ## [1] "92/100"
    ## 
    ## 
    ## 
    ## $Metascore
    ## [1] "92"
    ## 
    ## $imdbRating
    ## [1] "8.0"
    ## 
    ## $imdbVotes
    ## [1] "262,867"
    ## 
    ## $imdbID
    ## [1] "tt0075686"
    ## 
    ## $Type
    ## [1] "movie"
    ## 
    ## $DVD
    ## [1] "05 Jul 2000"
    ## 
    ## $BoxOffice
    ## [1] "$38,251,425"
    ## 
    ## $Production
    ## [1] "N/A"
    ## 
    ## $Website
    ## [1] "N/A"
    ## 
    ## $Response
    ## [1] "True"

### `The End`
