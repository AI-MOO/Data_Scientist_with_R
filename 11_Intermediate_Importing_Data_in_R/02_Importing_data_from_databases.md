Importing data from databases (Part 2)
================
Mohamad Osman
2022-06-22

# Section 02: Importing data from databases (Part 2)

### **`01-Query tweater (1)`**

`Query Example:`

    dbGetQuery(con, "SELECT age FROM people WHERE gender = 'male'")

A connection to the `tweater` database has already been coded for you.

-   Use
    [**`dbGetQuery()`**](https://www.rdocumentation.org/packages/DBI/functions/dbGetQuery)
    to create a data frame, `elisabeth`, that **selects** the `tweat_id`
    column **from** the `comments` table **where** elisabeth is the
    commenter, her `user_id` is 1

-   Print out `elisabeth` so you can see if you queried the database
    correctly.

``` r
# Connect to the database
library(DBI)
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "tweater",
                 host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com",
                 port = 3306,
                 user = "student",
                 password = "datacamp")

# Import tweat_id column of comments where user_id is 1: elisabeth
elisabeth <- dbGetQuery(con, "SELECT tweat_id FROM comments WHERE user_id = 1")

# Print elisabeth
elisabeth
```

    ##   tweat_id
    ## 1       87
    ## 2       49
    ## 3       77
    ## 4       77

### **`02-Query tweater (2)`**

-   Create a data frame, `latest`, that **selects** the `post` column
    **from** the `tweats` table observations **where** the `date` is
    higher than `'2015-09-21'`.

-   Print out `latest`.

``` r
# Import post column of tweats where date is higher than '2015-09-21': latest
latest <- dbGetQuery(con, "SELECT post FROM tweats 
                            WHERE date > '2015-09-21'")

# Print latest
latest
```

    ##                                                                  post
    ## 1               open and crush avocado. add shrimps. perfect starter.
    ## 2 nachos. add tomato sauce, minced meat and cheese. oven for 10 mins.
    ## 3                              just eat an apple. simply and healthy.

### **`03-Query tweater (3)`**

-   Create an R data frame, `specific`, that **selects** the `message`
    column **from** the `comments` table **where** the `tweat_id` is 77
    **and** the `user_id` is greater than 4.

-   Print `specific`.

``` r
# Create data frame specific
specific <- dbGetQuery(con, "SELECT message FROM comments
                             WHERE tweat_id = 77 AND user_id > 4")
# Print specific
specific
```

    ##   message
    ## 1  great!

### **`04-Query tweater (4)`**

There are also dedicated SQL functions that you can use in the `WHERE`
clause of an SQL query. For example, `CHAR_LENGTH()` returns the number
of characters in a string.

-   Create a data frame, `short`, that **selects** the `id` and `name`
    columns **from** the `users` table **where** the number of
    characters in the `name` is strictly less than 5.

-   Print `short`.

``` r
# Create data frame short
short <- dbGetQuery(con, "SELECT id, name FROM users 
                          WHERE CHAR_LENGTH(name) < 5")

# Print short
short
```

    ##   id name
    ## 1  2 mike
    ## 2  3 thea
    ## 3  6 kate

### **`05-Join the query madness!`**

`Inner Join !`

    SELECT name, post
      FROM users INNER JOIN tweats on users.id = user_id
        WHERE date > "2015-09-19"

    SELECT post, message
      FROM tweats INNER JOIN comments on tweats.id = tweat_id
        WHERE tweat_id = 77

``` r
# Create data frame short
short <- dbGetQuery(con, "SELECT post, message
                          FROM tweats INNER JOIN comments on tweats.id = tweat_id
                          WHERE tweat_id = 77")

# Print short
short
```

    ##                                            post            message
    ## 1 2 slices of bread. add cheese. grill. heaven.             great!
    ## 2 2 slices of bread. add cheese. grill. heaven.      not my thing!
    ## 3 2 slices of bread. add cheese. grill. heaven. couldn't be better
    ## 4 2 slices of bread. add cheese. grill. heaven.       saved my day

### **`06-Send - Fetch - Clear`**

You???ve used
[**`dbGetQuery()`**](https://www.rdocumentation.org/packages/DBI/functions/dbGetQuery)
multiple times now. This is a virtual function from the `DBI` package,
but is actually implemented by the `RMySQL` package. Behind the scenes,
the following steps are performed:

-   Sending the specified query with
    [**`dbSendQuery()`**](https://www.rdocumentation.org/packages/DBI/functions/dbSendQuery);

-   Fetching the result of executing the query on the database with
    [**`dbFetch()`**](https://www.rdocumentation.org/packages/DBI/functions/dbFetch);

-   Clearing the result with
    [**`dbClearResult()`**](https://www.rdocumentation.org/packages/DBI/functions/dbClearResult).

Let???s not use
[**`dbGetQuery()`**](https://www.rdocumentation.org/packages/DBI/functions/dbGetQuery)
this time and implement the steps above. This is tedious to write, but
it gives you the ability to fetch the query???s result in chunks rather
than all at once. You can do this by specifying the `n` argument inside
[**`dbFetch()`**](https://www.rdocumentation.org/packages/DBI/functions/dbFetch).

#### 

`Questions`

-   Inspect the
    [**`dbSendQuery()`**](https://www.rdocumentation.org/packages/DBI/functions/dbSendQuery)
    call that has already been coded for you. It selects the comments
    for the users with an id above 4.

-   Use
    [**`dbFetch()`**](https://www.rdocumentation.org/packages/DBI/functions/dbFetch)
    twice. In the first call, import only two records of the query
    result by setting the `n` argument to `2`. In the second call,
    import all remaining queries (don???t specify `n`). In both calls,
    simply print the resulting data frames.

-   Clear `res` with
    [**`dbClearResult()`**](https://www.rdocumentation.org/packages/DBI/functions/dbClearResult).

``` r
# Connect to the database
library(DBI)
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "tweater",
                 host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com",
                 port = 3306,
                 user = "student",
                 password = "datacamp")

# Send query to the database
res <- dbSendQuery(con, "SELECT * FROM comments WHERE user_id > 4")

# Use dbFetch() twice
dbFetch(res, n = 2)
```

    ##     id tweat_id user_id message
    ## 1 1022       87       7   nice!
    ## 2 1000       77       7  great!

``` r
dbFetch(res)
```

    ##     id tweat_id user_id  message
    ## 1 1011       49       5  love it
    ## 2 1010       88       6    yuck!
    ## 3 1030       75       6 so easy!

``` r
# Clear res
dbClearResult(res)
```

    ## [1] TRUE

### **`07-Be polite and ...`**

-   Using the technique you prefer, build a data frame `long_tweats`. It
    **selects** the `post` and `date` columns **from** the observations
    in `tweats` **where** the character length of the `post` variable
    exceeds `40`.

-   Print `long_tweats`.

-   Disconnect from the database by using
    [**`dbDisconnect()`**](https://www.rdocumentation.org/packages/DBI/functions/dbDisconnect).

``` r
# Connect to the database
library(DBI)
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "tweater",
                 host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com",
                 port = 3306,
                 user = "student",
                 password = "datacamp")

# Create the data frame  long_tweats
long_tweats <- dbGetQuery(con, "SELECT post, date FROM tweats
                                WHERE CHAR_LENGTH(post) > 40")

# Print long_tweats
print(long_tweats)
```

    ##                                                                  post
    ## 1                           wash strawberries. add ice. blend. enjoy.
    ## 2                       2 slices of bread. add cheese. grill. heaven.
    ## 3               open and crush avocado. add shrimps. perfect starter.
    ## 4 nachos. add tomato sauce, minced meat and cheese. oven for 10 mins.
    ##         date
    ## 1 2015-09-14
    ## 2 2015-09-21
    ## 3 2015-09-22
    ## 4 2015-09-22

``` r
# Disconnect from the database
dbDisconnect(con)
```

    ## [1] TRUE

### `The End`
