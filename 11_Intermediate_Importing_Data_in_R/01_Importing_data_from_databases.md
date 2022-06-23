Importing data from databases
================
Mohamad Osman
2022-06-22

# Section 01: Importing data from databases

### **`01-Establish a connection`**

The first step to import data from a SQL database is creating a
connection to it. As Filip explained, you need different packages
depending on the database you want to connect to. All of these packages
do this in a uniform way, as specified in the `DBI` package.

[**`dbConnect()`**](https://www.rdocumentation.org/packages/DBI/functions/dbConnect)
creates a connection between your R session and a SQL database. The
first argument has to be a `DBIdriver` object, that specifies how
connections are made and how data is mapped between R and the database.
Specifically for MySQL databases, you can build such a driver with
[**`RMySQL::MySQL()`**](https://www.rdocumentation.org/packages/RMySQL/functions/MySQLDriver-class).

If the MySQL database is a remote database hosted on a server, you’ll
also have to specify the following arguments in
[**`dbConnect()`**](https://www.rdocumentation.org/packages/DBI/functions/dbConnect):
`dbname`, `host`, `port`, `user` and `password`. Most of these details
have already been provided.

-   Load the `DBI` library, which is already installed on DataCamp’s
    servers.

-   Edit the
    [**`dbConnect()`**](https://www.rdocumentation.org/packages/DBI/functions/dbConnect)
    call to connect to the MySQL database. Change the `port` argument
    (`3306`) and `user` argument (`"student"`).

``` r
# install.packages("RMySQL")
```

``` r
# Load the DBI package
library(DBI)

# Edit dbConnect() call
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "tweater", 
                 host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                 port = 3306,
                 user = "student",
                 password = "datacamp")

con
```

    ## <MySQLConnection:0,0>

``` r
# disconnect to the database
dbDisconnect(con)
```

    ## [1] TRUE

### **`02-List the database tables`**

-   Add code to create a vector `tables`, that contains the tables in
    the tweater database. You can connect to this database through the
    `con` object.

-   Display the structure of `tables`; what’s the class of this vector?

``` r
# Load the DBI package
library(DBI)

# Connect to the MySQL database: con
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "tweater", 
                 host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                 port = 3306,
                 user = "student",
                 password = "datacamp")

# Build a vector of table names: tables
tables <- dbListTables(con)

# Display structure of tables
str(tables)
```

    ##  chr [1:3] "comments" "tweats" "users"

``` r
# disconnect to the database
dbDisconnect(con)
```

    ## [1] TRUE

### **`03-Import users`**

There are three tables: **`users`**, **`tweats`**, and **`comments`**
that have relations among them. Which ones, you ask? You’ll discover in
a moment!

Let’s start by importing the data on the users into your R session. You
do this with the
[**`dbReadTable()`**](https://www.rdocumentation.org/packages/DBI/functions/dbReadTable)
function. Simply pass it the connection object (`con`), followed by the
name of the table you want to import. The resulting object is a standard
R data frame.

-   Add code that imports the `"users"` table from the tweater database
    and store the resulting data frame as `users`.

-   Print the `users` data frame.

``` r
# Load the DBI package
library(DBI)

# Connect to the MySQL database: con
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "tweater", 
                 host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                 port = 3306,
                 user = "student",
                 password = "datacamp")

# Import the users table from tweater: users
users <- dbReadTable(con, "users")

# Print users
users
```

    ##   id      name     login
    ## 1  1 elisabeth  elismith
    ## 2  2      mike     mikey
    ## 3  3      thea   teatime
    ## 4  4    thomas tomatotom
    ## 5  5    oliver olivander
    ## 6  6      kate  katebenn
    ## 7  7    anjali    lianja

``` r
# disconnect to the database
dbDisconnect(con)
```

    ## [1] TRUE

### **`04-Import all tables`**

Next to the `users`, we’re also interested in the `tweats` and
`comments` tables. However, separate `dbReadTable()` calls for each and
every one of the tables in your database would mean a lot of code
duplication. Remember about the `lapply()` function? You can use it
again here! A connection is already coded for you, as well as a vector
`table_names`, containing the names of all the tables in the database.

-   Finish the `lapply()` function to import the `users`, `tweats` and
    `comments` tables in a single call. The result, a list of data
    frames, will be stored in the variable `tables`.

-   Print `tables` to check if you got it right.

``` r
# Load the DBI package
library(DBI)

# Connect to the MySQL database: con
con <- dbConnect(RMySQL::MySQL(), 
                 dbname = "tweater", 
                 host = "courses.csrrinzqubik.us-east-1.rds.amazonaws.com", 
                 port = 3306,
                 user = "student",
                 password = "datacamp")

# Get table names
table_names <- dbListTables(con)

# Import all tables
tables <- lapply(table_names, dbReadTable, conn = con)

# Print out tables
tables
```

    ## [[1]]
    ##      id tweat_id user_id            message
    ## 1  1022       87       7              nice!
    ## 2  1000       77       7             great!
    ## 3  1011       49       5            love it
    ## 4  1012       87       1   awesome! thanks!
    ## 5  1010       88       6              yuck!
    ## 6  1026       77       4      not my thing!
    ## 7  1004       49       1  this is fabulous!
    ## 8  1030       75       6           so easy!
    ## 9  1025       88       2             oh yes
    ## 10 1007       49       3           serious?
    ## 11 1020       77       1 couldn't be better
    ## 12 1014       77       1       saved my day
    ## 
    ## [[2]]
    ##   id user_id
    ## 1 75       3
    ## 2 88       4
    ## 3 77       6
    ## 4 87       5
    ## 5 49       1
    ## 6 24       7
    ##                                                                  post
    ## 1                                       break egg. bake egg. eat egg.
    ## 2                           wash strawberries. add ice. blend. enjoy.
    ## 3                       2 slices of bread. add cheese. grill. heaven.
    ## 4               open and crush avocado. add shrimps. perfect starter.
    ## 5 nachos. add tomato sauce, minced meat and cheese. oven for 10 mins.
    ## 6                              just eat an apple. simply and healthy.
    ##         date
    ## 1 2015-09-05
    ## 2 2015-09-14
    ## 3 2015-09-21
    ## 4 2015-09-22
    ## 5 2015-09-22
    ## 6 2015-09-24
    ## 
    ## [[3]]
    ##   id      name     login
    ## 1  1 elisabeth  elismith
    ## 2  2      mike     mikey
    ## 3  3      thea   teatime
    ## 4  4    thomas tomatotom
    ## 5  5    oliver olivander
    ## 6  6      kate  katebenn
    ## 7  7    anjali    lianja

``` r
# disconnect to the database
dbDisconnect(con)
```

    ## [1] TRUE

### **`The End`**

  
