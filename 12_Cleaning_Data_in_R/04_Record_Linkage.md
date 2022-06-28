Record Linkage
================
Mohamad Osman
2022-06-27

# Section 04: Record Linkage

### **`01-Small distance, small difference`**

-   Calculate the Damerau-Levenshtein distance between `"las angelos"`
    and `"los angeles"`.

``` r
library("stringdist")
```

``` r
# Calculate Damerau-Levenshtein distance
stringdist("las angelos", "los angeles", method = "dl")
```

    ## [1] 2

-   Calculate the Longest Common Substring (LCS) distance between
    `"las angelos"` and `"los angeles"`.

``` r
# Calculate LCS distance
stringdist("las angelos", "los angeles", method = "lcs")
```

    ## [1] 4

-   Calculate the Jaccard distance between `"las angelos"` and
    `"los angeles"`.

``` r
# Calculate Jaccard distance
stringdist("las angelos", "los angeles", method = "jaccard")
```

    ## [1] 0

Why is the LCS distance higher than the Damerau-Levenshtein distance
between `"las angelos"` and `"los angeles"`?

\- LCS distance only uses insertion and deletion, so it takes more
operations to change a string to another.

### **`02-Fixing typos with string distance`**

``` r
# Load packages 
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
    ## ✖ tidyr::extract() masks stringdist::extract()
    ## ✖ dplyr::filter()  masks stats::filter()
    ## ✖ dplyr::lag()     masks stats::lag()

``` r
library(fuzzyjoin)
```

``` r
city_actual <- c("new york", "los angeles", "atlanta", 
                  "san francisco", "las vegas")
cities <- data.frame(city_actual)
cities
```

    ##     city_actual
    ## 1      new york
    ## 2   los angeles
    ## 3       atlanta
    ## 4 san francisco
    ## 5     las vegas

``` r
file_path <- file.path("..", "00_Datasets", "zagat.rds")
zagat <- readRDS(file_path)
head(zagat, 3)
```

    ##   id          name                addr        city        phone         type
    ## 1  0 apple pan the 10801 w. pico blvd. los angeles 310-475-3585     american
    ## 2  1   asahi ramen 2027 sawtelle blvd. los angeles 310-479-2231 noodle shops
    ## 3  2    baja fresh     3345 kimber dr. los angeles 805-498-4049      mexican
    ##   class
    ## 1   534
    ## 2   535
    ## 3   536

-   Count the number of each variation of `city` name in `zagat`.

``` r
# Count the number of each city variation
zagat %>%
  count(city)
```

    ##            city  n
    ## 1       atlanta 64
    ## 2   los angeles 72
    ## 3      new york 98
    ## 4     las vegas 26
    ## 5 san francisco 50

-   Left join `zagat` and `cities` based on string distance using the
    `city` and `city_actual` columns.

-   Select the `name`, `city`, and `city_actual` columns.

``` r
# Count the number of each city variation
zagat %>%
  count(city)
```

    ##            city  n
    ## 1       atlanta 64
    ## 2   los angeles 72
    ## 3      new york 98
    ## 4     las vegas 26
    ## 5 san francisco 50

``` r
# Join zagat and cities and look at results
zagat %>%
  # Left join based on stringdist using city and city_actual cols
  stringdist_left_join(cities, by = c("city" = "city_actual")) %>%
  # Select the name, city, and city_actual cols
  select(name, city, city_actual)
```

    ##                                           name          city   city_actual
    ## 1                                apple pan the   los angeles   los angeles
    ## 2                                  asahi ramen   los angeles   los angeles
    ## 3                                   baja fresh   los angeles   los angeles
    ## 4                                belvedere the   los angeles   los angeles
    ## 5                              benita's frites   los angeles   los angeles
    ## 6                                    bernard's   los angeles   los angeles
    ## 7                                    bistro 45   los angeles   los angeles
    ## 8                         brighton coffee shop   los angeles   los angeles
    ## 9                    bristol farms market cafe   los angeles   los angeles
    ## 10                                    cafe'50s   los angeles   los angeles
    ## 11                                  cafe blanc   los angeles   los angeles
    ## 12                                   cassell's   los angeles   los angeles
    ## 13                                   diaghilev   los angeles   los angeles
    ## 14                               don antonio's   los angeles   los angeles
    ## 15                                      duke's   los angeles   los angeles
    ## 16                                falafel king   los angeles   los angeles
    ## 17                         feast from the east   los angeles   los angeles
    ## 18                               gumbo pot the   los angeles   los angeles
    ## 19                                   indo cafe   los angeles   los angeles
    ## 20                     jan's family restaurant   los angeles   los angeles
    ## 21                                     jiraffe   los angeles   los angeles
    ## 22               jody maroni's sausage kingdom   los angeles   los angeles
    ## 23                                       joe's   los angeles   los angeles
    ## 24                            john o  ` groats   los angeles   los angeles
    ## 25                       johnny rockets ( la )   los angeles   los angeles
    ## 26                               killer shrimp   los angeles   los angeles
    ## 27                                 kokomo cafe   los angeles   los angeles
    ## 28                                 koo koo roo   los angeles   los angeles
    ## 29                             la salsa ( la )   los angeles   los angeles
    ## 30                                    langer's   los angeles   los angeles
    ## 31                                local nochol   los angeles   los angeles
    ## 32                mani's bakery & espresso bar   los angeles   los angeles
    ## 33                   michael's ( los angeles )   los angeles   los angeles
    ## 34                                     mishima   los angeles   los angeles
    ## 35                       mo better meatty meat   los angeles   los angeles
    ## 36                                mulberry st.   los angeles   los angeles
    ## 37                             ocean park cafe   los angeles   los angeles
    ## 38                      original pantry bakery   los angeles   los angeles
    ## 39                               parkway grill   los angeles   los angeles
    ## 40                                     pho hoa   los angeles   los angeles
    ## 41                    pink's famous chili dogs   los angeles   los angeles
    ## 42                                        r-23   los angeles   los angeles
    ## 43                                       rae's   los angeles   los angeles
    ## 44                            rubin's red hots   los angeles   los angeles
    ## 45                               ruby's ( la )   los angeles   los angeles
    ## 46    ruth's chris steak house ( los angeles )   los angeles   los angeles
    ## 47                                       shiro   los angeles   los angeles
    ## 48                                sushi nozawa   los angeles   los angeles
    ## 49                             sweet lady jane   los angeles   los angeles
    ## 50                                     tommy's   los angeles   los angeles
    ## 51                                 water grill   los angeles   los angeles
    ## 52                          afghan kebab house      new york      new york
    ## 53                                     arcadia      new york      new york
    ## 54                            benny's burritos      new york      new york
    ## 55                              cafe con leche      new york      new york
    ## 56                               corner bistro      new york      new york
    ## 57                        cucina della fontana      new york      new york
    ## 58                             cucina di pesce      new york      new york
    ## 59                                      darbar      new york      new york
    ## 60                           ej's luncheonette      new york      new york
    ## 61                                 edison cafe      new york      new york
    ## 62                                elias corner      new york      new york
    ## 63                          good enough to eat      new york      new york
    ## 64                               gray's papaya      new york      new york
    ## 65                                   il mulino      new york      new york
    ## 66                               jackson diner      new york      new york
    ## 67                              joe's shanghai      new york      new york
    ## 68                             john's pizzeria      new york      new york
    ## 69                               kelley & ping      new york      new york
    ## 70                                        kiev      new york      new york
    ## 71                                kuruma zushi      new york      new york
    ## 72                                  la caridad      new york      new york
    ## 73                               la grenouille      new york      new york
    ## 74                            lemongrass grill      new york      new york
    ## 75                                  lombardi's      new york      new york
    ## 76                        marnie's noodle shop      new york      new york
    ## 77                               menchanko-tei      new york      new york
    ## 78                            mitali east-west      new york      new york
    ## 79                              monsoon ( ny )      new york      new york
    ## 80                                   moustache      new york      new york
    ## 81                                        nobu      new york      new york
    ## 82                         one if by land tibs      new york      new york
    ## 83                                  oyster bar      new york      new york
    ## 84                                        palm      new york      new york
    ## 85                                    palm too      new york      new york
    ## 86                               patsy's pizza      new york      new york
    ## 87                     peter luger steak house      new york      new york
    ## 88                               rose of india      new york      new york
    ## 89                           sam's noodle shop      new york      new york
    ## 90                                  sarabeth's      new york      new york
    ## 91                          sparks steak house      new york      new york
    ## 92                          stick to your ribs      new york      new york
    ## 93                                    sushisay      new york      new york
    ## 94                                    sylvia's      new york      new york
    ## 95                      szechuan hunan cottage      new york      new york
    ## 96                            szechuan kitchen      new york      new york
    ## 97                                    teresa's      new york      new york
    ## 98                             thai house cafe      new york      new york
    ## 99                         thailand restaurant      new york      new york
    ## 100                                    veselka      new york      new york
    ## 101                           westside cottage      new york      new york
    ## 102                       windows on the world      new york      new york
    ## 103                          wollensky's grill      new york      new york
    ## 104                                       yama      new york      new york
    ## 105                                     zarela      new york      new york
    ## 106                  andre's french restaurant     las vegas     las vegas
    ## 107                         buccaneer bay club     las vegas     las vegas
    ## 108                         buzio's in the rio     las vegas     las vegas
    ## 109          'em eril's new orleans fish house     las vegas     las vegas
    ## 110                  fiore rotisserie & grille     las vegas     las vegas
    ## 111                              hugo's cellar     las vegas     las vegas
    ## 112                             madame ching's     las vegas     las vegas
    ## 113                        mayflower cuisinier     las vegas     las vegas
    ## 114                    michael's ( las vegas )     las vegas     las vegas
    ## 115                                monte carlo     las vegas     las vegas
    ## 116                                   moongate     las vegas     las vegas
    ## 117          morton's of chicago ( las vegas )     las vegas     las vegas
    ## 118                              nicky blair's     las vegas     las vegas
    ## 119                         piero's restaurant     las vegas     las vegas
    ## 120                        spago ( las vegas )     las vegas     las vegas
    ## 121                             steakhouse the     las vegas     las vegas
    ## 122                                  stefano's     las vegas     las vegas
    ## 123                            sterling brunch     las vegas     las vegas
    ## 124                                   tre visi     las vegas     las vegas
    ## 125                                 ' 103 west       atlanta       atlanta
    ## 126                      alon's at the terrace       atlanta       atlanta
    ## 127                         baker's cajun cafe       atlanta       atlanta
    ## 128                           barbecue kitchen       atlanta       atlanta
    ## 129                                 bistro the       atlanta       atlanta
    ## 130             bobby & june's kountry kitchen       atlanta       atlanta
    ## 131                      bradshaw's restaurant       atlanta       atlanta
    ## 132                            brookhaven cafe       atlanta       atlanta
    ## 133                             cafe sunflower       atlanta       atlanta
    ## 134                                      canoe       atlanta       atlanta
    ## 135                                    carey's       atlanta       atlanta
    ## 136                             carey's corner       atlanta       atlanta
    ## 137                                      chops       atlanta       atlanta
    ## 138                                   chopstix       atlanta       atlanta
    ## 139        deacon burton's soulfood restaurant       atlanta       atlanta
    ## 140                                       eats       atlanta       atlanta
    ## 141                         flying biscuit the       atlanta       atlanta
    ## 142                                 frijoleros       atlanta       atlanta
    ## 143                                greenwood's       atlanta       atlanta
    ## 144                          harold's barbecue       atlanta       atlanta
    ## 145                       havana sandwich shop       atlanta       atlanta
    ## 146                            indian delights       atlanta       atlanta
    ## 147                                  java jive       atlanta       atlanta
    ## 148                      johnny rockets ( at )       atlanta       atlanta
    ## 149                        kalo's coffee house       atlanta       atlanta
    ## 150                            la fonda latina       atlanta       atlanta
    ## 151                lettuce souprise you ( at )       atlanta       atlanta
    ## 152                                   majestic       atlanta       atlanta
    ## 153            morton's of chicago ( atlanta )       atlanta       atlanta
    ## 154                                    my thai       atlanta       atlanta
    ## 155                                       nava       atlanta       atlanta
    ## 156                       nuevo laredo cantina       atlanta       atlanta
    ## 157              original pancake house ( at )       atlanta       atlanta
    ## 158                       palm the ( atlanta )       atlanta       atlanta
    ## 159                         rainbow restaurant       atlanta       atlanta
    ## 160                                    riviera       atlanta       atlanta
    ## 161                         silver skillet the       atlanta       atlanta
    ## 162                                       soto       atlanta       atlanta
    ## 163                           thelma's kitchen       atlanta       atlanta
    ## 164                                  tortillas       atlanta       atlanta
    ## 165                van gogh's restaurant & bar       atlanta       atlanta
    ## 166                                 veggieland       atlanta       atlanta
    ## 167                     white house restaurant       atlanta       atlanta
    ## 168                               bill's place san francisco san francisco
    ## 169                                 cafe flore san francisco san francisco
    ## 170                                caffe greco san francisco san francisco
    ## 171                                campo santo san francisco san francisco
    ## 172                              cha cha cha's san francisco san francisco
    ## 173                                   doidge's san francisco san francisco
    ## 174                    dottie's true blue cafe san francisco san francisco
    ## 175                                 dusit thai san francisco san francisco
    ## 176                                      ebisu san francisco san francisco
    ## 177                'em erald garden restaurant san francisco san francisco
    ## 178                  eric's chinese restaurant san francisco san francisco
    ## 179                           hamburger mary's san francisco san francisco
    ## 180                         kelly's on trinity san francisco san francisco
    ## 181                                  la cumbre san francisco san francisco
    ## 182                            la mediterranee san francisco san francisco
    ## 183                                la taqueria san francisco san francisco
    ## 184          mario's bohemian cigar store cafe san francisco san francisco
    ## 185                                marnee thai san francisco san francisco
    ## 186                             mel's drive-in san francisco san francisco
    ## 187                               mo's burgers san francisco san francisco
    ## 188            phnom penh cambodian restaurant san francisco san francisco
    ## 189                    roosevelt tamale parlor san francisco san francisco
    ## 190                      sally's cafe & bakery san francisco san francisco
    ## 191                          san francisco bbq san francisco san francisco
    ## 192                               slanted door san francisco san francisco
    ## 193                          swan oyster depot san francisco san francisco
    ## 194                                thep phanom san francisco san francisco
    ## 195                                    ti couz san francisco san francisco
    ## 196                                  trio cafe san francisco san francisco
    ## 197                                     tu lan san francisco san francisco
    ## 198                            vicolo pizzeria san francisco san francisco
    ## 199              wa-ha-ka oaxaca mexican grill san francisco san francisco
    ## 200                  arnie morton's of chicago   los angeles   los angeles
    ## 201                                 art's deli   los angeles   los angeles
    ## 202                              bel-air hotel   los angeles   los angeles
    ## 203                                  campanile   los angeles   los angeles
    ## 204                            chinois on main   los angeles   los angeles
    ## 205                                     citrus   los angeles   los angeles
    ## 206                        fenix at the argyle   los angeles   los angeles
    ## 207                                    granita   los angeles   los angeles
    ## 208                                  grill the   los angeles   los angeles
    ## 209                             l  ` orangerie   los angeles   los angeles
    ## 210              le chardonnay ( los angeles )   los angeles   los angeles
    ## 211                             locanda veneta   los angeles   los angeles
    ## 212                                  matsuhisa   los angeles   los angeles
    ## 213                   palm the ( los angeles )   los angeles   los angeles
    ## 214                                     patina   los angeles   los angeles
    ## 215                      philippe the original   los angeles   los angeles
    ## 216                               pinot bistro   los angeles   los angeles
    ## 217                          rex il ristorante   los angeles   los angeles
    ## 218                      spago ( los angeles )   los angeles   los angeles
    ## 219                                  valentino   los angeles   los angeles
    ## 220                              yujean kang's   los angeles   los angeles
    ## 221                                   '21 club      new york      new york
    ## 222                                    aquavit      new york      new york
    ## 223                                    aureole      new york      new york
    ## 224                                  cafe lalo      new york      new york
    ## 225                          cafe des artistes      new york      new york
    ## 226                                  carmine's      new york      new york
    ## 227                              carnegie deli      new york      new york
    ## 228                                chanterelle      new york      new york
    ## 229                                     daniel      new york      new york
    ## 230                                      dawat      new york      new york
    ## 231                                    felidia      new york      new york
    ## 232                               four seasons      new york      new york
    ## 233                         gotham bar & grill      new york      new york
    ## 234                            gramercy tavern      new york      new york
    ## 235                               island spice      new york      new york
    ## 236                                      jo jo      new york      new york
    ## 237                               la caravelle      new york      new york
    ## 238                             la cote basque      new york      new york
    ## 239                               le bernardin      new york      new york
    ## 240                             les celebrites      new york      new york
    ## 241               lespinasse ( new york city )      new york      new york
    ## 242                                     lutece      new york      new york
    ## 243                       manhattan ocean club      new york      new york
    ## 244                                      march      new york      new york
    ## 245                                 mesa grill      new york      new york
    ## 246                                  mi cocina      new york      new york
    ## 247                                 montrachet      new york      new york
    ## 248                                     oceana      new york      new york
    ## 249         park avenue cafe ( new york city )      new york      new york
    ## 250                                 petrossian      new york      new york
    ## 251                                  picholine      new york      new york
    ## 252                                     pisces      new york      new york
    ## 253                               rainbow room      new york      new york
    ## 254                                 river cafe      new york      new york
    ## 255                               san domenico      new york      new york
    ## 256                         second avenue deli      new york      new york
    ## 257                                     seryna      new york      new york
    ## 258                            shun lee palace      new york      new york
    ## 259                           sign of the dove      new york      new york
    ## 260                          smith & wollensky      new york      new york
    ## 261                        tavern on the green      new york      new york
    ## 262                               uncle nick's      new york      new york
    ## 263                          union square cafe      new york      new york
    ## 264                          virgil's real bbq      new york      new york
    ## 265                                     chin's     las vegas     las vegas
    ## 266                  coyote cafe ( las vegas )     las vegas     las vegas
    ## 267                       le montrachet bistro     las vegas     las vegas
    ## 268                               palace court     las vegas     las vegas
    ## 269                        second street grill     las vegas     las vegas
    ## 270                            steak house the     las vegas     las vegas
    ## 271                            'till erman the     las vegas     las vegas
    ## 272                                    abruzzi       atlanta       atlanta
    ## 273                                bacchanalia       atlanta       atlanta
    ## 274                          bone's restaurant       atlanta       atlanta
    ## 275                          brasserie le coze       atlanta       atlanta
    ## 276                             buckhead diner       atlanta       atlanta
    ## 277                      ciboulette restaurant       atlanta       atlanta
    ## 278                                delectables       atlanta       atlanta
    ## 279                             georgia grille       atlanta       atlanta
    ## 280                  hedgerose heights inn the       atlanta       atlanta
    ## 281                             heera of india       atlanta       atlanta
    ## 282                       indigo coastal grill       atlanta       atlanta
    ## 283                                  la grotta       atlanta       atlanta
    ## 284                        mary mac's tea room       atlanta       atlanta
    ## 285                             nikolai's roof       atlanta       atlanta
    ## 286                          pano's & paul  's       atlanta       atlanta
    ## 287             ritz-carlton cafe ( buckhead )       atlanta       atlanta
    ## 288      ritz-carlton dining room ( buckhead )       atlanta       atlanta
    ## 289                    ritz-carlton restaurant       atlanta       atlanta
    ## 290                                   toulouse       atlanta       atlanta
    ## 291                             veni vidi vici       atlanta       atlanta
    ## 292                             alain rondelli san francisco san francisco
    ## 293                                       aqua san francisco san francisco
    ## 294                                  boulevard san francisco san francisco
    ## 295                                cafe claude san francisco san francisco
    ## 296                              campton place san francisco san francisco
    ## 297                                chez michel san francisco san francisco
    ## 298                               fleur de lys san francisco san francisco
    ## 299                                   fringale san francisco san francisco
    ## 300                             hawthorne lane san francisco san francisco
    ## 301                       khan toke thai house san francisco san francisco
    ## 302                                   la folie san francisco san francisco
    ## 303                   lulu restaurant-bis-cafe san francisco san francisco
    ## 304                                     masa's san francisco san francisco
    ## 305                                     mifune san francisco san francisco
    ## 306                             plumpjack cafe san francisco san francisco
    ## 307                                    postrio san francisco san francisco
    ## 308 ritz-carlton dining room ( san francisco ) san francisco san francisco
    ## 309                               rose pistola san francisco san francisco
    ## 310              ritz-carlton cafe ( atlanta )       atlanta       atlanta

### **03-Pair blocking**

-   Load the `reclin` package.

-   Generate all possible pairs of records between the `zagat` and
    `fodors` datasets.

``` r
fodors_file <- file.path("..", "00_datasets", "fodors.rds")
fodors <- readRDS(fodors_file)
head(fodors)
```

    ##   id                      name                    addr        city        phone
    ## 1  0 arnie morton's of chicago 435 s. la cienega blv . los angeles 310-246-1501
    ## 2  1        art's delicatessen     12224 ventura blvd. los angeles 818-762-1221
    ## 3  2             hotel bel-air    701 stone canyon rd. los angeles 310-472-1211
    ## 4  3                cafe bizou     14016 ventura blvd. los angeles 818-788-3536
    ## 5  4                 campanile     624 s. la brea ave. los angeles 213-938-1447
    ## 6  5           chinois on main           2709 main st. los angeles 310-392-9025
    ##          type class
    ## 1    american     0
    ## 2    american     1
    ## 3 californian     2
    ## 4      french     3
    ## 5    american     4
    ## 6      french     5

``` r
# Load reclin
library(reclin)
```

    ## Loading required package: lvec

    ## 
    ## Attaching package: 'lvec'

    ## The following object is masked from 'package:base':
    ## 
    ##     order

    ## Loading required package: ldat

    ## Loading required package: Rcpp

    ## 
    ## Attaching package: 'ldat'

    ## The following objects are masked from 'package:base':
    ## 
    ##     append, match, table, which

    ## 
    ## Attaching package: 'reclin'

    ## The following object is masked from 'package:base':
    ## 
    ##     identical

``` r
# Generate all possible pairs
pair_blocking(zagat, fodors)
```

    ## Simple blocking
    ##   No blocking used.
    ##   First data set:  310 records
    ##   Second data set: 533 records
    ##   Total number of pairs: 165 230 pairs
    ## 
    ## ldat with 165 230 rows and 2 columns
    ##          x   y
    ## 1        1   1
    ## 2        2   1
    ## 3        3   1
    ## 4        4   1
    ## 5        5   1
    ## 6        6   1
    ## 7        7   1
    ## 8        8   1
    ## 9        9   1
    ## 10      10   1
    ## :        :   :
    ## 165221 301 533
    ## 165222 302 533
    ## 165223 303 533
    ## 165224 304 533
    ## 165225 305 533
    ## 165226 306 533
    ## 165227 307 533
    ## 165228 308 533
    ## 165229 309 533
    ## 165230 310 533

-   Use pair blocking to generate only pairs that have matching values
    in the `city` column.

``` r
# Load reclin
library(reclin)

# Generate pairs with same city
pair_blocking(zagat, fodors, blocking_var = "city")
```

    ## Simple blocking
    ##   Blocking variable(s): city
    ##   First data set:  310 records
    ##   Second data set: 533 records
    ##   Total number of pairs: 40 532 pairs
    ## 
    ## ldat with 40 532 rows and 2 columns
    ##         x   y
    ## 1       1   1
    ## 2       1   2
    ## 3       1   3
    ## 4       1   4
    ## 5       1   5
    ## 6       1   6
    ## 7       1   7
    ## 8       1   8
    ## 9       1   9
    ## 10      1  10
    ## :       :   :
    ## 40523 310 414
    ## 40524 310 415
    ## 40525 310 416
    ## 40526 310 417
    ## 40527 310 418
    ## 40528 310 419
    ## 40529 310 420
    ## 40530 310 421
    ## 40531 310 422
    ## 40532 310 423

-   Compare pairs by `name` using `lcs()` distance

``` r
# Generate pairs
pair_blocking(zagat, fodors, blocking_var = "city") %>%
  # Compare pairs by name using lcs()
  compare_pairs(by = "name",
      default_comparator = lcs())
```

    ## Compare
    ##   By: name
    ## 
    ## Simple blocking
    ##   Blocking variable(s): city
    ##   First data set:  310 records
    ##   Second data set: 533 records
    ##   Total number of pairs: 40 532 pairs
    ## 
    ## ldat with 40 532 rows and 3 columns
    ##         x   y      name
    ## 1       1   1 0.3157895
    ## 2       1   2 0.3225806
    ## 3       1   3 0.2307692
    ## 4       1   4 0.2608696
    ## 5       1   5 0.4545455
    ## 6       1   6 0.2142857
    ## 7       1   7 0.1052632
    ## 8       1   8 0.2222222
    ## 9       1   9 0.3000000
    ## 10      1  10 0.4516129
    ## :       :   :         :
    ## 40523 310 414 0.3606557
    ## 40524 310 415 0.2631579
    ## 40525 310 416 0.2105263
    ## 40526 310 417 0.3750000
    ## 40527 310 418 0.2978723
    ## 40528 310 419 0.2727273
    ## 40529 310 420 0.3437500
    ## 40530 310 421 0.3414634
    ## 40531 310 422 0.4081633
    ## 40532 310 423 0.1714286

-   Compare pairs by `name`, `phone`, and `addr` using `jaro_winkler()`.

``` r
# Generate pairs
pair_blocking(zagat, fodors, blocking_var = "city") %>%
  # Compare pairs by name, phone, addr
  compare_pairs(c("name", "phone", "addr"),
     default_comparator =  jaro_winkler())
```

    ## Compare
    ##   By: name, phone, addr
    ## 
    ## Simple blocking
    ##   Blocking variable(s): city
    ##   First data set:  310 records
    ##   Second data set: 533 records
    ##   Total number of pairs: 40 532 pairs
    ## 
    ## ldat with 40 532 rows and 5 columns
    ##         x   y      name     phone      addr
    ## 1       1   1 0.4871062 0.6746032 0.5703661
    ## 2       1   2 0.5234025 0.5555556 0.6140351
    ## 3       1   3 0.4564103 0.7222222 0.5486355
    ## 4       1   4 0.5102564 0.6746032 0.6842105
    ## 5       1   5 0.5982906 0.5793651 0.5515351
    ## 6       1   6 0.3581197 0.6746032 0.4825911
    ## 7       1   7 0.0000000 0.6269841 0.5457762
    ## 8       1   8 0.4256410 0.6269841 0.4979621
    ## 9       1   9 0.5013736 0.7777778 0.6342105
    ## 10      1  10 0.6011396 0.6746032 0.4654971
    ## :       :   :         :         :         :
    ## 40523 310 414 0.4972291 0.6666667 0.5158263
    ## 40524 310 415 0.5778143 0.6746032 0.5065359
    ## 40525 310 416 0.4426564 0.6666667 0.4294118
    ## 40526 310 417 0.5315404 0.7152778 0.7070387
    ## 40527 310 418 0.5271102 0.6111111 0.7135914
    ## 40528 310 419 0.5204981 0.6944444 0.5683007
    ## 40529 310 420 0.5635103 0.5833333 0.4928843
    ## 40530 310 421 0.4891899 0.6111111 0.6108883
    ## 40531 310 422 0.6204433 0.6746032 0.7774510
    ## 40532 310 423 0.4233716 0.6746032 0.7908497

### **`04-Score then select or select then score?`**

#### The record linkage process into the correct order:

-   `clean the datasets.`

-   `Generate pairs of records.`

-   `Compare separate columns of each pair.`

-   `Score pairs using summing or probability.`

-   `Select pairs that are matches based on their score.`

-   `Link the datasets together.`

### **05- Putting it together**

During this chapter, you’ve cleaned up the `city` column of `zagat`
using string similarity, as well as generated and compared pairs of
restaurants from `zagat` and `fodors`. The end is near - all that’s left
to do is score and select pairs and link the data together, and you’ll
be able to begin your analysis in no time!

`reclin` and `dplyr` are loaded and `zagat` and `fodors` are available.

-   Score the pairs of records probabilistically.

``` r
# Create pairs
pair_blocking(zagat, fodors, blocking_var = "city") %>%
  # Compare pairs
  compare_pairs(by = c("name", "addr"), default_comparator = jaro_winkler())%>%
  score_simsum()%>%

  # Score pairs
  score_problink()
```

    ## Compare
    ##   By: name, addr
    ## 
    ## Simple blocking
    ##   Blocking variable(s): city
    ##   First data set:  310 records
    ##   Second data set: 533 records
    ##   Total number of pairs: 40 532 pairs
    ## 
    ## ldat with 40 532 rows and 6 columns
    ##         x   y      name      addr    simsum      weight
    ## 1       1   1 0.4871062 0.5703661 1.0574724  0.21474661
    ## 2       1   2 0.5234025 0.6140351 1.1374376  0.49661684
    ## 3       1   3 0.4564103 0.5486355 1.0050457  0.03933683
    ## 4       1   4 0.5102564 0.6842105 1.1944669  0.74290375
    ## 5       1   5 0.5982906 0.5515351 1.1498257  0.51324766
    ## 6       1   6 0.3581197 0.4825911 0.8407108 -0.49275296
    ## 7       1   7 0.0000000 0.5457762 0.5457762 -1.40745700
    ## 8       1   8 0.4256410 0.4979621 0.9236031 -0.23676220
    ## 9       1   9 0.5013736 0.6342105 1.1355842  0.50589811
    ## 10      1  10 0.6011396 0.4654971 1.0666367  0.21562541
    ## :       :   :         :         :         :           :
    ## 40523 310 414 0.4972291 0.5158263 1.0130554  0.04770522
    ## 40524 310 415 0.5778143 0.5065359 1.0843502  0.27935313
    ## 40525 310 416 0.4426564 0.4294118 0.8720682 -0.42639125
    ## 40526 310 417 0.5315404 0.7070387 1.2385792  0.91346807
    ## 40527 310 418 0.5271102 0.7135914 1.2407016  0.92923085
    ## 40528 310 419 0.5204981 0.5683007 1.0887987  0.31320310
    ## 40529 310 420 0.5635103 0.4928843 1.0563946  0.18208988
    ## 40530 310 421 0.4891899 0.6108883 1.1000782  0.37536866
    ## 40531 310 422 0.6204433 0.7774510 1.3978943  1.57185393
    ## 40532 310 423 0.4233716 0.7908497 1.2142213  1.00444361

-   Select the pairs that are considered matches.

-   Link the two data frames together.

``` r
# Create pairs
pair_blocking(zagat, fodors, blocking_var = "city") %>%
  # Compare pairs
  compare_pairs(by = c("name", "addr"), default_comparator = jaro_winkler()) %>%
  # Score pairs
  score_problink() %>%
  # Select pairs
  select_n_to_m() %>%
  link()
```

    ##     id.x                                     name.x
    ## 1      0                              apple pan the
    ## 2      1                                asahi ramen
    ## 3      2                                 baja fresh
    ## 4      3                              belvedere the
    ## 5      4                            benita's frites
    ## 6      5                                  bernard's
    ## 7      6                                  bistro 45
    ## 8      8                       brighton coffee shop
    ## 9      9                  bristol farms market cafe
    ## 10    11                                   cafe'50s
    ## 11    12                                 cafe blanc
    ## 12    13                                  cassell's
    ## 13    15                                  diaghilev
    ## 14    16                              don antonio's
    ## 15    17                                     duke's
    ## 16    18                               falafel king
    ## 17    19                        feast from the east
    ## 18    20                              gumbo pot the
    ## 19    22                                  indo cafe
    ## 20    23                    jan's family restaurant
    ## 21    24                                    jiraffe
    ## 22    25              jody maroni's sausage kingdom
    ## 23    26                                      joe's
    ## 24    27                           john o  ` groats
    ## 25    30                      johnny rockets ( la )
    ## 26    31                              killer shrimp
    ## 27    32                                kokomo cafe
    ## 28    33                                koo koo roo
    ## 29    35                            la salsa ( la )
    ## 30    37                                   langer's
    ## 31    38                               local nochol
    ## 32    40               mani's bakery & espresso bar
    ## 33    43                  michael's ( los angeles )
    ## 34    44                                    mishima
    ## 35    45                      mo better meatty meat
    ## 36    46                               mulberry st.
    ## 37    47                            ocean park cafe
    ## 38    49                     original pantry bakery
    ## 39    50                              parkway grill
    ## 40    51                                    pho hoa
    ## 41    52                   pink's famous chili dogs
    ## 42    54                                       r-23
    ## 43    55                                      rae's
    ## 44    56                           rubin's red hots
    ## 45    57                              ruby's ( la )
    ## 46    59   ruth's chris steak house ( los angeles )
    ## 47    60                                      shiro
    ## 48    61                               sushi nozawa
    ## 49    62                            sweet lady jane
    ## 50    64                                    tommy's
    ## 51    66                                water grill
    ## 52    68                         afghan kebab house
    ## 53    69                                    arcadia
    ## 54    70                           benny's burritos
    ## 55    71                             cafe con leche
    ## 56    72                              corner bistro
    ## 57    73                       cucina della fontana
    ## 58    74                            cucina di pesce
    ## 59    75                                     darbar
    ## 60    76                          ej's luncheonette
    ## 61    77                                edison cafe
    ## 62    78                               elias corner
    ## 63    79                         good enough to eat
    ## 64    80                              gray's papaya
    ## 65    81                                  il mulino
    ## 66    82                              jackson diner
    ## 67    83                             joe's shanghai
    ## 68    84                            john's pizzeria
    ## 69    85                              kelley & ping
    ## 70    86                                       kiev
    ## 71    87                               kuruma zushi
    ## 72    88                                 la caridad
    ## 73    89                              la grenouille
    ## 74    90                           lemongrass grill
    ## 75    91                                 lombardi's
    ## 76    92                       marnie's noodle shop
    ## 77    93                              menchanko-tei
    ## 78    94                           mitali east-west
    ## 79    95                             monsoon ( ny )
    ## 80    96                                  moustache
    ## 81    97                                       nobu
    ## 82    98                        one if by land tibs
    ## 83    99                                 oyster bar
    ## 84   100                                       palm
    ## 85   101                                   palm too
    ## 86   102                              patsy's pizza
    ## 87   103                    peter luger steak house
    ## 88   104                              rose of india
    ## 89   105                          sam's noodle shop
    ## 90   106                                 sarabeth's
    ## 91   107                         sparks steak house
    ## 92   108                         stick to your ribs
    ## 93   109                                   sushisay
    ## 94   110                                   sylvia's
    ## 95   111                     szechuan hunan cottage
    ## 96   112                           szechuan kitchen
    ## 97   113                                   teresa's
    ## 98   114                            thai house cafe
    ## 99   115                        thailand restaurant
    ## 100  116                                    veselka
    ## 101  117                           westside cottage
    ## 102  118                       windows on the world
    ## 103  119                          wollensky's grill
    ## 104  120                                       yama
    ## 105  121                                     zarela
    ## 106  122                  andre's french restaurant
    ## 107  123                         buccaneer bay club
    ## 108  124                         buzio's in the rio
    ## 109  125          'em eril's new orleans fish house
    ## 110  126                  fiore rotisserie & grille
    ## 111  127                              hugo's cellar
    ## 112  128                             madame ching's
    ## 113  129                        mayflower cuisinier
    ## 114  130                    michael's ( las vegas )
    ## 115  131                                monte carlo
    ## 116  132                                   moongate
    ## 117  133          morton's of chicago ( las vegas )
    ## 118  134                              nicky blair's
    ## 119  135                         piero's restaurant
    ## 120  136                        spago ( las vegas )
    ## 121  137                             steakhouse the
    ## 122  138                                  stefano's
    ## 123  139                            sterling brunch
    ## 124  140                                   tre visi
    ## 125  141                                 ' 103 west
    ## 126  142                      alon's at the terrace
    ## 127  143                         baker's cajun cafe
    ## 128  144                           barbecue kitchen
    ## 129  145                                 bistro the
    ## 130  146             bobby & june's kountry kitchen
    ## 131  147                      bradshaw's restaurant
    ## 132  148                            brookhaven cafe
    ## 133  149                             cafe sunflower
    ## 134  150                                      canoe
    ## 135  151                                    carey's
    ## 136  152                             carey's corner
    ## 137  153                                      chops
    ## 138  155        deacon burton's soulfood restaurant
    ## 139  156                                       eats
    ## 140  157                         flying biscuit the
    ## 141  158                                 frijoleros
    ## 142  159                                greenwood's
    ## 143  160                          harold's barbecue
    ## 144  161                       havana sandwich shop
    ## 145  163                            indian delights
    ## 146  164                                  java jive
    ## 147  166                        kalo's coffee house
    ## 148  167                            la fonda latina
    ## 149  168                lettuce souprise you ( at )
    ## 150  169                                   majestic
    ## 151  170            morton's of chicago ( atlanta )
    ## 152  171                                    my thai
    ## 153  172                                       nava
    ## 154  173                       nuevo laredo cantina
    ## 155  174              original pancake house ( at )
    ## 156  175                       palm the ( atlanta )
    ## 157  176                         rainbow restaurant
    ## 158  177                                    riviera
    ## 159  178                         silver skillet the
    ## 160  179                                       soto
    ## 161  180                           thelma's kitchen
    ## 162  181                                  tortillas
    ## 163  182                van gogh's restaurant & bar
    ## 164  183                                 veggieland
    ## 165  184                     white house restaurant
    ## 166  186                               bill's place
    ## 167  187                                 cafe flore
    ## 168  188                                caffe greco
    ## 169  189                                campo santo
    ## 170  190                              cha cha cha's
    ## 171  191                                   doidge's
    ## 172  192                    dottie's true blue cafe
    ## 173  193                                 dusit thai
    ## 174  194                                      ebisu
    ## 175  195                'em erald garden restaurant
    ## 176  196                  eric's chinese restaurant
    ## 177  197                           hamburger mary's
    ## 178  198                         kelly's on trinity
    ## 179  199                                  la cumbre
    ## 180  200                            la mediterranee
    ## 181  201                                la taqueria
    ## 182  202          mario's bohemian cigar store cafe
    ## 183  203                                marnee thai
    ## 184  204                             mel's drive-in
    ## 185  205                               mo's burgers
    ## 186  206            phnom penh cambodian restaurant
    ## 187  207                    roosevelt tamale parlor
    ## 188  208                      sally's cafe & bakery
    ## 189  209                          san francisco bbq
    ## 190  210                               slanted door
    ## 191  211                          swan oyster depot
    ## 192  212                                thep phanom
    ## 193  213                                    ti couz
    ## 194  214                                  trio cafe
    ## 195  215                                     tu lan
    ## 196  216                            vicolo pizzeria
    ## 197  217              wa-ha-ka oaxaca mexican grill
    ## 198  218                  arnie morton's of chicago
    ## 199  219                                 art's deli
    ## 200  220                              bel-air hotel
    ## 201  222                                  campanile
    ## 202  223                            chinois on main
    ## 203  224                                     citrus
    ## 204  225                        fenix at the argyle
    ## 205  226                                    granita
    ## 206  227                                  grill the
    ## 207  229                             l  ` orangerie
    ## 208  230              le chardonnay ( los angeles )
    ## 209  231                             locanda veneta
    ## 210  232                                  matsuhisa
    ## 211  233                   palm the ( los angeles )
    ## 212  234                                     patina
    ## 213  235                      philippe the original
    ## 214  236                               pinot bistro
    ## 215  237                          rex il ristorante
    ## 216  238                      spago ( los angeles )
    ## 217  239                                  valentino
    ## 218  240                              yujean kang's
    ## 219  241                                   '21 club
    ## 220  242                                    aquavit
    ## 221  243                                    aureole
    ## 222  244                                  cafe lalo
    ## 223  245                          cafe des artistes
    ## 224  246                                  carmine's
    ## 225  247                              carnegie deli
    ## 226  248                                chanterelle
    ## 227  249                                     daniel
    ## 228  250                                      dawat
    ## 229  251                                    felidia
    ## 230  252                               four seasons
    ## 231  253                         gotham bar & grill
    ## 232  254                            gramercy tavern
    ## 233  255                               island spice
    ## 234  256                                      jo jo
    ## 235  257                               la caravelle
    ## 236  258                             la cote basque
    ## 237  259                               le bernardin
    ## 238  260                             les celebrites
    ## 239  261               lespinasse ( new york city )
    ## 240  262                                     lutece
    ## 241  263                       manhattan ocean club
    ## 242  264                                      march
    ## 243  265                                 mesa grill
    ## 244  266                                  mi cocina
    ## 245  267                                 montrachet
    ## 246  268                                     oceana
    ## 247  269         park avenue cafe ( new york city )
    ## 248  270                                 petrossian
    ## 249  271                                  picholine
    ## 250  272                                     pisces
    ## 251  273                               rainbow room
    ## 252  274                                 river cafe
    ## 253  275                               san domenico
    ## 254  276                         second avenue deli
    ## 255  277                                     seryna
    ## 256  278                            shun lee palace
    ## 257  279                           sign of the dove
    ## 258  280                          smith & wollensky
    ## 259  281                        tavern on the green
    ## 260  282                               uncle nick's
    ## 261  283                          union square cafe
    ## 262  284                          virgil's real bbq
    ## 263  285                                     chin's
    ## 264  286                  coyote cafe ( las vegas )
    ## 265  287                       le montrachet bistro
    ## 266  288                               palace court
    ## 267  289                        second street grill
    ## 268  290                            steak house the
    ## 269  291                            'till erman the
    ## 270  292                                    abruzzi
    ## 271  293                                bacchanalia
    ## 272  294                          bone's restaurant
    ## 273  295                          brasserie le coze
    ## 274  296                             buckhead diner
    ## 275  297                      ciboulette restaurant
    ## 276  298                                delectables
    ## 277  299                             georgia grille
    ## 278  300                  hedgerose heights inn the
    ## 279  301                             heera of india
    ## 280  302                       indigo coastal grill
    ## 281  303                                  la grotta
    ## 282  304                        mary mac's tea room
    ## 283  305                             nikolai's roof
    ## 284  306                          pano's & paul  's
    ## 285  307             ritz-carlton cafe ( buckhead )
    ## 286  308      ritz-carlton dining room ( buckhead )
    ## 287  309                    ritz-carlton restaurant
    ## 288  310                                   toulouse
    ## 289  311                             veni vidi vici
    ## 290  312                             alain rondelli
    ## 291  313                                       aqua
    ## 292  314                                  boulevard
    ## 293  315                                cafe claude
    ## 294  316                              campton place
    ## 295  317                                chez michel
    ## 296  318                               fleur de lys
    ## 297  319                                   fringale
    ## 298  320                             hawthorne lane
    ## 299  321                       khan toke thai house
    ## 300  322                                   la folie
    ## 301  323                   lulu restaurant-bis-cafe
    ## 302  324                                     masa's
    ## 303  325                                     mifune
    ## 304  326                             plumpjack cafe
    ## 305  327                                    postrio
    ## 306  328 ritz-carlton dining room ( san francisco )
    ## 307  329                               rose pistola
    ## 308  330              ritz-carlton cafe ( atlanta )
    ## 309  154                                   chopstix
    ## 310  165                      johnny rockets ( at )
    ## 311   NA                                       <NA>
    ## 312   NA                                       <NA>
    ## 313   NA                                       <NA>
    ## 314   NA                                       <NA>
    ## 315   NA                                       <NA>
    ## 316   NA                                       <NA>
    ## 317   NA                                       <NA>
    ## 318   NA                                       <NA>
    ## 319   NA                                       <NA>
    ## 320   NA                                       <NA>
    ## 321   NA                                       <NA>
    ## 322   NA                                       <NA>
    ## 323   NA                                       <NA>
    ## 324   NA                                       <NA>
    ## 325   NA                                       <NA>
    ## 326   NA                                       <NA>
    ## 327   NA                                       <NA>
    ## 328   NA                                       <NA>
    ## 329   NA                                       <NA>
    ## 330   NA                                       <NA>
    ## 331   NA                                       <NA>
    ## 332   NA                                       <NA>
    ## 333   NA                                       <NA>
    ## 334   NA                                       <NA>
    ## 335   NA                                       <NA>
    ## 336   NA                                       <NA>
    ## 337   NA                                       <NA>
    ## 338   NA                                       <NA>
    ## 339   NA                                       <NA>
    ## 340   NA                                       <NA>
    ## 341   NA                                       <NA>
    ## 342   NA                                       <NA>
    ## 343   NA                                       <NA>
    ## 344   NA                                       <NA>
    ## 345   NA                                       <NA>
    ## 346   NA                                       <NA>
    ## 347   NA                                       <NA>
    ## 348   NA                                       <NA>
    ## 349   NA                                       <NA>
    ## 350   NA                                       <NA>
    ## 351   NA                                       <NA>
    ## 352   NA                                       <NA>
    ## 353   NA                                       <NA>
    ## 354   NA                                       <NA>
    ## 355   NA                                       <NA>
    ## 356   NA                                       <NA>
    ## 357   NA                                       <NA>
    ## 358   NA                                       <NA>
    ## 359   NA                                       <NA>
    ## 360   NA                                       <NA>
    ## 361   NA                                       <NA>
    ## 362   NA                                       <NA>
    ## 363   NA                                       <NA>
    ## 364   NA                                       <NA>
    ## 365   NA                                       <NA>
    ## 366   NA                                       <NA>
    ## 367   NA                                       <NA>
    ## 368   NA                                       <NA>
    ## 369   NA                                       <NA>
    ## 370   NA                                       <NA>
    ## 371   NA                                       <NA>
    ## 372   NA                                       <NA>
    ## 373   NA                                       <NA>
    ## 374   NA                                       <NA>
    ## 375   NA                                       <NA>
    ## 376   NA                                       <NA>
    ## 377   NA                                       <NA>
    ## 378   NA                                       <NA>
    ## 379   NA                                       <NA>
    ## 380   NA                                       <NA>
    ## 381   NA                                       <NA>
    ## 382   NA                                       <NA>
    ## 383   NA                                       <NA>
    ## 384   NA                                       <NA>
    ## 385   NA                                       <NA>
    ## 386   NA                                       <NA>
    ## 387   NA                                       <NA>
    ## 388   NA                                       <NA>
    ## 389   NA                                       <NA>
    ## 390   NA                                       <NA>
    ## 391   NA                                       <NA>
    ## 392   NA                                       <NA>
    ## 393   NA                                       <NA>
    ## 394   NA                                       <NA>
    ## 395   NA                                       <NA>
    ## 396   NA                                       <NA>
    ## 397   NA                                       <NA>
    ## 398   NA                                       <NA>
    ## 399   NA                                       <NA>
    ## 400   NA                                       <NA>
    ## 401   NA                                       <NA>
    ## 402   NA                                       <NA>
    ## 403   NA                                       <NA>
    ## 404   NA                                       <NA>
    ## 405   NA                                       <NA>
    ## 406   NA                                       <NA>
    ## 407   NA                                       <NA>
    ## 408   NA                                       <NA>
    ## 409   NA                                       <NA>
    ## 410   NA                                       <NA>
    ## 411   NA                                       <NA>
    ## 412   NA                                       <NA>
    ## 413   NA                                       <NA>
    ## 414   NA                                       <NA>
    ## 415   NA                                       <NA>
    ## 416   NA                                       <NA>
    ## 417   NA                                       <NA>
    ## 418   NA                                       <NA>
    ## 419   NA                                       <NA>
    ## 420   NA                                       <NA>
    ## 421   NA                                       <NA>
    ## 422   NA                                       <NA>
    ## 423   NA                                       <NA>
    ## 424   NA                                       <NA>
    ## 425   NA                                       <NA>
    ## 426   NA                                       <NA>
    ## 427   NA                                       <NA>
    ## 428   NA                                       <NA>
    ## 429   NA                                       <NA>
    ## 430   NA                                       <NA>
    ## 431   NA                                       <NA>
    ## 432   NA                                       <NA>
    ## 433   NA                                       <NA>
    ## 434   NA                                       <NA>
    ## 435   NA                                       <NA>
    ## 436   NA                                       <NA>
    ## 437   NA                                       <NA>
    ## 438   NA                                       <NA>
    ## 439   NA                                       <NA>
    ## 440   NA                                       <NA>
    ## 441   NA                                       <NA>
    ## 442   NA                                       <NA>
    ## 443   NA                                       <NA>
    ## 444   NA                                       <NA>
    ## 445   NA                                       <NA>
    ## 446   NA                                       <NA>
    ## 447   NA                                       <NA>
    ## 448   NA                                       <NA>
    ## 449   NA                                       <NA>
    ## 450   NA                                       <NA>
    ## 451   NA                                       <NA>
    ## 452   NA                                       <NA>
    ## 453   NA                                       <NA>
    ## 454   NA                                       <NA>
    ## 455   NA                                       <NA>
    ## 456   NA                                       <NA>
    ## 457   NA                                       <NA>
    ## 458   NA                                       <NA>
    ## 459   NA                                       <NA>
    ## 460   NA                                       <NA>
    ## 461   NA                                       <NA>
    ## 462   NA                                       <NA>
    ## 463   NA                                       <NA>
    ## 464   NA                                       <NA>
    ## 465   NA                                       <NA>
    ## 466   NA                                       <NA>
    ## 467   NA                                       <NA>
    ## 468   NA                                       <NA>
    ## 469   NA                                       <NA>
    ## 470   NA                                       <NA>
    ## 471   NA                                       <NA>
    ## 472   NA                                       <NA>
    ## 473   NA                                       <NA>
    ## 474   NA                                       <NA>
    ## 475   NA                                       <NA>
    ## 476   NA                                       <NA>
    ## 477   NA                                       <NA>
    ## 478   NA                                       <NA>
    ## 479   NA                                       <NA>
    ## 480   NA                                       <NA>
    ## 481   NA                                       <NA>
    ## 482   NA                                       <NA>
    ## 483   NA                                       <NA>
    ## 484   NA                                       <NA>
    ## 485   NA                                       <NA>
    ## 486   NA                                       <NA>
    ## 487   NA                                       <NA>
    ## 488   NA                                       <NA>
    ## 489   NA                                       <NA>
    ## 490   NA                                       <NA>
    ## 491   NA                                       <NA>
    ## 492   NA                                       <NA>
    ## 493   NA                                       <NA>
    ## 494   NA                                       <NA>
    ## 495   NA                                       <NA>
    ## 496   NA                                       <NA>
    ## 497   NA                                       <NA>
    ## 498   NA                                       <NA>
    ## 499   NA                                       <NA>
    ## 500   NA                                       <NA>
    ## 501   NA                                       <NA>
    ## 502   NA                                       <NA>
    ## 503   NA                                       <NA>
    ## 504   NA                                       <NA>
    ## 505   NA                                       <NA>
    ## 506   NA                                       <NA>
    ## 507   NA                                       <NA>
    ## 508   NA                                       <NA>
    ## 509   NA                                       <NA>
    ## 510   NA                                       <NA>
    ## 511   NA                                       <NA>
    ## 512   NA                                       <NA>
    ## 513   NA                                       <NA>
    ## 514   NA                                       <NA>
    ## 515   NA                                       <NA>
    ## 516   NA                                       <NA>
    ## 517   NA                                       <NA>
    ## 518   NA                                       <NA>
    ## 519   NA                                       <NA>
    ## 520   NA                                       <NA>
    ## 521   NA                                       <NA>
    ## 522   NA                                       <NA>
    ## 523   NA                                       <NA>
    ## 524   NA                                       <NA>
    ## 525   NA                                       <NA>
    ## 526   NA                                       <NA>
    ## 527   NA                                       <NA>
    ## 528   NA                                       <NA>
    ## 529   NA                                       <NA>
    ## 530   NA                                       <NA>
    ## 531   NA                                       <NA>
    ## 532   NA                                       <NA>
    ## 533   NA                                       <NA>
    ## 534   NA                                       <NA>
    ## 535   NA                                       <NA>
    ##                             addr.x        city.x      phone.x
    ## 1              10801 w. pico blvd.   los angeles 310-475-3585
    ## 2              2027 sawtelle blvd.   los angeles 310-479-2231
    ## 3                  3345 kimber dr.   los angeles 805-498-4049
    ## 4   9882 little santa monica blvd.   los angeles 310-788-2306
    ## 5         1433 third st. promenade   los angeles 310-458-2889
    ## 6                 515 s. olive st.   los angeles 213-612-1580
    ## 7                45 s. mentor ave.   los angeles 818-795-2478
    ## 8                9600 brighton way   los angeles 310-276-7732
    ## 9           1570 rosecrans ave. s.   los angeles 310-643-5229
    ## 10               838 lincoln blvd.   los angeles 310-399-1955
    ## 11  9777 little santa monica blvd.   los angeles 310-888-0108
    ## 12               3266 w. sixth st.   los angeles 213-480-8668
    ## 13       1020 n. san vicente blvd.   los angeles 310-854-1111
    ## 14             1136 westwood blvd.   los angeles 310-209-1422
    ## 15               8909 sunset blvd.   los angeles 310-652-3100
    ## 16               1059 broxton ave.   los angeles 310-208-4444
    ## 17             1949 westwood blvd.   los angeles 310-475-0400
    ## 18               6333 w. third st.   los angeles 213-933-0358
    ## 19        10428 1/2 national blvd.   los angeles 310-815-1290
    ## 20              8424 beverly blvd.   los angeles 213-651-2866
    ## 21           502 santa monica blvd   los angeles 310-917-6671
    ## 22           2011 ocean front walk   los angeles 310-306-1995
    ## 23         1023 abbot kinney blvd.   los angeles 310-399-5811
    ## 24             10516 w. pico blvd.   los angeles 310-204-0692
    ## 25               7507 melrose ave.   los angeles 213-651-3361
    ## 26                4000 colfax ave.   los angeles 818-508-1570
    ## 27               6333 w. third st.   los angeles 213-933-0773
    ## 28           8393 w. beverly blvd.   los angeles 213-655-9045
    ## 29                       22800 pch   los angeles 310-456-6299
    ## 30             704 s. alvarado st.   los angeles 213-483-8050
    ## 31       30869 thousand oaks blvd.   los angeles 818-706-7706
    ## 32             519 s. fairfax ave.   los angeles 213-938-8800
    ## 33                  1147 third st.   los angeles 310-451-0843
    ## 34               8474 w. third st.   los angeles 213-782-0181
    ## 35               7261 melrose ave.   los angeles 213-935-5280
    ## 36             17040 ventura blvd.   los angeles 818-906-8881
    ## 37           3117 ocean park blvd.   los angeles 310-452-5728
    ## 38    875 s. figueroa st. downtown   los angeles 213-627-6879
    ## 39            510 s. arroyo pkwy .   los angeles 818-795-1001
    ## 40                    642 broadway   los angeles 213-626-5530
    ## 41             709 n. la brea ave.   los angeles 213-931-4223
    ## 42                923 e. third st.   los angeles 213-687-7178
    ## 43                 2901 pico blvd.   los angeles 310-828-7937
    ## 44             15322 ventura blvd.   los angeles 818-905-6515
    ## 45            45 s. fair oaks ave.   los angeles 818-796-7829
    ## 46              224 s. beverly dr.   los angeles 310-859-8744
    ## 47             1505 mission st. s.   los angeles 818-799-4774
    ## 48             11288 ventura blvd.   los angeles 818-508-7017
    ## 49               8360 melrose ave.   los angeles 213-653-7145
    ## 50              2575 beverly blvd.   los angeles 213-389-9060
    ## 51               544 s. grand ave.   los angeles 213-891-0900
    ## 52                  764 ninth ave.      new york 212-307-1612
    ## 53                  21 e. 62nd st.      new york 212-223-2900
    ## 54                       93 ave. a      new york 212-254-2054
    ## 55              424 amsterdam ave.      new york 212-595-7000
    ## 56               331 w. fourth st.      new york 212-242-9502
    ## 57                368 bleecker st.      new york 212-242-0636
    ## 58                87 e. fourth st.      new york 212-260-6800
    ## 59                  44 w. 56th st.      new york 212-432-7227
    ## 60                  432 sixth ave.      new york 212-473-5555
    ## 61                 228 w. 47th st.      new york 212-840-5000
    ## 62                  24-02 31st st.      new york 718-932-1510
    ## 63              483 amsterdam ave.      new york 212-496-0163
    ## 64                   2090 broadway      new york 212-799-0243
    ## 65                 86 w. third st.      new york 212-673-3783
    ## 66                  37-03 74th st.      new york 718-672-1232
    ## 67                      9 pell st.      new york 718-539-3838
    ## 68                  48 w. 65th st.      new york 212-721-7001
    ## 69                  127 greene st.      new york 212-228-1212
    ## 70                 117 second ave.      new york 212-674-4040
    ## 71                        2nd fl .      new york 212-317-2802
    ## 72                   2199 broadway      new york 212-874-2780
    ## 73                   3 e. 52nd st.      new york 212-752-1495
    ## 74                61a seventh ave.      new york 718-399-7100
    ## 75                   32 spring st.      new york 212-941-7994
    ## 76                  466 hudson st.      new york 212-741-3214
    ## 77                  39 w. 55th st.      new york 212-247-1585
    ## 78                296 bleecker st.      new york 212-989-1367
    ## 79              435 amsterdam ave.      new york 212-580-8686
    ## 80               405 atlantic ave.      new york 718-852-5555
    ## 81                  105 hudson st.      new york 212-219-0500
    ## 82                   17 barrow st.      new york 212-228-0822
    ## 83                   ` lower level      new york 212-490-6650
    ## 84                 837 second ave.      new york 212-687-2953
    ## 85                 840 second ave.      new york 212-697-5198
    ## 86               19 old fulton st.      new york 718-858-4300
    ## 87                    178 broadway      new york 718-387-7400
    ## 88                308 e. sixth st.      new york 212-533-5011
    ## 89                  411 third ave.      new york 212-213-2288
    ## 90               1295 madison ave.      new york 212-410-7335
    ## 91                 210 e. 46th st.      new york 212-687-4855
    ## 92                  5-16 51st ave.      new york 718-937-3030
    ## 93                  38 e. 51st st.      new york 212-755-1780
    ## 94                  328 lenox ave.      new york 212-996-0660
    ## 95                  1588 york ave.      new york 212-535-5223
    ## 96                 1460 first ave.      new york 212-249-4615
    ## 97                 80 montague st.      new york 718-520-2910
    ## 98                  151 hudson st.      new york 212-334-1085
    ## 99                  106 bayard st.      new york 212-349-3132
    ## 100                144 second ave.      new york 212-228-9682
    ## 101                 689 ninth ave.      new york 212-245-0800
    ## 102                     107th fl .      new york 212-524-7000
    ## 103                205 e. 49th st.      new york 212-753-0444
    ## 104                122 e. 17th st.      new york 212-475-0969
    ## 105                953 second ave.      new york 212-644-6740
    ## 106                 401 s. 6th st.     las vegas 702-385-5016
    ## 107        3300 las vegas blvd. s.     las vegas 702-894-7350
    ## 108           3700 w. flamingo rd.     las vegas 702-252-7697
    ## 109        3799 las vegas blvd. s.     las vegas 702-891-7374
    ## 110           3700 w. flamingo rd.     las vegas 702-252-7702
    ## 111             202 e. fremont st.     las vegas 702-385-4011
    ## 112        3300 las vegas blvd. s.     las vegas 702-894-7111
    ## 113            4750 w. sahara ave.     las vegas 702-870-8432
    ## 114        3595 las vegas blvd. s.     las vegas 702-737-7111
    ## 115        3145 las vegas blvd. s.     las vegas 702-733-4524
    ## 116        3400 las vegas blvd. s.     las vegas 702-791-7352
    ## 117        3200 las vegas blvd. s.     las vegas 702-893-0703
    ## 118              3925 paradise rd.     las vegas 702-792-9900
    ## 119      355 convention center dr.     las vegas 702-369-2305
    ## 120        3500 las vegas blvd. s.     las vegas 702-369-6300
    ## 121             128 e. fremont st.     las vegas 702-382-1600
    ## 122                129 fremont st.     las vegas 702-385-7111
    ## 123        3645 las vegas blvd. s.     las vegas 702-739-4651
    ## 124        3799 las vegas blvd. s.     las vegas 702-891-7331
    ## 125         103 w. paces ferry rd.       atlanta 404-233-5993
    ## 126              659 peachtree st.       atlanta 404-724-0444
    ## 127               1134 euclid ave.       atlanta 404-223-5039
    ## 128             1437 virginia ave.       atlanta 404-766-9906
    ## 129           56 e. andrews dr. nw       atlanta 404-231-5733
    ## 130                   375 14th st.       atlanta 404-876-3872
    ## 131            2911 s. pharr court       atlanta 404-261-7015
    ## 132             4274 peachtree rd.       atlanta 404-231-5907
    ## 133               5975 roswell rd.       atlanta 404-256-1675
    ## 134           4199 paces ferry rd.       atlanta 770-432-2663
    ## 135            1021 cobb pkwy . se       atlanta 770-422-8042
    ## 136          1215 powers ferry rd.       atlanta 770-933-0909
    ## 137          70 w. paces ferry rd.       atlanta 404-262-2675
    ## 138          1029 edgewood ave. se       atlanta 404-523-1929
    ## 139         600 ponce de leon ave.       atlanta 404-888-9149
    ## 140             1655 mclendon ave.       atlanta 404-687-8888
    ## 141          1031 peachtree st. ne       atlanta 404-892-8226
    ## 142                 1087 green st.       atlanta 770-992-5383
    ## 143            171 mcdonough blvd.       atlanta 404-627-9268
    ## 144              2905 buford hwy .       atlanta 404-636-4094
    ## 145           3675 satellite blvd.       atlanta 100-813-8212
    ## 146         790 ponce de leon ave.       atlanta 404-876-6161
    ## 147             1248 clairmont rd.       atlanta 404-325-3733
    ## 148               4427 roswell rd.       atlanta 404-303-8201
    ## 149                3525 mall blvd.       atlanta 770-418-9969
    ## 150        1031 ponce de leon ave.       atlanta 404-875-0276
    ## 151           303 peachtree st. ne       atlanta 404-577-4366
    ## 152             1248 clairmont rd.       atlanta 404-636-4280
    ## 153             3060 peachtree rd.       atlanta 404-240-1984
    ## 154     1495 chattahoochee ave. nw       atlanta 404-352-9009
    ## 155             4330 peachtree rd.       atlanta 404-237-4116
    ## 156          3391 peachtree rd. ne       atlanta 404-814-1955
    ## 157            2118 n. decatur rd.       atlanta 404-633-3538
    ## 158         519 e. paces ferry rd.       atlanta 404-262-7112
    ## 159                200 14th st. nw       atlanta 404-874-1388
    ## 160              3330 piedmont rd.       atlanta 404-233-2005
    ## 161            764 marietta st. nw       atlanta 404-688-5855
    ## 162      774 ponce de leon ave. ne       atlanta 404-892-0193
    ## 163           70 w. crossville rd.       atlanta 770-993-1156
    ## 164       220 sandy springs circle       atlanta 404-231-3111
    ## 165          3172 peachtree rd. ne       atlanta 404-237-7601
    ## 166               2315 clement st. san francisco 415-221-5262
    ## 167                2298 market st. san francisco 415-621-8579
    ## 168              423 columbus ave. san francisco 415-397-6261
    ## 169              240 columbus ave. san francisco 415-433-9623
    ## 170                1805 haight st. san francisco 415-386-5758
    ## 171                 2217 union st. san francisco 415-921-2149
    ## 172                  522 jones st. san francisco 415-885-2767
    ## 173               3221 mission st. san francisco 415-826-4639
    ## 174                1283 ninth ave. san francisco 415-566-1770
    ## 175            1550 california st. san francisco 415-673-1155
    ## 176                1500 church st. san francisco 415-282-0919
    ## 177                1582 folsom st. san francisco 415-626-1985
    ## 178                   333 bush st. san francisco 415-362-4454
    ## 179               515 valencia st. san francisco 415-863-8205
    ## 180                    288 noe st. san francisco 415-431-7210
    ## 181               2889 mission st. san francisco 415-285-7117
    ## 182                  2209 polk st. san francisco 415-776-8226
    ## 183                2225 irving st. san francisco 415-665-9500
    ## 184                 3355 geary st. san francisco 415-387-2244
    ## 185                 1322 grant st. san francisco 415-788-3779
    ## 186                 631 larkin st. san francisco 415-775-5979
    ## 187                  2817 24th st. san francisco 415-550-9213
    ## 188                300 de haro st. san francisco 415-626-6006
    ## 189                  1328 18th st. san francisco 415-431-8956
    ## 190               584 valencia st. san francisco 415-861-8032
    ## 191                  1517 polk st. san francisco 415-673-1101
    ## 192                 400 waller st. san francisco 415-431-2526
    ## 193                  3108 16th st. san francisco 415-252-7373
    ## 194              1870 fillmore st. san francisco 415-563-2248
    ## 195                    8 sixth st. san francisco 415-626-0927
    ## 196                    201 ivy st. san francisco 415-863-2382
    ## 197                  2141 polk st. san francisco 415-775-1055
    ## 198        435 s. la cienega blvd.   los angeles 310-246-1501
    ## 199            12224 ventura blvd.   los angeles 818-762-1221
    ## 200           701 stone canyon rd.   los angeles 310-472-1211
    ## 201            624 s. la brea ave.   los angeles 213-938-1447
    ## 202                  2709 main st.   los angeles 310-392-9025
    ## 203              6703 melrose ave.   los angeles 213-857-0034
    ## 204              8358 sunset blvd.   los angeles 213-848-6677
    ## 205            23725 w. malibu rd.   los angeles 310-456-0488
    ## 206                9560 dayton way   los angeles 310-276-0615
    ## 207        903 n. la cienega blvd.   los angeles 310-652-9770
    ## 208              8284 melrose ave.   los angeles 213-655-8880
    ## 209              8638 w. third st.   los angeles 310-274-1893
    ## 210        129 n. la cienega blvd.   los angeles 310-659-9639
    ## 211        9001 santa monica blvd.   los angeles 310-550-8811
    ## 212              5955 melrose ave.   los angeles 213-467-1108
    ## 213            1001 n. alameda st.   los angeles 213-628-3781
    ## 214            12969 ventura blvd.   los angeles 818-990-0500
    ## 215               617 s. olive st.   los angeles 213-627-2300
    ## 216              8795 sunset blvd.   los angeles 310-652-4025
    ## 217                3115 pico blvd.   los angeles 310-829-4313
    ## 218             67 n. raymond ave.   los angeles 818-585-0855
    ## 219                 21 w. 52nd st.      new york 212-582-7200
    ## 220                 13 w. 54th st.      new york 212-307-7311
    ## 221                 34 e. 61st st.      new york 212-319-1660
    ## 222                201 w. 83rd st.      new york 212-496-6031
    ## 223                  1 w. 67th st.      new york 212-877-3500
    ## 224                  2450 broadway      new york 212-362-2200
    ## 225               854 seventh ave.      new york 212-757-2245
    ## 226                 2 harrison st.      new york 212-966-6960
    ## 227                 20 e. 76th st.      new york 212-288-0033
    ## 228                210 e. 58th st.      new york 212-355-7555
    ## 229                243 e. 58th st.      new york 212-758-1479
    ## 230                 99 e. 52nd st.      new york 212-754-9494
    ## 231                 12 e. 12th st.      new york 212-620-4020
    ## 232                 42 e. 20th st.      new york 212-477-0777
    ## 233                402 w. 44th st.      new york 212-765-1737
    ## 234                160 e. 64th st.      new york 212-223-5656
    ## 235                 33 w. 55th st.      new york 212-586-4252
    ## 236                 60 w. 55th st.      new york 212-688-6525
    ## 237                155 w. 51st st.      new york 212-489-1515
    ## 238                155 w. 58th st.      new york 212-484-5113
    ## 239                  2 e. 55th st.      new york 212-339-6719
    ## 240                249 e. 50th st.      new york 212-752-2225
    ## 241                 57 w. 58th st.      new york 212-371-7777
    ## 242                405 e. 58th st.      new york 212-754-6272
    ## 243                 102 fifth ave.      new york 212-807-7400
    ## 244                    57 jane st.      new york 212-627-8273
    ## 245                239 w. broadway      new york 212-219-2777
    ## 246                 55 e. 54th st.      new york 212-759-5941
    ## 247                100 e. 63rd st.      new york 212-644-1900
    ## 248                182 w. 58th st.      new york 212-245-2214
    ## 249                 35 w. 64th st.      new york 212-724-8585
    ## 250                      95 ave. a      new york 212-260-6660
    ## 251           30 rockefeller plaza      new york 212-632-5000
    ## 252                    1 water st.      new york 718-522-5200
    ## 253            240 central park s.      new york 212-265-5959
    ## 254                156 second ave.      new york 212-677-0606
    ## 255                 11 e. 53rd st.      new york 212-980-9393
    ## 256                155 e. 55th st.      new york 212-371-8844
    ## 257                1110 third ave.      new york 212-861-8080
    ## 258                 797 third ave.      new york 212-753-1530
    ## 259            ` central park west      new york 212-873-3200
    ## 260                 747 ninth ave.      new york 212-245-7992
    ## 261                 21 e. 16th st.      new york 212-243-4020
    ## 262                152 w. 44th st.      new york 212-921-9494
    ## 263        3200 las vegas blvd. s.     las vegas 702-733-8899
    ## 264        3799 las vegas blvd. s.     las vegas 702-891-7349
    ## 265              3000 paradise rd.     las vegas 702-732-5651
    ## 266        3570 las vegas blvd. s.     las vegas 702-731-7110
    ## 267             200 e. fremont st.     las vegas 702-385-6277
    ## 268        2880 las vegas blvd. s.     las vegas 702-734-0410
    ## 269           2245 e. flamingo rd.     las vegas 702-731-4036
    ## 270          2355 peachtree rd. ne       atlanta 404-261-8186
    ## 271              3125 piedmont rd.       atlanta 404-365-0410
    ## 272           3130 piedmont rd. ne       atlanta 404-237-2663
    ## 273             3393 peachtree rd.       atlanta 404-266-1440
    ## 274              3073 piedmont rd.       atlanta 404-262-3336
    ## 275             1529 piedmont ave.       atlanta 404-874-7600
    ## 276        1 margaret mitchell sq.       atlanta 404-681-2909
    ## 277             2290 peachtree rd.       atlanta 404-352-3517
    ## 278      490 e. paces ferry rd. ne       atlanta 404-233-7673
    ## 279              595 piedmont ave.       atlanta 404-876-4408
    ## 280          1397 n. highland ave.       atlanta 404-876-0676
    ## 281          2637 peachtree rd. ne       atlanta 404-231-1368
    ## 282         224 ponce de leon ave.       atlanta 404-876-1800
    ## 283              255 courtland st.       atlanta 404-221-6362
    ## 284        1232 w. paces ferry rd.       atlanta 404-261-3662
    ## 285          3434 peachtree rd. ne       atlanta 404-237-2700
    ## 286          3434 peachtree rd. ne       atlanta 404-237-2700
    ## 287              181 peachtree st.       atlanta 404-659-0400
    ## 288            293-b peachtree rd.       atlanta 404-351-9533
    ## 289                    41 14th st.       atlanta 404-875-8424
    ## 290                126 clement st. san francisco 415-387-0408
    ## 291             252 california st. san francisco 415-956-9662
    ## 292                  1 mission st. san francisco 415-543-6084
    ## 293                  7 claude ln . san francisco 415-392-3505
    ## 294               340 stockton st. san francisco 415-955-5555
    ## 295            804 north point st. san francisco 415-775-7036
    ## 296                 777 sutter st. san francisco 415-673-7779
    ## 297                 570 fourth st. san francisco 415-543-0573
    ## 298               22 hawthorne st. san francisco 415-777-9779
    ## 299               5937 geary blvd. san francisco 415-668-6654
    ## 300                  2316 polk st. san francisco 415-776-5577
    ## 301                 816 folsom st. san francisco 415-495-5775
    ## 302                   648 bush st. san francisco 415-989-7154
    ## 303                  1737 post st. san francisco 415-922-0337
    ## 304              3127 fillmore st. san francisco 415-563-4755
    ## 305                   545 post st. san francisco 415-776-7825
    ## 306               600 stockton st. san francisco 415-296-7465
    ## 307              532 columbus ave. san francisco 415-399-0499
    ## 308              181 peachtree st.       atlanta 404-659-0400
    ## 309               4279 roswell rd.       atlanta 404-255-4868
    ## 310               2970 cobb pkwy .       atlanta 770-955-6068
    ## 311                           <NA>          <NA>         <NA>
    ## 312                           <NA>          <NA>         <NA>
    ## 313                           <NA>          <NA>         <NA>
    ## 314                           <NA>          <NA>         <NA>
    ## 315                           <NA>          <NA>         <NA>
    ## 316                           <NA>          <NA>         <NA>
    ## 317                           <NA>          <NA>         <NA>
    ## 318                           <NA>          <NA>         <NA>
    ## 319                           <NA>          <NA>         <NA>
    ## 320                           <NA>          <NA>         <NA>
    ## 321                           <NA>          <NA>         <NA>
    ## 322                           <NA>          <NA>         <NA>
    ## 323                           <NA>          <NA>         <NA>
    ## 324                           <NA>          <NA>         <NA>
    ## 325                           <NA>          <NA>         <NA>
    ## 326                           <NA>          <NA>         <NA>
    ## 327                           <NA>          <NA>         <NA>
    ## 328                           <NA>          <NA>         <NA>
    ## 329                           <NA>          <NA>         <NA>
    ## 330                           <NA>          <NA>         <NA>
    ## 331                           <NA>          <NA>         <NA>
    ## 332                           <NA>          <NA>         <NA>
    ## 333                           <NA>          <NA>         <NA>
    ## 334                           <NA>          <NA>         <NA>
    ## 335                           <NA>          <NA>         <NA>
    ## 336                           <NA>          <NA>         <NA>
    ## 337                           <NA>          <NA>         <NA>
    ## 338                           <NA>          <NA>         <NA>
    ## 339                           <NA>          <NA>         <NA>
    ## 340                           <NA>          <NA>         <NA>
    ## 341                           <NA>          <NA>         <NA>
    ## 342                           <NA>          <NA>         <NA>
    ## 343                           <NA>          <NA>         <NA>
    ## 344                           <NA>          <NA>         <NA>
    ## 345                           <NA>          <NA>         <NA>
    ## 346                           <NA>          <NA>         <NA>
    ## 347                           <NA>          <NA>         <NA>
    ## 348                           <NA>          <NA>         <NA>
    ## 349                           <NA>          <NA>         <NA>
    ## 350                           <NA>          <NA>         <NA>
    ## 351                           <NA>          <NA>         <NA>
    ## 352                           <NA>          <NA>         <NA>
    ## 353                           <NA>          <NA>         <NA>
    ## 354                           <NA>          <NA>         <NA>
    ## 355                           <NA>          <NA>         <NA>
    ## 356                           <NA>          <NA>         <NA>
    ## 357                           <NA>          <NA>         <NA>
    ## 358                           <NA>          <NA>         <NA>
    ## 359                           <NA>          <NA>         <NA>
    ## 360                           <NA>          <NA>         <NA>
    ## 361                           <NA>          <NA>         <NA>
    ## 362                           <NA>          <NA>         <NA>
    ## 363                           <NA>          <NA>         <NA>
    ## 364                           <NA>          <NA>         <NA>
    ## 365                           <NA>          <NA>         <NA>
    ## 366                           <NA>          <NA>         <NA>
    ## 367                           <NA>          <NA>         <NA>
    ## 368                           <NA>          <NA>         <NA>
    ## 369                           <NA>          <NA>         <NA>
    ## 370                           <NA>          <NA>         <NA>
    ## 371                           <NA>          <NA>         <NA>
    ## 372                           <NA>          <NA>         <NA>
    ## 373                           <NA>          <NA>         <NA>
    ## 374                           <NA>          <NA>         <NA>
    ## 375                           <NA>          <NA>         <NA>
    ## 376                           <NA>          <NA>         <NA>
    ## 377                           <NA>          <NA>         <NA>
    ## 378                           <NA>          <NA>         <NA>
    ## 379                           <NA>          <NA>         <NA>
    ## 380                           <NA>          <NA>         <NA>
    ## 381                           <NA>          <NA>         <NA>
    ## 382                           <NA>          <NA>         <NA>
    ## 383                           <NA>          <NA>         <NA>
    ## 384                           <NA>          <NA>         <NA>
    ## 385                           <NA>          <NA>         <NA>
    ## 386                           <NA>          <NA>         <NA>
    ## 387                           <NA>          <NA>         <NA>
    ## 388                           <NA>          <NA>         <NA>
    ## 389                           <NA>          <NA>         <NA>
    ## 390                           <NA>          <NA>         <NA>
    ## 391                           <NA>          <NA>         <NA>
    ## 392                           <NA>          <NA>         <NA>
    ## 393                           <NA>          <NA>         <NA>
    ## 394                           <NA>          <NA>         <NA>
    ## 395                           <NA>          <NA>         <NA>
    ## 396                           <NA>          <NA>         <NA>
    ## 397                           <NA>          <NA>         <NA>
    ## 398                           <NA>          <NA>         <NA>
    ## 399                           <NA>          <NA>         <NA>
    ## 400                           <NA>          <NA>         <NA>
    ## 401                           <NA>          <NA>         <NA>
    ## 402                           <NA>          <NA>         <NA>
    ## 403                           <NA>          <NA>         <NA>
    ## 404                           <NA>          <NA>         <NA>
    ## 405                           <NA>          <NA>         <NA>
    ## 406                           <NA>          <NA>         <NA>
    ## 407                           <NA>          <NA>         <NA>
    ## 408                           <NA>          <NA>         <NA>
    ## 409                           <NA>          <NA>         <NA>
    ## 410                           <NA>          <NA>         <NA>
    ## 411                           <NA>          <NA>         <NA>
    ## 412                           <NA>          <NA>         <NA>
    ## 413                           <NA>          <NA>         <NA>
    ## 414                           <NA>          <NA>         <NA>
    ## 415                           <NA>          <NA>         <NA>
    ## 416                           <NA>          <NA>         <NA>
    ## 417                           <NA>          <NA>         <NA>
    ## 418                           <NA>          <NA>         <NA>
    ## 419                           <NA>          <NA>         <NA>
    ## 420                           <NA>          <NA>         <NA>
    ## 421                           <NA>          <NA>         <NA>
    ## 422                           <NA>          <NA>         <NA>
    ## 423                           <NA>          <NA>         <NA>
    ## 424                           <NA>          <NA>         <NA>
    ## 425                           <NA>          <NA>         <NA>
    ## 426                           <NA>          <NA>         <NA>
    ## 427                           <NA>          <NA>         <NA>
    ## 428                           <NA>          <NA>         <NA>
    ## 429                           <NA>          <NA>         <NA>
    ## 430                           <NA>          <NA>         <NA>
    ## 431                           <NA>          <NA>         <NA>
    ## 432                           <NA>          <NA>         <NA>
    ## 433                           <NA>          <NA>         <NA>
    ## 434                           <NA>          <NA>         <NA>
    ## 435                           <NA>          <NA>         <NA>
    ## 436                           <NA>          <NA>         <NA>
    ## 437                           <NA>          <NA>         <NA>
    ## 438                           <NA>          <NA>         <NA>
    ## 439                           <NA>          <NA>         <NA>
    ## 440                           <NA>          <NA>         <NA>
    ## 441                           <NA>          <NA>         <NA>
    ## 442                           <NA>          <NA>         <NA>
    ## 443                           <NA>          <NA>         <NA>
    ## 444                           <NA>          <NA>         <NA>
    ## 445                           <NA>          <NA>         <NA>
    ## 446                           <NA>          <NA>         <NA>
    ## 447                           <NA>          <NA>         <NA>
    ## 448                           <NA>          <NA>         <NA>
    ## 449                           <NA>          <NA>         <NA>
    ## 450                           <NA>          <NA>         <NA>
    ## 451                           <NA>          <NA>         <NA>
    ## 452                           <NA>          <NA>         <NA>
    ## 453                           <NA>          <NA>         <NA>
    ## 454                           <NA>          <NA>         <NA>
    ## 455                           <NA>          <NA>         <NA>
    ## 456                           <NA>          <NA>         <NA>
    ## 457                           <NA>          <NA>         <NA>
    ## 458                           <NA>          <NA>         <NA>
    ## 459                           <NA>          <NA>         <NA>
    ## 460                           <NA>          <NA>         <NA>
    ## 461                           <NA>          <NA>         <NA>
    ## 462                           <NA>          <NA>         <NA>
    ## 463                           <NA>          <NA>         <NA>
    ## 464                           <NA>          <NA>         <NA>
    ## 465                           <NA>          <NA>         <NA>
    ## 466                           <NA>          <NA>         <NA>
    ## 467                           <NA>          <NA>         <NA>
    ## 468                           <NA>          <NA>         <NA>
    ## 469                           <NA>          <NA>         <NA>
    ## 470                           <NA>          <NA>         <NA>
    ## 471                           <NA>          <NA>         <NA>
    ## 472                           <NA>          <NA>         <NA>
    ## 473                           <NA>          <NA>         <NA>
    ## 474                           <NA>          <NA>         <NA>
    ## 475                           <NA>          <NA>         <NA>
    ## 476                           <NA>          <NA>         <NA>
    ## 477                           <NA>          <NA>         <NA>
    ## 478                           <NA>          <NA>         <NA>
    ## 479                           <NA>          <NA>         <NA>
    ## 480                           <NA>          <NA>         <NA>
    ## 481                           <NA>          <NA>         <NA>
    ## 482                           <NA>          <NA>         <NA>
    ## 483                           <NA>          <NA>         <NA>
    ## 484                           <NA>          <NA>         <NA>
    ## 485                           <NA>          <NA>         <NA>
    ## 486                           <NA>          <NA>         <NA>
    ## 487                           <NA>          <NA>         <NA>
    ## 488                           <NA>          <NA>         <NA>
    ## 489                           <NA>          <NA>         <NA>
    ## 490                           <NA>          <NA>         <NA>
    ## 491                           <NA>          <NA>         <NA>
    ## 492                           <NA>          <NA>         <NA>
    ## 493                           <NA>          <NA>         <NA>
    ## 494                           <NA>          <NA>         <NA>
    ## 495                           <NA>          <NA>         <NA>
    ## 496                           <NA>          <NA>         <NA>
    ## 497                           <NA>          <NA>         <NA>
    ## 498                           <NA>          <NA>         <NA>
    ## 499                           <NA>          <NA>         <NA>
    ## 500                           <NA>          <NA>         <NA>
    ## 501                           <NA>          <NA>         <NA>
    ## 502                           <NA>          <NA>         <NA>
    ## 503                           <NA>          <NA>         <NA>
    ## 504                           <NA>          <NA>         <NA>
    ## 505                           <NA>          <NA>         <NA>
    ## 506                           <NA>          <NA>         <NA>
    ## 507                           <NA>          <NA>         <NA>
    ## 508                           <NA>          <NA>         <NA>
    ## 509                           <NA>          <NA>         <NA>
    ## 510                           <NA>          <NA>         <NA>
    ## 511                           <NA>          <NA>         <NA>
    ## 512                           <NA>          <NA>         <NA>
    ## 513                           <NA>          <NA>         <NA>
    ## 514                           <NA>          <NA>         <NA>
    ## 515                           <NA>          <NA>         <NA>
    ## 516                           <NA>          <NA>         <NA>
    ## 517                           <NA>          <NA>         <NA>
    ## 518                           <NA>          <NA>         <NA>
    ## 519                           <NA>          <NA>         <NA>
    ## 520                           <NA>          <NA>         <NA>
    ## 521                           <NA>          <NA>         <NA>
    ## 522                           <NA>          <NA>         <NA>
    ## 523                           <NA>          <NA>         <NA>
    ## 524                           <NA>          <NA>         <NA>
    ## 525                           <NA>          <NA>         <NA>
    ## 526                           <NA>          <NA>         <NA>
    ## 527                           <NA>          <NA>         <NA>
    ## 528                           <NA>          <NA>         <NA>
    ## 529                           <NA>          <NA>         <NA>
    ## 530                           <NA>          <NA>         <NA>
    ## 531                           <NA>          <NA>         <NA>
    ## 532                           <NA>          <NA>         <NA>
    ## 533                           <NA>          <NA>         <NA>
    ## 534                           <NA>          <NA>         <NA>
    ## 535                           <NA>          <NA>         <NA>
    ##                       type.x class.x id.y
    ## 1                   american     534  124
    ## 2               noodle shops     535  141
    ## 3                    mexican     536  116
    ## 4           pacific new wave     537  135
    ## 5                  fast food     538  162
    ## 6                continental     539   10
    ## 7                californian     540  118
    ## 8               coffee shops     542  117
    ## 9                californian     543  163
    ## 10                  american     545  128
    ## 11          pacific new wave     546  123
    ## 12                hamburgers     547  138
    ## 13                   russian     549  174
    ## 14                   italian     550  172
    ## 15              coffee shops     551  148
    ## 16            middle eastern     552   20
    ## 17                   chinese     553  129
    ## 18              cajun/creole     554  127
    ## 19                indonesian     556  173
    ## 20              coffee shops     557  167
    ## 21               californian     558  131
    ## 22                  hot dogs     559  145
    ## 23          american ( new )     560  155
    ## 24              coffee shops     561  143
    ## 25                  american     564  168
    ## 26                   seafood     565  149
    ## 27                  american     566  150
    ## 28                   chicken     567  144
    ## 29                   mexican     569  122
    ## 30                     delis     571  157
    ## 31               health food     572  137
    ## 32                  desserts     574  125
    ## 33               californian     577  164
    ## 34              noodle shops     578  158
    ## 35                hamburgers     579  170
    ## 36                     pizza     580    3
    ## 37                  american     581  154
    ## 38                    diners     583  153
    ## 39               californian     584  120
    ## 40                vietnamese     585  160
    ## 41                  hot dogs     586  142
    ## 42                  japanese     588  132
    ## 43                    diners     589  133
    ## 44                  hot dogs     590  161
    ## 45                    diners     591  121
    ## 46               steakhouses     593  152
    ## 47          pacific new wave     594  165
    ## 48                  japanese     595  159
    ## 49                  desserts     596  151
    ## 50                hamburgers     598  147
    ## 51                   seafood     600  119
    ## 52                    afghan     602  300
    ## 53          american ( new )     603  277
    ## 54                   mexican     604  190
    ## 55                     cuban     605  206
    ## 56                hamburgers     606  284
    ## 57                   italian     607  202
    ## 58                   seafood     608  342
    ## 59                    indian     609  291
    ## 60                    diners     610  360
    ## 61                    diners     611  263
    ## 62                     greek     612  282
    ## 63                  american     613  316
    ## 64                  hot dogs     614  243
    ## 65                   italian     615  249
    ## 66                    indian     616  214
    ## 67                   chinese     617  176
    ## 68                     pizza     618  348
    ## 69                 pan-asian     619  278
    ## 70                 ukrainian     620  241
    ## 71                  japanese     621  298
    ## 72                     cuban     622  179
    ## 73        french ( classic )     623  247
    ## 74                      thai     624  209
    ## 75                     pizza     625  228
    ## 76                     asian     626  299
    ## 77                  japanese     627  334
    ## 78                    indian     628  323
    ## 79                      thai     629  331
    ## 80            middle eastern     630  312
    ## 81                  japanese     631  350
    ## 82               continental     632  274
    ## 83                   seafood     633  318
    ## 84               steakhouses     634  322
    ## 85               steakhouses     635  180
    ## 86                     pizza     636  257
    ## 87               steakhouses     637  260
    ## 88                    indian     638  246
    ## 89                   chinese     639  341
    ## 90                  american     640  344
    ## 91               steakhouses     641  189
    ## 92                       bbq     642  337
    ## 93                  japanese     643  366
    ## 94             southern/soul     644  192
    ## 95                   chinese     645  251
    ## 96                   chinese     646  264
    ## 97                    polish     647  358
    ## 98                      thai     648  321
    ## 99                      thai     649  283
    ## 100                ukrainian     650  328
    ## 101                  chinese     651  375
    ## 102                 eclectic     652  329
    ## 103              steakhouses     653  326
    ## 104                 japanese     654  223
    ## 105                  mexican     655  255
    ## 106       french ( classic )     656  441
    ## 107              continental     657  429
    ## 108                  seafood     658  435
    ## 109                  seafood     659  438
    ## 110                  italian     660  423
    ## 111              continental     661  442
    ## 112                    asian     662  448
    ## 113                  chinese     663  447
    ## 114              continental     664  431
    ## 115           french ( new )     665  449
    ## 116                  chinese     666  446
    ## 117              steakhouses     667  444
    ## 118                  italian     668  452
    ## 119                  italian     669  443
    ## 120              californian     670  433
    ## 121              steakhouses     671  440
    ## 122                  italian     672  428
    ## 123                 eclectic     673  424
    ## 124                  italian     674  427
    ## 125              continental     675  384
    ## 126               sandwiches     676  421
    ## 127             cajun/creole     677  406
    ## 128                      bbq     678  413
    ## 129            french bistro     679  397
    ## 130            southern/soul     680  409
    ## 131            southern/soul     681  399
    ## 132               vegetarian     682  400
    ## 133              health food     683  381
    ## 134         american ( new )     684  382
    ## 135               hamburgers     685  388
    ## 136               hamburgers     686  398
    ## 137              steakhouses     687  389
    ## 138            southern/soul     689  419
    ## 139                  italian     690  411
    ## 140                 eclectic     691  414
    ## 141                  tex-mex     692  396
    ## 142            southern/soul     693  385
    ## 143                      bbq     694  404
    ## 144                    cuban     695  415
    ## 145                   indian     697  420
    ## 146             coffee shops     698  387
    ## 147             coffeehouses     700  380
    ## 148                  spanish     701  383
    ## 149               cafeterias     702  407
    ## 150                   diners     703  379
    ## 151              steakhouses     704  401
    ## 152                     thai     705  395
    ## 153             southwestern     706  405
    ## 154                  mexican     707  416
    ## 155                 american     708  393
    ## 156              steakhouses     709  422
    ## 157               vegetarian     710  392
    ## 158            mediterranean     712  412
    ## 159             coffee shops     713  390
    ## 160                 japanese     714  418
    ## 161               cafeterias     715  417
    ## 162                  tex-mex     716  391
    ## 163         american ( new )     717  402
    ## 164               vegetarian     718  403
    ## 165                   diners     719  410
    ## 166               hamburgers     721  492
    ## 167              californian     722  500
    ## 168              continental     723  461
    ## 169                  mexican     724  490
    ## 170                caribbean     725  465
    ## 171                 american     726  506
    ## 172                   diners     727  478
    ## 173                     thai     728  494
    ## 174                 japanese     729  529
    ## 175               vietnamese     730  519
    ## 176                  chinese     731  482
    ## 177               hamburgers     732  493
    ## 178              californian     733  491
    ## 179                  mexican     734  454
    ## 180            mediterranean     735  511
    ## 181                  mexican     736  462
    ## 182                  italian     737  464
    ## 183                     thai     738  524
    ## 184               hamburgers     739  473
    ## 185               hamburgers     740  498
    ## 186                cambodian     741  504
    ## 187                  mexican     742  530
    ## 188                 american     743  515
    ## 189                     thai     744  516
    ## 190               vietnamese     745  513
    ## 191                  seafood     746  510
    ## 192                     thai     747  521
    ## 193                   french     748  459
    ## 194                 american     749  501
    ## 195               vietnamese     750  460
    ## 196                    pizza     751  456
    ## 197                  mexican     752  463
    ## 198              steakhouses       0    0
    ## 199                    delis       1    1
    ## 200              californian       2    2
    ## 201              californian       4    4
    ## 202         pacific new wave       5    5
    ## 203              californian       6    6
    ## 204           french ( new )       7    7
    ## 205              californian       8    8
    ## 206 american ( traditional )       9    9
    ## 207       french ( classic )      11   11
    ## 208            french bistro      12   12
    ## 209                  italian      13   13
    ## 210                  seafood      14   14
    ## 211              steakhouses      15   15
    ## 212              californian      16   16
    ## 213               cafeterias      17   17
    ## 214            french bistro      18   18
    ## 215     nuova cucina italian      19   19
    ## 216              californian      20  146
    ## 217                  italian      21   21
    ## 218                  chinese      22   22
    ## 219         american ( new )      23   23
    ## 220             scandinavian      24   24
    ## 221         american ( new )      25   25
    ## 222             coffeehouses      26   26
    ## 223       french ( classic )      27   27
    ## 224                  italian      28   28
    ## 225                    delis      29   29
    ## 226           french ( new )      30   30
    ## 227           french ( new )      31   31
    ## 228                   indian      32   32
    ## 229                  italian      33   33
    ## 230         american ( new )      34   34
    ## 231         american ( new )      35   35
    ## 232         american ( new )      36   36
    ## 233                caribbean      37   37
    ## 234            french bistro      38   38
    ## 235       french ( classic )      39   39
    ## 236       french ( classic )      40   40
    ## 237                  seafood      41   41
    ## 238       french ( classic )      42   42
    ## 239                    asian      43   43
    ## 240       french ( classic )      44   44
    ## 241                  seafood      45   45
    ## 242         american ( new )      46   46
    ## 243             southwestern      47   47
    ## 244                  mexican      48   48
    ## 245            french bistro      49   49
    ## 246                  seafood      50   50
    ## 247         american ( new )      51   51
    ## 248                  russian      52   52
    ## 249            mediterranean      53   53
    ## 250                  seafood      54   54
    ## 251         american ( new )      55   55
    ## 252         american ( new )      56   56
    ## 253                  italian      57   57
    ## 254                    delis      58   58
    ## 255                 japanese      59   59
    ## 256                  chinese      60   60
    ## 257         american ( new )      61   61
    ## 258              steakhouses      62   62
    ## 259         american ( new )      63   63
    ## 260                    greek      64   64
    ## 261         american ( new )      65   65
    ## 262                      bbq      66   66
    ## 263                  chinese      67   67
    ## 264             southwestern      68   68
    ## 265            french bistro      69   69
    ## 266           french ( new )      70   70
    ## 267              pacific rim      71   71
    ## 268              steakhouses      72   72
    ## 269              steakhouses      73   73
    ## 270                  italian      74   74
    ## 271              californian      75   75
    ## 272              steakhouses      76   76
    ## 273            french bistro      77   77
    ## 274         american ( new )      78   78
    ## 275           french ( new )      79   79
    ## 276               cafeterias      80   80
    ## 277             southwestern      81   81
    ## 278              continental      82   82
    ## 279                   indian      83   83
    ## 280                 eclectic      84   84
    ## 281                  italian      85   85
    ## 282            southern/soul      86   86
    ## 283              continental      87   87
    ## 284         american ( new )      88   88
    ## 285         american ( new )      89   89
    ## 286         american ( new )      90   90
    ## 287       french ( classic )      91   91
    ## 288           french ( new )      92   92
    ## 289                  italian      93   93
    ## 290           french ( new )      94   94
    ## 291         american ( new )      95   95
    ## 292         american ( new )      96   96
    ## 293            french bistro      97   97
    ## 294         american ( new )      98   98
    ## 295              californian      99   99
    ## 296           french ( new )     100  100
    ## 297            french bistro     101  101
    ## 298              californian     102  102
    ## 299                     thai     103  103
    ## 300           french ( new )     104  104
    ## 301            mediterranean     105  105
    ## 302           french ( new )     106  106
    ## 303                 japanese     107  107
    ## 304         american ( new )     108  108
    ## 305              californian     109  109
    ## 306           french ( new )     110  110
    ## 307                  italian     111  111
    ## 308         american ( new )     711  386
    ## 309                  chinese     688   NA
    ## 310                 american     699   NA
    ## 311                     <NA>      NA  112
    ## 312                     <NA>      NA  113
    ## 313                     <NA>      NA  114
    ## 314                     <NA>      NA  115
    ## 315                     <NA>      NA  126
    ## 316                     <NA>      NA  130
    ## 317                     <NA>      NA  134
    ## 318                     <NA>      NA  136
    ## 319                     <NA>      NA  139
    ## 320                     <NA>      NA  140
    ## 321                     <NA>      NA  156
    ## 322                     <NA>      NA  166
    ## 323                     <NA>      NA  169
    ## 324                     <NA>      NA  171
    ## 325                     <NA>      NA  175
    ## 326                     <NA>      NA  177
    ## 327                     <NA>      NA  178
    ## 328                     <NA>      NA  181
    ## 329                     <NA>      NA  182
    ## 330                     <NA>      NA  183
    ## 331                     <NA>      NA  184
    ## 332                     <NA>      NA  185
    ## 333                     <NA>      NA  186
    ## 334                     <NA>      NA  187
    ## 335                     <NA>      NA  188
    ## 336                     <NA>      NA  191
    ## 337                     <NA>      NA  193
    ## 338                     <NA>      NA  194
    ## 339                     <NA>      NA  195
    ## 340                     <NA>      NA  196
    ## 341                     <NA>      NA  197
    ## 342                     <NA>      NA  198
    ## 343                     <NA>      NA  199
    ## 344                     <NA>      NA  200
    ## 345                     <NA>      NA  201
    ## 346                     <NA>      NA  203
    ## 347                     <NA>      NA  204
    ## 348                     <NA>      NA  205
    ## 349                     <NA>      NA  207
    ## 350                     <NA>      NA  208
    ## 351                     <NA>      NA  210
    ## 352                     <NA>      NA  211
    ## 353                     <NA>      NA  212
    ## 354                     <NA>      NA  213
    ## 355                     <NA>      NA  215
    ## 356                     <NA>      NA  216
    ## 357                     <NA>      NA  217
    ## 358                     <NA>      NA  218
    ## 359                     <NA>      NA  219
    ## 360                     <NA>      NA  220
    ## 361                     <NA>      NA  221
    ## 362                     <NA>      NA  222
    ## 363                     <NA>      NA  224
    ## 364                     <NA>      NA  225
    ## 365                     <NA>      NA  226
    ## 366                     <NA>      NA  227
    ## 367                     <NA>      NA  229
    ## 368                     <NA>      NA  230
    ## 369                     <NA>      NA  231
    ## 370                     <NA>      NA  232
    ## 371                     <NA>      NA  233
    ## 372                     <NA>      NA  234
    ## 373                     <NA>      NA  235
    ## 374                     <NA>      NA  236
    ## 375                     <NA>      NA  237
    ## 376                     <NA>      NA  238
    ## 377                     <NA>      NA  239
    ## 378                     <NA>      NA  240
    ## 379                     <NA>      NA  242
    ## 380                     <NA>      NA  244
    ## 381                     <NA>      NA  245
    ## 382                     <NA>      NA  248
    ## 383                     <NA>      NA  250
    ## 384                     <NA>      NA  252
    ## 385                     <NA>      NA  253
    ## 386                     <NA>      NA  254
    ## 387                     <NA>      NA  256
    ## 388                     <NA>      NA  258
    ## 389                     <NA>      NA  259
    ## 390                     <NA>      NA  261
    ## 391                     <NA>      NA  262
    ## 392                     <NA>      NA  265
    ## 393                     <NA>      NA  266
    ## 394                     <NA>      NA  267
    ## 395                     <NA>      NA  268
    ## 396                     <NA>      NA  269
    ## 397                     <NA>      NA  270
    ## 398                     <NA>      NA  271
    ## 399                     <NA>      NA  272
    ## 400                     <NA>      NA  273
    ## 401                     <NA>      NA  275
    ## 402                     <NA>      NA  276
    ## 403                     <NA>      NA  279
    ## 404                     <NA>      NA  280
    ## 405                     <NA>      NA  281
    ## 406                     <NA>      NA  285
    ## 407                     <NA>      NA  286
    ## 408                     <NA>      NA  287
    ## 409                     <NA>      NA  288
    ## 410                     <NA>      NA  289
    ## 411                     <NA>      NA  290
    ## 412                     <NA>      NA  292
    ## 413                     <NA>      NA  293
    ## 414                     <NA>      NA  294
    ## 415                     <NA>      NA  295
    ## 416                     <NA>      NA  296
    ## 417                     <NA>      NA  297
    ## 418                     <NA>      NA  301
    ## 419                     <NA>      NA  302
    ## 420                     <NA>      NA  303
    ## 421                     <NA>      NA  304
    ## 422                     <NA>      NA  305
    ## 423                     <NA>      NA  306
    ## 424                     <NA>      NA  307
    ## 425                     <NA>      NA  308
    ## 426                     <NA>      NA  309
    ## 427                     <NA>      NA  310
    ## 428                     <NA>      NA  311
    ## 429                     <NA>      NA  313
    ## 430                     <NA>      NA  314
    ## 431                     <NA>      NA  315
    ## 432                     <NA>      NA  317
    ## 433                     <NA>      NA  319
    ## 434                     <NA>      NA  320
    ## 435                     <NA>      NA  324
    ## 436                     <NA>      NA  325
    ## 437                     <NA>      NA  327
    ## 438                     <NA>      NA  330
    ## 439                     <NA>      NA  332
    ## 440                     <NA>      NA  333
    ## 441                     <NA>      NA  335
    ## 442                     <NA>      NA  336
    ## 443                     <NA>      NA  338
    ## 444                     <NA>      NA  339
    ## 445                     <NA>      NA  340
    ## 446                     <NA>      NA  343
    ## 447                     <NA>      NA  345
    ## 448                     <NA>      NA  346
    ## 449                     <NA>      NA  347
    ## 450                     <NA>      NA  349
    ## 451                     <NA>      NA  351
    ## 452                     <NA>      NA  352
    ## 453                     <NA>      NA  353
    ## 454                     <NA>      NA  354
    ## 455                     <NA>      NA  355
    ## 456                     <NA>      NA  356
    ## 457                     <NA>      NA  357
    ## 458                     <NA>      NA  359
    ## 459                     <NA>      NA  361
    ## 460                     <NA>      NA  362
    ## 461                     <NA>      NA  363
    ## 462                     <NA>      NA  364
    ## 463                     <NA>      NA  365
    ## 464                     <NA>      NA  367
    ## 465                     <NA>      NA  368
    ## 466                     <NA>      NA  369
    ## 467                     <NA>      NA  370
    ## 468                     <NA>      NA  371
    ## 469                     <NA>      NA  372
    ## 470                     <NA>      NA  373
    ## 471                     <NA>      NA  374
    ## 472                     <NA>      NA  376
    ## 473                     <NA>      NA  377
    ## 474                     <NA>      NA  378
    ## 475                     <NA>      NA  394
    ## 476                     <NA>      NA  408
    ## 477                     <NA>      NA  425
    ## 478                     <NA>      NA  426
    ## 479                     <NA>      NA  430
    ## 480                     <NA>      NA  432
    ## 481                     <NA>      NA  434
    ## 482                     <NA>      NA  436
    ## 483                     <NA>      NA  437
    ## 484                     <NA>      NA  439
    ## 485                     <NA>      NA  445
    ## 486                     <NA>      NA  450
    ## 487                     <NA>      NA  451
    ## 488                     <NA>      NA  453
    ## 489                     <NA>      NA  455
    ## 490                     <NA>      NA  457
    ## 491                     <NA>      NA  458
    ## 492                     <NA>      NA  466
    ## 493                     <NA>      NA  467
    ## 494                     <NA>      NA  468
    ## 495                     <NA>      NA  469
    ## 496                     <NA>      NA  470
    ## 497                     <NA>      NA  471
    ## 498                     <NA>      NA  472
    ## 499                     <NA>      NA  474
    ## 500                     <NA>      NA  475
    ## 501                     <NA>      NA  476
    ## 502                     <NA>      NA  477
    ## 503                     <NA>      NA  479
    ## 504                     <NA>      NA  480
    ## 505                     <NA>      NA  481
    ## 506                     <NA>      NA  483
    ## 507                     <NA>      NA  484
    ## 508                     <NA>      NA  485
    ## 509                     <NA>      NA  486
    ## 510                     <NA>      NA  487
    ## 511                     <NA>      NA  488
    ## 512                     <NA>      NA  489
    ## 513                     <NA>      NA  495
    ## 514                     <NA>      NA  496
    ## 515                     <NA>      NA  497
    ## 516                     <NA>      NA  499
    ## 517                     <NA>      NA  502
    ## 518                     <NA>      NA  503
    ## 519                     <NA>      NA  505
    ## 520                     <NA>      NA  507
    ## 521                     <NA>      NA  508
    ## 522                     <NA>      NA  509
    ## 523                     <NA>      NA  512
    ## 524                     <NA>      NA  514
    ## 525                     <NA>      NA  517
    ## 526                     <NA>      NA  518
    ## 527                     <NA>      NA  520
    ## 528                     <NA>      NA  522
    ## 529                     <NA>      NA  523
    ## 530                     <NA>      NA  525
    ## 531                     <NA>      NA  526
    ## 532                     <NA>      NA  527
    ## 533                     <NA>      NA  528
    ## 534                     <NA>      NA  531
    ## 535                     <NA>      NA  532
    ##                                            name.y
    ## 1                        california pizza kitchen
    ## 2                    harry's bar & american grill
    ## 3                               barney greengrass
    ## 4                                         eclipse
    ## 5                                            remi
    ## 6                                restaurant katsu
    ## 7                                   bistro garden
    ## 8                                      beaurivage
    ## 9                             restaurant horikawa
    ## 10                                      chan dara
    ## 11                                     cafe pinot
    ## 12                                    gilliland's
    ## 13                                west beach cafe
    ## 14                                   trader vic's
    ## 15                                        le dome
    ## 16                                          spago
    ## 17                                clearwater cafe
    ## 18                                    cha cha cha
    ## 19                                           vida
    ## 20                                       swingers
    ## 21                                         dive !
    ## 22                                        jimmy's
    ## 23                                        orleans
    ## 24                             jack sprat's grill
    ## 25                                   tavola calda
    ## 26                             louise's trattoria
    ## 27                     mon kee seafood restaurant
    ## 28                                 jackson's farm
    ## 29                                  ca  ` del sol
    ## 30                                         paty's
    ## 31                                       el cholo
    ## 32                                       canter's
    ## 33      roscoe's house of chicken  ` n  ' waffles
    ## 34                                pinot hollywood
    ## 35                                   tommy tang's
    ## 36                                     cafe bizou
    ## 37                                   ocean avenue
    ## 38                                         nicola
    ## 39                                  broadway deli
    ## 40                                          prego
    ## 41                     il fornaio cucina italiana
    ## 42                                          drago
    ## 43                                         drai's
    ## 44                             rj's the rib joint
    ## 45                                     ca  ` brea
    ## 46                              nate  ` n' al  's
    ## 47                                schatzi on main
    ## 48                                          posto
    ## 49                                       morton's
    ## 50                                    le colonial
    ## 51                                   border grill
    ## 52                                  mangia e bevi
    ## 53                                   judson grill
    ## 54                                   ben benson's
    ## 55                                    cafe centro
    ## 56                            lattanzi ristorante
    ## 57                                  cafe botanica
    ## 58                                     san pietro
    ## 59                                      le marais
    ## 60                                 the coffee pot
    ## 61                                 hard rock cafe
    ## 62                                     la reserve
    ## 63                                          odeon
    ## 64                          fiorello's roman cafe
    ## 65                                      follonico
    ## 66                         caffe bondi ristorante
    ## 67                               ' 9 jones street
    ## 68                                         sfuzzi
    ## 69                                  l  ` absinthe
    ## 70                        fifty seven fifty seven
    ## 71                                       mad fish
    ## 72                                            aja
    ## 73                                  fleur de jour
    ## 74                          caffe dell  ` artista
    ## 75                                  dean & deluca
    ## 76                                    main street
    ## 77                                      red tulip
    ## 78                            parioli romanissimo
    ## 79                                   popover cafe
    ## 80                                    motown cafe
    ## 81                           sofia fabulous pizza
    ## 82                                 jewel of india
    ## 83                               osteria al droge
    ## 84                                          pamir
    ## 85                                          alamo
    ## 86                                       gianni's
    ## 87                                 golden unicorn
    ## 88                                   fishin eddie
    ## 89                  sammy's roumanian steak house
    ## 90                             sarabeth's kitchen
    ## 91                                       barbetta
    ## 92                                  rosa mexicano
    ## 93                                       tse yang
    ## 94                                        billy's
    ## 95                                   french roast
    ## 96                  hi-life restaurant and lounge
    ## 97                                       teresa's
    ## 98                                          palio
    ## 99                               lanza restaurant
    ## 100                                    persepolis
    ## 101                                          west
    ## 102                              planet hollywood
    ## 103                                  pen & pencil
    ## 104                                    coco pazzo
    ## 105                                    gabriela's
    ## 106                                  golden steer
    ## 107                                        bistro
    ## 108                                carnival world
    ## 109                               'em press court
    ## 110                                     antonio's
    ## 111                              lillie langtry's
    ## 112                                 ralph's diner
    ## 113                                  pamplemousse
    ## 114                                 bugsy's diner
    ## 115                                 the bacchanal
    ## 116                                        mikado
    ## 117                   margarita's mexican cantina
    ## 118                                       yolie's
    ## 119                                mandarin court
    ## 120                                     cafe roma
    ## 121                           golden nugget hotel
    ## 122                          binion's coffee shop
    ## 123                           bally's big kitchen
    ## 124                                   bertolini's
    ## 125                         beesley's of buckhead
    ## 126                          taste of new orleans
    ## 127                      mambo restaurante cubano
    ## 128              r.j.'s uptown kitchen & wine bar
    ## 129                            holt bros. bar-b-q
    ## 130                    nickiemoto's : a sushi bar
    ## 131                                 hsu's gourmet
    ## 132                                  imperial fez
    ## 133                           annie's thai castle
    ## 134                                      anthonys
    ## 135                                     camille's
    ## 136                             horseradish grill
    ## 137                                        cassis
    ## 138           stringer's fish camp and oyster bar
    ## 139                              pleasant peasant
    ## 140                                     rib ranch
    ## 141                      french quarter food shop
    ## 142                                   bertolini's
    ## 143                           lowcountry barbecue
    ## 144                                     sa tsu ki
    ## 145                                  sundown cafe
    ## 146                              cafe renaissance
    ## 147                       aleck's barbecue heaven
    ## 148                           atlanta fish market
    ## 149                          mckinnon's louisiane
    ## 150                                         abbey
    ## 151                                      kamogawa
    ## 152                          fat matt's rib shack
    ## 153                                       luna si
    ## 154                           sato sushi and thai
    ## 155               dante's down the hatch buckhead
    ## 156                                        tomtom
    ## 157                          colonnade restaurant
    ## 158                                        pricci
    ## 159                                    city grill
    ## 160                               south of france
    ## 161                            south city kitchen
    ## 162                                     coco loco
    ## 163             la grotta at ravinia dunwoody rd.
    ## 164                               little szechuan
    ## 165                                     palisades
    ## 166                                     le soleil
    ## 167                                    one market
    ## 168                                  cafe adriano
    ## 169                        l  ` osteria del forno
    ## 170                                         carta
    ## 171                                       perry's
    ## 172                            hayes street grill
    ## 173                                        manora
    ## 174                                  yaya cuisine
    ## 175                                  tadich grill
    ## 176                            hyde street bistro
    ## 177                                macarthur park
    ## 178                                    le central
    ## 179                                    acquarello
    ## 180                                scala's bistro
    ## 181                                  cafe marimba
    ## 182                                 capp's corner
    ## 183                             vivande porta via
    ## 184                       grand cafe hotel monaco
    ## 185                                       moose's
    ## 186                                   pane e vino
    ## 187                           yoyo tsumami bistro
    ## 188                                    stars cafe
    ## 189                               stoyanof's cafe
    ## 190                         splendido embarcadero
    ## 191                                        sanppo
    ## 192                                        thepin
    ## 193                                         bizou
    ## 194                                      oritalia
    ## 195                                 buca giovanni
    ## 196                                      betelnut
    ## 197                   california culinary academy
    ## 198                     arnie morton's of chicago
    ## 199                            art's delicatessen
    ## 200                                 hotel bel-air
    ## 201                                     campanile
    ## 202                               chinois on main
    ## 203                                        citrus
    ## 204                                         fenix
    ## 205                                       granita
    ## 206                            grill on the alley
    ## 207                                l  ` orangerie
    ## 208                                 le chardonnay
    ## 209                                locanda veneta
    ## 210                                     matsuhisa
    ## 211                                      the palm
    ## 212                                        patina
    ## 213                       philippe's the original
    ## 214                                  pinot bistro
    ## 215                             rex il ristorante
    ## 216                                          joss
    ## 217                                     valentino
    ## 218         yujean kang's gourmet chinese cuisine
    ## 219                                      '21 club
    ## 220                                       aquavit
    ## 221                                       aureole
    ## 222                                     cafe lalo
    ## 223                             cafe des artistes
    ## 224                                     carmine's
    ## 225                                 carnegie deli
    ## 226                                   chanterelle
    ## 227                                        daniel
    ## 228                                         dawat
    ## 229                                       felidia
    ## 230                       four seasons grill room
    ## 231                            gotham bar & grill
    ## 232                               gramercy tavern
    ## 233                                  island spice
    ## 234                                         jo jo
    ## 235                                  la caravelle
    ## 236                                la cote basque
    ## 237                                  le bernardin
    ## 238                                les celebrites
    ## 239                                    lespinasse
    ## 240                                        lutece
    ## 241                          manhattan ocean club
    ## 242                                         march
    ## 243                                    mesa grill
    ## 244                                     mi cocina
    ## 245                                    montrachet
    ## 246                                        oceana
    ## 247                              park avenue cafe
    ## 248                                    petrossian
    ## 249                                     picholine
    ## 250                                        pisces
    ## 251                                  rainbow room
    ## 252                                    river cafe
    ## 253                                  san domenico
    ## 254                            second avenue deli
    ## 255                                        seryna
    ## 256                                 shun lee west
    ## 257                              sign of the dove
    ## 258                             smith & wollensky
    ## 259                           tavern on the green
    ## 260                                  uncle nick's
    ## 261                             union square cafe
    ## 262                                      virgil's
    ## 263                                        chin's
    ## 264                                   coyote cafe
    ## 265                                 le montrachet
    ## 266                                  palace court
    ## 267                          second street grille
    ## 268                                   steak house
    ## 269                                     tillerman
    ## 270                                       abruzzi
    ## 271                                   bacchanalia
    ## 272                                        bone's
    ## 273                             brasserie le coze
    ## 274                                buckhead diner
    ## 275                                    ciboulette
    ## 276                                   delectables
    ## 277                                georgia grille
    ## 278                         hedgerose heights inn
    ## 279                                heera of india
    ## 280                          indigo coastal grill
    ## 281                                     la grotta
    ## 282                           mary mac's tea room
    ## 283                                nikolai's roof
    ## 284                           pano's and paul  's
    ## 285                    cafe ritz-carlton buckhead
    ## 286             dining room ritz-carlton buckhead
    ## 287               restaurant ritz-carlton atlanta
    ## 288                                      toulouse
    ## 289                                veni vidi vici
    ## 290                                alain rondelli
    ## 291                                          aqua
    ## 292                                     boulevard
    ## 293                                   cafe claude
    ## 294                                 campton place
    ## 295                                   chez michel
    ## 296                                  fleur de lys
    ## 297                                      fringale
    ## 298                                hawthorne lane
    ## 299                          khan toke thai house
    ## 300                                      la folie
    ## 301                                          lulu
    ## 302                                        masa's
    ## 303         mifune japan center kintetsu building
    ## 304                                plumpjack cafe
    ## 305                                       postrio
    ## 306       ritz-carlton restaurant and dining room
    ## 307                                  rose pistola
    ## 308                                      bistango
    ## 309                                          <NA>
    ## 310                                          <NA>
    ## 311                                          bolo
    ## 312                                       il nido
    ## 313                                          remi
    ## 314                          adriano's ristorante
    ## 315                                          cava
    ## 316                                   dining room
    ## 317                                  dynasty room
    ## 318                                  ed debevic's
    ## 319                                   gladstone's
    ## 320                                hard rock cafe
    ## 321                            pacific dining car
    ## 322                                          sofi
    ## 323                                  the mandarin
    ## 324                                    tra di noi
    ## 325                                      '20 mott
    ## 326                                      adrienne
    ## 327                                     agrotikon
    ## 328                                   alley's end
    ## 329                              ambassador grill
    ## 330                                american place
    ## 331                                  anche vivolo
    ## 332                                       arizona
    ## 333                                      arturo's
    ## 334                                   au mandarin
    ## 335                                     bar anise
    ## 336                                       big cup
    ## 337                                    boca chica
    ## 338                                      boonthai
    ## 339                                      bouterin
    ## 340                              brothers bar-b-q
    ## 341                                         bruno
    ## 342 bryant park grill roof restaurant and bp cafe
    ## 343                                            c3
    ## 344                                            ct
    ## 345                                   cafe bianco
    ## 346                               cafe la fortuna
    ## 347                               cafe luxembourg
    ## 348                                   cafe pierre
    ## 349                                      cafe fes
    ## 350                                   caffe dante
    ## 351                                    caffe lure
    ## 352                                  caffe reggio
    ## 353                                    caffe roma
    ## 354                                 caffe vivaldi
    ## 355                               capsouto freres
    ## 356                               captain's table
    ## 357                                 casa la femme
    ## 358          cendrillon asian grill & marimba bar
    ## 359                               chez jacqueline
    ## 360                                         chiam
    ## 361                                   china grill
    ## 362                                          cite
    ## 363                               columbus bakery
    ## 364                                  corrado cafe
    ## 365                                  cupcake cafe
    ## 366                                       da nico
    ## 367                                          diva
    ## 368                                   dix et sept
    ## 369                                         docks
    ## 370                               duane park cafe
    ## 371                                    el teddy's
    ## 372                                     'em ily's
    ## 373                                'em pire korea
    ## 374                                       ernie's
    ## 375                                evergreen cafe
    ## 376                      f. ille ponte ristorante
    ## 377                                         felix
    ## 378                                       ferrier
    ## 379                              film center cafe
    ## 380                                     firehouse
    ## 381                                         first
    ## 382                                       flowers
    ## 383                               fraunces tavern
    ## 384                             french roast cafe
    ## 385                                     frico bar
    ## 386                                 fujiyama mama
    ## 387                                   gallagher's
    ## 388                                        girafe
    ## 389                                        global
    ## 390                                  grand ticino
    ## 391                                       halcyon
    ## 392                                          home
    ## 393                             hudson river club
    ## 394                                    ' i trulli
    ## 395                                    il cortile
    ## 396                                    inca grill
    ## 397                                     indochine
    ## 398                                 internet cafe
    ## 399                                       ipanema
    ## 400                                  jean lafitte
    ## 401                                  jimmy sung's
    ## 402                                     joe allen
    ## 403                                  l  ` auberge
    ## 404                          l  ` auberge du midi
    ## 405                                      l  ` udo
    ## 406                                         layla
    ## 407                                  le chantilly
    ## 408                                   le colonial
    ## 409                                      le gamin
    ## 410                                     le jardin
    ## 411                                      le madri
    ## 412                                   le perigord
    ## 413                                     le select
    ## 414                                    les halles
    ## 415                                lincoln tavern
    ## 416                                          lola
    ## 417                                  lucky strike
    ## 418                                manhattan cafe
    ## 419                                 manila garden
    ## 420                                       marichu
    ## 421                            marquet patisserie
    ## 422                                         match
    ## 423                                     matthew's
    ## 424                                mavalli palace
    ## 425                     milan cafe and coffee bar
    ## 426                                    monkey bar
    ## 427                                       montien
    ## 428                                      morton's
    ## 429              new york kom tang soot bul house
    ## 430                           new york noodletown
    ## 431                                       newsbar
    ## 432                                          orso
    ## 433                                         otabe
    ## 434                                      pacifica
    ## 435                                        patria
    ## 436                                 peacock alley
    ## 437                                   penang soho
    ## 438                                       pomaire
    ## 439                                    post house
    ## 440                                          rain
    ## 441                                      republic
    ## 442                                roettelle a. g
    ## 443                                  ruth's chris
    ## 444                                       s.p.q.r
    ## 445                                 sal anthony's
    ## 446                                 sant ambroeus
    ## 447                                     sea grill
    ## 448                                   serendipity
    ## 449                 seventh regiment mess and bar
    ## 450                                         shaan
    ## 451        spring street natural restaurant & bar
    ## 452                                    stage deli
    ## 453                                      stingray
    ## 454                       sweet  ` n  ` tart cafe
    ## 455                                     ' t salon
    ## 456                                tang pavillion
    ## 457                                        tapika
    ## 458                                       terrace
    ## 459                             the savannah club
    ## 460                        trattoria dell  ` arte
    ## 461                                     triangolo
    ## 462                                 tribeca grill
    ## 463                                    trois jean
    ## 464                               turkish kitchen
    ## 465                                   two two two
    ## 466                         veniero's pasticceria
    ## 467                                       verbena
    ## 468                                 victor's cafe
    ## 469                               vince & eddie's
    ## 470                                          vong
    ## 471                                    water club
    ## 472                                         xunta
    ## 473                                    zen palate
    ## 474                                           zoe
    ## 475                        dante's down the hatch
    ## 476                          mi spia dunwoody rd.
    ## 477                                 bamboo garden
    ## 478                   battista's hole in the wall
    ## 479                                       broiler
    ## 480                                 cafe michelle
    ## 481                                   capozzoli's
    ## 482                      center stage plaza hotel
    ## 483                                 circus circus
    ## 484                                         feast
    ## 485                                  mary's diner
    ## 486                                      venetian
    ## 487                                viva mercado's
    ## 488                                          2223
    ## 489                                    bardelli's
    ## 490                                   bistro roti
    ## 491                                           bix
    ## 492                                        chevys
    ## 493                                  cypress club
    ## 494                                     des alpes
    ## 495                                           faz
    ## 496                                fog city diner
    ## 497                                  garden court
    ## 498                                     gaylord's
    ## 499                                        greens
    ## 500                                harbor village
    ## 501                                       harris'
    ## 502                                harry denton's
    ## 503                                       helmand
    ## 504                       hong kong flower lounge
    ## 505                               hong kong villa
    ## 506                       il fornaio levi's plaza
    ## 507                     izzy's steak & chop house
    ## 508                                        jack's
    ## 509                                  kabuto sushi
    ## 510                                       katia's
    ## 511                                      kuleto's
    ## 512                kyo-ya . sheraton palace hotel
    ## 513                                      maykadeh
    ## 514                          mccormick & kuleto's
    ## 515                                    millennium
    ## 516                                   north india
    ## 517                     pacific pan pacific hotel
    ## 518                               palio d  ` asti
    ## 519                                        pastis
    ## 520                                  r & g lounge
    ## 521                                       rubicon
    ## 522                                        rumpus
    ## 523                               south park cafe
    ## 524                                         stars
    ## 525                                  straits cafe
    ## 526                                   suppenkuche
    ## 527                                   the heights
    ## 528                                     ton kiang
    ## 529                                       vertigo
    ## 530                            vivande ristorante
    ## 531                                  world wrapps
    ## 532                                       wu kong
    ## 533                                     yank sing
    ## 534                                      zarzuela
    ## 535                             zuni cafe & grill
    ##                                                        addr.y        city.y
    ## 1                                          207 s. beverly dr.   los angeles
    ## 2                                      2020 ave. of the stars   los angeles
    ## 3                                         9570 wilshire blvd.   los angeles
    ## 4                                           8800 melrose ave.   los angeles
    ## 5                                           3rd st. promenade   los angeles
    ## 6                                      1972 n. hillhurst ave.   los angeles
    ## 7                                            176 n. canon dr.   los angeles
    ## 8                                   26025 pacific coast hwy .   los angeles
    ## 9                                        111 s. san pedro st.   los angeles
    ## 10                                     310 n. larchmont blvd.   los angeles
    ## 11                                           700 w. fifth st.   los angeles
    ## 12                                              2424 main st.   los angeles
    ## 13                                         60 n. venice blvd.   los angeles
    ## 14                                        9876 wilshire blvd.   los angeles
    ## 15                                          8720 sunset blvd.   los angeles
    ## 16                                             1114 horn ave.   los angeles
    ## 17                                      168 w. colorado blvd.   los angeles
    ## 18                                         656 n. virgil ave.   los angeles
    ## 19                                  1930 north hillhurst ave.   los angeles
    ## 20                                         8020 beverly blvd.   los angeles
    ## 21                                   10250 santa monica blvd.   los angeles
    ## 22                                             201 moreno dr.   los angeles
    ## 23                                       11705 national blvd.   los angeles
    ## 24                                        10668 w. pico blvd.   los angeles
    ## 25                                          7371 melrose ave.   los angeles
    ## 26                                       4500 los feliz blvd.   los angeles
    ## 27                                          679 n. spring st.   los angeles
    ## 28                                       439 n. beverly drive   los angeles
    ## 29                                        4100 cahuenga blvd.   los angeles
    ## 30                                        10001 riverside dr.   los angeles
    ## 31                                       1121 s. western ave.   los angeles
    ## 32                                        419 n. fairfax ave.   los angeles
    ## 33                                          1514 n. gower st.   los angeles
    ## 34                                          1448 n. gower st.   los angeles
    ## 35                                          7313 melrose ave.   los angeles
    ## 36                                        14016 ventura blvd.   los angeles
    ## 37                                            1401 ocean ave.   los angeles
    ## 38                                        601 s. figueroa st.   los angeles
    ## 39                                          3rd st. promenade   los angeles
    ## 40                                          362 n. camden dr.   los angeles
    ## 41                                         301 n. beverly dr.   los angeles
    ## 42                                        2628 wilshire blvd.   los angeles
    ## 43                                    730 n. la cienega blvd.   los angeles
    ## 44                                         252 n. beverly dr.   los angeles
    ## 45                                        346 s. la brea ave.   los angeles
    ## 46                                         414 n. beverly dr.   los angeles
    ## 47                                              3110 main st.   los angeles
    ## 48                                        14928 ventura blvd.   los angeles
    ## 49                                          8764 melrose ave.   los angeles
    ## 50                                         8783 beverly blvd.   los angeles
    ## 51                                                    4th st.   los angeles
    ## 52                                   800 9th ave. at 53rd st.      new york
    ## 53                                            152 w. 52nd st.      new york
    ## 54                                            123 w. 52nd st.      new york
    ## 55         200 park ave. between 45th st. and vanderbilt ave.      new york
    ## 56                                            361 w. 46th st.      new york
    ## 57                                         160 central park s      new york
    ## 58                                             18 e. 54th st.      new york
    ## 59                                            150 w. 46th st.      new york
    ## 60                                   350 9th ave. at 49th st.      new york
    ## 61                                            221 w. 57th st.      new york
    ## 62                                              4 w. 49th st.      new york
    ## 63                              145 w. broadway at thomas st.      new york
    ## 64                  1900 broadway between 63rd and 64th sts .      new york
    ## 65                                              6 w. 24th st.      new york
    ## 66                                              7 w. 20th st.      new york
    ## 67                                                9 jones st.      new york
    ## 68                                             58 w. 65th st.      new york
    ## 69                                            227 e. 67th st.      new york
    ## 70                                             57 e. 57th st.      new york
    ## 71                  2182 broadway between 77th and 78th sts .      new york
    ## 72                                   937 broadway at 22nd st.      new york
    ## 73                                            348 e. 62nd st.      new york
    ## 74                                          46 greenwich ave.      new york
    ## 75                                             121 prince st.      new york
    ## 76              446 columbus ave. between 81st and 82nd sts .      new york
    ## 77                                            439 e. 75th st.      new york
    ## 78                                             24 e. 81st st.      new york
    ## 79             551 amsterdam ave. between 86th and 87th sts .      new york
    ## 80                              104 w. 57th st. near 6th ave.      new york
    ## 81                            1022 madison ave. near 79th st.      new york
    ## 82                                             15 w. 44th st.      new york
    ## 83                                            142 w. 44th st.      new york
    ## 84                                  1065 1st ave. at 58th st.      new york
    ## 85                                            304 e. 48th st.      new york
    ## 86                                              15 fulton st.      new york
    ## 87                            18 e. broadway at catherine st.      new york
    ## 88                                             73 w. 71st st.      new york
    ## 89                           157 chrystie st. at delancey st.      new york
    ## 90             423 amsterdam ave. between 80th and 81st sts .      new york
    ## 91                                            321 w. 46th st.      new york
    ## 92                                  1063 1st ave. at 58th st.      new york
    ## 93                                             34 e. 51st st.      new york
    ## 94                   948 1st ave. between 52nd and 53rd sts .      new york
    ## 95                                   458 6th ave. at 11th st.      new york
    ## 96                                  1340 1st ave. at 72nd st.      new york
    ## 97                     103 1st ave. between 6th and 7th sts .      new york
    ## 98                                          151 w. 51st . st.      new york
    ## 99                   168 1st ave. between 10th and 11th sts .      new york
    ## 100                 1423 2nd ave. between 74th and 75th sts .      new york
    ## 101                     63rd street steakhouse 44 w. 63rd st.      new york
    ## 102                                           140 w. 57th st.      new york
    ## 103                                           205 e. 45th st.      new york
    ## 104                                            23 e. 74th st.      new york
    ## 105                            685 amsterdam ave. at 93rd st.      new york
    ## 106                                        308 w. sahara ave.     las vegas
    ## 107                                    3400 las vegas blvd. s     las vegas
    ## 108                                      3700 w. flamingo rd.     las vegas
    ## 109                                    3570 las vegas blvd. s     las vegas
    ## 110                                          3700 w. flamingo     las vegas
    ## 111                                        129 e. fremont st.     las vegas
    ## 112                                    3000 las vegas blvd. s     las vegas
    ## 113                                        400 e. sahara ave.     las vegas
    ## 114                                    3555 las vegas blvd. s     las vegas
    ## 115                                    3570 las vegas blvd. s     las vegas
    ## 116                                    3400 las vegas blvd. s     las vegas
    ## 117                                    3120 las vegas blvd. s     las vegas
    ## 118                                         3900 paradise rd.     las vegas
    ## 119                                      1510 e. flamingo rd.     las vegas
    ## 120                                    3570 las vegas blvd. s     las vegas
    ## 121                                        129 e. fremont st.     las vegas
    ## 122                                           128 fremont st.     las vegas
    ## 123                                    3645 las vegas blvd. s     las vegas
    ## 124                                    3570 las vegas blvd. s     las vegas
    ## 125                                   260 e. paces ferry road       atlanta
    ## 126                                      889 w. peachtree st.       atlanta
    ## 127                                     1402 n. highland ave.       atlanta
    ## 128                                      870 n. highland ave.       atlanta
    ## 129        6359 jimmy carter blvd. . at buford hwy . norcross       atlanta
    ## 130                        247 buckhead ave. east village sq.       atlanta
    ## 131          192 peachtree center ave. at international blvd.       atlanta
    ## 132         2285 peachtree rd. . peachtree battle condominium       atlanta
    ## 133                                          3195 roswell rd.       atlanta
    ## 134           3109 piedmont rd. . just south of peachtree rd.       atlanta
    ## 135                                     1186 n. highland ave.       atlanta
    ## 136                                     4320 powers ferry rd.       atlanta
    ## 137                          3300 peachtree rd. . grand hyatt       atlanta
    ## 138                           3384 shallowford rd. . chamblee       atlanta
    ## 139                          555 peachtree st. at linden ave.       atlanta
    ## 140                                              25 irby ave.       atlanta
    ## 141                              923 peachtree st. at 8th st.       atlanta
    ## 142                         3500 peachtree rd. . phipps plaza       atlanta
    ## 143      6301 roswell rd. . sandy springs plaza sandy springs       atlanta
    ## 144                                         3043 buford hwy .       atlanta
    ## 145                                  2165 cheshire bridge rd.       atlanta
    ## 146                        7050 jimmy carter blvd. . norcross       atlanta
    ## 147                            783 martin luther king jr. dr.       atlanta
    ## 148                                             265 pharr rd.       atlanta
    ## 149                                            3209 maple dr.       atlanta
    ## 150                                    163 ponce de leon ave.       atlanta
    ## 151                          3300 peachtree rd. . grand hyatt       atlanta
    ## 152               1811 piedmont ave. near cheshire bridge rd.       atlanta
    ## 153                                        1931 peachtree rd.       atlanta
    ## 154                            6050 peachtree pkwy . norcross       atlanta
    ## 155                                        3380 peachtree rd.       atlanta
    ## 156                                        3393 peachtree rd.       atlanta
    ## 157                                  1879 cheshire bridge rd.       atlanta
    ## 158                                             500 pharr rd.       atlanta
    ## 159                                             50 hurt plaza       atlanta
    ## 160                                  2345 cheshire bridge rd.       atlanta
    ## 161                                        1144 crescent ave.       atlanta
    ## 162      40 buckhead crossing mall on the sidney marcus blvd.       atlanta
    ## 163            ` holiday inn/crowne plaza at ravinia dunwoody       atlanta
    ## 164                 c buford hwy . northwoods plaza doraville       atlanta
    ## 165                                        1829 peachtree rd.       atlanta
    ## 166                                           133 clement st. san francisco
    ## 167                                              1 market st. san francisco
    ## 168                                         3347 fillmore st. san francisco
    ## 169                                         519 columbus ave. san francisco
    ## 170                                           1772 market st. san francisco
    ## 171                                            1944 union st. san francisco
    ## 172                                             320 hayes st. san francisco
    ## 173                                          3226 mission st. san francisco
    ## 174                                             1220 9th ave. san francisco
    ## 175                                        240 california st. san francisco
    ## 176                                             1521 hyde st. san francisco
    ## 177                                             607 front st. san francisco
    ## 178                                              453 bush st. san francisco
    ## 179                                       1722 sacramento st. san francisco
    ## 180                                            432 powell st. san francisco
    ## 181                                         2317 chestnut st. san francisco
    ## 182                                           1600 powell st. san francisco
    ## 183                                         2125 fillmore st. san francisco
    ## 184                                             501 geary st. san francisco
    ## 185                                         1652 stockton st. san francisco
    ## 186                                          3011 steiner st. san francisco
    ## 187                                             1611 post st. san francisco
    ## 188                                         500 van ness ave. san francisco
    ## 189                                             1240 9th ave. san francisco
    ## 190                                                         4 san francisco
    ## 191                                             1702 post st. san francisco
    ## 192                                             298 gough st. san francisco
    ## 193                                            598 fourth st. san francisco
    ## 194                                         1915 fillmore st. san francisco
    ## 195                                         800 greenwich st. san francisco
    ## 196                                            2030 union st. san francisco
    ## 197                                              625 polk st. san francisco
    ## 198                                   435 s. la cienega blv .   los angeles
    ## 199                                       12224 ventura blvd.   los angeles
    ## 200                                      701 stone canyon rd.   los angeles
    ## 201                                       624 s. la brea ave.   los angeles
    ## 202                                             2709 main st.   los angeles
    ## 203                                         6703 melrose ave.   los angeles
    ## 204                                    8358 sunset blvd. west   los angeles
    ## 205                                       23725 w. malibu rd.   los angeles
    ## 206                                           9560 dayton way   los angeles
    ## 207                                   903 n. la cienega blvd.   los angeles
    ## 208                                         8284 melrose ave.   los angeles
    ## 209                                                   3rd st.   los angeles
    ## 210                                   129 n. la cienega blvd.   los angeles
    ## 211                                   9001 santa monica blvd.   los angeles
    ## 212                                         5955 melrose ave.   los angeles
    ## 213                                       1001 n. alameda st.   los angeles
    ## 214                                       12969 ventura blvd.   los angeles
    ## 215                                          617 s. olive st.   los angeles
    ## 216                                         9255 sunset blvd.   los angeles
    ## 217                                           3115 pico blvd.   los angeles
    ## 218                                        67 n. raymond ave.   los angeles
    ## 219                                            21 w. 52nd st.      new york
    ## 220                                            13 w. 54th st.      new york
    ## 221                                            34 e. 61st st.      new york
    ## 222                                           201 w. 83rd st.      new york
    ## 223                                             1 w. 67th st.      new york
    ## 224                 2450 broadway between 90th and 91st sts .      new york
    ## 225                  854 7th ave. between 54th and 55th sts .      new york
    ## 226                            2 harrison st. near hudson st.      new york
    ## 227                                            20 e. 76th st.      new york
    ## 228                                           210 e. 58th st.      new york
    ## 229                                           243 e. 58th st.      new york
    ## 230                                            99 e. 52nd st.      new york
    ## 231                                            12 e. 12th st.      new york
    ## 232           42 e. 20th st. between park ave. s and broadway      new york
    ## 233                                           402 w. 44th st.      new york
    ## 234                                           160 e. 64th st.      new york
    ## 235                                            33 w. 55th st.      new york
    ## 236                   60 w. 55th st. between 5th and 6th ave.      new york
    ## 237                                           155 w. 51st st.      new york
    ## 238                                        160 central park s      new york
    ## 239                                             2 e. 55th st.      new york
    ## 240                                           249 e. 50th st.      new york
    ## 241                                            57 w. 58th st.      new york
    ## 242                                           405 e. 58th st.      new york
    ## 243                  102 5th ave. between 15th and 16th sts .      new york
    ## 244                                57 jane st. off hudson st.      new york
    ## 245            239 w. broadway between walker and white sts .      new york
    ## 246                                            55 e. 54th st.      new york
    ## 247                                           100 e. 63rd st.      new york
    ## 248                                           182 w. 58th st.      new york
    ## 249                                            35 w. 64th st.      new york
    ## 250                                      95 ave. a at 6th st.      new york
    ## 251                                      30 rockefeller plaza      new york
    ## 252                             1 water st. at the east river      new york
    ## 253                                        240 central park s      new york
    ## 254                                  156 2nd ave. at 10th st.      new york
    ## 255                                            11 e. 53rd st.      new york
    ## 256                                            43 w. 65th st.      new york
    ## 257                                 1110 3rd ave. at 65th st.      new york
    ## 258                                           201 e. 49th st.      new york
    ## 259                             ` in central park at 67th st.      new york
    ## 260                  747 9th ave. between 50th and 51st sts .      new york
    ## 261                                            21 e. 16th st.      new york
    ## 262                                           152 w. 44th st.      new york
    ## 263                                    3200 las vegas blvd. s     las vegas
    ## 264                                    3799 las vegas blvd. s     las vegas
    ## 265                                      3000 w. paradise rd.     las vegas
    ## 266                                    3570 las vegas blvd. s     las vegas
    ## 267                                        200 e. fremont st.     las vegas
    ## 268                                    2880 las vegas blvd. s     las vegas
    ## 269                                      2245 e. flamingo rd.     las vegas
    ## 270     2355 peachtree rd. . peachtree battle shopping center       atlanta
    ## 271                    3125 piedmont rd. . near peachtree rd.       atlanta
    ## 272                                        3130 piedmont road       atlanta
    ## 273 3393 peachtree rd. . lenox square mall near neiman marcus       atlanta
    ## 274                                        3073 piedmont road       atlanta
    ## 275                                        1529 piedmont ave.       atlanta
    ## 276                                   1 margaret mitchell sq.       atlanta
    ## 277     2290 peachtree rd. . peachtree square shopping center       atlanta
    ## 278                                    490 e. paces ferry rd.       atlanta
    ## 279                       595 piedmont ave. rio shopping mall       atlanta
    ## 280                                     1397 n. highland ave.       atlanta
    ## 281          2637 peachtree rd. . peachtree house condominium       atlanta
    ## 282                                    224 ponce de leon ave.       atlanta
    ## 283                           255 courtland st. at harris st.       atlanta
    ## 284                                   1232 w. paces ferry rd.       atlanta
    ## 285                                        3434 peachtree rd.       atlanta
    ## 286                                        3434 peachtree rd.       atlanta
    ## 287                                         181 peachtree st.       atlanta
    ## 288                                           b peachtree rd.       atlanta
    ## 289                                               41 14th st.       atlanta
    ## 290                                           126 clement st. san francisco
    ## 291                                        252 california st. san francisco
    ## 292                                             1 mission st. san francisco
    ## 293                                             7 claude la . san francisco
    ## 294                                          340 stockton st. san francisco
    ## 295                                            804 northpoint san francisco
    ## 296                                            777 sutter st. san francisco
    ## 297                                               570 4th st. san francisco
    ## 298                                          22 hawthorne st. san francisco
    ## 299                                          5937 geary blvd. san francisco
    ## 300                                             2316 polk st. san francisco
    ## 301                                            816 folsom st. san francisco
    ## 302                                              648 bush st. san francisco
    ## 303                                             1737 post st. san francisco
    ## 304                                         3201 fillmore st. san francisco
    ## 305                                              545 post st. san francisco
    ## 306                                          600 stockton st. san francisco
    ## 307                                         532 columbus ave. san francisco
    ## 308                                        1100 peachtree st.       atlanta
    ## 309                                                      <NA>          <NA>
    ## 310                                                      <NA>          <NA>
    ## 311                                            23 e. 22nd st.      new york
    ## 312                                           251 e. 53rd st.      new york
    ## 313                                           145 w. 53rd st.      new york
    ## 314                                  2930 beverly glen circle   los angeles
    ## 315                                                   3rd st.   los angeles
    ## 316                                       9500 wilshire blvd.   los angeles
    ## 317                                          930 hilgard ave.   los angeles
    ## 318                                         134 n. la cienega   los angeles
    ## 319          4 fish 17300 pacific coast hwy . at sunset blvd.   los angeles
    ## 320                                        8600 beverly blvd.   los angeles
    ## 321                                                   6th st.   los angeles
    ## 322                                                   3rd st.   los angeles
    ## 323                                         430 n. camden dr.   los angeles
    ## 324                                      3835 cross creek rd.   los angeles
    ## 325                   20 mott st. between bowery and pell st.      new york
    ## 326                                  700 5th ave. at 55th st.      new york
    ## 327                  322 e. 14 st. between 1st and 2nd aves .      new york
    ## 328                                           311 w. 17th st.      new york
    ## 329                        1 united nations plaza at 44th st.      new york
    ## 330                                   2 park ave. at 32nd st.      new york
    ## 331                222 e. 58th st. between 2nd and 3rd aves .      new york
    ## 332                                       206 206 e. 60th st.      new york
    ## 333                       106 w. houston st. off thompson st.      new york
    ## 334                  200-250 vesey st. world financial center      new york
    ## 335                 1022 3rd ave. between 60th and 61st sts .      new york
    ## 336                  228 8th ave. between 21st and 22nd sts .      new york
    ## 337                                  13 1st ave. near 1st st.      new york
    ## 338                1393a 2nd ave. between 72nd and 73rd sts .      new york
    ## 339                              420 e. 59th st. off 1st ave.      new york
    ## 340                           225 varick st. at clarkston st.      new york
    ## 341                                           240 e. 58th st.      new york
    ## 342                 25 w. 40th st. between 5th and 6th aves .      new york
    ## 343                      103 waverly pl . near washington sq.      new york
    ## 344    111 e. 22nd st. between park ave. s and lexington ave.      new york
    ## 345                 1486 2nd ave. between 77th and 78th sts .      new york
    ## 346                                            69 w. 71st st.      new york
    ## 347                                           200 w. 70th st.      new york
    ## 348                                             2 e. 61st st.      new york
    ## 349                             246 w. 4th st. at charles st.      new york
    ## 350        81 macdougal st. between houston and bleeker sts .      new york
    ## 351       169 sullivan st. between houston and bleecker sts .      new york
    ## 352          119 macdougal st. between 3rd and bleecker sts .      new york
    ## 353                                385 broome st. at mulberry      new york
    ## 354                              32 jones st. at bleecker st.      new york
    ## 355                         451 washington st. near watts st.      new york
    ## 356                                  860 2nd ave. at 46th st.      new york
    ## 357          150 wooster st. between houston and prince sts .      new york
    ## 358              45 mercer st. between broome and grand sts .      new york
    ## 359    72 macdougal st. between w. houston and bleecker sts .      new york
    ## 360                                           160 e. 48th st.      new york
    ## 361                                            60 w. 53rd st.      new york
    ## 362                                           120 w. 51st st.      new york
    ## 363                                                53rd sts .      new york
    ## 364                 1013 3rd ave. between 60th and 61st sts .      new york
    ## 365                                  522 9th ave. at 39th st.      new york
    ## 366           164 mulberry st. between grand and broome sts .      new york
    ## 367                            341 w. broadway near grand st.      new york
    ## 368                                           181 w. 10th st.      new york
    ## 369                                  633 3rd ave. at 40th st.      new york
    ## 370          157 duane st. between w. broadway and hudson st.      new york
    ## 371          219 w. broadway between franklin and white sts .      new york
    ## 372                                1325 5th ave. at 111th st.      new york
    ## 373                                             6 e. 32nd st.      new york
    ## 374                 2150 broadway between 75th and 76th sts .      new york
    ## 375                                 1288 1st ave. at 69th st.      new york
    ## 376                           39 desbrosses st. near west st.      new york
    ## 377                              340 w. broadway at grand st.      new york
    ## 378                                            29 e. 65th st.      new york
    ## 379                  635 9th ave. between 44th and 45th sts .      new york
    ## 380             522 columbus ave. between 85th and 86th sts .      new york
    ## 381                     87 1st ave. between 5th and 6th sts .      new york
    ## 382               21 west 17th st. between 5th and 6th aves .      new york
    ## 383                                 54 pearl st. at broad st.      new york
    ## 384                                 2340 broadway at 85th st.      new york
    ## 385                              402 w. 43rd st. off 9th ave.      new york
    ## 386             467 columbus ave. between 82nd and 83rd sts .      new york
    ## 387                                           228 w. 52nd st.      new york
    ## 388                208 e. 58th st. between 2nd and 3rd aves .      new york
    ## 389                  33 93 2nd ave. between 5th and 6th sts .      new york
    ## 390        228 thompson st. between w. 3rd and bleecker sts .      new york
    ## 391                  151 w. 54th st. in the rihga royal hotel      new york
    ## 392           20 cornelia st. between bleecker and w. 4th st.      new york
    ## 393                                  4 world financial center      new york
    ## 394         122 e. 27th st. between lexington and park aves .      new york
    ## 395           125 mulberry st. between canal and hester sts .      new york
    ## 396                           492 broome st. near w. broadway      new york
    ## 397          430 lafayette st. between 4th st. and astor pl .      new york
    ## 398                  82 e. 3rd st. between 1st and 2nd aves .      new york
    ## 399                                            13 w. 46th st.      new york
    ## 400                                            68 w. 58th st.      new york
    ## 401                219 e. 44th st. between 2nd and 3rd aves .      new york
    ## 402                                           326 w. 46th st.      new york
    ## 403                 1191 1st ave. between 64th and 65th sts .      new york
    ## 404             310 w. 4th st. between w. 12th and bank sts .      new york
    ## 405                         432 lafayette st. near astor pl .      new york
    ## 406                           211 w. broadway at franklin st.      new york
    ## 407                                           106 e. 57th st.      new york
    ## 408                                           149 e. 57th st.      new york
    ## 409         50 macdougal st. between houston and prince sts .      new york
    ## 410                         25 cleveland pl . near spring st.      new york
    ## 411                                           168 w. 18th st.      new york
    ## 412                                           405 e. 52nd st.      new york
    ## 413             507 columbus ave. between 84th and 85th sts .      new york
    ## 414               411 park ave. s between 28th and 29th sts .      new york
    ## 415                                            51 w. 64th st.      new york
    ## 416                 30 west 22nd st. between 5th and 6th ave.      new york
    ## 417          59 grand st. between wooster st. and w. broadway      new york
    ## 418                 1161 1st ave. between 63rd and 64th sts .      new york
    ## 419                325 e. 14th st. between 1st and 2nd aves .      new york
    ## 420                342 e. 46th st. between 1st and 2nd aves .      new york
    ## 421       15 e. 12th st. between 5th ave. and university pl .      new york
    ## 422           160 mercer st. between houston and prince sts .      new york
    ## 423                                 1030 3rd ave. at 61st st.      new york
    ## 424                                            46 e. 29th st.      new york
    ## 425                                           120 w. 23rd st.      new york
    ## 426                                            60 e. 54th st.      new york
    ## 427                 1134 1st ave. between 62nd and 63rd sts .      new york
    ## 428                                  551 5th ave. at 45th st.      new york
    ## 429                                            32 w. 32nd st.      new york
    ## 430                               28 1/2 bowery at bayard st.      new york
    ## 431                                             2 w. 19th st.      new york
    ## 432                                           322 w. 46th st.      new york
    ## 433                                            68 e. 56th st.      new york
    ## 434          138 lafayette st. between canal and howard sts .      new york
    ## 435                               250 park ave. s at 20th st.      new york
    ## 436                 301 park ave. between 49th and 50th sts .      new york
    ## 437            109 spring st. between greene and mercer sts .      new york
    ## 438                              371 w. 46th st. off 9th ave.      new york
    ## 439                                            28 e. 63rd st.      new york
    ## 440                                           100 w. 82nd st.      new york
    ## 441               37a union sq. w between 16th and 17th sts .      new york
    ## 442                126 e. 7th st. between 1st ave. and ave. a      new york
    ## 443                                           148 w. 51st st.      new york
    ## 444           133 mulberry st. between hester and grand sts .      new york
    ## 445                                            55 irving pl .      new york
    ## 446             1000 madison ave. between 77th and 78th sts .      new york
    ## 447                                            19 w. 49th st.      new york
    ## 448                                         3 225 e. 60th st.      new york
    ## 449                                 643 park ave. at 66th st.      new york
    ## 450                                            57 w. 48th st.      new york
    ## 451                            62 spring st. at lafayette st.      new york
    ## 452                  834 7th ave. between 53rd and 54th sts .      new york
    ## 453            428 amsterdam ave. between 80th and 81st sts .      new york
    ## 454                                  76 mott st. at canal st.      new york
    ## 455                              143 mercer st. at prince st.      new york
    ## 456                                            65 w. 55th st.      new york
    ## 457                                  950 8th ave. at 56th st.      new york
    ## 458 400 w. 119th st. between amsterdam and morningside aves .      new york
    ## 459                                 2420 broadway at 89th st.      new york
    ## 460                  900 7th ave. between 56th and 57th sts .      new york
    ## 461                                           345 e. 83rd st.      new york
    ## 462                       375 greenwich st. near franklin st.      new york
    ## 463          154 e. 79th st. between lexington and 3rd aves .      new york
    ## 464                  386 3rd ave. between 27th and 28th sts .      new york
    ## 465                                           222 w. 79th st.      new york
    ## 466                             342 e. 11th st. near 1st ave.      new york
    ## 467                                54 irving pl . at 17th st.      new york
    ## 468                                        52 236 w. 52nd st.      new york
    ## 469                                            70 w. 68th st.      new york
    ## 470                                           200 e. 54th st.      new york
    ## 471                                           500 e. 30th st.      new york
    ## 472                  174 1st ave. between 10th and 11th sts .      new york
    ## 473                                34 union sq. e at 16th st.      new york
    ## 474             90 prince st. between broadway and mercer st.      new york
    ## 475        ` underground underground mall underground atlanta       atlanta
    ## 476          ` park place across from perimeter mall dunwoody       atlanta
    ## 477                                         4850 flamingo rd.     las vegas
    ## 478                           4041 audrie st. at flamingo rd.     las vegas
    ## 479                                        4111 boulder hwy .     las vegas
    ## 480                                      1350 e. flamingo rd.     las vegas
    ## 481                                   3333 s. maryland pkwy .     las vegas
    ## 482                                                1 main st.     las vegas
    ## 483                                    2880 las vegas blvd. s     las vegas
    ## 484                                       2411 w. sahara ave.     las vegas
    ## 485                                     5111 w. boulder hwy .     las vegas
    ## 486                                       3713 w. sahara ave.     las vegas
    ## 487                                      6182 w. flamingo rd.     las vegas
    ## 488                                           2223 market st. san francisco
    ## 489                                    243 o \\ ` farrell st. san francisco
    ## 490                                           155 steuart st. san francisco
    ## 491                                               56 gold st. san francisco
    ## 492                                      4th and howard sts . san francisco
    ## 493                                           500 jackson st. san francisco
    ## 494                                              732 broadway san francisco
    ## 495                                            161 sutter st. san francisco
    ## 496                                          1300 battery st. san francisco
    ## 497                         ` market and new montgomery sts . san francisco
    ## 498                                         ` ghirardelli sq. san francisco
    ## 499                                      ` bldg. a fort mason san francisco
    ## 500                                      4 embarcadero center san francisco
    ## 501                                        2100 van ness ave. san francisco
    ## 502                                           161 steuart st. san francisco
    ## 503                                              430 broadway san francisco
    ## 504                                          5322 geary blvd. san francisco
    ## 505                                          2332 clement st. san francisco
    ## 506                                          1265 battery st. san francisco
    ## 507                                          3345 steiner st. san francisco
    ## 508                                        615 sacramento st. san francisco
    ## 509                                          5116 geary blvd. san francisco
    ## 510                                              600 5th ave. san francisco
    ## 511                                            221 powell st. san francisco
    ## 512                        2 new montgomery st. at market st. san francisco
    ## 513                                             470 green st. san francisco
    ## 514                                         ` ghirardelli sq. san francisco
    ## 515                                        246 mcallister st. san francisco
    ## 516                                          3131 webster st. san francisco
    ## 517                                              500 post st. san francisco
    ## 518                                        640 sacramento st. san francisco
    ## 519                                          1015 battery st. san francisco
    ## 520                                          631 b kearny st. san francisco
    ## 521                                        558 sacramento st. san francisco
    ## 522                                            1 tillman pl . san francisco
    ## 523                                            108 south park san francisco
    ## 524                                         150 redwood alley san francisco
    ## 525                                          3300 geary blvd. san francisco
    ## 526                                             601 hayes st. san francisco
    ## 527                                       3235 sacramento st. san francisco
    ## 528                                          3148 geary blvd. san francisco
    ## 529                                        600 montgomery st. san francisco
    ## 530                                      670 golden gate ave. san francisco
    ## 531                                         2257 chestnut st. san francisco
    ## 532                                             101 spear st. san francisco
    ## 533                                           427 battery st. san francisco
    ## 534                                             2000 hyde st. san francisco
    ## 535                                           1658 market st. san francisco
    ##               phone.y                         type.y class.y
    ## 1        310-275-1101                    californian     121
    ## 2        310-277-2333                        italian     138
    ## 3        310-777-5877                       american     113
    ## 4        310-724-5959                    californian     132
    ## 5        310-393-6545                        italian     159
    ## 6        213-665-1891                          asian      10
    ## 7        310-550-3900                    californian     115
    ## 8        310-456-5733                         french     114
    ## 9        213-680-9355                          asian     160
    ## 10       213-467-1052                          asian     125
    ## 11       213-239-6500                    californian     120
    ## 12       310-392-3901                       american     135
    ## 13       310-823-5396                       american     171
    ## 14       310-276-6345                          asian     169
    ## 15       310-659-6919                         french     145
    ## 16       310-652-4025                    californian      20
    ## 17       818-356-0959                    health food     126
    ## 18       213-664-7723                      caribbean     124
    ## 19       213-660-4446                       american     170
    ## 20       213-653-5858                       american     164
    ## 21           310-788-                  dive american     128
    ## 22       310-552-2394                    continental     142
    ## 23       310-479-4187                          cajun     152
    ## 24       310-837-6662                    health food     140
    ## 25       213-658-6340                        italian     165
    ## 26       213-667-0777                        italian     146
    ## 27       213-628-6717                          asian     147
    ## 28       310-273-5578                    californian     141
    ## 29       818-985-4669                        italian     119
    ## 30       818-761-9126                       american     154
    ## 31       213-734-2773                        mexican     134
    ## 32     213-651-2030 .                       american     122
    ## 33       213-466-9329                       american     161
    ## 34       213-461-8800                    californian     155
    ## 35       213-937-5733                          asian     167
    ## 36       818-788-3536                         french       3
    ## 37       310-394-5669                       american     151
    ## 38       213-485-0927                       american     150
    ## 39       310-451-0616                       american     117
    ## 40       310-277-7346                        italian     157
    ## 41       310-550-8330                        italian     139
    ## 42       310-828-1585                        italian     129
    ## 43       310-358-8585                         french     130
    ## 44       310-274-7427                       american     158
    ## 45       213-938-2863                        italian     118
    ## 46       310-274-0101                       american     149
    ## 47       310-399-4800                    continental     162
    ## 48       818-784-4400                        italian     156
    ## 49       310-276-5205                       american     148
    ## 50       310-289-0660                          asian     144
    ## 51       310-451-1655                        mexican     116
    ## 52       212-956-3976                        italian     299
    ## 53       212-582-5252                       american     276
    ## 54       212-581-8888                       american     187
    ## 55       212-818-1222                         french     204
    ## 56       212-315-0980                        italian     283
    ## 57       212-484-5120                         french     200
    ## 58       212-753-9015                        italian     342
    ## 59       212-869-0900                       american     290
    ## 60       212-265-3566                     coffee bar     360
    ## 61       212-489-6565                       american     261
    ## 62       212-247-2993                         french     281
    ## 63       212-233-0507                       american     315
    ## 64       212-595-5330                        italian     241
    ## 65       212-691-6359                        italian     247
    ## 66       212-691-8136                        italian     212
    ## 67       212-989-1220                       american     173
    ## 68       212-873-3700                       american     348
    ## 69       212-794-4950                         french     277
    ## 70       212-758-5757                       american     239
    ## 71       212-787-0202                        seafood     297
    ## 72       212-473-8388                       american     176
    ## 73       212-355-2020                     coffee bar     245
    ## 74       212-645-4431                     coffee bar     207
    ## 75       212-254-8776                     coffee bar     226
    ## 76       212-873-5025                       american     298
    ## 77       212-734-4893               eastern european     333
    ## 78       212-288-2391                        italian     322
    ## 79       212-595-8555                       american     330
    ## 80       212-581-8030                       american     311
    ## 81       212-734-2676                        italian     350
    ## 82       212-869-5544                          asian     273
    ## 83       212-944-3643                        italian     317
    ## 84       212-644-9258                 middle eastern     321
    ## 85  ' 212- 759-0590 '                        mexican     177
    ## 86       212-608-7300                        seafood     255
    ## 87  ' 212- 941-0911 '                          asian     258
    ## 88       212-874-3474                        seafood     244
    ## 89       212-673-0330                  east european     341
    ## 90       212-496-6280                       american     344
    ## 91       212-246-9171                        italian     186
    ## 92       212-753-7407                        mexican     337
    ## 93       212-688-5447                          asian     366
    ## 94       212-753-1870                       american     189
    ## 95       212-533-2233                         french     249
    ## 96       212-249-3600                       american     262
    ## 97       212-228-0604                  east european     358
    ## 98       212-245-4850                        italian     320
    ## 99       212-674-7014                        italian     282
    ## 100      212-535-1100                 middle eastern     327
    ## 101      212-246-6363                       american     375
    ## 102      212-333-7827                       american     328
    ## 103      212-682-8660                       american     325
    ## 104      212-794-0205                        italian     221
    ## 105      212-961-0574                        mexican     253
    ## 106      702-384-4470                   steak houses     441
    ## 107      702-791-7111                    continental     429
    ## 108      702-252-7777                        buffets     435
    ## 109      702-731-7888                          asian     438
    ## 110      702-252-7737                        italian     423
    ## 111      702-385-7111                          asian     442
    ## 112      702-732-6330            coffee shops/diners     448
    ## 113      702-733-2066                    continental     447
    ## 114      702-733-3111            coffee shops/diners     431
    ## 115      702-731-7525              only in las vegas     449
    ## 116      702-791-7111                          asian     446
    ## 117      702-794-8200                        mexican     444
    ## 118      702-794-0700                   steak houses     452
    ## 119      702-737-1234                          asian     443
    ## 120      702-731-7547            coffee shops/diners     433
    ## 121      702-385-7111                        buffets     440
    ## 122      702-382-1600            coffee shops/diners     428
    ## 123      702-739-4111                        buffets     424
    ## 124      702-735-4663                        italian     427
    ## 125      404-264-1334                    continental     384
    ## 126      404-874-5535                       southern     421
    ## 127      404-874-2626                      caribbean     406
    ## 128      404-875-7775                       american     413
    ## 129      770-242-3984                       barbecue     397
    ## 130      404-842-0334                         fusion     409
    ## 131      404-659-2788                          asian     399
    ## 132      404-351-0870                  mediterranean     400
    ## 133      404-264-9546                          asian     381
    ## 134      404-262-7379                       american     382
    ## 135      404-872-7203                        italian     388
    ## 136      404-255-7277                       southern     398
    ## 137      404-365-8100                  mediterranean     389
    ## 138      770-458-7145                       southern     419
    ## 139      404-874-3223                       american     411
    ## 140      404-233-7644                       barbecue     414
    ## 141      404-875-2489                       southern     396
    ## 142      404-233-2333                        italian     385
    ## 143      404-255-5160                       barbecue     404
    ## 144      404-325-5285                          asian     415
    ## 145      404-321-1118                       american     420
    ## 146    770-441-- 0291                       american     387
    ## 147      404-525-2062                       barbecue     380
    ## 148      404-262-3165                       american     383
    ## 149      404-237-1313                       southern     407
    ## 150      404-876-8532                  international     379
    ## 151      404-841-0314                          asian     401
    ## 152      404-607-1622                       barbecue     395
    ## 153      404-355-5993                    continental     405
    ## 154      770-449-0033                          asian     416
    ## 155      404-266-1600                    continental     393
    ## 156      404-264-1163                    continental     422
    ## 157      404-874-5642                       southern     392
    ## 158      404-237-2941                        italian     412
    ## 159      404-524-2489                  international     390
    ## 160      404-325-6963                         french     418
    ## 161      404-873-7358                       southern     417
    ## 162      404-364-0212                      caribbean     391
    ## 163      770-395-9925                        italian     402
    ## 164      770-451-0192                          asian     403
    ## 165      404-350-6755                    continental     410
    ## 166      415-668-4848                          asian     492
    ## 167      415-777-5577                       american     500
    ## 168      415-474-4180                        italian     461
    ## 169      415-982-1124                        italian     490
    ## 170      415-863-3516                       american     465
    ## 171      415-922-9022                       american     506
    ## 172      415-863-5545                        seafood     478
    ## 173      415-861-6224                          asian     494
    ## 174      415-566-6966       greek and middle eastern     529
    ## 175      415-391-2373                        seafood     519
    ## 176      415-441-7778                        italian     482
    ## 177      415-398-5700                       american     493
    ## 178      415-391-2233                         french     491
    ## 179      415-567-5432                        italian     454
    ## 180      415-395-8555                        italian     511
    ## 181      415-776-1506 mexican/latin american/spanish     462
    ## 182      415-989-2589                        italian     464
    ## 183      415-346-4430                        italian     524
    ## 184      415-292-0101                       american     473
    ## 185      415-989-7800                  mediterranean     498
    ## 186      415-346-2111                        italian     504
    ## 187      415-922-7788                         french     530
    ## 188      415-861-4344                       american     515
    ## 189      415-664-3664       greek and middle eastern     516
    ## 190      415-986-3222                  mediterranean     513
    ## 191      415-346-3486                          asian     510
    ## 192      415-863-9335                          asian     521
    ## 193      415-543-2222                         french     459
    ## 194      415-346-1333                        italian     501
    ## 195      415-776-7766                        italian     460
    ## 196      415-929-8855                          asian     456
    ## 197      415-771-3500                         french     463
    ## 198      310-246-1501                       american       0
    ## 199      818-762-1221                       american       1
    ## 200      310-472-1211                    californian       2
    ## 201      213-938-1447                       american       4
    ## 202      310-392-9025                         french       5
    ## 203      213-857-0034                    californian       6
    ## 204      213-848-6677                       american       7
    ## 205      310-456-0488                    californian       8
    ## 206      310-276-0615                       american       9
    ## 207      310-652-9770                         french      11
    ## 208      213-655-8880                         french      12
    ## 209      310-274-1893                        italian      13
    ## 210      310-659-9639                          asian      14
    ## 211      310-550-8811                       american      15
    ## 212      213-467-1108                    californian      16
    ## 213      213-628-3781                       american      17
    ## 214      818-990-0500                         french      18
    ## 215      213-627-2300                        italian      19
    ## 216      310-276-1886                          asian     143
    ## 217      310-829-4313                        italian      21
    ## 218      818-585-0855                          asian      22
    ## 219      212-582-7200                       american      23
    ## 220      212-307-7311                    continental      24
    ## 221 ' 212- 319-1660 '                       american      25
    ## 222      212-496-6031                     coffee bar      26
    ## 223      212-877-3500                    continental      27
    ## 224      212-362-2200                        italian      28
    ## 225      212-757-2245                   delicatessen      29
    ## 226      212-966-6960                       american      30
    ## 227      212-288-0033                         french      31
    ## 228      212-355-7555                          asian      32
    ## 229      212-758-1479                        italian      33
    ## 230      212-754-9494                       american      34
    ## 231      212-620-4020                       american      35
    ## 232      212-477-0777                       american      36
    ## 233      212-765-1737                  tel caribbean      37
    ## 234      212-223-5656                       american      38
    ## 235      212-586-4252                         french      39
    ## 236      212-688-6525                         french      40
    ## 237      212-489-1515                         french      41
    ## 238      212-484-5113                         french      42
    ## 239      212-339-6719                       american      43
    ## 240      212-752-2225                         french      44
    ## 241 ' 212- 371-7777 '                        seafood      45
    ## 242      212-754-6272                       american      46
    ## 243      212-807-7400                       american      47
    ## 244      212-627-8273                        mexican      48
    ## 245 ' 212- 219-2777 '                         french      49
    ## 246      212-759-5941                        seafood      50
    ## 247      212-644-1900                       american      51
    ## 248      212-245-2214                         french      52
    ## 249      212-724-8585                  mediterranean      53
    ## 250      212-260-6660                        seafood      54
    ## 251      212-632-5000                       american      55
    ## 252      718-522-5200                       american      56
    ## 253      212-265-5959                        italian      57
    ## 254      212-677-0606                   delicatessen      58
    ## 255      212-980-9393                          asian      59
    ## 256      212-371-8844                          asian      60
    ## 257      212-861-8080                       american      61
    ## 258      212-753-1530                       american      62
    ## 259      212-873-3200                       american      63
    ## 260      212-315-1726                  mediterranean      64
    ## 261      212-243-4020                       american      65
    ## 262 ' 212- 921-9494 '                       american      66
    ## 263      702-733-8899                          asian      67
    ## 264      702-891-7349                   southwestern      68
    ## 265      702-732-5111                    continental      69
    ## 266      702-731-7547                    continental      70
    ## 267      702-385-3232                        seafood      71
    ## 268      702-734-0410                   steak houses      72
    ## 269      702-731-4036                        seafood      73
    ## 270      404-261-8186                        italian      74
    ## 271      404-365-0410                  international      75
    ## 272      404-237-2663                       american      76
    ## 273      404-266-1440                         french      77
    ## 274      404-262-3336                       american      78
    ## 275      404-874-7600                         french      79
    ## 276      404-681-2909                       american      80
    ## 277      404-352-3517                       american      81
    ## 278      404-233-7673                  international      82
    ## 279      404-876-4408                          asian      83
    ## 280      404-876-0676                      caribbean      84
    ## 281      404-231-1368                        italian      85
    ## 282      404-876-1800                       southern      86
    ## 283      404-221-6362                    continental      87
    ## 284      404-261-3662                  international      88
    ## 285      404-237-2700                  international      89
    ## 286      404-237-2700                  international      90
    ## 287      404-659-0400                    continental      91
    ## 288      404-351-9533                         french      92
    ## 289      404-875-8424                        italian      93
    ## 290      415-387-0408                         french      94
    ## 291      415-956-9662                        seafood      95
    ## 292      415-543-6084                       american      96
    ## 293      415-392-3505                         french      97
    ## 294      415-955-5555                       american      98
    ## 295      415-775-7036                         french      99
    ## 296      415-673-7779                         french     100
    ## 297      415-543-0573                         french     101
    ## 298      415-777-9779                       american     102
    ## 299      415-668-6654                          asian     103
    ## 300      415-776-5577                         french     104
    ## 301      415-495-5775                  mediterranean     105
    ## 302      415-989-7154                         french     106
    ## 303      415-922-0337                          asian     107
    ## 304      415-563-4755                  mediterranean     108
    ## 305      415-776-7825                       american     109
    ## 306      415-296-7465                       american     110
    ## 307      415-399-0499                        italian     111
    ## 308      404-724-0901                  mediterranean     386
    ## 309              <NA>                           <NA>      NA
    ## 310              <NA>                           <NA>      NA
    ## 311      212-228-2200                  mediterranean     191
    ## 312      212-753-8450                        italian     267
    ## 313      212-581-4242                        italian     334
    ## 314      310-475-9807                        italian     112
    ## 315      213-658-8898                  mediterranean     123
    ## 316      310-275-5200                    californian     127
    ## 317      310-208-8765                    continental     131
    ## 318      310-659-1952                       american     133
    ## 319      310-454-3474                       american     136
    ## 320      310-276-7605                       american     137
    ## 321      213-483-6000                       american     153
    ## 322      213-651-0346                  mediterranean     163
    ## 323      310-859-0926                          asian     166
    ## 324      310-456-0169                        italian     168
    ## 325      212-964-0380                          asian     172
    ## 326      212-903-3918                         french     174
    ## 327      212-473-2602                  mediterranean     175
    ## 328      212-627-8899                       american     178
    ## 329      212-702-5014                       american     179
    ## 330      212-684-2122                       american     180
    ## 331      212-308-0112                        italian     181
    ## 332      212-838-0440                       american     182
    ## 333      212-677-3820                        italian     183
    ## 334      212-385-0313                          asian     184
    ## 335      212-355-1112                  mediterranean     185
    ## 336      212-206-0059                     coffee bar     188
    ## 337      212-473-0108                 latin american     190
    ## 338      212-249-8484                          asian     192
    ## 339      212-758-0323                         french     193
    ## 340      212-727-2775                       american     194
    ## 341      212-688-4190                        italian     195
    ## 342      212-840-6500                       american     196
    ## 343      212-254-1200                       american     197
    ## 344      212-995-8500                         french     198
    ## 345      212-988-2655                     coffee bar     199
    ## 346      212-724-5846                     coffee bar     201
    ## 347      212-873-7411                         french     202
    ## 348      212-940-8185                         french     203
    ## 349      212-924-7653                  mediterranean     205
    ## 350      212-982-5275                     coffee bar     206
    ## 351      212-473-2642                         french     208
    ## 352      212-475-9557                     coffee bar     209
    ## 353      212-226-8413                     coffee bar     210
    ## 354      212-691-7538                     coffee bar     211
    ## 355      212-966-4900                         french     213
    ## 356      212-697-9538                        seafood     214
    ## 357      212-505-0005                 middle eastern     215
    ## 358      212-343-9012                          asian     216
    ## 359      212-505-0727                         french     217
    ## 360      212-371-2323                          asian     218
    ## 361      212-333-7788                       american     219
    ## 362      212-956-7100                         french     220
    ## 363      212-421-0334                     coffee bar     222
    ## 364      212-753-5100                     coffee bar     223
    ## 365      212-465-1530                     coffee bar     224
    ## 366      212-343-1212                        italian     225
    ## 367      212-941-9024                        italian     227
    ## 368      212-645-8023                         french     228
    ## 369 ' 212- 986-8080 '                        seafood     229
    ## 370      212-732-5555                       american     230
    ## 371      212-941-7070                        mexican     231
    ## 372      212-996-1212                       american     232
    ## 373      212-725-1333                          asian     233
    ## 374      212-496-1588                       american     234
    ## 375      212-744-3266                          asian     235
    ## 376      212-226-4621                        italian     236
    ## 377      212-431-0021                         french     237
    ## 378      212-772-9000                         french     238
    ## 379 ' 212- 262-2525 '                       american     240
    ## 380      212-595-3139                       american     242
    ## 381      212-674-3823                       american     243
    ## 382      212-691-8888                       american     246
    ## 383      212-269-0144                       american     248
    ## 384      212-799-1533                     coffee bar     250
    ## 385      212-564-7272                        italian     251
    ## 386      212-769-1144                          asian     252
    ## 387      212-245-5336                       american     254
    ## 388      212-752-3054                        italian     256
    ## 389      212-477-8427                       american     257
    ## 390      212-777-5922                        italian     259
    ## 391      212-468-8888                       american     260
    ## 392      212-243-9579                       american     263
    ## 393      212-786-1500                       american     264
    ## 394      212-481-7372                        italian     265
    ## 395      212-226-6060                        italian     266
    ## 396      212-966-3371                 latin american     268
    ## 397      212-505-5111                          asian     269
    ## 398 ' 212- 614-0747 '                     coffee bar     270
    ## 399      212-730-5848                 latin american     271
    ## 400      212-751-2323                         french     272
    ## 401      212-682-5678                          asian     274
    ## 402      212-581-6464                       american     275
    ## 403      212-288-8791                 middle eastern     278
    ## 404      212-242-4705                         french     279
    ## 405      212-388-0978                         french     280
    ## 406      212-431-0700                 middle eastern     284
    ## 407      212-751-2931                         french     285
    ## 408 ' 212- 752-0808 '                          asian     286
    ## 409      212-254-4678                     coffee bar     287
    ## 410      212-343-9599                         french     288
    ## 411      212-727-8022                        italian     289
    ## 412      212-755-6244                         french     291
    ## 413      212-875-1993                       american     292
    ## 414      212-679-4111                         french     293
    ## 415      212-721-8271                       american     294
    ## 416      212-675-6700                       american     295
    ## 417      212-941-0479                       american     296
    ## 418      212-888-6556                       american     300
    ## 419      212-777-6314                          asian     301
    ## 420      212-370-1866                         french     302
    ## 421      212-229-9313                     coffee bar     303
    ## 422      212-906-9173                       american     304
    ## 423      212-838-4343                       american     305
    ## 424      212-679-5535                          asian     306
    ## 425      212-807-1801                     coffee bar     307
    ## 426      212-838-2600                       american     308
    ## 427      212-421-4433                          asian     309
    ## 428      212-972-3315                       american     310
    ## 429 ' 212- 947-8482 '                          asian     312
    ## 430      212-349-0923                          asian     313
    ## 431      212-255-3996                     coffee bar     314
    ## 432      212-489-7212                        italian     316
    ## 433      212-223-7575                          asian     318
    ## 434      212-941-4168                          asian     319
    ## 435      212-777-6211                 latin american     323
    ## 436      212-872-4895                         french     324
    ## 437      212-274-8883                          asian     326
    ## 438 ' 212- 956-3055 '                 latin american     329
    ## 439      212-935-2888                       american     331
    ## 440      212-501-0776                          asian     332
    ## 441      212-627-7172                          asian     335
    ## 442      212-674-4140                    continental     336
    ## 443      212-245-9600                       american     338
    ## 444      212-925-3120                        italian     339
    ## 445      212-982-9030                        italian     340
    ## 446      212-570-2211                     coffee bar     343
    ## 447      212-332-7610                        seafood     345
    ## 448      212-838-3531                       american     346
    ## 449      212-744-4107                       american     347
    ## 450 ' 212- 977-8400 '                          asian     349
    ## 451      212-966-0290                       american     351
    ## 452      212-245-7850                   delicatessen     352
    ## 453      212-501-7515                        seafood     353
    ## 454      212-334-8088                          asian     354
    ## 455      212-925-3700                     coffee bar     355
    ## 456      212-956-6888                          asian     356
    ## 457 ' 212- 397-3737 '                       american     357
    ## 458      212-666-9490                    continental     359
    ## 459      212-496-1066                       american     361
    ## 460      212-245-9800                        italian     362
    ## 461      212-472-4488                        italian     363
    ## 462      212-941-3900                       american     364
    ## 463      212-988-4858                     coffee bar     365
    ## 464      212-679-1810                 middle eastern     367
    ## 465      212-799-0400                       american     368
    ## 466      212-674-7264                     coffee bar     369
    ## 467      212-260-5454                       american     370
    ## 468      212-586-7714                 latin american     371
    ## 469      212-721-0068                       american     372
    ## 470      212-486-9592                       american     373
    ## 471      212-683-3333                       american     374
    ## 472      212-614-0620                  mediterranean     376
    ## 473      212-614-9291                          asian     377
    ## 474      212-966-6722                       american     378
    ## 475      404-577-1800                    continental     394
    ## 476      770-393-1333                        italian     408
    ## 477      702-871-3262                          asian     425
    ## 478      702-732-1424                        italian     426
    ## 479      702-432-7777                       american     430
    ## 480      702-735-8686                       american     432
    ## 481      702-731-5311                        italian     434
    ## 482      702-386-2512                       american     436
    ## 483      702-734-0410                        buffets     437
    ## 484      702-367-2411                        buffets     439
    ## 485      702-454-8073            coffee shops/diners     445
    ## 486      702-876-4190                        italian     450
    ## 487      702-871-8826                        mexican     451
    ## 488      415-431-0692                       american     453
    ## 489      415-982-0243              old san francisco     455
    ## 490      415-495-6500                         french     457
    ## 491      415-433-6300                       american     458
    ## 492      415-543-8060 mexican/latin american/spanish     466
    ## 493      415-296-8555                       american     467
    ## 494      415-788-9900                         french     468
    ## 495      415-362-0404       greek and middle eastern     469
    ## 496      415-982-2000                       american     470
    ## 497      415-546-5011              old san francisco     471
    ## 498      415-771-8822                          asian     472
    ## 499      415-771-6222                     vegetarian     474
    ## 500      415-781-8833                          asian     475
    ## 501      415-673-1888                   steak houses     476
    ## 502      415-882-1333                       american     477
    ## 503      415-362-0641       greek and middle eastern     479
    ## 504      415-668-8998                          asian     480
    ## 505      415-752-8833                          asian     481
    ## 506      415-986-0100                        italian     483
    ## 507      415-563-0487                   steak houses     484
    ## 508      415-986-9854              old san francisco     485
    ## 509      415-752-5652                          asian     486
    ## 510      415-668-9292                              '     487
    ## 511      415-397-7720                        italian     488
    ## 512      415-546-5000                          asian     489
    ## 513      415-362-8286       greek and middle eastern     495
    ## 514      415-929-1730                        seafood     496
    ## 515      415-487-9800                     vegetarian     497
    ## 516      415-931-1556                          asian     499
    ## 517      415-929-2087                         french     502
    ## 518      415-395-9800                        italian     503
    ## 519      415-391-2555                         french     505
    ## 520      415-982-7877                          asian     507
    ## 521      415-434-4100                       american     508
    ## 522      415-421-2300                       american     509
    ## 523      415-495-7275                         french     512
    ## 524      415-861-7827                       american     514
    ## 525      415-668-1783                          asian     517
    ## 526      415-252-9289                 russian/german     518
    ## 527      415-474-8890                         french     520
    ## 528      415-752-4440                          asian     522
    ## 529      415-433-7250                  mediterranean     523
    ## 530      415-673-9245                        italian     525
    ## 531      415-563-9727                       american     526
    ## 532      415-957-9300                          asian     527
    ## 533      415-541-4949                          asian     528
    ## 534      415-346-0800 mexican/latin american/spanish     531
    ## 535      415-552-2522                  mediterranean     532

### `The End`
