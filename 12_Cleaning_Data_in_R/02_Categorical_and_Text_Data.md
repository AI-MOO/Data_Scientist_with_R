Categorical and Text Data
================
Mohamad Osman
2022-06-26

# Section 02: **Categorical and Text Data**

### **`01-Not a member (membership constraints)`**

-   Count the number of occurrences of each `dest_size` in `sfo_survey`.

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
    ## ✔ tibble  3.1.7     ✔ dplyr   1.0.9
    ## ✔ tidyr   1.2.0     ✔ stringr 1.4.0
    ## ✔ readr   2.1.2     ✔ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
```

``` r
# file path
sfo_survey_file <- file.path("..", "00_Datasets", "sfo_survey_ch2_1.rds") 

# reading rds file
sfo_survey <- readRDS(sfo_survey_file)

# Count the number of occurrences of dest_size
sfo_survey %>%
  count(dest_size)
```

    ##   dest_size    n
    ## 1   Small      1
    ## 2       Hub    1
    ## 3       Hub 1756
    ## 4     Large  143
    ## 5   Large      1
    ## 6    Medium  682
    ## 7     Small  225

-   Use the correct type of filtering join on the `sfo_survey` data
    frame and the `dest_sizes` data frame to get the rows of
    `sfo_survey` with invalid `dest_size` values.

-   Get the `id`, `airline`, `destination`, and `dest_size` columns.

``` r
# Create a truth table for validation 
dest_size <- c("Small", "Medium", "Large", "Hub")
passengers_per_day <- c("0-20K", "20K-70K", "70K-100K", "100K+")

dest_sizes <- data.frame(dest_size, passengers_per_day, stringsAsFactors = FALSE)
```

``` r
# Find bad dest_size rows
sfo_survey %>% 
  # Join with dest_sizes data frame to get bad dest_size rows
  anti_join(dest_sizes) %>%
  # Select id, airline, destination, and dest_size cols
  select(id, airline, destination, dest_size)
```

    ## Joining, by = "dest_size"

    ##     id     airline       destination dest_size
    ## 1  982   LUFTHANSA            MUNICH       Hub
    ## 2 2063    AMERICAN      PHILADELPHIA   Large  
    ## 3  777 UNITED INTL SAN JOSE DEL CABO   Small

-   Use the correct filtering join on `sfo_survey` and `dest_sizes` to
    get the rows of `sfo_survey` that have a valid `dest_size`.

-   Count the number of times that each `dest_size` occurs to make sure
    there are no invalid values left behind

``` r
# Remove bad dest_size rows
sfo_survey %>% 
  # Join with dest_sizes
  semi_join(dest_sizes) %>%
  # Count the number of each dest_size
  count(dest_size)
```

    ## Joining, by = "dest_size"

    ##   dest_size    n
    ## 1       Hub 1756
    ## 2     Large  143
    ## 3    Medium  682
    ## 4     Small  225

### **`02-Identifying inconsistency`**

In this exercise, you’ll continue working with the `sfo_survey` dataset.
You’ll examine the `dest_size` column again as well as the `cleanliness`
column and determine what kind of issues, if any, these two categorical
variables face.

`dplyr` and is loaded and `sfo_survey` is available.

-   Count the number of occurrences of each category of the `dest_size`
    variable of `sfo_survey`.

``` r
# Count dest_size
sfo_survey %>%
  count(dest_size)
```

    ##   dest_size    n
    ## 1   Small      1
    ## 2       Hub    1
    ## 3       Hub 1756
    ## 4     Large  143
    ## 5   Large      1
    ## 6    Medium  682
    ## 7     Small  225

The categories in `dest_size` have inconsistent white space.

-   Count the number of occurrences of each category of the
    `cleanliness` variable of `sfo_survey`.

``` r
# Count dest_size
sfo_survey %>%
  count(dest_size)
```

    ##   dest_size    n
    ## 1   Small      1
    ## 2       Hub    1
    ## 3       Hub 1756
    ## 4     Large  143
    ## 5   Large      1
    ## 6    Medium  682
    ## 7     Small  225

``` r
# Count cleanliness
sfo_survey %>%
  count(cleanliness)
```

    ##      cleanliness    n
    ## 1        Average  433
    ## 2          Clean  970
    ## 3          Dirty    2
    ## 4 Somewhat clean 1254
    ## 5 Somewhat dirty   30
    ## 6           <NA>  120

### **`03-Correcting inconsistency`**

-   Add a column to `sfo_survey` called `dest_size_trimmed` that
    contains the values in the `dest_size` column with all leading and
    trailing whitespace removed.

-   Add another column called `cleanliness_lower` that contains the
    values in the `cleanliness` column converted to all lowercase.

-   Count the number of occurrences of each category in
    `dest_size_trimmed`.

-   Count the number of occurrences of each category in
    `cleanliness_lower`.

``` r
# Add new columns to sfo_survey
sfo_survey <- sfo_survey %>%
  # dest_size_trimmed: dest_size without whitespace
  mutate(dest_size_trimmed = str_trim(dest_size),
         # cleanliness_lower: cleanliness converted to lowercase
         cleanliness_lower = str_to_lower(cleanliness))

# Count values of dest_size_trimmed
sfo_survey %>%
  count(dest_size_trimmed)
```

    ##   dest_size_trimmed    n
    ## 1               Hub 1757
    ## 2             Large  144
    ## 3            Medium  682
    ## 4             Small  226

``` r
# Count values of cleanliness_lower
sfo_survey %>%
  count(cleanliness_lower)
```

    ##   cleanliness_lower    n
    ## 1           average  433
    ## 2             clean  970
    ## 3             dirty    2
    ## 4    somewhat clean 1254
    ## 5    somewhat dirty   30
    ## 6              <NA>  120

### **`04-Collapsing categories`**

-   Count the categories of `dest_region`.

``` r
# Count categories of dest_region
sfo_survey %>%
  count(dest_region)
```

    ##             dest_region   n
    ## 1                  Asia 260
    ## 2 Australia/New Zealand  66
    ## 3         Canada/Mexico 220
    ## 4 Central/South America  29
    ## 5               East US 498
    ## 6                Europe 401
    ## 7           Middle East  79
    ## 8            Midwest US 281
    ## 9               West US 975

`"EU"`, `"eur"`, and `"Europ"` need to be collapsed to `"Europe"`.

-   Create a vector called `europe_categories` containing the three
    values of `dest_region` that need to be collapsed.

-   Add a new column to `sfo_survey` called `dest_region_collapsed` that
    contains the values from the `dest_region` column, except the
    categories stored in `europe_categories` should be collapsed to
    `Europe`.

-   Count the categories of `dest_region_collapsed`.

``` r
library(forcats)

# Count categories of dest_region
sfo_survey %>%
  count(dest_region)
```

    ##             dest_region   n
    ## 1                  Asia 260
    ## 2 Australia/New Zealand  66
    ## 3         Canada/Mexico 220
    ## 4 Central/South America  29
    ## 5               East US 498
    ## 6                Europe 401
    ## 7           Middle East  79
    ## 8            Midwest US 281
    ## 9               West US 975

``` r
# Categories to map to Europe
europe_categories <- c("EU", "eur", "Europ")

# Add a new col dest_region_collapsed
sfo_survey %>%
  # Map all categories in europe_categories to Europe
  mutate(dest_region_collapsed = fct_collapse(dest_region, 
                                     Europe = europe_categories)) %>%
  # Count categories of dest_region_collapsed
  count(dest_region_collapsed)
```

    ## Warning: Unknown levels in `f`: EU, eur, Europ

    ##   dest_region_collapsed   n
    ## 1                  Asia 260
    ## 2 Australia/New Zealand  66
    ## 3         Canada/Mexico 220
    ## 4 Central/South America  29
    ## 5               East US 498
    ## 6                Europe 401
    ## 7           Middle East  79
    ## 8            Midwest US 281
    ## 9               West US 975

### **`05-Detecting inconsistent text data`**

`dplyr` and `stringr` are loaded, and `sfo_survey` is available.

-   Filter for rows with phone numbers that contain `"-"`s.

-   Filter for rows with phone numbers that contain `"("`, or `")"`.
    Remember to use `fixed()` when searching for parentheses.

``` r
library(stringr)

# file path
sfo_survey_file <- file.path("..", "00_Datasets", "sfo_survey_phone.csv") 

# reading rds file
sfo_survey <- read.csv(sfo_survey_file)
```

``` r
# Filter for rows with "-" in the phone column
sfo_survey %>%
  filter(str_detect(phone, "-"))
```

    ##        id             airline          destination          phone
    ## 1    1844    TURKISH AIRLINES             ISTANBUL   731-813-2043
    ## 2    1840    TURKISH AIRLINES             ISTANBUL   563-732-6802
    ## 3    3010            AMERICAN                MIAMI (637) 782-6989
    ## 4    2097         UNITED INTL          MEXICO CITY (359) 803-9809
    ## 5    1835    TURKISH AIRLINES             ISTANBUL (416) 788-2844
    ## 6    1849    TURKISH AIRLINES             ISTANBUL   311-305-4367
    ## 7    2289              QANTAS               SYDNEY   817-400-0481
    ## 8     105              UNITED WASHINGTON DC-DULLES (729) 609-4819
    ## 9    1973      CATHAY PACIFIC            HONG KONG (201) 737-4409
    ## 10   2385         UNITED INTL               SYDNEY (137) 611-3694
    ## 11    517              UNITED       FT. LAUDERDALE (812) 869-6263
    ## 12   2885             EVA AIR               TAIPEI (194) 198-0504
    ## 13   2128            FRONTIER               DENVER (299) 137-6993
    ## 14   1376         UNITED INTL             SHANGHAI   653-786-5985
    ## 15      4           SOUTHWEST          LOS ANGELES   362-136-1153
    ## 16   1541           AIR CHINA              BEIJING   376-456-0697
    ## 17   1039          AIR CANADA              CALGARY   962-918-6117
    ## 18   2460              ALASKA       SALT LAKE CITY   805-877-3887
    ## 19   2132            FRONTIER               DENVER (739) 710-2966
    ## 20   2455              ALASKA       SALT LAKE CITY   588-693-9875
    ## 21    294              UNITED            SAN DIEGO   681-308-7915
    ## 22   1579         UNITED INTL            HONG KONG   322-884-3020
    ## 23   1269      CATHAY PACIFIC            HONG KONG   176-313-5403
    ## 24    519            EMIRATES                DUBAI (477) 182-4689
    ## 25    656              UNITED               NEWARK (525) 362-5532
    ## 26   2649           SOUTHWEST               DENVER (687) 887-6766
    ## 27   3045            AMERICAN     DALLAS-FT. WORTH   733-154-0094
    ## 28    204            AMERICAN          LOS ANGELES   291-830-3017
    ## 29   1049          AIR CANADA              CALGARY (637) 100-0509
    ## 30   3040            AMERICAN     DALLAS-FT. WORTH (603) 149-7268
    ## 31    844              ALASKA             PORTLAND (364) 792-5553
    ## 32    881              ALASKA       RALEIGH-DURHAM   496-429-1314
    ## 33   1527              ALASKA          LOS ANGELES   486-268-3312
    ## 34   1195           SOUTHWEST            LAS VEGAS   497-518-4050
    ## 35   1042          AIR CANADA              CALGARY (535) 685-8273
    ## 36   2380         UNITED INTL               SYDNEY (826) 738-8316
    ## 37    102              UNITED WASHINGTON DC-DULLES   724-134-3870
    ## 38   2486              UNITED               NEWARK (554) 269-8937
    ## 39    342           LUFTHANSA            FRANKFURT   125-578-4253
    ## 40    322              UNITED              BURBANK   487-232-4449
    ## 41   1090               DELTA MINNEAPOLIS-ST. PAUL (298) 135-0900
    ## 42   2211           SOUTHWEST              PHOENIX (392) 183-7831
    ## 43   1555               DELTA              ATLANTA (606) 596-1029
    ## 44   2249              UNITED          BAKERSFIELD (384) 953-4795
    ## 45    879              ALASKA       RALEIGH-DURHAM (855) 811-8811
    ## 46    546              ALASKA              SEATTLE   253-374-7102
    ## 47   2956            AMERICAN         NEW YORK-JFK (419) 295-9580
    ## 48    889          AIR CANADA              TORONTO   787-624-8443
    ## 49   1566               DELTA              DETROIT (919) 486-4251
    ## 50   1585               DELTA       SALT LAKE CITY (392) 495-7961
    ## 51    326              UNITED              BURBANK   473-238-3324
    ## 52   2670           SOUTHWEST               DENVER (506) 760-3043
    ## 53    869              ALASKA              SEATTLE   876-834-0624
    ## 54   3201              UNITED      ONTARIO (CALIF) (146) 699-3488
    ## 55   1504               DELTA          LOS ANGELES   182-535-3412
    ## 56   2765                 WOW            REYKJAVIK (302) 339-0791
    ## 57    272              UNITED          LOS ANGELES (446) 229-4342
    ## 58   1360         UNITED INTL      PARIS-DE GAULLE (249) 602-6985
    ## 59   3046            AMERICAN          LOS ANGELES (150) 905-6938
    ## 60    670      AIR FRANCE/KLM      PARIS-DE GAULLE   656-941-5355
    ## 61   2673      AIR FRANCE/KLM      PARIS-DE GAULLE (116) 689-6617
    ## 62    750          AIR CANADA              TORONTO (896) 993-8555
    ## 63    938          AIR CANADA            VANCOUVER   105-687-6500
    ## 64   1458            FRONTIER               DENVER (466) 912-8401
    ## 65    327              UNITED              BURBANK   783-463-4865
    ## 66   2179              QANTAS               SYDNEY   853-803-9900
    ## 67   1487        HAWAIIAN AIR             HONOLULU (347) 851-5388
    ## 68    402              UNITED         HOUSTON-BUSH   316-212-7309
    ## 69    893          AIR CANADA              TORONTO   381-883-5497
    ## 70   2180              QANTAS               SYDNEY   100-531-4642
    ## 71   1454              UNITED            LAS VEGAS (441) 445-6532
    ## 72    419              UNITED         INDIANAPOLIS   566-482-9004
    ## 73   2181              QANTAS               SYDNEY   491-727-7162
    ## 74   3043            AMERICAN     DALLAS-FT. WORTH   167-336-5660
    ## 75   2533             JETBLUE           LONG BEACH (205) 382-5599
    ## 76   2904    TURKISH AIRLINES             ISTANBUL (208) 794-9612
    ## 77   2138            FRONTIER               DENVER   380-918-8572
    ## 78   2193          AER LINGUS               DUBLIN (905) 742-3525
    ## 79    469              ALASKA             PORTLAND   185-321-6877
    ## 80   3087              ALASKA               NEWARK (147) 535-3529
    ## 81   2686      AIR FRANCE/KLM      PARIS-DE GAULLE (152) 912-4118
    ## 82    318              UNITED              BURBANK (634) 521-4714
    ## 83   2802           SOUTHWEST              PHOENIX   670-248-0186
    ## 84   2182              QANTAS               SYDNEY (121) 509-7306
    ## 85   2628             JETBLUE         NEW YORK-JFK (105) 635-5212
    ## 86   2521             JETBLUE           LONG BEACH (732) 168-0110
    ## 87   2034              UNITED               NEWARK (214) 250-8756
    ## 88    965           LUFTHANSA               MUNICH (119) 975-8484
    ## 89   1197           SOUTHWEST            LAS VEGAS (489) 534-6272
    ## 90    325              UNITED              BURBANK (610) 716-5732
    ## 91    846              ALASKA             PORTLAND (456) 925-4236
    ## 92   2239           SOUTHWEST              PHOENIX   743-103-7645
    ## 93   1329         UNITED INTL             SHANGHAI   432-281-3682
    ## 94   1588         UNITED INTL            HONG KONG (167) 144-9470
    ## 95   1981      AIR FRANCE/KLM            AMSTERDAM   946-558-5801
    ## 96   1851              ALASKA              KAHULUI (848) 149-5208
    ## 97   2226           SOUTHWEST            SANTA ANA (970) 908-2298
    ## 98   2248              UNITED          LOS ANGELES (843) 120-5653
    ## 99   1896              UNITED               BOSTON   170-641-3537
    ## 100  2183              QANTAS               SYDNEY   814-895-6610
    ## 101   297              UNITED            SAN DIEGO (139) 727-9901
    ## 102  2418              UNITED             HONOLULU (817) 824-3849
    ## 103  2002      AIR FRANCE/KLM      PARIS-DE GAULLE   503-671-4901
    ## 104  2379         UNITED INTL              BEIJING   538-393-2243
    ## 105  1327         UNITED INTL             SHANGHAI (540) 362-7136
    ## 106   664              UNITED               NEWARK (802) 910-1742
    ## 107  1523             JETBLUE         NEW YORK-JFK   784-458-8425
    ## 108  2292                 WOW            REYKJAVIK (365) 217-0634
    ## 109  1161               DELTA         NEW YORK-JFK (594) 797-7729
    ## 110   103              UNITED WASHINGTON DC-DULLES   477-307-3338
    ## 111  2254              UNITED          LOS ANGELES   744-301-1148
    ## 112  2383         UNITED INTL               SYDNEY (900) 462-1379
    ## 113   842            AMERICAN     DALLAS-FT. WORTH   878-636-2294
    ## 114   413              UNITED              PHOENIX (998) 692-1900
    ## 115  2569            INTERJET          GUADALAJARA   304-225-5895
    ## 116  2803           SOUTHWEST              PHOENIX   931-522-5498
    ## 117  1558               DELTA              ATLANTA (507) 483-3618
    ## 118  3061                COPA          PANAMA CITY (380) 449-7849
    ## 119   886              ALASKA       RALEIGH-DURHAM   589-975-0198
    ## 120  1462            FRONTIER               DENVER   501-668-7869
    ## 121   741              ALASKA         NEW YORK-JFK (706) 836-7047
    ## 122  2299              UNITED          SAN ANTONIO (698) 462-6742
    ## 123   271              UNITED          LOS ANGELES   525-552-4162
    ## 124  1452              UNITED            LAS VEGAS   635-714-8302
    ## 125  2112            AMERICAN              PHOENIX   453-556-0852
    ## 126  2456              UNITED               DENVER   131-641-1331
    ## 127  2659           SOUTHWEST               DENVER (753) 726-0123
    ## 128  2846              UNITED              SEATTLE   609-332-7370
    ## 129   963           LUFTHANSA               MUNICH   426-182-1365
    ## 130  2537              UNITED       CHICAGO-O'HARE (347) 782-5787
    ## 131   615              ALASKA WASHINGTON DC-DULLES   898-210-6218
    ## 132  2195           SOUTHWEST            LAS VEGAS   658-861-4306
    ## 133  2453              UNITED               DENVER (716) 184-1232
    ## 134   323              UNITED              BURBANK   380-105-1757
    ## 135   748          AIR CANADA              TORONTO (969) 555-0453
    ## 136   241              UNITED            NASHVILLE   343-973-0193
    ## 137  1589           SOUTHWEST          LOS ANGELES (265) 286-5671
    ## 138  1044          AIR CANADA              CALGARY   413-727-2672
    ## 139  1855              ALASKA              KAHULUI (162) 332-5838
    ## 140  1178           SOUTHWEST            LAS VEGAS (229) 604-7790
    ## 141  1009          AIR CANADA            VANCOUVER (892) 301-0333
    ## 142  1853              ALASKA              KAHULUI (331) 472-8624
    ## 143  2134            FRONTIER               DENVER   522-286-5318
    ## 144  2676           SOUTHWEST               DENVER   314-360-4288
    ## 145  1235       CHINA EASTERN             SHANGHAI (301) 534-5754
    ## 146  2189          AER LINGUS               DUBLIN (341) 473-0639
    ## 147     5           SOUTHWEST          LOS ANGELES (835) 882-3693
    ## 148  1529              ALASKA          LOS ANGELES   465-550-6610
    ## 149  2294              UNITED          SAN ANTONIO (589) 194-0523
    ## 150  2381         UNITED INTL              BEIJING   221-190-1449
    ## 151  1047          AIR CANADA              CALGARY (322) 843-0185
    ## 152  2590            INTERJET          GUADALAJARA   676-614-9095
    ## 153  1586         UNITED INTL            HONG KONG (190) 975-2514
    ## 154  2621          AEROMEXICO          MEXICO CITY (909) 382-3774
    ## 155  1040          AIR CANADA              CALGARY (347) 896-3463
    ## 156  1095           SOUTHWEST            SAN DIEGO   691-318-3535
    ## 157  2111         UNITED INTL          MEXICO CITY   852-386-6029
    ## 158   662              UNITED               NEWARK (355) 550-1392
    ## 159  2391         UNITED INTL              BEIJING (705) 456-1905
    ## 160  3058            AMERICAN                MIAMI (836) 207-8419
    ## 161  1100           SOUTHWEST            SAN DIEGO (306) 552-1875
    ## 162     6           SOUTHWEST          LOS ANGELES   729-102-7511
    ## 163  2926           SOUTHWEST            SAN DIEGO   700-431-3918
    ## 164  2149          AIR CANADA            VANCOUVER   887-657-4143
    ## 165   954              ALASKA         NEW YORK-JFK   574-438-5329
    ## 166  2524             JETBLUE               BOSTON (944) 189-7555
    ## 167  2836              UNITED       CHICAGO-O'HARE   998-931-4783
    ## 168   270              UNITED          LOS ANGELES   362-178-6307
    ## 169  3060            AMERICAN                MIAMI (458) 404-9558
    ## 170  3012              UNITED             PORTLAND (481) 522-1039
    ## 171  1485        HAWAIIAN AIR             HONOLULU (376) 611-4588
    ## 172  2000              UNITED               NEWARK (641) 544-6549
    ## 173   100              UNITED WASHINGTON DC-DULLES   797-870-7818
    ## 174  1810         UNITED INTL      LONDON HEATHROW   649-379-5361
    ## 175  2482              ALASKA       SALT LAKE CITY (572) 748-6932
    ## 176  2640             JETBLUE         NEW YORK-JFK   395-892-5646
    ## 177  3262              UNITED            SAN DIEGO   221-628-9561
    ## 178  2031     BRITISH AIRWAYS      LONDON HEATHROW (227) 801-6148
    ## 179  2700              UNITED              ORLANDO   549-649-1864
    ## 180  3074                COPA          PANAMA CITY (519) 573-6576
    ## 181  2014     BRITISH AIRWAYS      LONDON HEATHROW   919-342-0230
    ## 182  2036     BRITISH AIRWAYS      LONDON HEATHROW   364-759-2705
    ## 183  3089              UNITED              SPOKANE (900) 586-1787
    ## 184  2920               DELTA              SEATTLE   308-607-9855
    ## 185  2284                 WOW            REYKJAVIK   472-337-8838
    ## 186  2732      AIR FRANCE/KLM      PARIS-DE GAULLE   365-832-0674
    ## 187  1428              UNITED            LAS VEGAS   123-282-3494
    ## 188   725      AIR FRANCE/KLM      PARIS-DE GAULLE   129-377-8159
    ## 189  2909               DELTA              SEATTLE   162-451-0594
    ## 190   728      CATHAY PACIFIC            HONG KONG (239) 325-5321
    ## 191  2171            FRONTIER               DENVER   436-422-6171
    ## 192  2592          AEROMEXICO          MEXICO CITY   929-102-5905
    ## 193  2046     BRITISH AIRWAYS      LONDON HEATHROW   452-811-8088
    ## 194   649         UNITED INTL               MUNICH   119-444-0817
    ## 195  1864              UNITED          LOS ANGELES   945-998-0444
    ## 196  2689      AIR FRANCE/KLM      PARIS-DE GAULLE (367) 897-7969
    ## 197  1441              UNITED            LAS VEGAS   594-176-5811
    ## 198  2602            INTERJET          GUADALAJARA (621) 874-9973
    ## 199  2281              QANTAS               SYDNEY   894-593-7953
    ## 200  1658            EMIRATES                DUBAI   561-266-7842
    ## 201   370              ALASKA    SAN JOSE DEL CABO   354-958-8052
    ## 202  1416              UNITED            LAS VEGAS (426) 342-7378
    ## 203  2122      CATHAY PACIFIC            HONG KONG   333-520-4811
    ## 204  2322              QANTAS               SYDNEY (765) 191-1797
    ## 205  1193           SOUTHWEST            LAS VEGAS (400) 250-0871
    ## 206  2939           SOUTHWEST            SAN DIEGO (434) 725-0561
    ## 207  2038     BRITISH AIRWAYS      LONDON HEATHROW (969) 207-3261
    ## 208   478              UNITED               DENVER   879-154-4494
    ## 209   220            AMERICAN          LOS ANGELES (994) 688-3259
    ## 210  1353         UNITED INTL             SHANGHAI   841-717-4447
    ## 211   192            AMERICAN          LOS ANGELES   397-353-6309
    ## 212  2187              QANTAS               SYDNEY   307-323-6861
    ## 213  1678      CATHAY PACIFIC            HONG KONG (494) 308-3048
    ## 214   667              UNITED               NEWARK   102-957-6486
    ## 215  1073          AIR CANADA              CALGARY (715) 288-8832
    ## 216  2298              QANTAS               SYDNEY (594) 448-6242
    ## 217   346           LUFTHANSA            FRANKFURT   182-227-4838
    ## 218   681      AIR FRANCE/KLM      PARIS-DE GAULLE   728-404-5558
    ## 219  2434     AIR NEW ZEALAND             AUCKLAND   427-665-3475
    ## 220  1478              ALASKA         NEW YORK-JFK   375-978-3305
    ## 221  2490              UNITED       CHICAGO-O'HARE   181-708-2089
    ## 222  3200      CATHAY PACIFIC            HONG KONG (802) 810-5574
    ## 223  3165               DELTA          LOS ANGELES   242-540-4234
    ## 224  1759           LUFTHANSA               MUNICH (867) 891-0871
    ## 225  3152               DELTA       SALT LAKE CITY (533) 213-4368
    ## 226  1328         UNITED INTL               KANSAI (324) 188-6781
    ## 227  2262              UNITED          BAKERSFIELD         0244-5
    ## 228  1866              ALASKA              KAHULUI   397-362-5469
    ## 229  1990      AIR FRANCE/KLM            AMSTERDAM (102) 928-7959
    ## 230  1356         UNITED INTL               KANSAI (439) 568-6611
    ## 231  2511              ALASKA       SALT LAKE CITY (767) 205-0604
    ## 232  2468              ALASKA       SALT LAKE CITY (890) 548-9219
    ## 233  2500              ALASKA       SALT LAKE CITY   769-472-2992
    ## 234   453              ALASKA            LAS VEGAS (649) 925-8489
    ## 235  2442             JETBLUE               BOSTON (156) 837-4491
    ## 236   924          AIR CANADA              TORONTO   178-232-0815
    ## 237  2716           SOUTHWEST               DENVER (882) 304-9032
    ## 238   304              UNITED            SAN DIEGO (971) 548-6611
    ## 239  2250                 WOW            REYKJAVIK (828) 153-5819
    ## 240   295           SOUTHWEST          LOS ANGELES   900-871-9056
    ## 241  2039     BRITISH AIRWAYS      LONDON HEATHROW   406-167-1379
    ## 242  2491              ALASKA       SALT LAKE CITY (906) 850-9192
    ## 243  1255         UNITED INTL         TOKYO-NARITA (859) 777-8245
    ## 244  2041     BRITISH AIRWAYS      LONDON HEATHROW   641-635-8466
    ## 245  1718      AIR FRANCE/KLM      PARIS-DE GAULLE   807-671-6158
    ## 246  2047     BRITISH AIRWAYS      LONDON HEATHROW (589) 270-7518
    ## 247  3235            AMERICAN              PHOENIX   928-179-7556
    ## 248  2020     BRITISH AIRWAYS      LONDON HEATHROW   905-903-5258
    ## 249  2172            FRONTIER               DENVER (146) 129-5118
    ## 250  1903           LUFTHANSA               MUNICH (927) 747-9822
    ## 251  2139            FRONTIER               DENVER (481) 479-7013
    ## 252  1404         UNITED INTL      PARIS-DE GAULLE (716) 777-3762
    ## 253  2377              UNITED               AUSTIN   274-863-3205
    ## 254  3138               DELTA       SALT LAKE CITY (217) 589-0596
    ## 255  2892                 WOW            REYKJAVIK   731-813-2043
    ## 256  2721              UNITED              KAHULUI   563-732-6802
    ## 257   841              ALASKA              SEATTLE (637) 782-6989
    ## 258  2272              UNITED          BAKERSFIELD (359) 803-9809
    ## 259    11           SOUTHWEST          LOS ANGELES (416) 788-2844
    ## 260   904          AIR CANADA              TORONTO   311-305-4367
    ## 261  2937           SOUTHWEST            SAN DIEGO   817-400-0481
    ## 262  1167               DELTA         NEW YORK-JFK (729) 609-4819
    ## 263   302              UNITED            SAN DIEGO (201) 737-4409
    ## 264  1607     VIRGIN ATLANTIC      LONDON HEATHROW (137) 611-3694
    ## 265   911          AIR CANADA              TORONTO (812) 869-6263
    ## 266  1241       CHINA EASTERN             SHANGHAI (194) 198-0504
    ## 267  3023              UNITED             PORTLAND (299) 137-6993
    ## 268  1183           SOUTHWEST            LAS VEGAS   653-786-5985
    ## 269  3232            AMERICAN              PHOENIX   362-136-1153
    ## 270  1287             JETBLUE         NEW YORK-JFK   376-456-0697
    ## 271  3180            AMERICAN     DALLAS-FT. WORTH   962-918-6117
    ## 272  3277           SOUTHWEST               DENVER   805-877-3887
    ## 273   556              ALASKA              SEATTLE (739) 710-2966
    ## 274  2763      CATHAY PACIFIC            HONG KONG   588-693-9875
    ## 275  1442              UNITED             PORTLAND   681-308-7915
    ## 276  1926      AIR FRANCE/KLM      PARIS-DE GAULLE   322-884-3020
    ## 277  2093            AMERICAN         PHILADELPHIA   176-313-5403
    ## 278   334           SOUTHWEST              PHOENIX (477) 182-4689
    ## 279  2682              UNITED       CHICAGO-O'HARE (525) 362-5532
    ## 280  1673            EMIRATES                DUBAI (687) 887-6766
    ## 281  1736          AIR CANADA              TORONTO   733-154-0094
    ## 282  2788                 WOW            REYKJAVIK   291-830-3017
    ## 283  2941    TURKISH AIRLINES             ISTANBUL (637) 100-0509
    ## 284  3144              UNITED            SAN DIEGO (603) 149-7268
    ## 285  1313             JETBLUE         NEW YORK-JFK (364) 792-5553
    ## 286  2530             JETBLUE               BOSTON   496-429-1314
    ## 287   545            EMIRATES                DUBAI   486-268-3312
    ## 288  1785              UNITED       RALEIGH-DURHAM   497-518-4050
    ## 289  2896               DELTA       SALT LAKE CITY (535) 685-8273
    ## 290   826              ALASKA            NASHVILLE (826) 738-8316
    ## 291   694      AIR FRANCE/KLM      PARIS-DE GAULLE   724-134-3870
    ## 292   660         UNITED INTL               MUNICH (554) 269-8937
    ## 293   822              ALASKA            NASHVILLE   125-578-4253
    ## 294   982           LUFTHANSA               MUNICH   487-232-4449
    ## 295  1515               DELTA          LOS ANGELES (298) 135-0900
    ## 296  3097              UNITED              SPOKANE (392) 183-7831
    ## 297  2462              UNITED       CHICAGO-O'HARE (606) 596-1029
    ## 298  1538               DELTA         NEW YORK-JFK (384) 953-4795
    ## 299  2330               DELTA MINNEAPOLIS-ST. PAUL (855) 811-8811
    ## 300  2297              QANTAS               SYDNEY   253-374-7102
    ## 301  2005      AIR FRANCE/KLM      PARIS-DE GAULLE (419) 295-9580
    ## 302  3178            AMERICAN     DALLAS-FT. WORTH   787-624-8443
    ## 303   320           SOUTHWEST          LOS ANGELES (919) 486-4251
    ## 304   292           SOUTHWEST          LOS ANGELES (392) 495-7961
    ## 305  1635          KOREAN AIR                SEOUL   473-238-3324
    ## 306  2820               DELTA          LOS ANGELES (506) 760-3043
    ## 307  2068            AMERICAN         PHILADELPHIA   876-834-0624
    ## 308  3121              ALASKA               NEWARK (146) 699-3488
    ## 309   555            EMIRATES                DUBAI   182-535-3412
    ## 310  2748      AIR FRANCE/KLM      PARIS-DE GAULLE (302) 339-0791
    ## 311   264          AIR CANADA              TORONTO (446) 229-4342
    ## 312  2317              QANTAS               SYDNEY (249) 602-6985
    ## 313  2076             JETBLUE           LONG BEACH (150) 905-6938
    ## 314  3076 PHILIPPINE AIRLINES               MANILA   656-941-5355
    ## 315   827              ALASKA              SEATTLE (116) 689-6617
    ## 316  2318              QANTAS               SYDNEY (896) 993-8555
    ## 317  3256              UNITED            SAN DIEGO   105-687-6500
    ## 318  2636             JETBLUE         NEW YORK-JFK (466) 912-8401
    ## 319  1164           SOUTHWEST          LOS ANGELES   783-463-4865
    ## 320  2950           SOUTHWEST            SAN DIEGO   853-803-9900
    ## 321  1670            EMIRATES                DUBAI (347) 851-5388
    ## 322  1369         UNITED INTL               KANSAI   316-212-7309
    ## 323  1383         UNITED INTL             SHANGHAI   381-883-5497
    ## 324  2762               DELTA              ATLANTA   100-531-4642
    ## 325  1336         UNITED INTL               KANSAI (441) 445-6532
    ## 326  3263              UNITED            SAN DIEGO   566-482-9004
    ## 327  1614     VIRGIN ATLANTIC      LONDON HEATHROW   491-727-7162
    ## 328  2471              ALASKA       SALT LAKE CITY   167-336-5660
    ## 329  2561             JETBLUE           LONG BEACH (205) 382-5599
    ## 330   669              UNITED               NEWARK (208) 794-9612
    ## 331  2987            AMERICAN            CHARLOTTE   380-918-8572
    ## 332  3124                COPA          PANAMA CITY (905) 742-3525
    ## 333   857              ALASKA             PORTLAND   185-321-6877
    ## 334  1738          AIR CANADA              TORONTO (147) 535-3529
    ## 335  1698              ALASKA            SANTA ANA (152) 912-4118
    ## 336  1035          AIR CANADA            VANCOUVER (634) 521-4714
    ## 337  1916      AIR FRANCE/KLM      PARIS-DE GAULLE   670-248-0186
    ## 338   939          AIR CANADA              TORONTO (121) 509-7306
    ## 339  1461              ALASKA         NEW YORK-JFK (105) 635-5212
    ## 340  2908    TURKISH AIRLINES             ISTANBUL (732) 168-0110
    ## 341  1412              UNITED          KANSAS CITY (214) 250-8756
    ## 342  3240            AMERICAN              PHOENIX (119) 975-8484
    ## 343  1660            EMIRATES                DUBAI (489) 534-6272
    ## 344   688      AIR FRANCE/KLM      PARIS-DE GAULLE (610) 716-5732
    ## 345  1102               DELTA MINNEAPOLIS-ST. PAUL (456) 925-4236
    ## 346  2213      CATHAY PACIFIC            HONG KONG   743-103-7645
    ## 347  1551               DELTA         NEW YORK-JFK   432-281-3682
    ## 348  1298             JETBLUE         NEW YORK-JFK (167) 144-9470
    ## 349  1134           SOUTHWEST            SAN DIEGO   946-558-5801
    ## 350  2049     BRITISH AIRWAYS      LONDON HEATHROW (848) 149-5208
    ## 351   789            AMERICAN          LOS ANGELES (970) 908-2298
    ## 352  2804           SOUTHWEST              PHOENIX (843) 120-5653
    ## 353  1756           LUFTHANSA               MUNICH   170-641-3537
    ## 354   429              UNITED             HONOLULU   814-895-6610
    ## 355   815              ALASKA             PORTLAND (139) 727-9901
    ## 356  3252              UNITED            SAN DIEGO (817) 824-3849
    ## 357   577              ALASKA              SEATTLE   503-671-4901
    ## 358   848            AMERICAN     DALLAS-FT. WORTH   538-393-2243
    ## 359  2605              UNITED       CHICAGO-O'HARE (540) 362-7136
    ## 360  2012      AIR FRANCE/KLM      PARIS-DE GAULLE (802) 910-1742
    ## 361  2203      CATHAY PACIFIC            HONG KONG   784-458-8425
    ## 362  3120                COPA          PANAMA CITY (365) 217-0634
    ## 363  3149               DELTA       SALT LAKE CITY (594) 797-7729
    ## 364  1646     VIRGIN ATLANTIC      LONDON HEATHROW   477-307-3338
    ## 365  1648     VIRGIN ATLANTIC      LONDON HEATHROW   744-301-1148
    ## 366  1946      AIR FRANCE/KLM      PARIS-DE GAULLE (900) 462-1379
    ## 367   722      AIR FRANCE/KLM      PARIS-DE GAULLE   878-636-2294
    ## 368  1520            INTERJET          GUADALAJARA (998) 692-1900
    ## 369  1865              ALASKA              KAHULUI   304-225-5895
    ## 370  2499              UNITED               DENVER   931-522-5498
    ## 371  2776                 WOW            REYKJAVIK (507) 483-3618
    ## 372   328              UNITED              BURBANK (380) 449-7849
    ## 373  2819               DELTA          LOS ANGELES   589-975-0198
    ## 374  1021          AIR CANADA            VANCOUVER   501-668-7869
    ## 375   766              ALASKA         NEW YORK-JFK (706) 836-7047
    ## 376  2888              UNITED               AUSTIN (698) 462-6742
    ## 377  2882             EVA AIR               TAIPEI   525-552-4162
    ## 378  1961      CATHAY PACIFIC            HONG KONG   635-714-8302
    ## 379  1909              UNITED               BOSTON   453-556-0852
    ## 380  1779              UNITED       RALEIGH-DURHAM   131-641-1331
    ## 381  2483              UNITED               DENVER (753) 726-0123
    ## 382   817              ALASKA             PORTLAND   609-332-7370
    ## 383  1987      AIR FRANCE/KLM            AMSTERDAM   426-182-1365
    ## 384  1672            EMIRATES                DUBAI (347) 782-5787
    ## 385  3161              UNITED            SAN DIEGO   898-210-6218
    ## 386  1633          KOREAN AIR                SEOUL   658-861-4306
    ## 387   834              ALASKA              SEATTLE (716) 184-1232
    ## 388  2392         UNITED INTL              BEIJING   380-105-1757
    ## 389  2074             JETBLUE           LONG BEACH (969) 555-0453
    ## 390  1341         UNITED INTL             SHANGHAI   343-973-0193
    ## 391  2895                 WOW            REYKJAVIK (265) 286-5671
    ## 392   525              ALASKA      PUERTO VALLARTA   413-727-2672
    ## 393   724      CATHAY PACIFIC            HONG KONG (162) 332-5838
    ## 394   381              ALASKA    SAN JOSE DEL CABO (229) 604-7790
    ## 395  1760           LUFTHANSA               MUNICH (892) 301-0333
    ## 396  2202          AER LINGUS               DUBLIN (331) 472-8624
    ## 397  3290           SOUTHWEST               DENVER   522-286-5318
    ## 398  2572             JETBLUE           LONG BEACH   314-360-4288
    ## 399   308              UNITED            SAN DIEGO (301) 534-5754
    ## 400   344           LUFTHANSA            FRANKFURT (341) 473-0639
    ## 401  1729          AIR CANADA              TORONTO (835) 882-3693
    ## 402  2702              UNITED              ORLANDO   465-550-6610
    ## 403   599             JETBLUE               BOSTON (589) 194-0523
    ## 404  1911      AIR FRANCE/KLM      PARIS-DE GAULLE   221-190-1449
    ## 405   704      AIR FRANCE/KLM      PARIS-DE GAULLE (322) 843-0185
    ## 406  2567             JETBLUE           LONG BEACH   676-614-9095
    ## 407  3127               DELTA       SALT LAKE CITY (190) 975-2514
    ## 408  1250       CHINA EASTERN             SHANGHAI (909) 382-3774
    ## 409  2691              UNITED              ORLANDO (347) 896-3463
    ## 410   427              UNITED             HONOLULU   691-318-3535
    ## 411   832              ALASKA              SEATTLE   852-386-6029
    ## 412  2849              UNITED              SEATTLE (355) 550-1392
    ## 413  1557               DELTA         NEW YORK-JFK (705) 456-1905
    ## 414  2329            AMERICAN       CHICAGO-O'HARE (836) 207-8419
    ## 415   997           LUFTHANSA               MUNICH (306) 552-1875
    ## 416  1906           LUFTHANSA               MUNICH   729-102-7511
    ## 417  2961           SOUTHWEST          LOS ANGELES   700-431-3918
    ## 418  1217           SOUTHWEST            LAS VEGAS   887-657-4143
    ## 419  1086          AIR CANADA              CALGARY   574-438-5329
    ## 420   347              ALASKA            SAN DIEGO (944) 189-7555
    ## 421   633              ALASKA WASHINGTON DC-DULLES   998-931-4783
    ## 422   613              ALASKA WASHINGTON DC-DULLES   362-178-6307
    ## 423  1580               DELTA              DETROIT (458) 404-9558
    ## 424   268          AIR CANADA              TORONTO (481) 522-1039
    ## 425   446              UNITED             HONOLULU (376) 611-4588
    ## 426  1664            EMIRATES                DUBAI (641) 544-6549
    ## 427  2025     BRITISH AIRWAYS      LONDON HEATHROW   797-870-7818
    ## 428   423              UNITED             HONOLULU   649-379-5361
    ## 429  2276              UNITED          LOS ANGELES (572) 748-6932
    ## 430  2747      AIR FRANCE/KLM      PARIS-DE GAULLE   395-892-5646
    ## 431  2155      CATHAY PACIFIC            HONG KONG   221-628-9561
    ## 432   199            AMERICAN          LOS ANGELES (227) 801-6148
    ## 433   561              ALASKA              SEATTLE   549-649-1864
    ## 434  2017     BRITISH AIRWAYS      LONDON HEATHROW (519) 573-6576
    ## 435   567              ALASKA              SEATTLE   919-342-0230
    ## 436   607             JETBLUE               BOSTON   364-759-2705
    ## 437  1889  SINGAPORE AIRLINES            SINGAPORE (900) 586-1787
    ## 438  2818               DELTA          LOS ANGELES   308-607-9855
    ## 439  1974              UNITED            BALTIMORE   472-337-8838
    ## 440   202            AMERICAN          LOS ANGELES   365-832-0674
    ## 441  2176            FRONTIER               DENVER   123-282-3494
    ## 442  1115               DELTA MINNEAPOLIS-ST. PAUL   129-377-8159
    ## 443  1099               DELTA MINNEAPOLIS-ST. PAUL   162-451-0594
    ## 444  1710      AIR FRANCE/KLM      PARIS-DE GAULLE (239) 325-5321
    ## 445   263          AIR CANADA              TORONTO   436-422-6171
    ## 446  2053              UNITED               NEWARK   929-102-5905
    ## 447  2672             JETBLUE               BOSTON   452-811-8088
    ## 448  1727          AIR CANADA              TORONTO   119-444-0817
    ## 449  2481              UNITED       CHICAGO-O'HARE   945-998-0444
    ## 450  1080          AIR CANADA              CALGARY (367) 897-7969
    ## 451  2953           SOUTHWEST            SAN DIEGO   594-176-5811
    ## 452   431              UNITED         INDIANAPOLIS (621) 874-9973
    ## 453  2634             JETBLUE         NEW YORK-JFK   894-593-7953
    ## 454  1895           LUFTHANSA               MUNICH   561-266-7842
    ## 455  2029     BRITISH AIRWAYS      LONDON HEATHROW   354-958-8052
    ## 456   898          AIR CANADA              TORONTO (426) 342-7378
    ## 457  1418              UNITED            LAS VEGAS   333-520-4811
    ## 458   643              ALASKA          NEW ORLEANS (765) 191-1797
    ## 459  1574         UNITED INTL            HONG KONG (400) 250-0871
    ## 460  2169            FRONTIER               DENVER (434) 725-0561
    ## 461  2365             JETBLUE       FT. LAUDERDALE (969) 207-3261
    ## 462   512              UNITED              KAHULUI   879-154-4494
    ## 463  1577         UNITED INTL            HONG KONG (994) 688-3259
    ## 464  2403         UNITED INTL               SYDNEY   841-717-4447
    ## 465  2388         UNITED INTL               SYDNEY   397-353-6309
    ## 466  1857              ALASKA              KAHULUI   307-323-6861
    ## 467  2655             JETBLUE               BOSTON (494) 308-3048
    ## 468  3085              UNITED              SPOKANE   102-957-6486
    ## 469  3009            AMERICAN                MIAMI (715) 288-8832
    ## 470  3142               DELTA       SALT LAKE CITY (594) 448-6242
    ## 471  1339         UNITED INTL               KANSAI   182-227-4838
    ## 472  2238          AER LINGUS               DUBLIN   728-404-5558
    ## 473   985           LUFTHANSA               MUNICH   427-665-3475
    ## 474  1893           LUFTHANSA               MUNICH   375-978-3305
    ## 475  2738               DELTA          LOS ANGELES   181-708-2089
    ## 476  1619     VIRGIN ATLANTIC      LONDON HEATHROW (802) 810-5574
    ## 477   448              UNITED             HONOLULU   242-540-4234
    ## 478  1508        HAWAIIAN AIR             HONOLULU (867) 891-0871
    ## 479  2564             JETBLUE           LONG BEACH (533) 213-4368
    ## 480   763          AIR CANADA              TORONTO (324) 188-6781
    ## 481  2595             JETBLUE               BOSTON   397-362-5469
    ## 482  1921      AIR FRANCE/KLM      PARIS-DE GAULLE (102) 928-7959
    ## 483  1387         UNITED INTL      PARIS-DE GAULLE (439) 568-6611
    ## 484  1480              ALASKA         NEW YORK-JFK (767) 205-0604
    ## 485  1743      CATHAY PACIFIC            HONG KONG (890) 548-9219
    ## 486  1488            FRONTIER               DENVER   769-472-2992
    ## 487  2508              UNITED               NEWARK (649) 925-8489
    ## 488  2277              UNITED          BAKERSFIELD (156) 837-4491
    ## 489  1595           SOUTHWEST          LOS ANGELES   178-232-0815
    ## 490  2099         UNITED INTL          MEXICO CITY (882) 304-9032
    ## 491  3172            AMERICAN     DALLAS-FT. WORTH (971) 548-6611
    ## 492  1920      AIR FRANCE/KLM      PARIS-DE GAULLE (828) 153-5819
    ## 493  2142          AIR CANADA            VANCOUVER   900-871-9056
    ## 494  2755      AIR FRANCE/KLM      PARIS-DE GAULLE   406-167-1379
    ## 495  1986      AIR FRANCE/KLM            AMSTERDAM (906) 850-9192
    ## 496   529            EMIRATES                DUBAI (859) 777-8245
    ## 497  3157              UNITED            SAN DIEGO   641-635-8466
    ## 498  1782              UNITED       RALEIGH-DURHAM   807-671-6158
    ## 499  3068              UNITED              SPOKANE (589) 270-7518
    ## 500  2680              UNITED              ORLANDO   928-179-7556
    ## 501  2321           SOUTHWEST          LOS ANGELES   905-903-5258
    ## 502  3038              UNITED             PORTLAND (146) 129-5118
    ## 503  1634          KOREAN AIR                SEOUL (927) 747-9822
    ## 504  2877             EVA AIR               TAIPEI (481) 479-7013
    ## 505  1600  SINGAPORE AIRLINES            SINGAPORE (716) 777-3762
    ## 506  2778              UNITED               AUSTIN   274-863-3205
    ## 507  2562             JETBLUE               BOSTON (217) 589-0596
    ## 508  1486            FRONTIER               DENVER   731-813-2043
    ## 509  1803              ALASKA       RALEIGH-DURHAM   563-732-6802
    ## 510  2161            FRONTIER               DENVER (637) 782-6989
    ## 511  1019          AIR CANADA            VANCOUVER (359) 803-9809
    ## 512  2867              UNITED WASHINGTON DC-DULLES (416) 788-2844
    ## 513   477              UNITED               DENVER   311-305-4367
    ## 514   677      AIR FRANCE/KLM      PARIS-DE GAULLE   817-400-0481
    ## 515  2707               DELTA          LOS ANGELES (729) 609-4819
    ## 516   992           LUFTHANSA               MUNICH (201) 737-4409
    ## 517  2395         UNITED INTL              BEIJING (137) 611-3694
    ## 518  3219              ALASKA             PORTLAND (812) 869-6263
    ## 519  1377         UNITED INTL      PARIS-DE GAULLE (194) 198-0504
    ## 520  2925    TURKISH AIRLINES             ISTANBUL (299) 137-6993
    ## 521  2190      CATHAY PACIFIC            HONG KONG   653-786-5985
    ## 522   692      CATHAY PACIFIC            HONG KONG   362-136-1153
    ## 523  3116              ALASKA               NEWARK   376-456-0697
    ## 524  1097               DELTA MINNEAPOLIS-ST. PAUL   962-918-6117
    ## 525  2368              UNITED               AUSTIN   805-877-3887
    ## 526  2708      AIR FRANCE/KLM      PARIS-DE GAULLE (739) 710-2966
    ## 527  1309             JETBLUE         NEW YORK-JFK   588-693-9875
    ## 528  3283           SOUTHWEST               DENVER   681-308-7915
    ## 529  2946           SOUTHWEST            SAN DIEGO   322-884-3020
    ## 530   428              UNITED         INDIANAPOLIS   176-313-5403
    ## 531  3141              ALASKA               NEWARK (477) 182-4689
    ## 532  1432              UNITED            LAS VEGAS (525) 362-5532
    ## 533  1891  SINGAPORE AIRLINES            SINGAPORE (687) 887-6766
    ## 534  2768                 WOW            REYKJAVIK   733-154-0094
    ## 535  2218          AER LINGUS               DUBLIN   291-830-3017
    ## 536  1433              UNITED             PORTLAND (637) 100-0509
    ## 537  2687              UNITED              ORLANDO (603) 149-7268
    ## 538  1325             JETBLUE         NEW YORK-JFK (364) 792-5553
    ## 539   912          AIR CANADA              TORONTO   496-429-1314
    ## 540  1871              ALASKA              KAHULUI   486-268-3312
    ## 541   888              ALASKA       RALEIGH-DURHAM   497-518-4050
    ## 542  2287                 WOW            REYKJAVIK (535) 685-8273
    ## 543  2007      AIR FRANCE/KLM      PARIS-DE GAULLE (826) 738-8316
    ## 544   267          AIR CANADA              TORONTO   724-134-3870
    ## 545  2420     AIR NEW ZEALAND             AUCKLAND (554) 269-8937
    ## 546   627              ALASKA               NEWARK   125-578-4253
    ## 547  2758               DELTA              ATLANTA   487-232-4449
    ## 548  1304             JETBLUE         NEW YORK-JFK (298) 135-0900
    ## 549  2643             JETBLUE         NEW YORK-JFK (392) 183-7831
    ## 550  1992      AIR FRANCE/KLM            AMSTERDAM (606) 596-1029
    ## 551   353              ALASKA               NEWARK (384) 953-4795
    ## 552   698      CATHAY PACIFIC            HONG KONG (855) 811-8811
    ## 553  1628          KOREAN AIR                SEOUL   253-374-7102
    ## 554  2011      AIR FRANCE/KLM      PARIS-DE GAULLE (419) 295-9580
    ## 555  1783              UNITED       RALEIGH-DURHAM   787-624-8443
    ## 556  3125              ALASKA               NEWARK (919) 486-4251
    ## 557  3229            AMERICAN              PHOENIX (392) 495-7961
    ## 558   489              ALASKA             PORTLAND   473-238-3324
    ## 559  1733          AIR CANADA              TORONTO (506) 760-3043
    ## 560  2137          AIR CANADA            VANCOUVER   876-834-0624
    ## 561  2999            AMERICAN                MIAMI (146) 699-3488
    ## 562  3078 PHILIPPINE AIRLINES               MANILA   182-535-3412
    ## 563  3197      CATHAY PACIFIC            HONG KONG (302) 339-0791
    ## 564   437              UNITED             HONOLULU (446) 229-4342
    ## 565  1869              ALASKA              KAHULUI (249) 602-6985
    ## 566   550            EMIRATES                DUBAI (150) 905-6938
    ## 567  1321             JETBLUE         NEW YORK-JFK   656-941-5355
    ## 568  3114                COPA          PANAMA CITY (116) 689-6617
    ## 569  2876             EVA AIR               TAIPEI (896) 993-8555
    ## 570   371           LUFTHANSA            FRANKFURT   105-687-6500
    ## 571  2597             JETBLUE               BOSTON (466) 912-8401
    ## 572  1739          AIR CANADA              TORONTO   783-463-4865
    ## 573   734      CATHAY PACIFIC            HONG KONG   853-803-9900
    ## 574   811              ALASKA             PORTLAND (347) 851-5388
    ## 575   522              ALASKA      PUERTO VALLARTA   316-212-7309
    ## 576   855              ALASKA             PORTLAND   381-883-5497
    ## 577  2106             JETBLUE           LONG BEACH   100-531-4642
    ## 578  3194               DELTA          LOS ANGELES (441) 445-6532
    ## 579   774            AMERICAN          LOS ANGELES   566-482-9004
    ## 580  2616            INTERJET          GUADALAJARA   491-727-7162
    ## 581   934          AIR CANADA              TORONTO   167-336-5660
    ## 582  2160      CATHAY PACIFIC            HONG KONG (205) 382-5599
    ## 583  1228           SOUTHWEST            LAS VEGAS (208) 794-9612
    ## 584  2632             JETBLUE         NEW YORK-JFK   380-918-8572
    ## 585  2791                 WOW            REYKJAVIK (905) 742-3525
    ## 586  2808               DELTA          LOS ANGELES   185-321-6877
    ## 587  1985              UNITED            BALTIMORE (147) 535-3529
    ## 588  1147           SOUTHWEST            SAN DIEGO (152) 912-4118
    ## 589  2267              UNITED          BAKERSFIELD (634) 521-4714
    ## 590  1153           SOUTHWEST            SAN DIEGO   670-248-0186
    ## 591  2496              UNITED               DENVER (121) 509-7306
    ## 592  2806               DELTA          LOS ANGELES (105) 635-5212
    ## 593  1771            AMERICAN       CHICAGO-O'HARE (732) 168-0110
    ## 594  2131          AIR CANADA            VANCOUVER (214) 250-8756
    ## 595  1421              UNITED            LAS VEGAS (119) 975-8484
    ## 596  1179               DELTA         NEW YORK-JFK (489) 534-6272
    ## 597   695      CATHAY PACIFIC            HONG KONG (610) 716-5732
    ## 598  1705      AIR FRANCE/KLM      PARIS-DE GAULLE (456) 925-4236
    ## 599  1530             JETBLUE         NEW YORK-JFK   743-103-7645
    ## 600  2848              UNITED              SEATTLE   432-281-3682
    ## 601  2265              UNITED          LOS ANGELES (167) 144-9470
    ## 602  2792               DELTA       SALT LAKE CITY   946-558-5801
    ## 603   824              ALASKA            NASHVILLE (848) 149-5208
    ## 604  1922      AIR FRANCE/KLM      PARIS-DE GAULLE (970) 908-2298
    ## 605  1821              ALASKA       RALEIGH-DURHAM (843) 120-5653
    ## 606  1010          AIR CANADA            VANCOUVER   170-641-3537
    ## 607   254          AIR CANADA              TORONTO   814-895-6610
    ## 608  2415     AIR NEW ZEALAND             AUCKLAND (139) 727-9901
    ## 609  1688      CATHAY PACIFIC            HONG KONG (817) 824-3849
    ## 610   247              UNITED            NASHVILLE   503-671-4901
    ## 611  2493              ALASKA       SALT LAKE CITY   538-393-2243
    ## 612  2113             JETBLUE           LONG BEACH (540) 362-7136
    ## 613   581             JETBLUE               BOSTON (802) 910-1742
    ## 614   382              ALASKA          LOS ANGELES   784-458-8425
    ## 615   358              ALASKA               NEWARK (365) 217-0634
    ## 616  1950      AIR FRANCE/KLM      PARIS-DE GAULLE (594) 797-7729
    ## 617  3185               DELTA          LOS ANGELES   477-307-3338
    ## 618   584             JETBLUE               BOSTON   744-301-1148
    ## 619  2934           SOUTHWEST            SAN DIEGO (900) 462-1379
    ## 620  2827              UNITED              BURBANK   878-636-2294
    ## 621   257          AIR CANADA              TORONTO (998) 692-1900
    ## 622   850            AMERICAN     DALLAS-FT. WORTH   304-225-5895
    ## 623   194            AMERICAN          LOS ANGELES   931-522-5498
    ## 624  2624              UNITED       CHICAGO-O'HARE (507) 483-3618
    ## 625  1676      AIR FRANCE/KLM      PARIS-DE GAULLE (380) 449-7849
    ## 626  1643     VIRGIN ATLANTIC      LONDON HEATHROW   589-975-0198
    ## 627  2772              UNITED               AUSTIN   501-668-7869
    ## 628  3146               DELTA       SALT LAKE CITY (706) 836-7047
    ## 629  3237            AMERICAN              PHOENIX (698) 462-6742
    ## 630  2333            AMERICAN       CHICAGO-O'HARE   525-552-4162
    ## 631  2342              UNITED               AUSTIN   635-714-8302
    ## 632  2526             JETBLUE           LONG BEACH   453-556-0852
    ## 633  1532             JETBLUE         NEW YORK-JFK   131-641-1331
    ## 634  2332               DELTA MINNEAPOLIS-ST. PAUL (753) 726-0123
    ## 635  2817               DELTA          LOS ANGELES   609-332-7370
    ## 636  3182            AMERICAN     DALLAS-FT. WORTH   426-182-1365
    ## 637  3281           SOUTHWEST               DENVER (347) 782-5787
    ## 638   225            AMERICAN          LOS ANGELES   898-210-6218
    ## 639  1757           LUFTHANSA               MUNICH   658-861-4306
    ## 640   315              UNITED            SAN DIEGO (716) 184-1232
    ## 641   274              UNITED          LOS ANGELES   380-105-1757
    ## 642   465              UNITED               DENVER (969) 555-0453
    ## 643  1565         UNITED INTL            HONG KONG   343-973-0193
    ## 644  3218            AMERICAN          LOS ANGELES (265) 286-5671
    ## 645   867            AMERICAN     DALLAS-FT. WORTH   413-727-2672
    ## 646  1629          KOREAN AIR                SEOUL (229) 604-7790
    ## 647  1201           SOUTHWEST            LAS VEGAS (892) 301-0333
    ## 648   505              ALASKA             PORTLAND (331) 472-8624
    ## 649   564            EMIRATES                DUBAI   522-286-5318
    ## 650  3220            AMERICAN          LOS ANGELES   314-360-4288
    ## 651   528              ALASKA      PUERTO VALLARTA (301) 534-5754
    ## 652  1937      AIR FRANCE/KLM      PARIS-DE GAULLE (341) 473-0639
    ## 653  2444              UNITED             HONOLULU (835) 882-3693
    ## 654  2553             JETBLUE               BOSTON   465-550-6610
    ## 655  3022              UNITED             PORTLAND (589) 194-0523
    ## 656  2086            AMERICAN         PHILADELPHIA   221-190-1449
    ## 657  2360              UNITED               AUSTIN (322) 843-0185
    ## 658   390              ALASKA          LOS ANGELES   676-614-9095
    ## 659  1278         UNITED INTL      LONDON HEATHROW (190) 975-2514
    ## 660  1949      AIR FRANCE/KLM      PARIS-DE GAULLE (909) 382-3774
    ## 661  1936              UNITED               BOSTON (347) 896-3463
    ## 662   374              ALASKA    SAN JOSE DEL CABO   691-318-3535
    ## 663   592             JETBLUE               BOSTON   852-386-6029
    ## 664  1976              UNITED            BALTIMORE (355) 550-1392
    ## 665    12           SOUTHWEST          LOS ANGELES (705) 456-1905
    ## 666  1140           SOUTHWEST          LOS ANGELES (836) 207-8419
    ## 667  1652     VIRGIN ATLANTIC      LONDON HEATHROW (306) 552-1875
    ## 668  1907           LUFTHANSA               MUNICH   729-102-7511
    ## 669  2735               DELTA          LOS ANGELES   700-431-3918
    ## 670   501              UNITED              KAHULUI   887-657-4143
    ## 671  1207           SOUTHWEST            LAS VEGAS   574-438-5329
    ## 672  2449             JETBLUE               BOSTON (944) 189-7555
    ## 673  1016          AIR CANADA            VANCOUVER   998-931-4783
    ## 674  1347         UNITED INTL             SHANGHAI   362-178-6307
    ## 675  1645     VIRGIN ATLANTIC      LONDON HEATHROW (458) 404-9558
    ## 676  2883             EVA AIR               TAIPEI (481) 522-1039
    ## 677  3098                COPA          PANAMA CITY (376) 611-4588
    ## 678  1552               DELTA         NEW YORK-JFK (641) 544-6549
    ## 679  2927    TURKISH AIRLINES             ISTANBUL   797-870-7818
    ## 680  2291              QANTAS               SYDNEY   649-379-5361
    ## 681  2790                 WOW            REYKJAVIK (572) 748-6932
    ## 682   444              UNITED             HONOLULU   395-892-5646
    ## 683  1223           SOUTHWEST            LAS VEGAS   221-628-9561
    ## 684   946          AIR CANADA            VANCOUVER (227) 801-6148
    ## 685  2734      AIR FRANCE/KLM      PARIS-DE GAULLE   549-649-1864
    ## 686  1611     VIRGIN ATLANTIC      LONDON HEATHROW (519) 573-6576
    ## 687  1812              ALASKA       RALEIGH-DURHAM   919-342-0230
    ## 688  2840              UNITED       CHICAGO-O'HARE   364-759-2705
    ## 689   608             JETBLUE               BOSTON (900) 586-1787
    ## 690  2386         UNITED INTL               SYDNEY   308-607-9855
    ## 691  3183               DELTA          LOS ANGELES   472-337-8838
    ## 692  1884  SINGAPORE AIRLINES            SINGAPORE   365-832-0674
    ## 693  2813               DELTA          LOS ANGELES   123-282-3494
    ## 694   195            AMERICAN          LOS ANGELES   129-377-8159
    ## 695  1596  SINGAPORE AIRLINES            SINGAPORE   162-451-0594
    ## 696  1915      AIR FRANCE/KLM      PARIS-DE GAULLE (239) 325-5321
    ## 697  2207          AER LINGUS               DUBLIN   436-422-6171
    ## 698  1438              UNITED             PORTLAND   929-102-5905
    ## 699  2538             JETBLUE               BOSTON   452-811-8088
    ## 700  1423              UNITED          KANSAS CITY   119-444-0817
    ## 701  3059 PHILIPPINE AIRLINES               MANILA   945-998-0444
    ## 702   462              ALASKA            LAS VEGAS (367) 897-7969
    ## 703  1498        HAWAIIAN AIR             HONOLULU   594-176-5811
    ## 704  1642     VIRGIN ATLANTIC      LONDON HEATHROW (621) 874-9973
    ## 705   511              ALASKA             PORTLAND   894-593-7953
    ## 706  1587               DELTA       SALT LAKE CITY   561-266-7842
    ## 707  2587             JETBLUE               BOSTON   354-958-8052
    ## 708   666         UNITED INTL               MUNICH (426) 342-7378
    ## 709   441              UNITED             HONOLULU   333-520-4811
    ## 710  2786                 WOW            REYKJAVIK (765) 191-1797
    ## 711  2200          AER LINGUS               DUBLIN (400) 250-0871
    ## 712   752          AIR CANADA              TORONTO (434) 725-0561
    ## 713  1661            EMIRATES                DUBAI (969) 207-3261
    ## 714  1711      AIR FRANCE/KLM      PARIS-DE GAULLE   879-154-4494
    ## 715  2517              UNITED               NEWARK (994) 688-3259
    ## 716  2696              UNITED              ORLANDO   841-717-4447
    ## 717  2965            AMERICAN            CHARLOTTE   397-353-6309
    ## 718  2003      AIR FRANCE/KLM      PARIS-DE GAULLE   307-323-6861
    ## 719  2726      AIR FRANCE/KLM      PARIS-DE GAULLE (494) 308-3048
    ## 720  1933      AIR FRANCE/KLM      PARIS-DE GAULLE   102-957-6486
    ## 721  1666            EMIRATES                DUBAI (715) 288-8832
    ## 722  1398         UNITED INTL             SHANGHAI (594) 448-6242
    ## 723  2428     AIR NEW ZEALAND             AUCKLAND   182-227-4838
    ## 724  1572         UNITED INTL            HONG KONG   728-404-5558
    ## 725  2585             JETBLUE           LONG BEACH   427-665-3475
    ## 726  2545             JETBLUE           LONG BEACH   375-978-3305
    ## 727  3093              UNITED              SPOKANE   181-708-2089
    ## 728  2376             JETBLUE       FT. LAUDERDALE (802) 810-5574
    ## 729  2540             JETBLUE           LONG BEACH   242-540-4234
    ## 730  1890              UNITED               BOSTON (867) 891-0871
    ## 731  1702      AIR FRANCE/KLM      PARIS-DE GAULLE (533) 213-4368
    ## 732  1464              ALASKA         NEW YORK-JFK (324) 188-6781
    ## 733  1472              ALASKA         NEW YORK-JFK   397-362-5469
    ## 734   227            AMERICAN          LOS ANGELES (102) 928-7959
    ## 735  1872              ALASKA              KAHULUI (439) 568-6611
    ## 736  2024              UNITED               NEWARK (767) 205-0604
    ## 737  2127            FRONTIER               DENVER (890) 548-9219
    ## 738   332           SOUTHWEST              PHOENIX   769-472-2992
    ## 739   497              UNITED               DENVER (649) 925-8489
    ## 740   809              ALASKA             PORTLAND (156) 837-4491
    ## 741  1599  SINGAPORE AIRLINES            SINGAPORE   178-232-0815
    ## 742  2644             JETBLUE         NEW YORK-JFK (882) 304-9032
    ## 743  1979      CATHAY PACIFIC            HONG KONG (971) 548-6611
    ## 744  2638             JETBLUE         NEW YORK-JFK (828) 153-5819
    ## 745  3033              UNITED             PORTLAND   900-871-9056
    ## 746  2560             JETBLUE           LONG BEACH   406-167-1379
    ## 747  3071 PHILIPPINE AIRLINES               MANILA (906) 850-9192
    ## 748  1543           AIR CHINA              BEIJING (859) 777-8245
    ## 749  1238       CHINA EASTERN             SHANGHAI   641-635-8466
    ## 750  1731          AIR CANADA              TORONTO   807-671-6158
    ## 751  1968      CATHAY PACIFIC            HONG KONG (589) 270-7518
    ## 752  2541             JETBLUE           LONG BEACH   928-179-7556
    ## 753  2235      CATHAY PACIFIC            HONG KONG   905-903-5258
    ## 754  2210          AER LINGUS               DUBLIN (146) 129-5118
    ## 755  1359         UNITED INTL               KANSAI (927) 747-9822
    ## 756  1900              UNITED               BOSTON (481) 479-7013
    ## 757  1038          AIR CANADA            VANCOUVER (716) 777-3762
    ## 758  2566             JETBLUE           LONG BEACH   274-863-3205
    ## 759  2984            AMERICAN            CHARLOTTE (217) 589-0596
    ## 760  3137              ALASKA               NEWARK   731-813-2043
    ## 761  1476              ALASKA         NEW YORK-JFK   563-732-6802
    ## 762  1126               DELTA MINNEAPOLIS-ST. PAUL (637) 782-6989
    ## 763  2631             JETBLUE         NEW YORK-JFK (359) 803-9809
    ## 764   907          AIR CANADA              TORONTO (416) 788-2844
    ## 765  2952           SOUTHWEST          LOS ANGELES   311-305-4367
    ## 766  3166            AMERICAN     DALLAS-FT. WORTH   817-400-0481
    ## 767   527            EMIRATES                DUBAI (729) 609-4819
    ## 768  2559             JETBLUE               BOSTON (201) 737-4409
    ## 769  2599          AEROMEXICO          MEXICO CITY (137) 611-3694
    ## 770  1128               DELTA MINNEAPOLIS-ST. PAUL        665-803
    ## 771  2153            FRONTIER               DENVER (812) 869-6263
    ## 772  1777              UNITED       RALEIGH-DURHAM (194) 198-0504
    ## 773   999           LUFTHANSA               MUNICH (299) 137-6993
    ## 774   587              ALASKA WASHINGTON DC-DULLES   653-786-5985
    ## 775  2439     AIR NEW ZEALAND             AUCKLAND   362-136-1153
    ## 776   602              ALASKA WASHINGTON DC-DULLES   376-456-0697
    ## 777  2370              UNITED               AUSTIN   962-918-6117
    ## 778  2219           SOUTHWEST              PHOENIX   805-877-3887
    ## 779   383              ALASKA          LOS ANGELES (739) 710-2966
    ## 780  1331         UNITED INTL               KANSAI   588-693-9875
    ## 781  1647     VIRGIN ATLANTIC      LONDON HEATHROW   681-308-7915
    ## 782  2507              UNITED       CHICAGO-O'HARE   322-884-3020
    ## 783  1796              ALASKA       RALEIGH-DURHAM   176-313-5403
    ## 784  2150            FRONTIER               DENVER (477) 182-4689
    ## 785  3202      CATHAY PACIFIC            HONG KONG (525) 362-5532
    ## 786   542            EMIRATES                DUBAI (687) 887-6766
    ## 787  1501        HAWAIIAN AIR             HONOLULU   733-154-0094
    ## 788  1380         UNITED INTL      PARIS-DE GAULLE   291-830-3017
    ## 789  1346         UNITED INTL               KANSAI (637) 100-0509
    ## 790   697      AIR FRANCE/KLM      PARIS-DE GAULLE (603) 149-7268
    ## 791   836              ALASKA              SEATTLE (364) 792-5553
    ## 792  1918      AIR FRANCE/KLM      PARIS-DE GAULLE   496-429-1314
    ## 793  2879             EVA AIR               TAIPEI   486-268-3312
    ## 794  1839              ALASKA       RALEIGH-DURHAM   497-518-4050
    ## 795   445              UNITED         INDIANAPOLIS (535) 685-8273
    ## 796   928              ALASKA       RALEIGH-DURHAM (826) 738-8316
    ## 797  1852           SOUTHWEST            SAN DIEGO   724-134-3870
    ## 798   559              ALASKA              SEATTLE (554) 269-8937
    ## 799  2136      CATHAY PACIFIC            HONG KONG   125-578-4253
    ## 800   612             JETBLUE               BOSTON   487-232-4449
    ## 801  2529             JETBLUE           LONG BEACH (298) 135-0900
    ## 802   835              ALASKA            NASHVILLE (392) 183-7831
    ## 803  1002           LUFTHANSA               MUNICH (606) 596-1029
    ## 804   861            AMERICAN     DALLAS-FT. WORTH (384) 953-4795
    ## 805  2662              UNITED       RALEIGH-DURHAM (855) 811-8811
    ## 806  1620     VIRGIN ATLANTIC      LONDON HEATHROW   253-374-7102
    ## 807  3126              ALASKA               NEWARK (419) 295-9580
    ## 808   829              ALASKA              SEATTLE   787-624-8443
    ## 809  1822         UNITED INTL      LONDON HEATHROW (919) 486-4251
    ## 810  2556             JETBLUE           LONG BEACH (392) 495-7961
    ## 811  2935    TURKISH AIRLINES             ISTANBUL   473-238-3324
    ## 812  2266                 WOW            REYKJAVIK (506) 760-3043
    ## 813  2044     BRITISH AIRWAYS      LONDON HEATHROW   876-834-0624
    ## 814  2301              QANTAS               SYDNEY (146) 699-3488
    ## 815  1408              UNITED          KANSAS CITY   182-535-3412
    ## 816   558            EMIRATES                DUBAI (302) 339-0791
    ## 817  1919      AIR FRANCE/KLM      PARIS-DE GAULLE (446) 229-4342
    ## 818  1382         UNITED INTL      PARIS-DE GAULLE (249) 602-6985
    ## 819   808              ALASKA            BALTIMORE (150) 905-6938
    ## 820  2913    TURKISH AIRLINES             ISTANBUL   656-941-5355
    ## 821  3140               DELTA       SALT LAKE CITY (116) 689-6617
    ## 822  2212          AER LINGUS               DUBLIN (896) 993-8555
    ## 823  2416     AIR NEW ZEALAND             AUCKLAND   105-687-6500
    ## 824  2579             JETBLUE           LONG BEACH (466) 912-8401
    ## 825  2878             EVA AIR               TAIPEI   783-463-4865
    ## 826  3109              UNITED              SPOKANE   853-803-9900
    ## 827   363           LUFTHANSA            FRANKFURT (347) 851-5388
    ## 828   818              ALASKA            BALTIMORE   316-212-7309
    ## 829  1659            EMIRATES                DUBAI   381-883-5497
    ## 830   548            EMIRATES                DUBAI   100-531-4642
    ## 831   416              UNITED         HOUSTON-BUSH (441) 445-6532
    ## 832  2476              UNITED       CHICAGO-O'HARE   566-482-9004
    ## 833  1753           LUFTHANSA               MUNICH   491-727-7162
    ## 834   351           LUFTHANSA            FRANKFURT   167-336-5660
    ## 835  3222              ALASKA             PORTLAND (205) 382-5599
    ## 836  2671              UNITED       RALEIGH-DURHAM (208) 794-9612
    ## 837  1166           SOUTHWEST          LOS ANGELES   380-918-8572
    ## 838  2306            AMERICAN       CHICAGO-O'HARE (905) 742-3525
    ## 839  1892  SINGAPORE AIRLINES            SINGAPORE   185-321-6877
    ## 840   216            AMERICAN          LOS ANGELES (147) 535-3529
    ## 841  1768            AMERICAN       CHICAGO-O'HARE (152) 912-4118
    ## 842  3156               DELTA       SALT LAKE CITY (634) 521-4714
    ## 843  2719               DELTA          LOS ANGELES   670-248-0186
    ## 844  2756               DELTA              ATLANTA (121) 509-7306
    ## 845   214            AMERICAN          LOS ANGELES (105) 635-5212
    ## 846   212            AMERICAN          LOS ANGELES (732) 168-0110
    ## 847  2824              UNITED              BURBANK (214) 250-8756
    ## 848  2348              UNITED               AUSTIN (119) 975-8484
    ## 849  3245            AMERICAN              PHOENIX (489) 534-6272
    ## 850  3270              UNITED            LAS VEGAS (610) 716-5732
    ## 851  1118           SOUTHWEST            SAN DIEGO (456) 925-4236
    ## 852  1142           SOUTHWEST            SAN DIEGO   743-103-7645
    ## 853  1823              ALASKA       RALEIGH-DURHAM   432-281-3682
    ## 854  2096             JETBLUE           LONG BEACH (167) 144-9470
    ## 855   878              ALASKA              SEATTLE   946-558-5801
    ## 856  1618     VIRGIN ATLANTIC      LONDON HEATHROW (848) 149-5208
    ## 857  2485              UNITED               DENVER (970) 908-2298
    ## 858  2787               DELTA              ATLANTA (843) 120-5653
    ## 859  2028              UNITED               NEWARK   170-641-3537
    ## 860  1679      CATHAY PACIFIC            HONG KONG   814-895-6610
    ## 861  2880             EVA AIR               TAIPEI (139) 727-9901
    ## 862   941          AIR CANADA            VANCOUVER (817) 824-3849
    ## 863  2782               DELTA              ATLANTA   503-671-4901
    ## 864  1684      CATHAY PACIFIC            HONG KONG   538-393-2243
    ## 865  1999              UNITED            BALTIMORE (540) 362-7136
    ## 866  1312         UNITED INTL      LONDON HEATHROW (802) 910-1742
    ## 867  2563              UNITED       CHICAGO-O'HARE   784-458-8425
    ## 868  2690              UNITED       CHICAGO-O'HARE (365) 217-0634
    ## 869  2830              UNITED              BURBANK (594) 797-7729
    ## 870  2568             JETBLUE           LONG BEACH   477-307-3338
    ## 871  2570              UNITED       CHICAGO-O'HARE   744-301-1148
    ## 872  2646             JETBLUE         NEW YORK-JFK (900) 462-1379
    ## 873  2079             JETBLUE           LONG BEACH   878-636-2294
    ## 874   593             JETBLUE               BOSTON (998) 692-1900
    ## 875   802            AMERICAN          LOS ANGELES   304-225-5895
    ## 876  2851              UNITED              SEATTLE   931-522-5498
    ## 877  2591             JETBLUE               BOSTON (507) 483-3618
    ## 878  2303              UNITED          SAN ANTONIO (380) 449-7849
    ## 879  1675      AIR FRANCE/KLM      PARIS-DE GAULLE   589-975-0198
    ## 880   357           LUFTHANSA            FRANKFURT   501-668-7869
    ## 881  2495              UNITED       CHICAGO-O'HARE (706) 836-7047
    ## 882  2829              UNITED              BURBANK (698) 462-6742
    ## 883   537            EMIRATES                DUBAI   525-552-4162
    ## 884   762          AIR CANADA              TORONTO   635-714-8302
    ## 885  1364         UNITED INTL               KANSAI   453-556-0852
    ## 886   619              ALASKA               NEWARK   131-641-1331
    ## 887   768              ALASKA         NEW YORK-JFK (753) 726-0123
    ## 888  2271              UNITED          LOS ANGELES   609-332-7370
    ## 889   472              UNITED               DENVER   426-182-1365
    ## 890   644              ALASKA WASHINGTON DC-DULLES (347) 782-5787
    ## 891   417              UNITED              PHOENIX   898-210-6218
    ## 892  2633              UNITED       CHICAGO-O'HARE   658-861-4306
    ## 893   378              ALASKA    SAN JOSE DEL CABO (716) 184-1232
    ## 894  2201           SOUTHWEST            LAS VEGAS   380-105-1757
    ## 895  3221            AMERICAN          LOS ANGELES (969) 555-0453
    ## 896  2419     AIR NEW ZEALAND             AUCKLAND   343-973-0193
    ## 897  2639              UNITED       CHICAGO-O'HARE (265) 286-5671
    ## 898  2972            AMERICAN            CHARLOTTE   413-727-2672
    ## 899  1631          KOREAN AIR                SEOUL (162) 332-5838
    ## 900  1994              UNITED            BALTIMORE (229) 604-7790
    ## 901   310              UNITED WASHINGTON DC-DULLES (892) 301-0333
    ## 902   439              UNITED             HONOLULU (331) 472-8624
    ## 903  1697              ALASKA            SANTA ANA   522-286-5318
    ## 904  1146           SOUTHWEST          LOS ANGELES   314-360-4288
    ## 905  2641              UNITED       CHICAGO-O'HARE (301) 534-5754
    ## 906  2800               DELTA       SALT LAKE CITY (341) 473-0639
    ## 907  3052            AMERICAN                MIAMI (835) 882-3693
    ## 908   234            AMERICAN          LOS ANGELES   465-550-6610
    ## 909  1361         UNITED INTL               KANSAI (589) 194-0523
    ## 910   777         UNITED INTL    SAN JOSE DEL CABO   221-190-1449
    ## 911   606             JETBLUE               BOSTON (322) 843-0185
    ## 912   307           SOUTHWEST          LOS ANGELES   676-614-9095
    ## 913  1394         UNITED INTL      PARIS-DE GAULLE (190) 975-2514
    ## 914  2611          AEROMEXICO          MEXICO CITY (909) 382-3774
    ## 915  3094                COPA          PANAMA CITY (347) 896-3463
    ## 916   779            AMERICAN          LOS ANGELES   691-318-3535
    ## 917  1258       CHINA EASTERN             SHANGHAI   852-386-6029
    ## 918  1934      AIR FRANCE/KLM      PARIS-DE GAULLE (355) 550-1392
    ## 919  1475              ALASKA         NEW YORK-JFK (705) 456-1905
    ## 920  1742      CATHAY PACIFIC            HONG KONG (836) 207-8419
    ## 921  1940      AIR FRANCE/KLM      PARIS-DE GAULLE (306) 552-1875
    ## 922  2114         UNITED INTL          MEXICO CITY   729-102-7511
    ## 923  1867              UNITED          LOS ANGELES   700-431-3918
    ## 924   957              ALASKA         NEW YORK-JFK   887-657-4143
    ## 925  2295              QANTAS               SYDNEY   574-438-5329
    ## 926   991           LUFTHANSA               MUNICH (944) 189-7555
    ## 927   484              ALASKA             PORTLAND   998-931-4783
    ## 928  2110             JETBLUE           LONG BEACH   362-178-6307
    ## 929  2369             JETBLUE       FT. LAUDERDALE (458) 404-9558
    ## 930   395              ALASKA          LOS ANGELES (481) 522-1039
    ## 931  1456              ALASKA         NEW YORK-JFK (376) 611-4588
    ## 932  1625          KOREAN AIR                SEOUL (641) 544-6549
    ## 933  1494        HAWAIIAN AIR             HONOLULU   797-870-7818
    ## 934  3191               DELTA          LOS ANGELES   649-379-5361
    ## 935  1013          AIR CANADA            VANCOUVER (572) 748-6932
    ## 936  1663            EMIRATES                DUBAI   395-892-5646
    ## 937  1136           SOUTHWEST          LOS ANGELES   221-628-9561
    ## 938  1713      AIR FRANCE/KLM      PARIS-DE GAULLE (227) 801-6148
    ## 939  1185               DELTA         NEW YORK-JFK   549-649-1864
    ## 940  1956              UNITED               BOSTON (519) 573-6576
    ## 941  1988      AIR FRANCE/KLM            AMSTERDAM   919-342-0230
    ## 942  2220           SOUTHWEST              PHOENIX   364-759-2705
    ## 943  2798               DELTA       SALT LAKE CITY (900) 586-1787
    ## 944  2525              UNITED               NEWARK   308-607-9855
    ## 945  1640     VIRGIN ATLANTIC      LONDON HEATHROW   472-337-8838
    ## 946  1285      CHINA SOUTHERN                WUHAN   365-832-0674
    ## 947  2881             EVA AIR               TAIPEI   123-282-3494
    ## 948  2225          AER LINGUS               DUBLIN   129-377-8159
    ## 949  1843           SOUTHWEST            SAN DIEGO   162-451-0594
    ## 950   361           LUFTHANSA            FRANKFURT (239) 325-5321
    ## 951   454              UNITED     DALLAS-FT. WORTH   436-422-6171
    ## 952   799              ALASKA            BALTIMORE   929-102-5905
    ## 953  2237          AER LINGUS               DUBLIN   452-811-8088
    ## 954  3110              ALASKA               NEWARK   119-444-0817
    ## 955   926              ALASKA       RALEIGH-DURHAM   945-998-0444
    ## 956  1322         UNITED INTL      LONDON HEATHROW (367) 897-7969
    ## 957  1917      AIR FRANCE/KLM      PARIS-DE GAULLE   594-176-5811
    ## 958  1709      AIR FRANCE/KLM      PARIS-DE GAULLE (621) 874-9973
    ## 959  3210            AMERICAN          LOS ANGELES   894-593-7953
    ## 960  2737              UNITED              KAHULUI   561-266-7842
    ## 961  1390         UNITED INTL      PARIS-DE GAULLE   354-958-8052
    ## 962  2364              UNITED               AUSTIN (426) 342-7378
    ## 963   433              UNITED             HONOLULU   333-520-4811
    ## 964  3075              UNITED              SPOKANE (765) 191-1797
    ## 965   758              ALASKA         NEW YORK-JFK (400) 250-0871
    ## 966  1689      CATHAY PACIFIC            HONG KONG (434) 725-0561
    ## 967  1762           LUFTHANSA               MUNICH (969) 207-3261
    ## 968  2124             JETBLUE           LONG BEACH   879-154-4494
    ## 969  2100             JETBLUE           LONG BEACH (994) 688-3259
    ## 970   787            AMERICAN          LOS ANGELES   841-717-4447
    ## 971  2130      CATHAY PACIFIC            HONG KONG   397-353-6309
    ## 972  2625              UNITED       CHICAGO-O'HARE   307-323-6861
    ## 973  1512               DELTA          LOS ANGELES (494) 308-3048
    ## 974  2121         UNITED INTL          MEXICO CITY   102-957-6486
    ## 975  2875             EVA AIR               TAIPEI (715) 288-8832
    ## 976   414              UNITED         HOUSTON-BUSH (594) 448-6242
    ## 977  2185              QANTAS               SYDNEY   182-227-4838
    ## 978   300              UNITED            SAN DIEGO   728-404-5558
    ## 979  1734          AIR CANADA              TORONTO   427-665-3475
    ## 980  2326              QANTAS               SYDNEY   375-978-3305
    ## 981  2361             JETBLUE       FT. LAUDERDALE   181-708-2089
    ## 982  2314              UNITED          SAN ANTONIO (802) 810-5574
    ## 983   509              ALASKA             PORTLAND   242-540-4234
    ## 984  1162           SOUTHWEST          LOS ANGELES (867) 891-0871
    ## 985  1296         UNITED INTL      LONDON HEATHROW (533) 213-4368
    ## 986  2711              UNITED        SANTA BARBARA (324) 188-6781
    ## 987  3088              UNITED              SPOKANE   397-362-5469
    ## 988  2098             JETBLUE           LONG BEACH (102) 928-7959
    ## 989  2664          AEROMEXICO          MEXICO CITY (439) 568-6611
    ## 990  2302            AMERICAN       CHICAGO-O'HARE (767) 205-0604
    ## 991   713      AIR FRANCE/KLM      PARIS-DE GAULLE (890) 548-9219
    ## 992  1641     VIRGIN ATLANTIC      LONDON HEATHROW   769-472-2992
    ## 993  3199      CATHAY PACIFIC            HONG KONG (649) 925-8489
    ## 994  3072              UNITED              SPOKANE (156) 837-4491
    ## 995  1519               DELTA          LOS ANGELES   178-232-0815
    ## 996  2066            AMERICAN         PHILADELPHIA (882) 304-9032
    ## 997  2123             JETBLUE           LONG BEACH (971) 548-6611
    ## 998  2065              UNITED               NEWARK (828) 153-5819
    ## 999  2705              UNITED        SANTA BARBARA   900-871-9056
    ## 1000  716      CATHAY PACIFIC            HONG KONG   406-167-1379
    ## 1001 3236              ALASKA             PORTLAND (906) 850-9192
    ## 1002 1765            AMERICAN       CHICAGO-O'HARE (859) 777-8245
    ## 1003  533              ALASKA      PUERTO VALLARTA   641-635-8466
    ## 1004 3276           SOUTHWEST               DENVER   807-671-6158
    ## 1005  790         UNITED INTL    SAN JOSE DEL CABO (589) 270-7518
    ## 1006  504              UNITED              KAHULUI   928-179-7556
    ## 1007 3167               DELTA          LOS ANGELES   905-903-5258
    ## 1008  708      AIR FRANCE/KLM      PARIS-DE GAULLE (146) 129-5118
    ## 1009 2775               DELTA              ATLANTA (927) 747-9822
    ## 1010  923              ALASKA       RALEIGH-DURHAM (481) 479-7013
    ## 1011 2940    TURKISH AIRLINES             ISTANBUL (716) 777-3762
    ## 1012  261          AIR CANADA              TORONTO   274-863-3205
    ## 1013 2825              UNITED              BURBANK (217) 589-0596
    ## 1014 2440     AIR NEW ZEALAND             AUCKLAND   731-813-2043
    ## 1015 2839              UNITED       CHICAGO-O'HARE   563-732-6802
    ## 1016 2860              UNITED              SEATTLE (637) 782-6989
    ## 1017 2146            FRONTIER               DENVER (359) 803-9809
    ## 1018 2474              ALASKA       SALT LAKE CITY (416) 788-2844
    ## 1019 2506              ALASKA       SALT LAKE CITY   311-305-4367
    ## 1020 2349              UNITED               AUSTIN   817-400-0481
    ## 1021 3216            AMERICAN          LOS ANGELES (729) 609-4819
    ## 1022 3164            AMERICAN     DALLAS-FT. WORTH (201) 737-4409
    ## 1023 1261       CHINA EASTERN             SHANGHAI (137) 611-3694
    ## 1024  348           LUFTHANSA            FRANKFURT (812) 869-6263
    ## 1025  646              ALASKA WASHINGTON DC-DULLES (194) 198-0504
    ## 1026 1430              UNITED            LAS VEGAS (299) 137-6993
    ## 1027 1568         UNITED INTL            HONG KONG   653-786-5985
    ## 1028 2270                 WOW            REYKJAVIK   362-136-1153
    ## 1029 3177               DELTA          LOS ANGELES   376-456-0697
    ## 1030 2157            FRONTIER               DENVER   962-918-6117
    ## 1031 1948              UNITED               BOSTON   805-877-3887
    ## 1032 2050     BRITISH AIRWAYS      LONDON HEATHROW (739) 710-2966
    ## 1033 3103              ALASKA               NEWARK   588-693-9875
    ## 1034  839              ALASKA              SEATTLE   681-308-7915
    ## 1035  408              UNITED              PHOENIX   322-884-3020
    ## 1036 1630          KOREAN AIR                SEOUL   176-313-5403
    ## 1037 1517               DELTA          LOS ANGELES (477) 182-4689
    ## 1038 2037              UNITED               NEWARK (525) 362-5532
    ## 1039 2684              UNITED              ORLANDO (687) 887-6766
    ## 1040 1716      AIR FRANCE/KLM      PARIS-DE GAULLE   733-154-0094
    ## 1041 1531              ALASKA          LOS ANGELES   291-830-3017
    ## 1042 2351              UNITED               AUSTIN (637) 100-0509
    ## 1043 2216           SOUTHWEST              PHOENIX (603) 149-7268
    ## 1044  549              ALASKA              SEATTLE (364) 792-5553
    ## 1045  782         UNITED INTL    SAN JOSE DEL CABO   496-429-1314
    ## 1046 1425              UNITED            LAS VEGAS   486-268-3312
    ## 1047 1828         UNITED INTL      LONDON HEATHROW   497-518-4050
    ## 1048  967           LUFTHANSA               MUNICH (535) 685-8273
    ## 1049 2010              UNITED               NEWARK (826) 738-8316
    ## 1050 1409              UNITED          KANSAS CITY   724-134-3870
    ## 1051 2461              UNITED               DENVER (554) 269-8937
    ## 1052  883              ALASKA              SEATTLE   125-578-4253
    ## 1053 2090             JETBLUE           LONG BEACH   487-232-4449
    ## 1054  979           LUFTHANSA               MUNICH (298) 135-0900
    ## 1055  854              ALASKA             PORTLAND (392) 183-7831
    ## 1056 1637          KOREAN AIR                SEOUL (606) 596-1029
    ## 1057 3239            AMERICAN              PHOENIX (384) 953-4795
    ## 1058  816              ALASKA            BALTIMORE (855) 811-8811
    ## 1059 1154           SOUTHWEST          LOS ANGELES   253-374-7102
    ## 1060 1145           SOUTHWEST            SAN DIEGO (419) 295-9580
    ## 1061  853            AMERICAN     DALLAS-FT. WORTH   787-624-8443
    ## 1062 2075            AMERICAN         PHILADELPHIA (919) 486-4251
    ## 1063 1993              UNITED            BALTIMORE (392) 495-7961
    ## 1064 2081             JETBLUE           LONG BEACH   473-238-3324
    ## 1065 2598              UNITED       CHICAGO-O'HARE (506) 760-3043
    ## 1066  590              ALASKA WASHINGTON DC-DULLES   876-834-0624
    ## 1067 2832              UNITED              BURBANK (146) 699-3488
    ## 1068 2656              UNITED       RALEIGH-DURHAM   182-535-3412
    ## 1069  679              UNITED               NEWARK (302) 339-0791
    ## 1070 1772            AMERICAN       CHICAGO-O'HARE (446) 229-4342
    ## 1071 2052              UNITED               NEWARK (249) 602-6985
    ## 1072 2821               DELTA          LOS ANGELES (150) 905-6938
    ## 1073 3130               DELTA       SALT LAKE CITY   656-941-5355
    ## 1074 2502              UNITED       CHICAGO-O'HARE (116) 689-6617
    ## 1075 1084          AIR CANADA              CALGARY (896) 993-8555
    ## 1076 2947           SOUTHWEST          LOS ANGELES   105-687-6500
    ## 1077  955          AIR CANADA            VANCOUVER (466) 912-8401
    ## 1078 2955           SOUTHWEST          LOS ANGELES   783-463-4865
    ## 1079 1593           SOUTHWEST          LOS ANGELES   853-803-9900
    ## 1080 2257              UNITED          LOS ANGELES (347) 851-5388
    ## 1081 2823              UNITED              BURBANK   316-212-7309
    ## 1082 3271              UNITED            LAS VEGAS   381-883-5497
    ## 1083 2770              UNITED               AUSTIN   100-531-4642
    ## 1084  801              ALASKA            BALTIMORE (441) 445-6532
    ## 1085  447              UNITED         INDIANAPOLIS   566-482-9004
    ## 1086 2901               DELTA       SALT LAKE CITY   491-727-7162
    ## 1087 1450              UNITED            LAS VEGAS   167-336-5660
    ## 1088 2581             JETBLUE               BOSTON (205) 382-5599
    ## 1089 2760               DELTA              ATLANTA (208) 794-9612
    ## 1090 1894           LUFTHANSA               MUNICH   380-918-8572
    ## 1091 1323         UNITED INTL      LONDON HEATHROW (905) 742-3525
    ## 1092 1061          AIR CANADA              CALGARY   185-321-6877
    ## 1093   13           SOUTHWEST          LOS ANGELES (147) 535-3529
    ## 1094 2576             JETBLUE           LONG BEACH (152) 912-4118
    ## 1095 2528             JETBLUE           LONG BEACH (634) 521-4714
    ## 1096 1151           SOUTHWEST          LOS ANGELES   670-248-0186
    ## 1097 2532             JETBLUE           LONG BEACH (121) 509-7306
    ## 1098 3128              UNITED            SAN DIEGO (105) 635-5212
    ## 1099   14           SOUTHWEST          LOS ANGELES (732) 168-0110
    ## 1100 1537               DELTA         NEW YORK-JFK (214) 250-8756
    ## 1101 2304            AMERICAN       CHICAGO-O'HARE (119) 975-8484
    ## 1102  430              UNITED         INDIANAPOLIS (489) 534-6272
    ## 1103  800            AMERICAN          LOS ANGELES (610) 716-5732
    ## 1104  871              ALASKA              SEATTLE (456) 925-4236
    ## 1105 1544               DELTA         NEW YORK-JFK   743-103-7645
    ## 1106 2056              UNITED               NEWARK   432-281-3682
    ## 1107 2300            AMERICAN       CHICAGO-O'HARE (167) 144-9470
    ## 1108 3247            AMERICAN              PHOENIX   946-558-5801
    ## 1109 1913      AIR FRANCE/KLM      PARIS-DE GAULLE (848) 149-5208
    ## 1110 2423     AIR NEW ZEALAND             AUCKLAND (970) 908-2298
    ## 1111 2430              UNITED             HONOLULU (843) 120-5653
    ## 1112  759          AIR CANADA              TORONTO   170-641-3537
    ## 1113  597              ALASKA WASHINGTON DC-DULLES   814-895-6610
    ## 1114 1726          AIR CANADA              TORONTO (139) 727-9901
    ## 1115 3003            AMERICAN                MIAMI (817) 824-3849
    ## 1116 2917               DELTA              SEATTLE   503-671-4901
    ## 1117 3006            AMERICAN                MIAMI   538-393-2243
    ## 1118 1493            FRONTIER               DENVER (540) 362-7136
    ## 1119 1592           SOUTHWEST          LOS ANGELES (802) 910-1742
    ## 1120 1357         UNITED INTL             SHANGHAI   784-458-8425
    ## 1121 1873              UNITED               BOSTON (365) 217-0634
    ## 1122 2853              UNITED              SEATTLE (594) 797-7729
    ## 1123 2174            FRONTIER               DENVER   477-307-3338
    ## 1124 3050            AMERICAN          LOS ANGELES   744-301-1148
    ## 1125 2774                 WOW            REYKJAVIK (900) 462-1379
    ## 1126  379              ALASKA    SAN JOSE DEL CABO   878-636-2294
    ## 1127  455              ALASKA            LAS VEGAS (998) 692-1900
    ## 1128 2637              UNITED       CHICAGO-O'HARE   304-225-5895
    ## 1129 2779                 WOW            REYKJAVIK   931-522-5498
    ## 1130  884              ALASKA              SEATTLE (507) 483-3618
    ## 1131 1793              UNITED       RALEIGH-DURHAM (380) 449-7849
    ## 1132 2273                 WOW            REYKJAVIK   589-975-0198
    ## 1133 3117              UNITED              SPOKANE   501-668-7869
    ## 1134  411              UNITED              PHOENIX (706) 836-7047
    ## 1135 1665            EMIRATES                DUBAI (698) 462-6742
    ## 1136 2484              UNITED       CHICAGO-O'HARE   525-552-4162
    ## 1137 1446              UNITED            LAS VEGAS   635-714-8302
    ## 1138 1991              UNITED            BALTIMORE   453-556-0852
    ## 1139 1324         UNITED INTL      LONDON HEATHROW   131-641-1331
    ## 1140 1638          KOREAN AIR                SEOUL (753) 726-0123
    ## 1141  640              ALASKA          NEW ORLEANS   609-332-7370
    ## 1142 2197          AER LINGUS               DUBLIN   426-182-1365
    ## 1143  397              ALASKA          LOS ANGELES (347) 782-5787
    ## 1144  864            AMERICAN     DALLAS-FT. WORTH   898-210-6218
    ## 1145 1863              ALASKA              KAHULUI   658-861-4306
    ## 1146 1677      AIR FRANCE/KLM      PARIS-DE GAULLE (716) 184-1232
    ## 1147  896              ALASKA       RALEIGH-DURHAM   380-105-1757
    ## 1148 1384         UNITED INTL      PARIS-DE GAULLE (969) 555-0453
    ## 1149 1023          AIR CANADA            VANCOUVER   343-973-0193
    ## 1150 1242         UNITED INTL         TOKYO-NARITA (265) 286-5671
    ## 1151 3186               DELTA          LOS ANGELES   413-727-2672
    ## 1152 1807         UNITED INTL      LONDON HEATHROW (162) 332-5838
    ## 1153 1505        HAWAIIAN AIR             HONOLULU (229) 604-7790
    ## 1154  367              ALASKA    SAN JOSE DEL CABO (892) 301-0333
    ## 1155 1286         UNITED INTL      LONDON HEATHROW (331) 472-8624
    ## 1156 1449              UNITED            LAS VEGAS   522-286-5318
    ## 1157 2710      AIR FRANCE/KLM      PARIS-DE GAULLE   314-360-4288
    ## 1158  317              UNITED            SAN DIEGO (301) 534-5754
    ## 1159 1886  SINGAPORE AIRLINES            SINGAPORE (341) 473-0639
    ## 1160 2534             JETBLUE               BOSTON (835) 882-3693
    ## 1161 1953      AIR FRANCE/KLM      PARIS-DE GAULLE   465-550-6610
    ## 1162 2954           SOUTHWEST          LOS ANGELES (589) 194-0523
    ## 1163 2648             JETBLUE               BOSTON   221-190-1449
    ## 1164 1959              UNITED               BOSTON (322) 843-0185
    ## 1165 2706      AIR FRANCE/KLM      PARIS-DE GAULLE   676-614-9095
    ## 1166 1553           AIR CHINA              BEIJING (190) 975-2514
    ## 1167 2694              UNITED       CHICAGO-O'HARE (909) 382-3774
    ## 1168    2           SOUTHWEST            SAN DIEGO (347) 896-3463
    ## 1169 1236           SOUTHWEST            LAS VEGAS   691-318-3535
    ## 1170 1615     VIRGIN ATLANTIC      LONDON HEATHROW   852-386-6029
    ## 1171  868            AMERICAN     DALLAS-FT. WORTH (355) 550-1392
    ## 1172 1413              UNITED            LAS VEGAS (705) 456-1905
    ## 1173 3266              UNITED            LAS VEGAS (836) 207-8419
    ## 1174 2492              UNITED               DENVER (306) 552-1875
    ## 1175 1931      AIR FRANCE/KLM      PARIS-DE GAULLE   729-102-7511
    ## 1176  951          AIR CANADA            VANCOUVER   700-431-3918
    ## 1177  638              ALASKA WASHINGTON DC-DULLES   887-657-4143
    ## 1178  197            AMERICAN          LOS ANGELES   574-438-5329
    ## 1179 1172               DELTA         NEW YORK-JFK (944) 189-7555
    ## 1180 1232           SOUTHWEST            LAS VEGAS   998-931-4783
    ## 1181 1260       CHINA EASTERN             SHANGHAI   362-178-6307
    ## 1182 1401         UNITED INTL      PARIS-DE GAULLE (458) 404-9558
    ## 1183 1703      AIR FRANCE/KLM      PARIS-DE GAULLE (481) 522-1039
    ## 1184 1767            AMERICAN       CHICAGO-O'HARE (376) 611-4588
    ## 1185 2019              UNITED               NEWARK (641) 544-6549
    ## 1186 2054              UNITED               NEWARK   797-870-7818
    ## 1187 2642             JETBLUE         NEW YORK-JFK   649-379-5361
    ## 1188 2695      AIR FRANCE/KLM      PARIS-DE GAULLE (572) 748-6932
    ## 1189 2960            AMERICAN         NEW YORK-JFK   395-892-5646
    ## 1190 2997            AMERICAN                MIAMI   221-628-9561
    ## 1191 3001            AMERICAN                MIAMI (227) 801-6148
    ## 1192 3049            AMERICAN          LOS ANGELES   549-649-1864
    ## 1193 2816               DELTA          LOS ANGELES (519) 573-6576
    ## 1194 2855              UNITED              SEATTLE   919-342-0230
    ## 1195 1211           SOUTHWEST            LAS VEGAS   364-759-2705
    ## 1196 1506        HAWAIIAN AIR             HONOLULU (900) 586-1787
    ## 1197 2443             JETBLUE               BOSTON   308-607-9855
    ## 1198  345              ALASKA            SAN DIEGO   472-337-8838
    ## 1199 3029            AMERICAN     DALLAS-FT. WORTH   365-832-0674
    ## 1200 1435              UNITED             PORTLAND   123-282-3494
    ## 1201 2477              UNITED             HONOLULU   129-377-8159
    ## 1202 1616     VIRGIN ATLANTIC      LONDON HEATHROW   162-451-0594
    ## 1203 3196               DELTA          LOS ANGELES (239) 325-5321
    ## 1204  563              ALASKA              SEATTLE   436-422-6171
    ## 1205  457              ALASKA            LAS VEGAS   929-102-5905
    ## 1206  972           LUFTHANSA               MUNICH   452-811-8088
    ## 1207  354           LUFTHANSA            FRANKFURT   119-444-0817
    ## 1208 1301             JETBLUE         NEW YORK-JFK   945-998-0444
    ## 1209 1608     VIRGIN ATLANTIC      LONDON HEATHROW (367) 897-7969
    ## 1210 1878  SINGAPORE AIRLINES            SINGAPORE   594-176-5811
    ## 1211 2427     AIR NEW ZEALAND             AUCKLAND (621) 874-9973
    ## 1212  398              ALASKA         INDIANAPOLIS   894-593-7953
    ## 1213  675              UNITED               NEWARK   561-266-7842
    ## 1214  944          AIR CANADA            VANCOUVER   354-958-8052
    ## 1215 1407              UNITED            LAS VEGAS (426) 342-7378
    ## 1216 1466              ALASKA         NEW YORK-JFK   333-520-4811
    ## 1217 1610     VIRGIN ATLANTIC      LONDON HEATHROW (765) 191-1797
    ## 1218 1800         UNITED INTL      LONDON HEATHROW (400) 250-0871
    ## 1219 1923      AIR FRANCE/KLM      PARIS-DE GAULLE (434) 725-0561
    ## 1220 2173            FRONTIER               DENVER (969) 207-3261
    ## 1221 2397         UNITED INTL              BEIJING   879-154-4494
    ## 1222 2600            INTERJET          GUADALAJARA (994) 688-3259
    ## 1223 2607            INTERJET          GUADALAJARA   841-717-4447
    ## 1224 2793               DELTA       SALT LAKE CITY   397-353-6309
    ## 1225 3111                COPA          PANAMA CITY   307-323-6861
    ## 1226 1623     VIRGIN ATLANTIC      LONDON HEATHROW (494) 308-3048
    ## 1227 1901           LUFTHANSA               MUNICH   102-957-6486
    ## 1228 3084                COPA          PANAMA CITY (715) 288-8832
    ## 1229 2899               DELTA       SALT LAKE CITY (594) 448-6242
    ## 1230 2435              UNITED             HONOLULU   182-227-4838
    ## 1231 1419              UNITED          KANSAS CITY   728-404-5558
    ## 1232 3233              ALASKA             PORTLAND   427-665-3475
    ## 1233 2438              UNITED             HONOLULU   375-978-3305
    ## 1234 1338         UNITED INTL               KANSAI   181-708-2089
    ## 1235  463              ALASKA            LAS VEGAS (802) 810-5574
    ## 1236 2107         UNITED INTL          MEXICO CITY   242-540-4234
    ## 1237 2309              UNITED          SAN ANTONIO (867) 891-0871
    ## 1238 2864              UNITED              SEATTLE (533) 213-4368
    ## 1239 2593              UNITED       CHICAGO-O'HARE (324) 188-6781
    ## 1240 1117               DELTA MINNEAPOLIS-ST. PAUL   397-362-5469
    ## 1241 1847           SOUTHWEST            SAN DIEGO (102) 928-7959
    ## 1242 2141            FRONTIER               DENVER (439) 568-6611
    ## 1243  298              UNITED            SAN DIEGO (767) 205-0604
    ## 1244 2603             JETBLUE               BOSTON (890) 548-9219
    ## 1245 1832              ALASKA       RALEIGH-DURHAM   769-472-2992
    ## 1246 2040              UNITED               NEWARK (649) 925-8489
    ## 1247 2862              UNITED              SEATTLE (156) 837-4491
    ## 1248  436              UNITED         INDIANAPOLIS   178-232-0815
    ## 1249  415              UNITED              PHOENIX (882) 304-9032
    ## 1250 3248            AMERICAN              PHOENIX (971) 548-6611
    ## 1251  715      CATHAY PACIFIC            HONG KONG (828) 153-5819
    ## 1252 3123              ALASKA               NEWARK   900-871-9056
    ## 1253 2547             JETBLUE           LONG BEACH   406-167-1379
    ## 1254 2574             JETBLUE           LONG BEACH (906) 850-9192
    ## 1255 3108                COPA          PANAMA CITY (859) 777-8245
    ## 1256 1177               DELTA         NEW YORK-JFK   641-635-8466
    ## 1257 1805         UNITED INTL      LONDON HEATHROW   807-671-6158
    ## 1258  500              ALASKA             PORTLAND (589) 270-7518
    ## 1259  335           SOUTHWEST              PHOENIX   928-179-7556
    ## 1260  943          AIR CANADA            VANCOUVER   905-903-5258
    ## 1261 1295             JETBLUE         NEW YORK-JFK (146) 129-5118
    ## 1262  736      CATHAY PACIFIC            HONG KONG (927) 747-9822
    ## 1263 1482            FRONTIER               DENVER (481) 479-7013
    ## 1264  392              ALASKA          LOS ANGELES (716) 777-3762
    ## 1265 1160           SOUTHWEST          LOS ANGELES   274-863-3205
    ## 1266 2594            INTERJET          GUADALAJARA (217) 589-0596
    ## 1267   10           SOUTHWEST          LOS ANGELES   731-813-2043
    ## 1268  513              ALASKA             PORTLAND   563-732-6802
    ## 1269 1131               DELTA MINNEAPOLIS-ST. PAUL (637) 782-6989
    ## 1270  641              ALASKA WASHINGTON DC-DULLES (359) 803-9809
    ## 1271 2083             JETBLUE           LONG BEACH (416) 788-2844
    ## 1272 2316              UNITED          SAN ANTONIO   311-305-4367
    ## 1273 2692      AIR FRANCE/KLM      PARIS-DE GAULLE   817-400-0481
    ## 1274  580              ALASKA              SEATTLE (729) 609-4819
    ## 1275  859            AMERICAN     DALLAS-FT. WORTH (201) 737-4409
    ## 1276 1578               DELTA              DETROIT (137) 611-3694
    ## 1277 2715               DELTA          LOS ANGELES (812) 869-6263
    ## 1278  749              ALASKA         NEW YORK-JFK (194) 198-0504
    ## 1279 3171               DELTA          LOS ANGELES (299) 137-6993
    ## 1280 1784              UNITED       RALEIGH-DURHAM   653-786-5985
    ## 1281 1148           SOUTHWEST          LOS ANGELES   362-136-1153
    ## 1282 3037            AMERICAN     DALLAS-FT. WORTH   376-456-0697
    ## 1283 3278           SOUTHWEST               DENVER   962-918-6117
    ## 1284 2147      CATHAY PACIFIC            HONG KONG   805-877-3887
    ## 1285 1105               DELTA MINNEAPOLIS-ST. PAUL (739) 710-2966
    ## 1286 1110           SOUTHWEST            SAN DIEGO   588-693-9875
    ## 1287 1113               DELTA MINNEAPOLIS-ST. PAUL   681-308-7915
    ## 1288 2362             JETBLUE       FT. LAUDERDALE   322-884-3020
    ## 1289 2509              ALASKA       SALT LAKE CITY   176-313-5403
    ## 1290 1908              UNITED               BOSTON (477) 182-4689
    ## 1291  438              UNITED         INDIANAPOLIS (525) 362-5532
    ## 1292 2108             JETBLUE           LONG BEACH (687) 887-6766
    ## 1293 1669            EMIRATES                DUBAI   733-154-0094
    ## 1294 1874            AMERICAN       CHICAGO-O'HARE   291-830-3017
    ## 1295  521            EMIRATES                DUBAI (637) 100-0509
    ## 1296  290           SOUTHWEST          LOS ANGELES (603) 149-7268
    ## 1297  319           SOUTHWEST          LOS ANGELES (364) 792-5553
    ## 1298  377              ALASKA          LOS ANGELES   496-429-1314
    ## 1299  765          AIR CANADA              TORONTO   486-268-3312
    ## 1300 1761           LUFTHANSA               MUNICH   497-518-4050
    ## 1301 1797              ALASKA       RALEIGH-DURHAM (535) 685-8273
    ## 1302 2143            FRONTIER               DENVER (826) 738-8316
    ## 1303 2554             JETBLUE           LONG BEACH   724-134-3870
    ## 1304 2588             JETBLUE           LONG BEACH (554) 269-8937
    ## 1305 2976            AMERICAN            CHARLOTTE   125-578-4253
    ## 1306 2367             JETBLUE       FT. LAUDERDALE   487-232-4449
    ## 1307 1899           LUFTHANSA               MUNICH (298) 135-0900
    ## 1308 1483             AVIANCA         SAN SALVADOR (392) 183-7831
    ## 1309 1391         UNITED INTL             SHANGHAI (606) 596-1029
    ## 1310 3042              UNITED             PORTLAND (384) 953-4795
    ## 1311 1996              UNITED            BALTIMORE (855) 811-8811
    ## 1312 1549           AIR CHINA              BEIJING   253-374-7102
    ## 1313 2497              UNITED       CHICAGO-O'HARE (419) 295-9580
    ## 1314 2645              UNITED       CHICAGO-O'HARE   787-624-8443
    ## 1315 2551             JETBLUE               BOSTON (919) 486-4251
    ## 1316 2906    TURKISH AIRLINES             ISTANBUL (392) 495-7961
    ## 1317 3260              UNITED            SAN DIEGO   473-238-3324
    ## 1318 1850           SOUTHWEST            SAN DIEGO (506) 760-3043
    ## 1319 3115              UNITED              SPOKANE   876-834-0624
    ## 1320 1704      AIR FRANCE/KLM      PARIS-DE GAULLE (146) 699-3488
    ## 1321  249              UNITED            NASHVILLE   182-535-3412
    ## 1322 2334               DELTA MINNEAPOLIS-ST. PAUL (302) 339-0791
    ## 1323 1386         UNITED INTL             SHANGHAI (446) 229-4342
    ## 1324 3285           SOUTHWEST               DENVER (249) 602-6985
    ## 1325 2073            AMERICAN         PHILADELPHIA (150) 905-6938
    ## 1326 1766            AMERICAN       CHICAGO-O'HARE   656-941-5355
    ## 1327 2944           SOUTHWEST            SAN DIEGO (116) 689-6617
    ## 1328 2513              UNITED               NEWARK (896) 993-8555
    ## 1329 1169           SOUTHWEST          LOS ANGELES   105-687-6500
    ## 1330 1319             JETBLUE         NEW YORK-JFK (466) 912-8401
    ## 1331  628              ALASKA WASHINGTON DC-DULLES   783-463-4865
    ## 1332 1685      CATHAY PACIFIC            HONG KONG   853-803-9900
    ## 1333  426              UNITED         INDIANAPOLIS (347) 851-5388
    ## 1334  900              ALASKA       RALEIGH-DURHAM   316-212-7309
    ## 1335  369              ALASKA    SAN JOSE DEL CABO   381-883-5497
    ## 1336 2565            INTERJET          GUADALAJARA   100-531-4642
    ## 1337 1290             JETBLUE         NEW YORK-JFK (441) 445-6532
    ## 1338 2857              UNITED              SEATTLE   566-482-9004
    ## 1339 2582              UNITED       CHICAGO-O'HARE   491-727-7162
    ## 1340 2720      AIR FRANCE/KLM      PARIS-DE GAULLE   167-336-5660
    ## 1341  650              ALASKA WASHINGTON DC-DULLES (205) 382-5599
    ## 1342 2518              ALASKA       SALT LAKE CITY (208) 794-9612
    ## 1343  571            EMIRATES                DUBAI   380-918-8572
    ## 1344  717      AIR FRANCE/KLM      PARIS-DE GAULLE (905) 742-3525
    ## 1345 2586              UNITED       CHICAGO-O'HARE   185-321-6877
    ## 1346 2232           SOUTHWEST            SANTA ANA (147) 535-3529
    ## 1347 2400         UNITED INTL              BEIJING (152) 912-4118
    ## 1348  452              UNITED     DALLAS-FT. WORTH (634) 521-4714
    ## 1349 1613     VIRGIN ATLANTIC      LONDON HEATHROW   670-248-0186
    ## 1350 2515              ALASKA       SALT LAKE CITY (121) 509-7306
    ## 1351  568            EMIRATES                DUBAI (105) 635-5212
    ## 1352 2269              UNITED          LOS ANGELES (732) 168-0110
    ## 1353 1554               DELTA         NEW YORK-JFK (214) 250-8756
    ## 1354 1556               DELTA         NEW YORK-JFK (119) 975-8484
    ## 1355 2661          AEROMEXICO          MEXICO CITY (489) 534-6272
    ## 1356 2842              UNITED       CHICAGO-O'HARE (610) 716-5732
    ## 1357 3162              UNITED            SAN DIEGO (456) 925-4236
    ## 1358 2129         UNITED INTL          MEXICO CITY   743-103-7645
    ## 1359  686      CATHAY PACIFIC            HONG KONG   432-281-3682
    ## 1360 2168            FRONTIER               DENVER (167) 144-9470
    ## 1361 1962              UNITED               BOSTON   946-558-5801
    ## 1362 3016              UNITED             PORTLAND (848) 149-5208
    ## 1363 1604     VIRGIN ATLANTIC      LONDON HEATHROW (970) 908-2298
    ## 1364 2402         UNITED INTL              BEIJING (843) 120-5653
    ## 1365  368           LUFTHANSA            FRANKFURT   170-641-3537
    ## 1366  672      AIR FRANCE/KLM      PARIS-DE GAULLE   814-895-6610
    ## 1367 2667          AEROMEXICO          MEXICO CITY (139) 727-9901
    ## 1368 2743               DELTA              ATLANTA (817) 824-3849
    ## 1369 2519              UNITED       CHICAGO-O'HARE   503-671-4901
    ## 1370 1158           SOUTHWEST            SAN DIEGO   538-393-2243
    ## 1371 1224           SOUTHWEST            LAS VEGAS (540) 362-7136
    ## 1372 2178       CHINA EASTERN              QINGDAO (802) 910-1742
    ## 1373  450              UNITED         INDIANAPOLIS   784-458-8425
    ## 1374 3251              UNITED            SAN DIEGO (365) 217-0634
    ## 1375  372           LUFTHANSA            FRANKFURT (594) 797-7729
    ## 1376 2015     BRITISH AIRWAYS      LONDON HEATHROW   477-307-3338
    ## 1377 2520              ALASKA       SALT LAKE CITY   744-301-1148
    ## 1378 1453              UNITED             PORTLAND (900) 462-1379
    ## 1379 3187               DELTA          LOS ANGELES   878-636-2294
    ## 1380 2324              QANTAS               SYDNEY (998) 692-1900
    ## 1381 1518            INTERJET          GUADALAJARA   304-225-5895
    ## 1382 3030              UNITED             PORTLAND   931-522-5498
    ## 1383 1132           SOUTHWEST            SAN DIEGO (507) 483-3618
    ## 1384  566            EMIRATES                DUBAI (380) 449-7849
    ## 1385 2922               DELTA              SEATTLE   589-975-0198
    ## 1386 2596          AEROMEXICO          MEXICO CITY   501-668-7869
    ## 1387 3100                COPA          PANAMA CITY (706) 836-7047
    ## 1388 1411              UNITED            LAS VEGAS (698) 462-6742
    ## 1389  435              UNITED             HONOLULU   525-552-4162
    ## 1390 1605     VIRGIN ATLANTIC      LONDON HEATHROW   635-714-8302
    ## 1391 1405              UNITED          KANSAS CITY   453-556-0852
    ## 1392  539            EMIRATES                DUBAI   131-641-1331
    ## 1393 3083 PHILIPPINE AIRLINES               MANILA (753) 726-0123
    ## 1394 2919    TURKISH AIRLINES             ISTANBUL   609-332-7370
    ## 1395 1333         UNITED INTL               KANSAI   426-182-1365
    ## 1396 3113              ALASKA               NEWARK (347) 782-5787
    ## 1397 2045              UNITED               NEWARK   898-210-6218
    ## 1398 1912      AIR FRANCE/KLM      PARIS-DE GAULLE   658-861-4306
    ## 1399 3082              UNITED              SPOKANE (716) 184-1232
    ## 1400 1978              UNITED            BALTIMORE   380-105-1757
    ## 1401 2162       CHINA EASTERN              QINGDAO (969) 555-0453
    ## 1402 1521               DELTA          LOS ANGELES   343-973-0193
    ## 1403 1279      CHINA SOUTHERN                WUHAN (265) 286-5671
    ## 1404 2902               DELTA              SEATTLE   413-727-2672
    ## 1405 2958           SOUTHWEST          LOS ANGELES (162) 332-5838
    ## 1406 3225            AMERICAN              PHOENIX (229) 604-7790
    ## 1407  474              UNITED               DENVER (892) 301-0333
    ## 1408 1775              UNITED       RALEIGH-DURHAM (331) 472-8624
    ## 1409 3005              UNITED            NASHVILLE   522-286-5318
    ## 1410 3017              ALASKA      PUERTO VALLARTA   314-360-4288
    ## 1411 2993              UNITED            BALTIMORE (301) 534-5754
    ## 1412 2998              UNITED              BURBANK (341) 473-0639
    ## 1413 3027              ALASKA              SEATTLE (835) 882-3693
    ## 1414 3008               DELTA         NEW YORK-JFK   465-550-6610
    ## 1415 3011              ALASKA WASHINGTON DC-DULLES (589) 194-0523
    ## 1416 3015              UNITED WASHINGTON DC-DULLES   221-190-1449
    ## 1417 3000               DELTA MINNEAPOLIS-ST. PAUL (322) 843-0185
    ## 1418 2991              UNITED              KAHULUI   676-614-9095
    ## 1419 2969              UNITED             HONOLULU (190) 975-2514
    ## 1420 3020              ALASKA              SEATTLE (909) 382-3774
    ## 1421 2971              UNITED       CHICAGO-O'HARE (347) 896-3463

-   Filter for rows with phone numbers that contain `"("`, or `")"`.
    Remember to use `fixed()` when searching for parentheses.

``` r
# Filter for rows with "(" or ")" in the phone column
sfo_survey %>%
  filter(str_detect(phone, fixed("(")) | str_detect(phone, fixed(")")))
```

    ##       id             airline          destination          phone
    ## 1   3010            AMERICAN                MIAMI (637) 782-6989
    ## 2   2097         UNITED INTL          MEXICO CITY (359) 803-9809
    ## 3   1835    TURKISH AIRLINES             ISTANBUL (416) 788-2844
    ## 4    105              UNITED WASHINGTON DC-DULLES (729) 609-4819
    ## 5   1973      CATHAY PACIFIC            HONG KONG (201) 737-4409
    ## 6   2385         UNITED INTL               SYDNEY (137) 611-3694
    ## 7    517              UNITED       FT. LAUDERDALE (812) 869-6263
    ## 8   2885             EVA AIR               TAIPEI (194) 198-0504
    ## 9   2128            FRONTIER               DENVER (299) 137-6993
    ## 10  2132            FRONTIER               DENVER (739) 710-2966
    ## 11   519            EMIRATES                DUBAI (477) 182-4689
    ## 12   656              UNITED               NEWARK (525) 362-5532
    ## 13  2649           SOUTHWEST               DENVER (687) 887-6766
    ## 14  1049          AIR CANADA              CALGARY (637) 100-0509
    ## 15  3040            AMERICAN     DALLAS-FT. WORTH (603) 149-7268
    ## 16   844              ALASKA             PORTLAND (364) 792-5553
    ## 17  1042          AIR CANADA              CALGARY (535) 685-8273
    ## 18  2380         UNITED INTL               SYDNEY (826) 738-8316
    ## 19  2486              UNITED               NEWARK (554) 269-8937
    ## 20  1090               DELTA MINNEAPOLIS-ST. PAUL (298) 135-0900
    ## 21  2211           SOUTHWEST              PHOENIX (392) 183-7831
    ## 22  1555               DELTA              ATLANTA (606) 596-1029
    ## 23  2249              UNITED          BAKERSFIELD (384) 953-4795
    ## 24   879              ALASKA       RALEIGH-DURHAM (855) 811-8811
    ## 25  2956            AMERICAN         NEW YORK-JFK (419) 295-9580
    ## 26  1566               DELTA              DETROIT (919) 486-4251
    ## 27  1585               DELTA       SALT LAKE CITY (392) 495-7961
    ## 28  2670           SOUTHWEST               DENVER (506) 760-3043
    ## 29  3201              UNITED      ONTARIO (CALIF) (146) 699-3488
    ## 30  2765                 WOW            REYKJAVIK (302) 339-0791
    ## 31   272              UNITED          LOS ANGELES (446) 229-4342
    ## 32  1360         UNITED INTL      PARIS-DE GAULLE (249) 602-6985
    ## 33  3046            AMERICAN          LOS ANGELES (150) 905-6938
    ## 34  2673      AIR FRANCE/KLM      PARIS-DE GAULLE (116) 689-6617
    ## 35   750          AIR CANADA              TORONTO (896) 993-8555
    ## 36  1458            FRONTIER               DENVER (466) 912-8401
    ## 37  1487        HAWAIIAN AIR             HONOLULU (347) 851-5388
    ## 38  1454              UNITED            LAS VEGAS (441) 445-6532
    ## 39  2533             JETBLUE           LONG BEACH (205) 382-5599
    ## 40  2904    TURKISH AIRLINES             ISTANBUL (208) 794-9612
    ## 41  2193          AER LINGUS               DUBLIN (905) 742-3525
    ## 42  3087              ALASKA               NEWARK (147) 535-3529
    ## 43  2686      AIR FRANCE/KLM      PARIS-DE GAULLE (152) 912-4118
    ## 44   318              UNITED              BURBANK (634) 521-4714
    ## 45  2182              QANTAS               SYDNEY (121) 509-7306
    ## 46  2628             JETBLUE         NEW YORK-JFK (105) 635-5212
    ## 47  2521             JETBLUE           LONG BEACH (732) 168-0110
    ## 48  2034              UNITED               NEWARK (214) 250-8756
    ## 49   965           LUFTHANSA               MUNICH (119) 975-8484
    ## 50  1197           SOUTHWEST            LAS VEGAS (489) 534-6272
    ## 51   325              UNITED              BURBANK (610) 716-5732
    ## 52   846              ALASKA             PORTLAND (456) 925-4236
    ## 53  1588         UNITED INTL            HONG KONG (167) 144-9470
    ## 54  1851              ALASKA              KAHULUI (848) 149-5208
    ## 55  2226           SOUTHWEST            SANTA ANA (970) 908-2298
    ## 56  2248              UNITED          LOS ANGELES (843) 120-5653
    ## 57   297              UNITED            SAN DIEGO (139) 727-9901
    ## 58  2418              UNITED             HONOLULU (817) 824-3849
    ## 59  1327         UNITED INTL             SHANGHAI (540) 362-7136
    ## 60   664              UNITED               NEWARK (802) 910-1742
    ## 61  2292                 WOW            REYKJAVIK (365) 217-0634
    ## 62  1161               DELTA         NEW YORK-JFK (594) 797-7729
    ## 63  2383         UNITED INTL               SYDNEY (900) 462-1379
    ## 64   413              UNITED              PHOENIX (998) 692-1900
    ## 65  1558               DELTA              ATLANTA (507) 483-3618
    ## 66  3061                COPA          PANAMA CITY (380) 449-7849
    ## 67   741              ALASKA         NEW YORK-JFK (706) 836-7047
    ## 68  2299              UNITED          SAN ANTONIO (698) 462-6742
    ## 69  2659           SOUTHWEST               DENVER (753) 726-0123
    ## 70  2537              UNITED       CHICAGO-O'HARE (347) 782-5787
    ## 71  2453              UNITED               DENVER (716) 184-1232
    ## 72   748          AIR CANADA              TORONTO (969) 555-0453
    ## 73  1589           SOUTHWEST          LOS ANGELES (265) 286-5671
    ## 74  1855              ALASKA              KAHULUI (162) 332-5838
    ## 75  1178           SOUTHWEST            LAS VEGAS (229) 604-7790
    ## 76  1009          AIR CANADA            VANCOUVER (892) 301-0333
    ## 77  1853              ALASKA              KAHULUI (331) 472-8624
    ## 78  1235       CHINA EASTERN             SHANGHAI (301) 534-5754
    ## 79  2189          AER LINGUS               DUBLIN (341) 473-0639
    ## 80     5           SOUTHWEST          LOS ANGELES (835) 882-3693
    ## 81  2294              UNITED          SAN ANTONIO (589) 194-0523
    ## 82  1047          AIR CANADA              CALGARY (322) 843-0185
    ## 83  1586         UNITED INTL            HONG KONG (190) 975-2514
    ## 84  2621          AEROMEXICO          MEXICO CITY (909) 382-3774
    ## 85  1040          AIR CANADA              CALGARY (347) 896-3463
    ## 86   662              UNITED               NEWARK (355) 550-1392
    ## 87  2391         UNITED INTL              BEIJING (705) 456-1905
    ## 88  3058            AMERICAN                MIAMI (836) 207-8419
    ## 89  1100           SOUTHWEST            SAN DIEGO (306) 552-1875
    ## 90  2524             JETBLUE               BOSTON (944) 189-7555
    ## 91  3060            AMERICAN                MIAMI (458) 404-9558
    ## 92  3012              UNITED             PORTLAND (481) 522-1039
    ## 93  1485        HAWAIIAN AIR             HONOLULU (376) 611-4588
    ## 94  2000              UNITED               NEWARK (641) 544-6549
    ## 95  2482              ALASKA       SALT LAKE CITY (572) 748-6932
    ## 96  2031     BRITISH AIRWAYS      LONDON HEATHROW (227) 801-6148
    ## 97  3074                COPA          PANAMA CITY (519) 573-6576
    ## 98  3089              UNITED              SPOKANE (900) 586-1787
    ## 99   728      CATHAY PACIFIC            HONG KONG (239) 325-5321
    ## 100 2689      AIR FRANCE/KLM      PARIS-DE GAULLE (367) 897-7969
    ## 101 2602            INTERJET          GUADALAJARA (621) 874-9973
    ## 102 1416              UNITED            LAS VEGAS (426) 342-7378
    ## 103 2322              QANTAS               SYDNEY (765) 191-1797
    ## 104 1193           SOUTHWEST            LAS VEGAS (400) 250-0871
    ## 105 2939           SOUTHWEST            SAN DIEGO (434) 725-0561
    ## 106 2038     BRITISH AIRWAYS      LONDON HEATHROW (969) 207-3261
    ## 107  220            AMERICAN          LOS ANGELES (994) 688-3259
    ## 108 1678      CATHAY PACIFIC            HONG KONG (494) 308-3048
    ## 109 1073          AIR CANADA              CALGARY (715) 288-8832
    ## 110 2298              QANTAS               SYDNEY (594) 448-6242
    ## 111 3200      CATHAY PACIFIC            HONG KONG (802) 810-5574
    ## 112 1759           LUFTHANSA               MUNICH (867) 891-0871
    ## 113 3152               DELTA       SALT LAKE CITY (533) 213-4368
    ## 114 1328         UNITED INTL               KANSAI (324) 188-6781
    ## 115 1990      AIR FRANCE/KLM            AMSTERDAM (102) 928-7959
    ## 116 1356         UNITED INTL               KANSAI (439) 568-6611
    ## 117 2511              ALASKA       SALT LAKE CITY (767) 205-0604
    ## 118 2468              ALASKA       SALT LAKE CITY (890) 548-9219
    ## 119  453              ALASKA            LAS VEGAS (649) 925-8489
    ## 120 2442             JETBLUE               BOSTON (156) 837-4491
    ## 121 2716           SOUTHWEST               DENVER (882) 304-9032
    ## 122  304              UNITED            SAN DIEGO (971) 548-6611
    ## 123 2250                 WOW            REYKJAVIK (828) 153-5819
    ## 124 2491              ALASKA       SALT LAKE CITY (906) 850-9192
    ## 125 1255         UNITED INTL         TOKYO-NARITA (859) 777-8245
    ## 126 2047     BRITISH AIRWAYS      LONDON HEATHROW (589) 270-7518
    ## 127 2172            FRONTIER               DENVER (146) 129-5118
    ## 128 1903           LUFTHANSA               MUNICH (927) 747-9822
    ## 129 2139            FRONTIER               DENVER (481) 479-7013
    ## 130 1404         UNITED INTL      PARIS-DE GAULLE (716) 777-3762
    ## 131 3138               DELTA       SALT LAKE CITY (217) 589-0596
    ## 132  841              ALASKA              SEATTLE (637) 782-6989
    ## 133 2272              UNITED          BAKERSFIELD (359) 803-9809
    ## 134   11           SOUTHWEST          LOS ANGELES (416) 788-2844
    ## 135 1167               DELTA         NEW YORK-JFK (729) 609-4819
    ## 136  302              UNITED            SAN DIEGO (201) 737-4409
    ## 137 1607     VIRGIN ATLANTIC      LONDON HEATHROW (137) 611-3694
    ## 138  911          AIR CANADA              TORONTO (812) 869-6263
    ## 139 1241       CHINA EASTERN             SHANGHAI (194) 198-0504
    ## 140 3023              UNITED             PORTLAND (299) 137-6993
    ## 141  556              ALASKA              SEATTLE (739) 710-2966
    ## 142  334           SOUTHWEST              PHOENIX (477) 182-4689
    ## 143 2682              UNITED       CHICAGO-O'HARE (525) 362-5532
    ## 144 1673            EMIRATES                DUBAI (687) 887-6766
    ## 145 2941    TURKISH AIRLINES             ISTANBUL (637) 100-0509
    ## 146 3144              UNITED            SAN DIEGO (603) 149-7268
    ## 147 1313             JETBLUE         NEW YORK-JFK (364) 792-5553
    ## 148 2896               DELTA       SALT LAKE CITY (535) 685-8273
    ## 149  826              ALASKA            NASHVILLE (826) 738-8316
    ## 150  660         UNITED INTL               MUNICH (554) 269-8937
    ## 151 1515               DELTA          LOS ANGELES (298) 135-0900
    ## 152 3097              UNITED              SPOKANE (392) 183-7831
    ## 153 2462              UNITED       CHICAGO-O'HARE (606) 596-1029
    ## 154 1538               DELTA         NEW YORK-JFK (384) 953-4795
    ## 155 2330               DELTA MINNEAPOLIS-ST. PAUL (855) 811-8811
    ## 156 2005      AIR FRANCE/KLM      PARIS-DE GAULLE (419) 295-9580
    ## 157  320           SOUTHWEST          LOS ANGELES (919) 486-4251
    ## 158  292           SOUTHWEST          LOS ANGELES (392) 495-7961
    ## 159 2820               DELTA          LOS ANGELES (506) 760-3043
    ## 160 3121              ALASKA               NEWARK (146) 699-3488
    ## 161 2748      AIR FRANCE/KLM      PARIS-DE GAULLE (302) 339-0791
    ## 162  264          AIR CANADA              TORONTO (446) 229-4342
    ## 163 2317              QANTAS               SYDNEY (249) 602-6985
    ## 164 2076             JETBLUE           LONG BEACH (150) 905-6938
    ## 165  827              ALASKA              SEATTLE (116) 689-6617
    ## 166 2318              QANTAS               SYDNEY (896) 993-8555
    ## 167 2636             JETBLUE         NEW YORK-JFK (466) 912-8401
    ## 168 1670            EMIRATES                DUBAI (347) 851-5388
    ## 169 1336         UNITED INTL               KANSAI (441) 445-6532
    ## 170 2561             JETBLUE           LONG BEACH (205) 382-5599
    ## 171  669              UNITED               NEWARK (208) 794-9612
    ## 172 3124                COPA          PANAMA CITY (905) 742-3525
    ## 173 1738          AIR CANADA              TORONTO (147) 535-3529
    ## 174 1698              ALASKA            SANTA ANA (152) 912-4118
    ## 175 1035          AIR CANADA            VANCOUVER (634) 521-4714
    ## 176  939          AIR CANADA              TORONTO (121) 509-7306
    ## 177 1461              ALASKA         NEW YORK-JFK (105) 635-5212
    ## 178 2908    TURKISH AIRLINES             ISTANBUL (732) 168-0110
    ## 179 1412              UNITED          KANSAS CITY (214) 250-8756
    ## 180 3240            AMERICAN              PHOENIX (119) 975-8484
    ## 181 1660            EMIRATES                DUBAI (489) 534-6272
    ## 182  688      AIR FRANCE/KLM      PARIS-DE GAULLE (610) 716-5732
    ## 183 1102               DELTA MINNEAPOLIS-ST. PAUL (456) 925-4236
    ## 184 1298             JETBLUE         NEW YORK-JFK (167) 144-9470
    ## 185 2049     BRITISH AIRWAYS      LONDON HEATHROW (848) 149-5208
    ## 186  789            AMERICAN          LOS ANGELES (970) 908-2298
    ## 187 2804           SOUTHWEST              PHOENIX (843) 120-5653
    ## 188  815              ALASKA             PORTLAND (139) 727-9901
    ## 189 3252              UNITED            SAN DIEGO (817) 824-3849
    ## 190 2605              UNITED       CHICAGO-O'HARE (540) 362-7136
    ## 191 2012      AIR FRANCE/KLM      PARIS-DE GAULLE (802) 910-1742
    ## 192 3120                COPA          PANAMA CITY (365) 217-0634
    ## 193 3149               DELTA       SALT LAKE CITY (594) 797-7729
    ## 194 1946      AIR FRANCE/KLM      PARIS-DE GAULLE (900) 462-1379
    ## 195 1520            INTERJET          GUADALAJARA (998) 692-1900
    ## 196 2776                 WOW            REYKJAVIK (507) 483-3618
    ## 197  328              UNITED              BURBANK (380) 449-7849
    ## 198  766              ALASKA         NEW YORK-JFK (706) 836-7047
    ## 199 2888              UNITED               AUSTIN (698) 462-6742
    ## 200 2483              UNITED               DENVER (753) 726-0123
    ## 201 1672            EMIRATES                DUBAI (347) 782-5787
    ## 202  834              ALASKA              SEATTLE (716) 184-1232
    ## 203 2074             JETBLUE           LONG BEACH (969) 555-0453
    ## 204 2895                 WOW            REYKJAVIK (265) 286-5671
    ## 205  724      CATHAY PACIFIC            HONG KONG (162) 332-5838
    ## 206  381              ALASKA    SAN JOSE DEL CABO (229) 604-7790
    ## 207 1760           LUFTHANSA               MUNICH (892) 301-0333
    ## 208 2202          AER LINGUS               DUBLIN (331) 472-8624
    ## 209  308              UNITED            SAN DIEGO (301) 534-5754
    ## 210  344           LUFTHANSA            FRANKFURT (341) 473-0639
    ## 211 1729          AIR CANADA              TORONTO (835) 882-3693
    ## 212  599             JETBLUE               BOSTON (589) 194-0523
    ## 213  704      AIR FRANCE/KLM      PARIS-DE GAULLE (322) 843-0185
    ## 214 3127               DELTA       SALT LAKE CITY (190) 975-2514
    ## 215 1250       CHINA EASTERN             SHANGHAI (909) 382-3774
    ## 216 2691              UNITED              ORLANDO (347) 896-3463
    ## 217 2849              UNITED              SEATTLE (355) 550-1392
    ## 218 1557               DELTA         NEW YORK-JFK (705) 456-1905
    ## 219 2329            AMERICAN       CHICAGO-O'HARE (836) 207-8419
    ## 220  997           LUFTHANSA               MUNICH (306) 552-1875
    ## 221  347              ALASKA            SAN DIEGO (944) 189-7555
    ## 222 1580               DELTA              DETROIT (458) 404-9558
    ## 223  268          AIR CANADA              TORONTO (481) 522-1039
    ## 224  446              UNITED             HONOLULU (376) 611-4588
    ## 225 1664            EMIRATES                DUBAI (641) 544-6549
    ## 226 2276              UNITED          LOS ANGELES (572) 748-6932
    ## 227  199            AMERICAN          LOS ANGELES (227) 801-6148
    ## 228 2017     BRITISH AIRWAYS      LONDON HEATHROW (519) 573-6576
    ## 229 1889  SINGAPORE AIRLINES            SINGAPORE (900) 586-1787
    ## 230 1710      AIR FRANCE/KLM      PARIS-DE GAULLE (239) 325-5321
    ## 231 1080          AIR CANADA              CALGARY (367) 897-7969
    ## 232  431              UNITED         INDIANAPOLIS (621) 874-9973
    ## 233  898          AIR CANADA              TORONTO (426) 342-7378
    ## 234  643              ALASKA          NEW ORLEANS (765) 191-1797
    ## 235 1574         UNITED INTL            HONG KONG (400) 250-0871
    ## 236 2169            FRONTIER               DENVER (434) 725-0561
    ## 237 2365             JETBLUE       FT. LAUDERDALE (969) 207-3261
    ## 238 1577         UNITED INTL            HONG KONG (994) 688-3259
    ## 239 2655             JETBLUE               BOSTON (494) 308-3048
    ## 240 3009            AMERICAN                MIAMI (715) 288-8832
    ## 241 3142               DELTA       SALT LAKE CITY (594) 448-6242
    ## 242 1619     VIRGIN ATLANTIC      LONDON HEATHROW (802) 810-5574
    ## 243 1508        HAWAIIAN AIR             HONOLULU (867) 891-0871
    ## 244 2564             JETBLUE           LONG BEACH (533) 213-4368
    ## 245  763          AIR CANADA              TORONTO (324) 188-6781
    ## 246 1921      AIR FRANCE/KLM      PARIS-DE GAULLE (102) 928-7959
    ## 247 1387         UNITED INTL      PARIS-DE GAULLE (439) 568-6611
    ## 248 1480              ALASKA         NEW YORK-JFK (767) 205-0604
    ## 249 1743      CATHAY PACIFIC            HONG KONG (890) 548-9219
    ## 250 2508              UNITED               NEWARK (649) 925-8489
    ## 251 2277              UNITED          BAKERSFIELD (156) 837-4491
    ## 252 2099         UNITED INTL          MEXICO CITY (882) 304-9032
    ## 253 3172            AMERICAN     DALLAS-FT. WORTH (971) 548-6611
    ## 254 1920      AIR FRANCE/KLM      PARIS-DE GAULLE (828) 153-5819
    ## 255 1986      AIR FRANCE/KLM            AMSTERDAM (906) 850-9192
    ## 256  529            EMIRATES                DUBAI (859) 777-8245
    ## 257 3068              UNITED              SPOKANE (589) 270-7518
    ## 258 3038              UNITED             PORTLAND (146) 129-5118
    ## 259 1634          KOREAN AIR                SEOUL (927) 747-9822
    ## 260 2877             EVA AIR               TAIPEI (481) 479-7013
    ## 261 1600  SINGAPORE AIRLINES            SINGAPORE (716) 777-3762
    ## 262 2562             JETBLUE               BOSTON (217) 589-0596
    ## 263 2161            FRONTIER               DENVER (637) 782-6989
    ## 264 1019          AIR CANADA            VANCOUVER (359) 803-9809
    ## 265 2867              UNITED WASHINGTON DC-DULLES (416) 788-2844
    ## 266 2707               DELTA          LOS ANGELES (729) 609-4819
    ## 267  992           LUFTHANSA               MUNICH (201) 737-4409
    ## 268 2395         UNITED INTL              BEIJING (137) 611-3694
    ## 269 3219              ALASKA             PORTLAND (812) 869-6263
    ## 270 1377         UNITED INTL      PARIS-DE GAULLE (194) 198-0504
    ## 271 2925    TURKISH AIRLINES             ISTANBUL (299) 137-6993
    ## 272 2708      AIR FRANCE/KLM      PARIS-DE GAULLE (739) 710-2966
    ## 273 3141              ALASKA               NEWARK (477) 182-4689
    ## 274 1432              UNITED            LAS VEGAS (525) 362-5532
    ## 275 1891  SINGAPORE AIRLINES            SINGAPORE (687) 887-6766
    ## 276 1433              UNITED             PORTLAND (637) 100-0509
    ## 277 2687              UNITED              ORLANDO (603) 149-7268
    ## 278 1325             JETBLUE         NEW YORK-JFK (364) 792-5553
    ## 279 2287                 WOW            REYKJAVIK (535) 685-8273
    ## 280 2007      AIR FRANCE/KLM      PARIS-DE GAULLE (826) 738-8316
    ## 281 2420     AIR NEW ZEALAND             AUCKLAND (554) 269-8937
    ## 282 1304             JETBLUE         NEW YORK-JFK (298) 135-0900
    ## 283 2643             JETBLUE         NEW YORK-JFK (392) 183-7831
    ## 284 1992      AIR FRANCE/KLM            AMSTERDAM (606) 596-1029
    ## 285  353              ALASKA               NEWARK (384) 953-4795
    ## 286  698      CATHAY PACIFIC            HONG KONG (855) 811-8811
    ## 287 2011      AIR FRANCE/KLM      PARIS-DE GAULLE (419) 295-9580
    ## 288 3125              ALASKA               NEWARK (919) 486-4251
    ## 289 3229            AMERICAN              PHOENIX (392) 495-7961
    ## 290 1733          AIR CANADA              TORONTO (506) 760-3043
    ## 291 2999            AMERICAN                MIAMI (146) 699-3488
    ## 292 3197      CATHAY PACIFIC            HONG KONG (302) 339-0791
    ## 293  437              UNITED             HONOLULU (446) 229-4342
    ## 294 1869              ALASKA              KAHULUI (249) 602-6985
    ## 295  550            EMIRATES                DUBAI (150) 905-6938
    ## 296 3114                COPA          PANAMA CITY (116) 689-6617
    ## 297 2876             EVA AIR               TAIPEI (896) 993-8555
    ## 298 2597             JETBLUE               BOSTON (466) 912-8401
    ## 299  811              ALASKA             PORTLAND (347) 851-5388
    ## 300 3194               DELTA          LOS ANGELES (441) 445-6532
    ## 301 2160      CATHAY PACIFIC            HONG KONG (205) 382-5599
    ## 302 1228           SOUTHWEST            LAS VEGAS (208) 794-9612
    ## 303 2791                 WOW            REYKJAVIK (905) 742-3525
    ## 304 1985              UNITED            BALTIMORE (147) 535-3529
    ## 305 1147           SOUTHWEST            SAN DIEGO (152) 912-4118
    ## 306 2267              UNITED          BAKERSFIELD (634) 521-4714
    ## 307 2496              UNITED               DENVER (121) 509-7306
    ## 308 2806               DELTA          LOS ANGELES (105) 635-5212
    ## 309 1771            AMERICAN       CHICAGO-O'HARE (732) 168-0110
    ## 310 2131          AIR CANADA            VANCOUVER (214) 250-8756
    ## 311 1421              UNITED            LAS VEGAS (119) 975-8484
    ## 312 1179               DELTA         NEW YORK-JFK (489) 534-6272
    ## 313  695      CATHAY PACIFIC            HONG KONG (610) 716-5732
    ## 314 1705      AIR FRANCE/KLM      PARIS-DE GAULLE (456) 925-4236
    ## 315 2265              UNITED          LOS ANGELES (167) 144-9470
    ## 316  824              ALASKA            NASHVILLE (848) 149-5208
    ## 317 1922      AIR FRANCE/KLM      PARIS-DE GAULLE (970) 908-2298
    ## 318 1821              ALASKA       RALEIGH-DURHAM (843) 120-5653
    ## 319 2415     AIR NEW ZEALAND             AUCKLAND (139) 727-9901
    ## 320 1688      CATHAY PACIFIC            HONG KONG (817) 824-3849
    ## 321 2113             JETBLUE           LONG BEACH (540) 362-7136
    ## 322  581             JETBLUE               BOSTON (802) 910-1742
    ## 323  358              ALASKA               NEWARK (365) 217-0634
    ## 324 1950      AIR FRANCE/KLM      PARIS-DE GAULLE (594) 797-7729
    ## 325 2934           SOUTHWEST            SAN DIEGO (900) 462-1379
    ## 326  257          AIR CANADA              TORONTO (998) 692-1900
    ## 327 2624              UNITED       CHICAGO-O'HARE (507) 483-3618
    ## 328 1676      AIR FRANCE/KLM      PARIS-DE GAULLE (380) 449-7849
    ## 329 3146               DELTA       SALT LAKE CITY (706) 836-7047
    ## 330 3237            AMERICAN              PHOENIX (698) 462-6742
    ## 331 2332               DELTA MINNEAPOLIS-ST. PAUL (753) 726-0123
    ## 332 3281           SOUTHWEST               DENVER (347) 782-5787
    ## 333  315              UNITED            SAN DIEGO (716) 184-1232
    ## 334  465              UNITED               DENVER (969) 555-0453
    ## 335 3218            AMERICAN          LOS ANGELES (265) 286-5671
    ## 336 1629          KOREAN AIR                SEOUL (229) 604-7790
    ## 337 1201           SOUTHWEST            LAS VEGAS (892) 301-0333
    ## 338  505              ALASKA             PORTLAND (331) 472-8624
    ## 339  528              ALASKA      PUERTO VALLARTA (301) 534-5754
    ## 340 1937      AIR FRANCE/KLM      PARIS-DE GAULLE (341) 473-0639
    ## 341 2444              UNITED             HONOLULU (835) 882-3693
    ## 342 3022              UNITED             PORTLAND (589) 194-0523
    ## 343 2360              UNITED               AUSTIN (322) 843-0185
    ## 344 1278         UNITED INTL      LONDON HEATHROW (190) 975-2514
    ## 345 1949      AIR FRANCE/KLM      PARIS-DE GAULLE (909) 382-3774
    ## 346 1936              UNITED               BOSTON (347) 896-3463
    ## 347 1976              UNITED            BALTIMORE (355) 550-1392
    ## 348   12           SOUTHWEST          LOS ANGELES (705) 456-1905
    ## 349 1140           SOUTHWEST          LOS ANGELES (836) 207-8419
    ## 350 1652     VIRGIN ATLANTIC      LONDON HEATHROW (306) 552-1875
    ## 351 2449             JETBLUE               BOSTON (944) 189-7555
    ## 352 1645     VIRGIN ATLANTIC      LONDON HEATHROW (458) 404-9558
    ## 353 2883             EVA AIR               TAIPEI (481) 522-1039
    ## 354 3098                COPA          PANAMA CITY (376) 611-4588
    ## 355 1552               DELTA         NEW YORK-JFK (641) 544-6549
    ## 356 2790                 WOW            REYKJAVIK (572) 748-6932
    ## 357  946          AIR CANADA            VANCOUVER (227) 801-6148
    ## 358 1611     VIRGIN ATLANTIC      LONDON HEATHROW (519) 573-6576
    ## 359  608             JETBLUE               BOSTON (900) 586-1787
    ## 360 1915      AIR FRANCE/KLM      PARIS-DE GAULLE (239) 325-5321
    ## 361  462              ALASKA            LAS VEGAS (367) 897-7969
    ## 362 1642     VIRGIN ATLANTIC      LONDON HEATHROW (621) 874-9973
    ## 363  666         UNITED INTL               MUNICH (426) 342-7378
    ## 364 2786                 WOW            REYKJAVIK (765) 191-1797
    ## 365 2200          AER LINGUS               DUBLIN (400) 250-0871
    ## 366  752          AIR CANADA              TORONTO (434) 725-0561
    ## 367 1661            EMIRATES                DUBAI (969) 207-3261
    ## 368 2517              UNITED               NEWARK (994) 688-3259
    ## 369 2726      AIR FRANCE/KLM      PARIS-DE GAULLE (494) 308-3048
    ## 370 1666            EMIRATES                DUBAI (715) 288-8832
    ## 371 1398         UNITED INTL             SHANGHAI (594) 448-6242
    ## 372 2376             JETBLUE       FT. LAUDERDALE (802) 810-5574
    ## 373 1890              UNITED               BOSTON (867) 891-0871
    ## 374 1702      AIR FRANCE/KLM      PARIS-DE GAULLE (533) 213-4368
    ## 375 1464              ALASKA         NEW YORK-JFK (324) 188-6781
    ## 376  227            AMERICAN          LOS ANGELES (102) 928-7959
    ## 377 1872              ALASKA              KAHULUI (439) 568-6611
    ## 378 2024              UNITED               NEWARK (767) 205-0604
    ## 379 2127            FRONTIER               DENVER (890) 548-9219
    ## 380  497              UNITED               DENVER (649) 925-8489
    ## 381  809              ALASKA             PORTLAND (156) 837-4491
    ## 382 2644             JETBLUE         NEW YORK-JFK (882) 304-9032
    ## 383 1979      CATHAY PACIFIC            HONG KONG (971) 548-6611
    ## 384 2638             JETBLUE         NEW YORK-JFK (828) 153-5819
    ## 385 3071 PHILIPPINE AIRLINES               MANILA (906) 850-9192
    ## 386 1543           AIR CHINA              BEIJING (859) 777-8245
    ## 387 1968      CATHAY PACIFIC            HONG KONG (589) 270-7518
    ## 388 2210          AER LINGUS               DUBLIN (146) 129-5118
    ## 389 1359         UNITED INTL               KANSAI (927) 747-9822
    ## 390 1900              UNITED               BOSTON (481) 479-7013
    ## 391 1038          AIR CANADA            VANCOUVER (716) 777-3762
    ## 392 2984            AMERICAN            CHARLOTTE (217) 589-0596
    ## 393 1126               DELTA MINNEAPOLIS-ST. PAUL (637) 782-6989
    ## 394 2631             JETBLUE         NEW YORK-JFK (359) 803-9809
    ## 395  907          AIR CANADA              TORONTO (416) 788-2844
    ## 396  527            EMIRATES                DUBAI (729) 609-4819
    ## 397 2559             JETBLUE               BOSTON (201) 737-4409
    ## 398 2599          AEROMEXICO          MEXICO CITY (137) 611-3694
    ## 399 2153            FRONTIER               DENVER (812) 869-6263
    ## 400 1777              UNITED       RALEIGH-DURHAM (194) 198-0504
    ## 401  999           LUFTHANSA               MUNICH (299) 137-6993
    ## 402  383              ALASKA          LOS ANGELES (739) 710-2966
    ## 403 2150            FRONTIER               DENVER (477) 182-4689
    ## 404 3202      CATHAY PACIFIC            HONG KONG (525) 362-5532
    ## 405  542            EMIRATES                DUBAI (687) 887-6766
    ## 406 1346         UNITED INTL               KANSAI (637) 100-0509
    ## 407  697      AIR FRANCE/KLM      PARIS-DE GAULLE (603) 149-7268
    ## 408  836              ALASKA              SEATTLE (364) 792-5553
    ## 409  445              UNITED         INDIANAPOLIS (535) 685-8273
    ## 410  928              ALASKA       RALEIGH-DURHAM (826) 738-8316
    ## 411  559              ALASKA              SEATTLE (554) 269-8937
    ## 412 2529             JETBLUE           LONG BEACH (298) 135-0900
    ## 413  835              ALASKA            NASHVILLE (392) 183-7831
    ## 414 1002           LUFTHANSA               MUNICH (606) 596-1029
    ## 415  861            AMERICAN     DALLAS-FT. WORTH (384) 953-4795
    ## 416 2662              UNITED       RALEIGH-DURHAM (855) 811-8811
    ## 417 3126              ALASKA               NEWARK (419) 295-9580
    ## 418 1822         UNITED INTL      LONDON HEATHROW (919) 486-4251
    ## 419 2556             JETBLUE           LONG BEACH (392) 495-7961
    ## 420 2266                 WOW            REYKJAVIK (506) 760-3043
    ## 421 2301              QANTAS               SYDNEY (146) 699-3488
    ## 422  558            EMIRATES                DUBAI (302) 339-0791
    ## 423 1919      AIR FRANCE/KLM      PARIS-DE GAULLE (446) 229-4342
    ## 424 1382         UNITED INTL      PARIS-DE GAULLE (249) 602-6985
    ## 425  808              ALASKA            BALTIMORE (150) 905-6938
    ## 426 3140               DELTA       SALT LAKE CITY (116) 689-6617
    ## 427 2212          AER LINGUS               DUBLIN (896) 993-8555
    ## 428 2579             JETBLUE           LONG BEACH (466) 912-8401
    ## 429  363           LUFTHANSA            FRANKFURT (347) 851-5388
    ## 430  416              UNITED         HOUSTON-BUSH (441) 445-6532
    ## 431 3222              ALASKA             PORTLAND (205) 382-5599
    ## 432 2671              UNITED       RALEIGH-DURHAM (208) 794-9612
    ## 433 2306            AMERICAN       CHICAGO-O'HARE (905) 742-3525
    ## 434  216            AMERICAN          LOS ANGELES (147) 535-3529
    ## 435 1768            AMERICAN       CHICAGO-O'HARE (152) 912-4118
    ## 436 3156               DELTA       SALT LAKE CITY (634) 521-4714
    ## 437 2756               DELTA              ATLANTA (121) 509-7306
    ## 438  214            AMERICAN          LOS ANGELES (105) 635-5212
    ## 439  212            AMERICAN          LOS ANGELES (732) 168-0110
    ## 440 2824              UNITED              BURBANK (214) 250-8756
    ## 441 2348              UNITED               AUSTIN (119) 975-8484
    ## 442 3245            AMERICAN              PHOENIX (489) 534-6272
    ## 443 3270              UNITED            LAS VEGAS (610) 716-5732
    ## 444 1118           SOUTHWEST            SAN DIEGO (456) 925-4236
    ## 445 2096             JETBLUE           LONG BEACH (167) 144-9470
    ## 446 1618     VIRGIN ATLANTIC      LONDON HEATHROW (848) 149-5208
    ## 447 2485              UNITED               DENVER (970) 908-2298
    ## 448 2787               DELTA              ATLANTA (843) 120-5653
    ## 449 2880             EVA AIR               TAIPEI (139) 727-9901
    ## 450  941          AIR CANADA            VANCOUVER (817) 824-3849
    ## 451 1999              UNITED            BALTIMORE (540) 362-7136
    ## 452 1312         UNITED INTL      LONDON HEATHROW (802) 910-1742
    ## 453 2690              UNITED       CHICAGO-O'HARE (365) 217-0634
    ## 454 2830              UNITED              BURBANK (594) 797-7729
    ## 455 2646             JETBLUE         NEW YORK-JFK (900) 462-1379
    ## 456  593             JETBLUE               BOSTON (998) 692-1900
    ## 457 2591             JETBLUE               BOSTON (507) 483-3618
    ## 458 2303              UNITED          SAN ANTONIO (380) 449-7849
    ## 459 2495              UNITED       CHICAGO-O'HARE (706) 836-7047
    ## 460 2829              UNITED              BURBANK (698) 462-6742
    ## 461  768              ALASKA         NEW YORK-JFK (753) 726-0123
    ## 462  644              ALASKA WASHINGTON DC-DULLES (347) 782-5787
    ## 463  378              ALASKA    SAN JOSE DEL CABO (716) 184-1232
    ## 464 3221            AMERICAN          LOS ANGELES (969) 555-0453
    ## 465 2639              UNITED       CHICAGO-O'HARE (265) 286-5671
    ## 466 1631          KOREAN AIR                SEOUL (162) 332-5838
    ## 467 1994              UNITED            BALTIMORE (229) 604-7790
    ## 468  310              UNITED WASHINGTON DC-DULLES (892) 301-0333
    ## 469  439              UNITED             HONOLULU (331) 472-8624
    ## 470 2641              UNITED       CHICAGO-O'HARE (301) 534-5754
    ## 471 2800               DELTA       SALT LAKE CITY (341) 473-0639
    ## 472 3052            AMERICAN                MIAMI (835) 882-3693
    ## 473 1361         UNITED INTL               KANSAI (589) 194-0523
    ## 474  606             JETBLUE               BOSTON (322) 843-0185
    ## 475 1394         UNITED INTL      PARIS-DE GAULLE (190) 975-2514
    ## 476 2611          AEROMEXICO          MEXICO CITY (909) 382-3774
    ## 477 3094                COPA          PANAMA CITY (347) 896-3463
    ## 478 1934      AIR FRANCE/KLM      PARIS-DE GAULLE (355) 550-1392
    ## 479 1475              ALASKA         NEW YORK-JFK (705) 456-1905
    ## 480 1742      CATHAY PACIFIC            HONG KONG (836) 207-8419
    ## 481 1940      AIR FRANCE/KLM      PARIS-DE GAULLE (306) 552-1875
    ## 482  991           LUFTHANSA               MUNICH (944) 189-7555
    ## 483 2369             JETBLUE       FT. LAUDERDALE (458) 404-9558
    ## 484  395              ALASKA          LOS ANGELES (481) 522-1039
    ## 485 1456              ALASKA         NEW YORK-JFK (376) 611-4588
    ## 486 1625          KOREAN AIR                SEOUL (641) 544-6549
    ## 487 1013          AIR CANADA            VANCOUVER (572) 748-6932
    ## 488 1713      AIR FRANCE/KLM      PARIS-DE GAULLE (227) 801-6148
    ## 489 1956              UNITED               BOSTON (519) 573-6576
    ## 490 2798               DELTA       SALT LAKE CITY (900) 586-1787
    ## 491  361           LUFTHANSA            FRANKFURT (239) 325-5321
    ## 492 1322         UNITED INTL      LONDON HEATHROW (367) 897-7969
    ## 493 1709      AIR FRANCE/KLM      PARIS-DE GAULLE (621) 874-9973
    ## 494 2364              UNITED               AUSTIN (426) 342-7378
    ## 495 3075              UNITED              SPOKANE (765) 191-1797
    ## 496  758              ALASKA         NEW YORK-JFK (400) 250-0871
    ## 497 1689      CATHAY PACIFIC            HONG KONG (434) 725-0561
    ## 498 1762           LUFTHANSA               MUNICH (969) 207-3261
    ## 499 2100             JETBLUE           LONG BEACH (994) 688-3259
    ## 500 1512               DELTA          LOS ANGELES (494) 308-3048
    ## 501 2875             EVA AIR               TAIPEI (715) 288-8832
    ## 502  414              UNITED         HOUSTON-BUSH (594) 448-6242
    ## 503 2314              UNITED          SAN ANTONIO (802) 810-5574
    ## 504 1162           SOUTHWEST          LOS ANGELES (867) 891-0871
    ## 505 1296         UNITED INTL      LONDON HEATHROW (533) 213-4368
    ## 506 2711              UNITED        SANTA BARBARA (324) 188-6781
    ## 507 2098             JETBLUE           LONG BEACH (102) 928-7959
    ## 508 2664          AEROMEXICO          MEXICO CITY (439) 568-6611
    ## 509 2302            AMERICAN       CHICAGO-O'HARE (767) 205-0604
    ## 510  713      AIR FRANCE/KLM      PARIS-DE GAULLE (890) 548-9219
    ## 511 3199      CATHAY PACIFIC            HONG KONG (649) 925-8489
    ## 512 3072              UNITED              SPOKANE (156) 837-4491
    ## 513 2066            AMERICAN         PHILADELPHIA (882) 304-9032
    ## 514 2123             JETBLUE           LONG BEACH (971) 548-6611
    ## 515 2065              UNITED               NEWARK (828) 153-5819
    ## 516 3236              ALASKA             PORTLAND (906) 850-9192
    ## 517 1765            AMERICAN       CHICAGO-O'HARE (859) 777-8245
    ## 518  790         UNITED INTL    SAN JOSE DEL CABO (589) 270-7518
    ## 519  708      AIR FRANCE/KLM      PARIS-DE GAULLE (146) 129-5118
    ## 520 2775               DELTA              ATLANTA (927) 747-9822
    ## 521  923              ALASKA       RALEIGH-DURHAM (481) 479-7013
    ## 522 2940    TURKISH AIRLINES             ISTANBUL (716) 777-3762
    ## 523 2825              UNITED              BURBANK (217) 589-0596
    ## 524 2860              UNITED              SEATTLE (637) 782-6989
    ## 525 2146            FRONTIER               DENVER (359) 803-9809
    ## 526 2474              ALASKA       SALT LAKE CITY (416) 788-2844
    ## 527 3216            AMERICAN          LOS ANGELES (729) 609-4819
    ## 528 3164            AMERICAN     DALLAS-FT. WORTH (201) 737-4409
    ## 529 1261       CHINA EASTERN             SHANGHAI (137) 611-3694
    ## 530  348           LUFTHANSA            FRANKFURT (812) 869-6263
    ## 531  646              ALASKA WASHINGTON DC-DULLES (194) 198-0504
    ## 532 1430              UNITED            LAS VEGAS (299) 137-6993
    ## 533 2050     BRITISH AIRWAYS      LONDON HEATHROW (739) 710-2966
    ## 534 1517               DELTA          LOS ANGELES (477) 182-4689
    ## 535 2037              UNITED               NEWARK (525) 362-5532
    ## 536 2684              UNITED              ORLANDO (687) 887-6766
    ## 537 2351              UNITED               AUSTIN (637) 100-0509
    ## 538 2216           SOUTHWEST              PHOENIX (603) 149-7268
    ## 539  549              ALASKA              SEATTLE (364) 792-5553
    ## 540  967           LUFTHANSA               MUNICH (535) 685-8273
    ## 541 2010              UNITED               NEWARK (826) 738-8316
    ## 542 2461              UNITED               DENVER (554) 269-8937
    ## 543  979           LUFTHANSA               MUNICH (298) 135-0900
    ## 544  854              ALASKA             PORTLAND (392) 183-7831
    ## 545 1637          KOREAN AIR                SEOUL (606) 596-1029
    ## 546 3239            AMERICAN              PHOENIX (384) 953-4795
    ## 547  816              ALASKA            BALTIMORE (855) 811-8811
    ## 548 1145           SOUTHWEST            SAN DIEGO (419) 295-9580
    ## 549 2075            AMERICAN         PHILADELPHIA (919) 486-4251
    ## 550 1993              UNITED            BALTIMORE (392) 495-7961
    ## 551 2598              UNITED       CHICAGO-O'HARE (506) 760-3043
    ## 552 2832              UNITED              BURBANK (146) 699-3488
    ## 553  679              UNITED               NEWARK (302) 339-0791
    ## 554 1772            AMERICAN       CHICAGO-O'HARE (446) 229-4342
    ## 555 2052              UNITED               NEWARK (249) 602-6985
    ## 556 2821               DELTA          LOS ANGELES (150) 905-6938
    ## 557 2502              UNITED       CHICAGO-O'HARE (116) 689-6617
    ## 558 1084          AIR CANADA              CALGARY (896) 993-8555
    ## 559  955          AIR CANADA            VANCOUVER (466) 912-8401
    ## 560 2257              UNITED          LOS ANGELES (347) 851-5388
    ## 561  801              ALASKA            BALTIMORE (441) 445-6532
    ## 562 2581             JETBLUE               BOSTON (205) 382-5599
    ## 563 2760               DELTA              ATLANTA (208) 794-9612
    ## 564 1323         UNITED INTL      LONDON HEATHROW (905) 742-3525
    ## 565   13           SOUTHWEST          LOS ANGELES (147) 535-3529
    ## 566 2576             JETBLUE           LONG BEACH (152) 912-4118
    ## 567 2528             JETBLUE           LONG BEACH (634) 521-4714
    ## 568 2532             JETBLUE           LONG BEACH (121) 509-7306
    ## 569 3128              UNITED            SAN DIEGO (105) 635-5212
    ## 570   14           SOUTHWEST          LOS ANGELES (732) 168-0110
    ## 571 1537               DELTA         NEW YORK-JFK (214) 250-8756
    ## 572 2304            AMERICAN       CHICAGO-O'HARE (119) 975-8484
    ## 573  430              UNITED         INDIANAPOLIS (489) 534-6272
    ## 574  800            AMERICAN          LOS ANGELES (610) 716-5732
    ## 575  871              ALASKA              SEATTLE (456) 925-4236
    ## 576 2300            AMERICAN       CHICAGO-O'HARE (167) 144-9470
    ## 577 1913      AIR FRANCE/KLM      PARIS-DE GAULLE (848) 149-5208
    ## 578 2423     AIR NEW ZEALAND             AUCKLAND (970) 908-2298
    ## 579 2430              UNITED             HONOLULU (843) 120-5653
    ## 580 1726          AIR CANADA              TORONTO (139) 727-9901
    ## 581 3003            AMERICAN                MIAMI (817) 824-3849
    ## 582 1493            FRONTIER               DENVER (540) 362-7136
    ## 583 1592           SOUTHWEST          LOS ANGELES (802) 910-1742
    ## 584 1873              UNITED               BOSTON (365) 217-0634
    ## 585 2853              UNITED              SEATTLE (594) 797-7729
    ## 586 2774                 WOW            REYKJAVIK (900) 462-1379
    ## 587  455              ALASKA            LAS VEGAS (998) 692-1900
    ## 588  884              ALASKA              SEATTLE (507) 483-3618
    ## 589 1793              UNITED       RALEIGH-DURHAM (380) 449-7849
    ## 590  411              UNITED              PHOENIX (706) 836-7047
    ## 591 1665            EMIRATES                DUBAI (698) 462-6742
    ## 592 1638          KOREAN AIR                SEOUL (753) 726-0123
    ## 593  397              ALASKA          LOS ANGELES (347) 782-5787
    ## 594 1677      AIR FRANCE/KLM      PARIS-DE GAULLE (716) 184-1232
    ## 595 1384         UNITED INTL      PARIS-DE GAULLE (969) 555-0453
    ## 596 1242         UNITED INTL         TOKYO-NARITA (265) 286-5671
    ## 597 1807         UNITED INTL      LONDON HEATHROW (162) 332-5838
    ## 598 1505        HAWAIIAN AIR             HONOLULU (229) 604-7790
    ## 599  367              ALASKA    SAN JOSE DEL CABO (892) 301-0333
    ## 600 1286         UNITED INTL      LONDON HEATHROW (331) 472-8624
    ## 601  317              UNITED            SAN DIEGO (301) 534-5754
    ## 602 1886  SINGAPORE AIRLINES            SINGAPORE (341) 473-0639
    ## 603 2534             JETBLUE               BOSTON (835) 882-3693
    ## 604 2954           SOUTHWEST          LOS ANGELES (589) 194-0523
    ## 605 1959              UNITED               BOSTON (322) 843-0185
    ## 606 1553           AIR CHINA              BEIJING (190) 975-2514
    ## 607 2694              UNITED       CHICAGO-O'HARE (909) 382-3774
    ## 608    2           SOUTHWEST            SAN DIEGO (347) 896-3463
    ## 609  868            AMERICAN     DALLAS-FT. WORTH (355) 550-1392
    ## 610 1413              UNITED            LAS VEGAS (705) 456-1905
    ## 611 3266              UNITED            LAS VEGAS (836) 207-8419
    ## 612 2492              UNITED               DENVER (306) 552-1875
    ## 613 1172               DELTA         NEW YORK-JFK (944) 189-7555
    ## 614 1401         UNITED INTL      PARIS-DE GAULLE (458) 404-9558
    ## 615 1703      AIR FRANCE/KLM      PARIS-DE GAULLE (481) 522-1039
    ## 616 1767            AMERICAN       CHICAGO-O'HARE (376) 611-4588
    ## 617 2019              UNITED               NEWARK (641) 544-6549
    ## 618 2695      AIR FRANCE/KLM      PARIS-DE GAULLE (572) 748-6932
    ## 619 3001            AMERICAN                MIAMI (227) 801-6148
    ## 620 2816               DELTA          LOS ANGELES (519) 573-6576
    ## 621 1506        HAWAIIAN AIR             HONOLULU (900) 586-1787
    ## 622 3196               DELTA          LOS ANGELES (239) 325-5321
    ## 623 1608     VIRGIN ATLANTIC      LONDON HEATHROW (367) 897-7969
    ## 624 2427     AIR NEW ZEALAND             AUCKLAND (621) 874-9973
    ## 625 1407              UNITED            LAS VEGAS (426) 342-7378
    ## 626 1610     VIRGIN ATLANTIC      LONDON HEATHROW (765) 191-1797
    ## 627 1800         UNITED INTL      LONDON HEATHROW (400) 250-0871
    ## 628 1923      AIR FRANCE/KLM      PARIS-DE GAULLE (434) 725-0561
    ## 629 2173            FRONTIER               DENVER (969) 207-3261
    ## 630 2600            INTERJET          GUADALAJARA (994) 688-3259
    ## 631 1623     VIRGIN ATLANTIC      LONDON HEATHROW (494) 308-3048
    ## 632 3084                COPA          PANAMA CITY (715) 288-8832
    ## 633 2899               DELTA       SALT LAKE CITY (594) 448-6242
    ## 634  463              ALASKA            LAS VEGAS (802) 810-5574
    ## 635 2309              UNITED          SAN ANTONIO (867) 891-0871
    ## 636 2864              UNITED              SEATTLE (533) 213-4368
    ## 637 2593              UNITED       CHICAGO-O'HARE (324) 188-6781
    ## 638 1847           SOUTHWEST            SAN DIEGO (102) 928-7959
    ## 639 2141            FRONTIER               DENVER (439) 568-6611
    ## 640  298              UNITED            SAN DIEGO (767) 205-0604
    ## 641 2603             JETBLUE               BOSTON (890) 548-9219
    ## 642 2040              UNITED               NEWARK (649) 925-8489
    ## 643 2862              UNITED              SEATTLE (156) 837-4491
    ## 644  415              UNITED              PHOENIX (882) 304-9032
    ## 645 3248            AMERICAN              PHOENIX (971) 548-6611
    ## 646  715      CATHAY PACIFIC            HONG KONG (828) 153-5819
    ## 647 2574             JETBLUE           LONG BEACH (906) 850-9192
    ## 648 3108                COPA          PANAMA CITY (859) 777-8245
    ## 649  500              ALASKA             PORTLAND (589) 270-7518
    ## 650 1295             JETBLUE         NEW YORK-JFK (146) 129-5118
    ## 651  736      CATHAY PACIFIC            HONG KONG (927) 747-9822
    ## 652 1482            FRONTIER               DENVER (481) 479-7013
    ## 653  392              ALASKA          LOS ANGELES (716) 777-3762
    ## 654 2594            INTERJET          GUADALAJARA (217) 589-0596
    ## 655 1131               DELTA MINNEAPOLIS-ST. PAUL (637) 782-6989
    ## 656  641              ALASKA WASHINGTON DC-DULLES (359) 803-9809
    ## 657 2083             JETBLUE           LONG BEACH (416) 788-2844
    ## 658  580              ALASKA              SEATTLE (729) 609-4819
    ## 659  859            AMERICAN     DALLAS-FT. WORTH (201) 737-4409
    ## 660 1578               DELTA              DETROIT (137) 611-3694
    ## 661 2715               DELTA          LOS ANGELES (812) 869-6263
    ## 662  749              ALASKA         NEW YORK-JFK (194) 198-0504
    ## 663 3171               DELTA          LOS ANGELES (299) 137-6993
    ## 664 1105               DELTA MINNEAPOLIS-ST. PAUL (739) 710-2966
    ## 665 1908              UNITED               BOSTON (477) 182-4689
    ## 666  438              UNITED         INDIANAPOLIS (525) 362-5532
    ## 667 2108             JETBLUE           LONG BEACH (687) 887-6766
    ## 668  521            EMIRATES                DUBAI (637) 100-0509
    ## 669  290           SOUTHWEST          LOS ANGELES (603) 149-7268
    ## 670  319           SOUTHWEST          LOS ANGELES (364) 792-5553
    ## 671 1797              ALASKA       RALEIGH-DURHAM (535) 685-8273
    ## 672 2143            FRONTIER               DENVER (826) 738-8316
    ## 673 2588             JETBLUE           LONG BEACH (554) 269-8937
    ## 674 1899           LUFTHANSA               MUNICH (298) 135-0900
    ## 675 1483             AVIANCA         SAN SALVADOR (392) 183-7831
    ## 676 1391         UNITED INTL             SHANGHAI (606) 596-1029
    ## 677 3042              UNITED             PORTLAND (384) 953-4795
    ## 678 1996              UNITED            BALTIMORE (855) 811-8811
    ## 679 2497              UNITED       CHICAGO-O'HARE (419) 295-9580
    ## 680 2551             JETBLUE               BOSTON (919) 486-4251
    ## 681 2906    TURKISH AIRLINES             ISTANBUL (392) 495-7961
    ## 682 1850           SOUTHWEST            SAN DIEGO (506) 760-3043
    ## 683 1704      AIR FRANCE/KLM      PARIS-DE GAULLE (146) 699-3488
    ## 684 2334               DELTA MINNEAPOLIS-ST. PAUL (302) 339-0791
    ## 685 1386         UNITED INTL             SHANGHAI (446) 229-4342
    ## 686 3285           SOUTHWEST               DENVER (249) 602-6985
    ## 687 2073            AMERICAN         PHILADELPHIA (150) 905-6938
    ## 688 2944           SOUTHWEST            SAN DIEGO (116) 689-6617
    ## 689 2513              UNITED               NEWARK (896) 993-8555
    ## 690 1319             JETBLUE         NEW YORK-JFK (466) 912-8401
    ## 691  426              UNITED         INDIANAPOLIS (347) 851-5388
    ## 692 1290             JETBLUE         NEW YORK-JFK (441) 445-6532
    ## 693  650              ALASKA WASHINGTON DC-DULLES (205) 382-5599
    ## 694 2518              ALASKA       SALT LAKE CITY (208) 794-9612
    ## 695  717      AIR FRANCE/KLM      PARIS-DE GAULLE (905) 742-3525
    ## 696 2232           SOUTHWEST            SANTA ANA (147) 535-3529
    ## 697 2400         UNITED INTL              BEIJING (152) 912-4118
    ## 698  452              UNITED     DALLAS-FT. WORTH (634) 521-4714
    ## 699 2515              ALASKA       SALT LAKE CITY (121) 509-7306
    ## 700  568            EMIRATES                DUBAI (105) 635-5212
    ## 701 2269              UNITED          LOS ANGELES (732) 168-0110
    ## 702 1554               DELTA         NEW YORK-JFK (214) 250-8756
    ## 703 1556               DELTA         NEW YORK-JFK (119) 975-8484
    ## 704 2661          AEROMEXICO          MEXICO CITY (489) 534-6272
    ## 705 2842              UNITED       CHICAGO-O'HARE (610) 716-5732
    ## 706 3162              UNITED            SAN DIEGO (456) 925-4236
    ## 707 2168            FRONTIER               DENVER (167) 144-9470
    ## 708 3016              UNITED             PORTLAND (848) 149-5208
    ## 709 1604     VIRGIN ATLANTIC      LONDON HEATHROW (970) 908-2298
    ## 710 2402         UNITED INTL              BEIJING (843) 120-5653
    ## 711 2667          AEROMEXICO          MEXICO CITY (139) 727-9901
    ## 712 2743               DELTA              ATLANTA (817) 824-3849
    ## 713 1224           SOUTHWEST            LAS VEGAS (540) 362-7136
    ## 714 2178       CHINA EASTERN              QINGDAO (802) 910-1742
    ## 715 3251              UNITED            SAN DIEGO (365) 217-0634
    ## 716  372           LUFTHANSA            FRANKFURT (594) 797-7729
    ## 717 1453              UNITED             PORTLAND (900) 462-1379
    ## 718 2324              QANTAS               SYDNEY (998) 692-1900
    ## 719 1132           SOUTHWEST            SAN DIEGO (507) 483-3618
    ## 720  566            EMIRATES                DUBAI (380) 449-7849
    ## 721 3100                COPA          PANAMA CITY (706) 836-7047
    ## 722 1411              UNITED            LAS VEGAS (698) 462-6742
    ## 723 3083 PHILIPPINE AIRLINES               MANILA (753) 726-0123
    ## 724 3113              ALASKA               NEWARK (347) 782-5787
    ## 725 3082              UNITED              SPOKANE (716) 184-1232
    ## 726 2162       CHINA EASTERN              QINGDAO (969) 555-0453
    ## 727 1279      CHINA SOUTHERN                WUHAN (265) 286-5671
    ## 728 2958           SOUTHWEST          LOS ANGELES (162) 332-5838
    ## 729 3225            AMERICAN              PHOENIX (229) 604-7790
    ## 730  474              UNITED               DENVER (892) 301-0333
    ## 731 1775              UNITED       RALEIGH-DURHAM (331) 472-8624
    ## 732 2993              UNITED            BALTIMORE (301) 534-5754
    ## 733 2998              UNITED              BURBANK (341) 473-0639
    ## 734 3027              ALASKA              SEATTLE (835) 882-3693
    ## 735 3011              ALASKA WASHINGTON DC-DULLES (589) 194-0523
    ## 736 3000               DELTA MINNEAPOLIS-ST. PAUL (322) 843-0185
    ## 737 2969              UNITED             HONOLULU (190) 975-2514
    ## 738 3020              ALASKA              SEATTLE (909) 382-3774
    ## 739 2971              UNITED       CHICAGO-O'HARE (347) 896-3463

### **`06-Replacing and removing`**

-   Remove opening and closing parentheses from the `phone` column.
    Store this as a variable called `phone_no_parens`. Remember to use
    `fixed()`!

-   Add a new column to `sfo_survey` called `phone_no_parens` that
    contains the contents of `phone_no_parens`.

-   Create a new column of `sfo_survey` called `phone_clean` containing
    the values of `phone_no_parens` with all hyphens replaced with
    spaces.

``` r
# Remove parentheses from phone column
phone_no_parens <- sfo_survey$phone %>%
  # Remove "("s
  str_remove_all(fixed("(")) %>%
  # Remove ")"s
  str_remove_all(fixed(")"))

# Add phone_no_parens as column
sfo_survey %>%
  mutate(phone_no_parens = phone_no_parens,
  # Replace all hyphens in phone_no_parens with spaces
         phone_clean = str_replace_all(phone_no_parens, "-", " "))
```

    ##        id             airline          destination          phone
    ## 1    1842    TURKISH AIRLINES             ISTANBUL   858 990 5153
    ## 2    1844    TURKISH AIRLINES             ISTANBUL   731-813-2043
    ## 3    1840    TURKISH AIRLINES             ISTANBUL   563-732-6802
    ## 4    1837    TURKISH AIRLINES             ISTANBUL   145 725 4021
    ## 5    1833    TURKISH AIRLINES             ISTANBUL   931 311 5801
    ## 6    3010            AMERICAN                MIAMI (637) 782-6989
    ## 7    1838    TURKISH AIRLINES             ISTANBUL   172 990 3485
    ## 8    1845    TURKISH AIRLINES             ISTANBUL   872 325 4341
    ## 9    2097         UNITED INTL          MEXICO CITY (359) 803-9809
    ## 10   1846    TURKISH AIRLINES             ISTANBUL   152 790 8238
    ## 11   1831    TURKISH AIRLINES             ISTANBUL   330 561 9257
    ## 12   1848    TURKISH AIRLINES             ISTANBUL   437 420 7546
    ## 13   1841    TURKISH AIRLINES             ISTANBUL   495 632 4027
    ## 14   1835    TURKISH AIRLINES             ISTANBUL (416) 788-2844
    ## 15   1849    TURKISH AIRLINES             ISTANBUL   311-305-4367
    ## 16   2289              QANTAS               SYDNEY   817-400-0481
    ## 17    977           LUFTHANSA               MUNICH   430 723 1079
    ## 18    105              UNITED WASHINGTON DC-DULLES (729) 609-4819
    ## 19   1973      CATHAY PACIFIC            HONG KONG (201) 737-4409
    ## 20   2385         UNITED INTL               SYDNEY (137) 611-3694
    ## 21   3209              UNITED        SANTA BARBARA   226 490 8696
    ## 22   1794              ALASKA       RALEIGH-DURHAM   123 570 8640
    ## 23    278              UNITED            BALTIMORE   665 803 2453
    ## 24    517              UNITED       FT. LAUDERDALE (812) 869-6263
    ## 25   1265      CATHAY PACIFIC            HONG KONG   639 132 6386
    ## 26   2885             EVA AIR               TAIPEI (194) 198-0504
    ## 27   2457              UNITED       CHICAGO-O'HARE   437 886 0753
    ## 28   3198              UNITED      ONTARIO (CALIF)   626 756 5089
    ## 29   2128            FRONTIER               DENVER (299) 137-6993
    ## 30   2199           SOUTHWEST            LAS VEGAS   714 950 3364
    ## 31   1376         UNITED INTL             SHANGHAI   653-786-5985
    ## 32   2152          AIR CANADA            VANCOUVER   518 286 5956
    ## 33    621              ALASKA WASHINGTON DC-DULLES   194 960 2145
    ## 34      4           SOUTHWEST          LOS ANGELES   362-136-1153
    ## 35   1541           AIR CHINA              BEIJING   376-456-0697
    ## 36   2242                 WOW            REYKJAVIK   657 832 1189
    ## 37   1039          AIR CANADA              CALGARY   962-918-6117
    ## 38    475              ALASKA             PORTLAND   692 929 3592
    ## 39   2460              ALASKA       SALT LAKE CITY   805-877-3887
    ## 40   2132            FRONTIER               DENVER (739) 710-2966
    ## 41   1594  SINGAPORE AIRLINES            SINGAPORE   819 732 4132
    ## 42   1536           AIR CHINA              BEIJING   367 221 9710
    ## 43   1582               DELTA       SALT LAKE CITY   361 154 1789
    ## 44    684      CATHAY PACIFIC            HONG KONG   680 488 1182
    ## 45    101              UNITED WASHINGTON DC-DULLES   928 638 1186
    ## 46   2455              ALASKA       SALT LAKE CITY   588-693-9875
    ## 47    294              UNITED            SAN DIEGO   681-308-7915
    ## 48   1005          AIR CANADA            VANCOUVER   783 647 8490
    ## 49    464              ALASKA             PORTLAND   897 847 0632
    ## 50   1583         UNITED INTL            HONG KONG   150 952 4453
    ## 51   1579         UNITED INTL            HONG KONG   322-884-3020
    ## 52   1269      CATHAY PACIFIC            HONG KONG   176-313-5403
    ## 53   2654          AEROMEXICO          MEXICO CITY   487 109 4196
    ## 54    519            EMIRATES                DUBAI (477) 182-4689
    ## 55    863              ALASKA              SEATTLE   544 382 8289
    ## 56   1516            INTERJET          GUADALAJARA   781 543 7456
    ## 57    659              UNITED               NEWARK   911 829 6476
    ## 58    656              UNITED               NEWARK (525) 362-5532
    ## 59   2929           SOUTHWEST            SAN DIEGO   517 986 3426
    ## 60    280              UNITED            BALTIMORE   838 220 5397
    ## 61   2649           SOUTHWEST               DENVER (687) 887-6766
    ## 62   1500               DELTA          LOS ANGELES   179 163 0902
    ## 63   2450              UNITED       CHICAGO-O'HARE   539 137 8983
    ## 64   3045            AMERICAN     DALLAS-FT. WORTH   733-154-0094
    ## 65    614              ALASKA               NEWARK   639 881 3693
    ## 66    204            AMERICAN          LOS ANGELES   291-830-3017
    ## 67   1049          AIR CANADA              CALGARY (637) 100-0509
    ## 68   2209           SOUTHWEST            LAS VEGAS   750 520 0167
    ## 69   1471             AVIANCA         SAN SALVADOR   676 485 8963
    ## 70   2296                 WOW            REYKJAVIK   135 566 5090
    ## 71   2412     AIR NEW ZEALAND             AUCKLAND   337 260 4996
    ## 72   2407     AIR NEW ZEALAND             AUCKLAND   371 185 2377
    ## 73   1514            INTERJET          GUADALAJARA   280 461 1386
    ## 74   3040            AMERICAN     DALLAS-FT. WORTH (603) 149-7268
    ## 75    844              ALASKA             PORTLAND (364) 792-5553
    ## 76    881              ALASKA       RALEIGH-DURHAM   496-429-1314
    ## 77   2409     AIR NEW ZEALAND             AUCKLAND   459 671 4698
    ## 78   1527              ALASKA          LOS ANGELES   486-268-3312
    ## 79   1195           SOUTHWEST            LAS VEGAS   497-518-4050
    ## 80   1042          AIR CANADA              CALGARY (535) 685-8273
    ## 81   2957            AMERICAN         NEW YORK-JFK   859 495 4050
    ## 82   2380         UNITED INTL               SYDNEY (826) 738-8316
    ## 83    102              UNITED WASHINGTON DC-DULLES   724-134-3870
    ## 84   2486              UNITED               NEWARK (554) 269-8937
    ## 85    342           LUFTHANSA            FRANKFURT   125-578-4253
    ## 86   1808         UNITED INTL      LONDON HEATHROW   614 800 2861
    ## 87    322              UNITED              BURBANK   487-232-4449
    ## 88   1090               DELTA MINNEAPOLIS-ST. PAUL (298) 135-0900
    ## 89   2211           SOUTHWEST              PHOENIX (392) 183-7831
    ## 90   1555               DELTA              ATLANTA (606) 596-1029
    ## 91   2249              UNITED          BAKERSFIELD (384) 953-4795
    ## 92    879              ALASKA       RALEIGH-DURHAM (855) 811-8811
    ## 93    546              ALASKA              SEATTLE   253-374-7102
    ## 94   2956            AMERICAN         NEW YORK-JFK (419) 295-9580
    ## 95   1439            FRONTIER               DENVER   802 102 8345
    ## 96    243              UNITED            NASHVILLE   417 393 0050
    ## 97    889          AIR CANADA              TORONTO   787-624-8443
    ## 98   1566               DELTA              DETROIT (919) 486-4251
    ## 99    104              UNITED WASHINGTON DC-DULLES   341 824 5322
    ## 100  2338              UNITED               AUSTIN   415 551 1608
    ## 101  1585               DELTA       SALT LAKE CITY (392) 495-7961
    ## 102   326              UNITED              BURBANK   473-238-3324
    ## 103  2670           SOUTHWEST               DENVER (506) 760-3043
    ## 104   869              ALASKA              SEATTLE   876-834-0624
    ## 105   279              UNITED            BALTIMORE   919 611 6170
    ## 106  3201              UNITED      ONTARIO (CALIF) (146) 699-3488
    ## 107  1363         UNITED INTL      PARIS-DE GAULLE   261 434 7760
    ## 108  1365         UNITED INTL      PARIS-DE GAULLE   617 310 2684
    ## 109  1504               DELTA          LOS ANGELES   182-535-3412
    ## 110   745          AIR CANADA              TORONTO   506 129 1694
    ## 111  2765                 WOW            REYKJAVIK (302) 339-0791
    ## 112   272              UNITED          LOS ANGELES (446) 229-4342
    ## 113  1360         UNITED INTL      PARIS-DE GAULLE (249) 602-6985
    ## 114  3046            AMERICAN          LOS ANGELES (150) 905-6938
    ## 115  2088            AMERICAN              PHOENIX   313 990 8823
    ## 116   670      AIR FRANCE/KLM      PARIS-DE GAULLE   656-941-5355
    ## 117  2673      AIR FRANCE/KLM      PARIS-DE GAULLE (116) 689-6617
    ## 118  2252              UNITED          LOS ANGELES   955 324 5981
    ## 119  2677      AIR FRANCE/KLM      PARIS-DE GAULLE   175 808 2189
    ## 120   750          AIR CANADA              TORONTO (896) 993-8555
    ## 121   938          AIR CANADA            VANCOUVER   105-687-6500
    ## 122  1958      CATHAY PACIFIC            HONG KONG   757 524 2964
    ## 123  2834              UNITED       CHICAGO-O'HARE   201 374 2424
    ## 124  1458            FRONTIER               DENVER (466) 912-8401
    ## 125  2229           SOUTHWEST            SANTA ANA   766 112 6143
    ## 126   327              UNITED              BURBANK   783-463-4865
    ## 127  2179              QANTAS               SYDNEY   853-803-9900
    ## 128  1487        HAWAIIAN AIR             HONOLULU (347) 851-5388
    ## 129  1507               DELTA          LOS ANGELES   992 114 6973
    ## 130   402              UNITED         HOUSTON-BUSH   316-212-7309
    ## 131   949              ALASKA         NEW YORK-JFK   301 672 1092
    ## 132     1           SOUTHWEST            SAN DIEGO   795 137 0201
    ## 133   893          AIR CANADA              TORONTO   381-883-5497
    ## 134  2180              QANTAS               SYDNEY   100-531-4642
    ## 135  1858              UNITED          LOS ANGELES   994 923 6634
    ## 136  2001      AIR FRANCE/KLM      PARIS-DE GAULLE   920 355 8404
    ## 137  1454              UNITED            LAS VEGAS (441) 445-6532
    ## 138  2389         UNITED INTL              BEIJING   325 795 2455
    ## 139  2059            AMERICAN         PHILADELPHIA   593 829 6250
    ## 140   419              UNITED         INDIANAPOLIS   566-482-9004
    ## 141  1276      CHINA SOUTHERN                WUHAN   542 537 6770
    ## 142  1434            FRONTIER               DENVER   716 191 1741
    ## 143  2181              QANTAS               SYDNEY   491-727-7162
    ## 144  3043            AMERICAN     DALLAS-FT. WORTH   167-336-5660
    ## 145  2548             JETBLUE               BOSTON   358 831 0725
    ## 146  2703              UNITED       CHICAGO-O'HARE   432 979 7292
    ## 147  2533             JETBLUE           LONG BEACH (205) 382-5599
    ## 148  2904    TURKISH AIRLINES             ISTANBUL (208) 794-9612
    ## 149  2101            AMERICAN              PHOENIX   728 662 3934
    ## 150  2138            FRONTIER               DENVER   380-918-8572
    ## 151  2193          AER LINGUS               DUBLIN (905) 742-3525
    ## 152  3047            AMERICAN          LOS ANGELES   151 434 6989
    ## 153  2845              UNITED              SEATTLE   755 544 2629
    ## 154  1957      CATHAY PACIFIC            HONG KONG   633 181 4494
    ## 155  1370         UNITED INTL      PARIS-DE GAULLE   346 706 5964
    ## 156  1427              UNITED          KANSAS CITY   688 690 2184
    ## 157  1975      CATHAY PACIFIC            HONG KONG   618 717 1697
    ## 158   469              ALASKA             PORTLAND   185-321-6877
    ## 159  3087              ALASKA               NEWARK (147) 535-3529
    ## 160  2686      AIR FRANCE/KLM      PARIS-DE GAULLE (152) 912-4118
    ## 161  1830    TURKISH AIRLINES             ISTANBUL   726 943 7486
    ## 162   318              UNITED              BURBANK (634) 521-4714
    ## 163  2802           SOUTHWEST              PHOENIX   670-248-0186
    ## 164  2182              QANTAS               SYDNEY (121) 509-7306
    ## 165  2628             JETBLUE         NEW YORK-JFK (105) 635-5212
    ## 166  2521             JETBLUE           LONG BEACH (732) 168-0110
    ## 167   291              UNITED            SAN DIEGO   364 834 3150
    ## 168  2340              UNITED               AUSTIN   176 508 2778
    ## 169  1274         UNITED INTL      LONDON HEATHROW   120 941 0833
    ## 170  2356             JETBLUE       FT. LAUDERDALE   670 902 3199
    ## 171  2034              UNITED               NEWARK (214) 250-8756
    ## 172   965           LUFTHANSA               MUNICH (119) 975-8484
    ## 173  2835              UNITED       CHICAGO-O'HARE   297 484 3285
    ## 174  1197           SOUTHWEST            LAS VEGAS (489) 534-6272
    ## 175   325              UNITED              BURBANK (610) 716-5732
    ## 176   846              ALASKA             PORTLAND (456) 925-4236
    ## 177  2239           SOUTHWEST              PHOENIX   743-103-7645
    ## 178  1329         UNITED INTL             SHANGHAI   432-281-3682
    ## 179  1588         UNITED INTL            HONG KONG (167) 144-9470
    ## 180  2683      AIR FRANCE/KLM      PARIS-DE GAULLE   648 685 6188
    ## 181  2844              UNITED              SEATTLE   548 191 4898
    ## 182  2240           SOUTHWEST              PHOENIX   288 110 9483
    ## 183  1981      AIR FRANCE/KLM            AMSTERDAM   946-558-5801
    ## 184  1007          AIR CANADA            VANCOUVER   388 744 9637
    ## 185  1373         UNITED INTL             SHANGHAI   506 463 9129
    ## 186  1851              ALASKA              KAHULUI (848) 149-5208
    ## 187  2226           SOUTHWEST            SANTA ANA (970) 908-2298
    ## 188  2248              UNITED          LOS ANGELES (843) 120-5653
    ## 189   849              ALASKA             PORTLAND   306 394 8640
    ## 190  1896              UNITED               BOSTON   170-641-3537
    ## 191  1489        HAWAIIAN AIR             HONOLULU   860 723 5066
    ## 192  2183              QANTAS               SYDNEY   814-895-6610
    ## 193   297              UNITED            SAN DIEGO (139) 727-9901
    ## 194  2903    TURKISH AIRLINES             ISTANBUL   598 735 8557
    ## 195   845            AMERICAN     DALLAS-FT. WORTH   593 895 6761
    ## 196  2418              UNITED             HONOLULU (817) 824-3849
    ## 197   516      AIR FRANCE/KLM      PARIS-DE GAULLE   508 484 9738
    ## 198  1914      AIR FRANCE/KLM      PARIS-DE GAULLE   719 489 4724
    ## 199  2002      AIR FRANCE/KLM      PARIS-DE GAULLE   503-671-4901
    ## 200  3102                COPA          PANAMA CITY   275 649 8183
    ## 201  2843              UNITED              SEATTLE   968 130 7012
    ## 202  1372         UNITED INTL      PARIS-DE GAULLE   290 367 6676
    ## 203   252          AIR CANADA              TORONTO   499 766 9941
    ## 204  2379         UNITED INTL              BEIJING   538-393-2243
    ## 205  1327         UNITED INTL             SHANGHAI (540) 362-7136
    ## 206   664              UNITED               NEWARK (802) 910-1742
    ## 207  2550              UNITED       CHICAGO-O'HARE   845 544 4748
    ## 208  1523             JETBLUE         NEW YORK-JFK   784-458-8425
    ## 209  2292                 WOW            REYKJAVIK (365) 217-0634
    ## 210  1546           AIR CHINA              BEIJING   708 500 2758
    ## 211  1161               DELTA         NEW YORK-JFK (594) 797-7729
    ## 212   837            AMERICAN     DALLAS-FT. WORTH   982 555 9504
    ## 213   103              UNITED WASHINGTON DC-DULLES   477-307-3338
    ## 214  2254              UNITED          LOS ANGELES   744-301-1148
    ## 215  1764            AMERICAN       CHICAGO-O'HARE   389 484 8888
    ## 216  2158       CHINA EASTERN              QINGDAO   739 303 6128
    ## 217  2557              UNITED       CHICAGO-O'HARE   175 905 9962
    ## 218  2383         UNITED INTL               SYDNEY (900) 462-1379
    ## 219   842            AMERICAN     DALLAS-FT. WORTH   878-636-2294
    ## 220   413              UNITED              PHOENIX (998) 692-1900
    ## 221   401              UNITED         HOUSTON-BUSH   994 421 8642
    ## 222  2569            INTERJET          GUADALAJARA   304-225-5895
    ## 223  2803           SOUTHWEST              PHOENIX   931-522-5498
    ## 224   293              UNITED            SAN DIEGO   838 898 2275
    ## 225  1509               DELTA          LOS ANGELES   718 187 7125
    ## 226  1192           SOUTHWEST            LAS VEGAS   943 561 8955
    ## 227   191            AMERICAN          LOS ANGELES   341 261 6456
    ## 228  1558               DELTA              ATLANTA (507) 483-3618
    ## 229  1447            FRONTIER               DENVER   826 266 0205
    ## 230  3061                COPA          PANAMA CITY (380) 449-7849
    ## 231   886              ALASKA       RALEIGH-DURHAM   589-975-0198
    ## 232   330              ALASKA            SAN DIEGO   657 680 8781
    ## 233  1462            FRONTIER               DENVER   501-668-7869
    ## 234  2746               DELTA              ATLANTA   224 365 8299
    ## 235  3206              UNITED               EUGENE   747 588 1968
    ## 236   741              ALASKA         NEW YORK-JFK (706) 836-7047
    ## 237  2299              UNITED          SAN ANTONIO (698) 462-6742
    ## 238     3           SOUTHWEST          LOS ANGELES   748 446 1257
    ## 239   271              UNITED          LOS ANGELES   525-552-4162
    ## 240  1452              UNITED            LAS VEGAS   635-714-8302
    ## 241  3041            AMERICAN     DALLAS-FT. WORTH   114 668 2834
    ## 242  2543              UNITED       CHICAGO-O'HARE   404 788 2855
    ## 243   576             JETBLUE               BOSTON   673 292 2444
    ## 244  1584               DELTA       SALT LAKE CITY   136 788 1426
    ## 245  1133           SOUTHWEST          LOS ANGELES   564 677 3934
    ## 246  2191          AER LINGUS               DUBLIN   213 981 7762
    ## 247  2112            AMERICAN              PHOENIX   453-556-0852
    ## 248   866              ALASKA              SEATTLE   423 593 4483
    ## 249  3122               DELTA       SALT LAKE CITY   820 609 7454
    ## 250  2456              UNITED               DENVER   131-641-1331
    ## 251   847            AMERICAN     DALLAS-FT. WORTH   824 540 9579
    ## 252  2659           SOUTHWEST               DENVER (753) 726-0123
    ## 253  3208            AMERICAN          LOS ANGELES   739 737 9041
    ## 254  2846              UNITED              SEATTLE   609-332-7370
    ## 255   963           LUFTHANSA               MUNICH   426-182-1365
    ## 256  2537              UNITED       CHICAGO-O'HARE (347) 782-5787
    ## 257   615              ALASKA WASHINGTON DC-DULLES   898-210-6218
    ## 258  1173           SOUTHWEST            LAS VEGAS   938 394 0411
    ## 259  2652           SOUTHWEST               DENVER   904 844 1759
    ## 260  2195           SOUTHWEST            LAS VEGAS   658-861-4306
    ## 261  2453              UNITED               DENVER (716) 184-1232
    ## 262   323              UNITED              BURBANK   380-105-1757
    ## 263   748          AIR CANADA              TORONTO (969) 555-0453
    ## 264   840            AMERICAN     DALLAS-FT. WORTH   249 452 4370
    ## 265  2451              ALASKA       SALT LAKE CITY   443 384 8253
    ## 266  1429              UNITED          KANSAS CITY   711 289 2247
    ## 267   241              UNITED            NASHVILLE   343-973-0193
    ## 268  2458              ALASKA       SALT LAKE CITY   367 650 3720
    ## 269  1589           SOUTHWEST          LOS ANGELES (265) 286-5671
    ## 270  2404      AIR FRANCE/KLM      PARIS-DE GAULLE   148 630 8560
    ## 271  1044          AIR CANADA              CALGARY   413-727-2672
    ## 272  1855              ALASKA              KAHULUI (162) 332-5838
    ## 273  1178           SOUTHWEST            LAS VEGAS (229) 604-7790
    ## 274  1009          AIR CANADA            VANCOUVER (892) 301-0333
    ## 275  2192           SOUTHWEST            LAS VEGAS   148 501 5084
    ## 276  3091              ALASKA               NEWARK   564 780 8272
    ## 277  1266      CATHAY PACIFIC            HONG KONG   524 190 0899
    ## 278  3207            AMERICAN          LOS ANGELES   463 792 2782
    ## 279  1853              ALASKA              KAHULUI (331) 472-8624
    ## 280  2134            FRONTIER               DENVER   522-286-5318
    ## 281  1569               DELTA              DETROIT   310 719 4550
    ## 282  2676           SOUTHWEST               DENVER   314-360-4288
    ## 283  1235       CHINA EASTERN             SHANGHAI (301) 534-5754
    ## 284  1525              ALASKA          LOS ANGELES   822 271 5719
    ## 285  2189          AER LINGUS               DUBLIN (341) 473-0639
    ## 286     5           SOUTHWEST          LOS ANGELES (835) 882-3693
    ## 287  1529              ALASKA          LOS ANGELES   465-550-6610
    ## 288  1443            FRONTIER               DENVER   388 100 1482
    ## 289  2294              UNITED          SAN ANTONIO (589) 194-0523
    ## 290  1368         UNITED INTL      PARIS-DE GAULLE   982 842 4913
    ## 291  1539           AIR CHINA              BEIJING   671 913 4563
    ## 292  3163            AMERICAN     DALLAS-FT. WORTH   144 468 5864
    ## 293  2421              UNITED             HONOLULU   194 344 4039
    ## 294  2411     AIR NEW ZEALAND             AUCKLAND   331 747 5714
    ## 295  2381         UNITED INTL              BEIJING   221-190-1449
    ## 296  1047          AIR CANADA              CALGARY (322) 843-0185
    ## 297  2590            INTERJET          GUADALAJARA   676-614-9095
    ## 298  1586         UNITED INTL            HONG KONG (190) 975-2514
    ## 299  2621          AEROMEXICO          MEXICO CITY (909) 382-3774
    ## 300   277              UNITED            BALTIMORE   956 257 9319
    ## 301  1239         UNITED INTL         TOKYO-NARITA   362 145 8268
    ## 302   375              ALASKA          LOS ANGELES   570 727 3998
    ## 303  2290                 WOW            REYKJAVIK   225 964 9193
    ## 304  2288              UNITED          SAN ANTONIO   227 419 9482
    ## 305   618              ALASKA WASHINGTON DC-DULLES   949 360 7605
    ## 306  1040          AIR CANADA              CALGARY (347) 896-3463
    ## 307  2145          AIR CANADA            VANCOUVER   794 939 9735
    ## 308  1795              ALASKA       RALEIGH-DURHAM   413 754 3034
    ## 309   387              ALASKA         INDIANAPOLIS   806 730 0459
    ## 310  2789               DELTA       SALT LAKE CITY   791 195 8909
    ## 311  1095           SOUTHWEST            SAN DIEGO   691-318-3535
    ## 312  2111         UNITED INTL          MEXICO CITY   852-386-6029
    ## 313  2777               DELTA              ATLANTA   380 682 7795
    ## 314   662              UNITED               NEWARK (355) 550-1392
    ## 315  1176           SOUTHWEST            LAS VEGAS   247 586 4579
    ## 316  1470             AVIANCA         SAN SALVADOR   235 257 1041
    ## 317  2391         UNITED INTL              BEIJING (705) 456-1905
    ## 318  2573            INTERJET          GUADALAJARA   106 756 2785
    ## 319  3058            AMERICAN                MIAMI (836) 207-8419
    ## 320  1100           SOUTHWEST            SAN DIEGO (306) 552-1875
    ## 321     6           SOUTHWEST          LOS ANGELES   729-102-7511
    ## 322  1825         UNITED INTL      LONDON HEATHROW   795 583 0958
    ## 323  2926           SOUTHWEST            SAN DIEGO   700-431-3918
    ## 324   384              ALASKA         INDIANAPOLIS   929 632 1068
    ## 325  1942              UNITED               BOSTON   763 906 2495
    ## 326  2943           SOUTHWEST          LOS ANGELES   469 976 6796
    ## 327  2149          AIR CANADA            VANCOUVER   887-657-4143
    ## 328   954              ALASKA         NEW YORK-JFK   574-438-5329
    ## 329  1103           SOUTHWEST            SAN DIEGO   319 127 9518
    ## 330   400              UNITED              PHOENIX   429 960 9710
    ## 331  2454              UNITED       CHICAGO-O'HARE   419 646 0299
    ## 332  2236           SOUTHWEST              PHOENIX   192 343 8515
    ## 333   296              UNITED            SAN DIEGO   521 336 8581
    ## 334  1564               DELTA              DETROIT   776 367 6109
    ## 335  3070              UNITED              SPOKANE   470 367 1392
    ## 336  2524             JETBLUE               BOSTON (944) 189-7555
    ## 337  2836              UNITED       CHICAGO-O'HARE   998-931-4783
    ## 338   270              UNITED          LOS ANGELES   362-178-6307
    ## 339  3060            AMERICAN                MIAMI (458) 404-9558
    ## 340  1431            FRONTIER               DENVER   212 286 7936
    ## 341  3012              UNITED             PORTLAND (481) 522-1039
    ## 342  1485        HAWAIIAN AIR             HONOLULU (376) 611-4588
    ## 343  3264              UNITED            SAN DIEGO   936 193 9690
    ## 344  2000              UNITED               NEWARK (641) 544-6549
    ## 345   100              UNITED WASHINGTON DC-DULLES   797-870-7818
    ## 346  2891              UNITED               AUSTIN   693 907 5353
    ## 347   303           SOUTHWEST          LOS ANGELES   332 973 4943
    ## 348  1379         UNITED INTL             SHANGHAI   929 622 9077
    ## 349  1810         UNITED INTL      LONDON HEATHROW   649-379-5361
    ## 350  2482              ALASKA       SALT LAKE CITY (572) 748-6932
    ## 351  2640             JETBLUE         NEW YORK-JFK   395-892-5646
    ## 352  3262              UNITED            SAN DIEGO   221-628-9561
    ## 353  2031     BRITISH AIRWAYS      LONDON HEATHROW (227) 801-6148
    ## 354  2700              UNITED              ORLANDO   549-649-1864
    ## 355  2035     BRITISH AIRWAYS      LONDON HEATHROW   342 941 0439
    ## 356  3066                COPA          PANAMA CITY   701 390 9814
    ## 357  3074                COPA          PANAMA CITY (519) 573-6576
    ## 358  2014     BRITISH AIRWAYS      LONDON HEATHROW   919-342-0230
    ## 359  2036     BRITISH AIRWAYS      LONDON HEATHROW   364-759-2705
    ## 360  2244                 WOW            REYKJAVIK   949 543 7906
    ## 361  1695              ALASKA            SANTA ANA   942 732 6403
    ## 362  3089              UNITED              SPOKANE (900) 586-1787
    ## 363  2920               DELTA              SEATTLE   308-607-9855
    ## 364   958          AIR CANADA            VANCOUVER   764 645 5740
    ## 365  2284                 WOW            REYKJAVIK   472-337-8838
    ## 366  1181           SOUTHWEST            LAS VEGAS   791 847 7278
    ## 367   830              ALASKA            NASHVILLE   128 805 3828
    ## 368  2732      AIR FRANCE/KLM      PARIS-DE GAULLE   365-832-0674
    ## 369  1428              UNITED            LAS VEGAS   123-282-3494
    ## 370  3039              UNITED             PORTLAND   285 424 4318
    ## 371  2251                 WOW            REYKJAVIK   452 352 1387
    ## 372   725      AIR FRANCE/KLM      PARIS-DE GAULLE   129-377-8159
    ## 373  2425     AIR NEW ZEALAND             AUCKLAND   222 143 3131
    ## 374  2909               DELTA              SEATTLE   162-451-0594
    ## 375   728      CATHAY PACIFIC            HONG KONG (239) 325-5321
    ## 376  2171            FRONTIER               DENVER   436-422-6171
    ## 377   506              ALASKA             PORTLAND   605 284 4260
    ## 378  2592          AEROMEXICO          MEXICO CITY   929-102-5905
    ## 379  3258              UNITED            SAN DIEGO   847 507 8268
    ## 380  2046     BRITISH AIRWAYS      LONDON HEATHROW   452-811-8088
    ## 381  2736      AIR FRANCE/KLM      PARIS-DE GAULLE   799 143 1677
    ## 382   820              ALASKA             PORTLAND   196 756 4555
    ## 383   649         UNITED INTL               MUNICH   119-444-0817
    ## 384  2319              QANTAS               SYDNEY   885 454 0883
    ## 385  1864              UNITED          LOS ANGELES   945-998-0444
    ## 386  2689      AIR FRANCE/KLM      PARIS-DE GAULLE (367) 897-7969
    ## 387  1028          AIR CANADA            VANCOUVER   163 241 9321
    ## 388  1441              UNITED            LAS VEGAS   594-176-5811
    ## 389  2602            INTERJET          GUADALAJARA (621) 874-9973
    ## 390  2030     BRITISH AIRWAYS      LONDON HEATHROW   332 963 4103
    ## 391  2214           SOUTHWEST              PHOENIX   389 318 3975
    ## 392  2281              QANTAS               SYDNEY   894-593-7953
    ## 393  1658            EMIRATES                DUBAI   561-266-7842
    ## 394   370              ALASKA    SAN JOSE DEL CABO   354-958-8052
    ## 395  2117             JETBLUE           LONG BEACH   151 921 2775
    ## 396   498              ALASKA             PORTLAND   901 140 3759
    ## 397  1416              UNITED            LAS VEGAS (426) 342-7378
    ## 398  2122      CATHAY PACIFIC            HONG KONG   333-520-4811
    ## 399  2322              QANTAS               SYDNEY (765) 191-1797
    ## 400  2215          AER LINGUS               DUBLIN   850 914 9348
    ## 401   901          AIR CANADA              TORONTO   246 272 9019
    ## 402  1193           SOUTHWEST            LAS VEGAS (400) 250-0871
    ## 403  2900                 WOW            REYKJAVIK   504 419 9191
    ## 404  2939           SOUTHWEST            SAN DIEGO (434) 725-0561
    ## 405   987           LUFTHANSA               MUNICH   231 863 7554
    ## 406  2038     BRITISH AIRWAYS      LONDON HEATHROW (969) 207-3261
    ## 407  3019              UNITED             PORTLAND   939 253 9048
    ## 408   478              UNITED               DENVER   879-154-4494
    ## 409  2366              UNITED               AUSTIN   577 786 2546
    ## 410   220            AMERICAN          LOS ANGELES (994) 688-3259
    ## 411  1353         UNITED INTL             SHANGHAI   841-717-4447
    ## 412   192            AMERICAN          LOS ANGELES   397-353-6309
    ## 413  1967      CATHAY PACIFIC            HONG KONG   558 191 4548
    ## 414  1984      AIR FRANCE/KLM            AMSTERDAM   905 768 2297
    ## 415  2740      AIR FRANCE/KLM      PARIS-DE GAULLE   348 522 2051
    ## 416  2187              QANTAS               SYDNEY   307-323-6861
    ## 417   872            AMERICAN     DALLAS-FT. WORTH   663 886 2487
    ## 418   617              ALASKA               NEWARK   274 944 6097
    ## 419  1678      CATHAY PACIFIC            HONG KONG (494) 308-3048
    ## 420  1898           LUFTHANSA               MUNICH   328 307 0875
    ## 421  1706      AIR FRANCE/KLM      PARIS-DE GAULLE   673 524 3504
    ## 422  2198      CATHAY PACIFIC            HONG KONG   934 721 0615
    ## 423   667              UNITED               NEWARK   102-957-6486
    ## 424  1073          AIR CANADA              CALGARY (715) 288-8832
    ## 425  2612             JETBLUE               BOSTON   794 925 8846
    ## 426  2698      AIR FRANCE/KLM      PARIS-DE GAULLE   637 281 4111
    ## 427  2298              QANTAS               SYDNEY (594) 448-6242
    ## 428  2156            FRONTIER               DENVER   335 802 2651
    ## 429   286              UNITED            NASHVILLE   560 699 9908
    ## 430   601             JETBLUE               BOSTON   451 163 0102
    ## 431  1696              ALASKA            SANTA ANA   613 800 0835
    ## 432  2905               DELTA              SEATTLE   370 453 5800
    ## 433  2894               DELTA       SALT LAKE CITY   192 507 5411
    ## 434   346           LUFTHANSA            FRANKFURT   182-227-4838
    ## 435  1108               DELTA MINNEAPOLIS-ST. PAUL   647 126 2332
    ## 436  2811               DELTA          LOS ANGELES   606 125 6957
    ## 437   681      AIR FRANCE/KLM      PARIS-DE GAULLE   728-404-5558
    ## 438  2615          AEROMEXICO          MEXICO CITY   506 812 6052
    ## 439  2434     AIR NEW ZEALAND             AUCKLAND   427-665-3475
    ## 440  1982      AIR FRANCE/KLM            AMSTERDAM   808 739 7162
    ## 441  1478              ALASKA         NEW YORK-JFK   375-978-3305
    ## 442  2490              UNITED       CHICAGO-O'HARE   181-708-2089
    ## 443  3200      CATHAY PACIFIC            HONG KONG (802) 810-5574
    ## 444  3165               DELTA          LOS ANGELES   242-540-4234
    ## 445  1880              UNITED               BOSTON   105 116 9695
    ## 446  1759           LUFTHANSA               MUNICH (867) 891-0871
    ## 447  2764               DELTA              ATLANTA   945 144 7892
    ## 448  2914               DELTA              SEATTLE   531 895 6695
    ## 449  3152               DELTA       SALT LAKE CITY (533) 213-4368
    ## 450  3269              UNITED            LAS VEGAS   894 810 2674
    ## 451   502              ALASKA             PORTLAND   943 812 6349
    ## 452   683      AIR FRANCE/KLM      PARIS-DE GAULLE   476 168 4235
    ## 453   635              ALASKA WASHINGTON DC-DULLES   931 385 6757
    ## 454  1328         UNITED INTL               KANSAI (324) 188-6781
    ## 455  2869              UNITED WASHINGTON DC-DULLES   100 378 8095
    ## 456  2262              UNITED          BAKERSFIELD         0244-5
    ## 457  1866              ALASKA              KAHULUI   397-362-5469
    ## 458  1990      AIR FRANCE/KLM            AMSTERDAM (102) 928-7959
    ## 459  1356         UNITED INTL               KANSAI (439) 568-6611
    ## 460  2511              ALASKA       SALT LAKE CITY (767) 205-0604
    ## 461  2468              ALASKA       SALT LAKE CITY (890) 548-9219
    ## 462  1686      CATHAY PACIFIC            HONG KONG   938 982 5585
    ## 463  2500              ALASKA       SALT LAKE CITY   769-472-2992
    ## 464  2305              QANTAS               SYDNEY   190 204 1154
    ## 465   453              ALASKA            LAS VEGAS (649) 925-8489
    ## 466  1063          AIR CANADA              CALGARY   321 616 1013
    ## 467  2442             JETBLUE               BOSTON (156) 837-4491
    ## 468   924          AIR CANADA              TORONTO   178-232-0815
    ## 469  2716           SOUTHWEST               DENVER (882) 304-9032
    ## 470  1206           SOUTHWEST            LAS VEGAS   828 549 6666
    ## 471   360              ALASKA               NEWARK   280 544 4554
    ## 472   479              ALASKA             PORTLAND   183 208 5054
    ## 473   304              UNITED            SAN DIEGO (971) 548-6611
    ## 474  2250                 WOW            REYKJAVIK (828) 153-5819
    ## 475  1414              UNITED          KANSAS CITY   203 448 1522
    ## 476   295           SOUTHWEST          LOS ANGELES   900-871-9056
    ## 477  2039     BRITISH AIRWAYS      LONDON HEATHROW   406-167-1379
    ## 478  2491              ALASKA       SALT LAKE CITY (906) 850-9192
    ## 479  1255         UNITED INTL         TOKYO-NARITA (859) 777-8245
    ## 480  2041     BRITISH AIRWAYS      LONDON HEATHROW   641-635-8466
    ## 481  1718      AIR FRANCE/KLM      PARIS-DE GAULLE   807-671-6158
    ## 482  2047     BRITISH AIRWAYS      LONDON HEATHROW (589) 270-7518
    ## 483  2253                 WOW            REYKJAVIK   768 529 8051
    ## 484   754              ALASKA         NEW YORK-JFK   220 660 0306
    ## 485  3235            AMERICAN              PHOENIX   928-179-7556
    ## 486  3204      CATHAY PACIFIC            HONG KONG   153 756 0278
    ## 487   551            EMIRATES                DUBAI   273 829 9197
    ## 488  1770            AMERICAN       CHICAGO-O'HARE   269 463 0911
    ## 489  2874             EVA AIR               TAIPEI   696 984 8826
    ## 490  2020     BRITISH AIRWAYS      LONDON HEATHROW   905-903-5258
    ## 491  2021     BRITISH AIRWAYS      LONDON HEATHROW   267 332 4709
    ## 492  2172            FRONTIER               DENVER (146) 129-5118
    ## 493  1903           LUFTHANSA               MUNICH (927) 747-9822
    ## 494  2023     BRITISH AIRWAYS      LONDON HEATHROW   534 216 6666
    ## 495  2139            FRONTIER               DENVER (481) 479-7013
    ## 496  3106              ALASKA               NEWARK   971 175 2968
    ## 497  1404         UNITED INTL      PARIS-DE GAULLE (716) 777-3762
    ## 498  2377              UNITED               AUSTIN   274-863-3205
    ## 499  3138               DELTA       SALT LAKE CITY (217) 589-0596
    ## 500   773         UNITED INTL    SAN JOSE DEL CABO   928 445 5474
    ## 501  1545               DELTA         NEW YORK-JFK   858 990 5153
    ## 502  2892                 WOW            REYKJAVIK   731-813-2043
    ## 503  2721              UNITED              KAHULUI   563-732-6802
    ## 504   819              ALASKA             PORTLAND   145 725 4021
    ## 505  3272              UNITED            LAS VEGAS   931 311 5801
    ## 506   841              ALASKA              SEATTLE (637) 782-6989
    ## 507   253          AIR CANADA              TORONTO   172 990 3485
    ## 508   266          AIR CANADA              TORONTO   872 325 4341
    ## 509  2272              UNITED          BAKERSFIELD (359) 803-9809
    ## 510  1196           SOUTHWEST            LAS VEGAS   152 790 8238
    ## 511  2363              UNITED               AUSTIN   330 561 9257
    ## 512  3181               DELTA          LOS ANGELES   437 420 7546
    ## 513  2918               DELTA              SEATTLE   495 632 4027
    ## 514    11           SOUTHWEST          LOS ANGELES (416) 788-2844
    ## 515   904          AIR CANADA              TORONTO   311-305-4367
    ## 516  2937           SOUTHWEST            SAN DIEGO   817-400-0481
    ## 517  3143               DELTA       SALT LAKE CITY   430 723 1079
    ## 518  1167               DELTA         NEW YORK-JFK (729) 609-4819
    ## 519   302              UNITED            SAN DIEGO (201) 737-4409
    ## 520  1607     VIRGIN ATLANTIC      LONDON HEATHROW (137) 611-3694
    ## 521  1758           LUFTHANSA               MUNICH   226 490 8696
    ## 522  1740          AIR CANADA              TORONTO   123 570 8640
    ## 523   265          AIR CANADA              TORONTO   665 803 2453
    ## 524   911          AIR CANADA              TORONTO (812) 869-6263
    ## 525  3158               DELTA       SALT LAKE CITY   639 132 6386
    ## 526  1241       CHINA EASTERN             SHANGHAI (194) 198-0504
    ## 527  1562               DELTA              ATLANTA   437 886 0753
    ## 528  2375              UNITED               AUSTIN   626 756 5089
    ## 529  3023              UNITED             PORTLAND (299) 137-6993
    ## 530  3032              UNITED             PORTLAND   714 950 3364
    ## 531  1183           SOUTHWEST            LAS VEGAS   653-786-5985
    ## 532  2354              UNITED               AUSTIN   518 286 5956
    ## 533  2751      AIR FRANCE/KLM      PARIS-DE GAULLE   194 960 2145
    ## 534  3232            AMERICAN              PHOENIX   362-136-1153
    ## 535  1287             JETBLUE         NEW YORK-JFK   376-456-0697
    ## 536   852              ALASKA             PORTLAND   657 832 1189
    ## 537  3180            AMERICAN     DALLAS-FT. WORTH   962-918-6117
    ## 538  2501              ALASKA       SALT LAKE CITY   692 929 3592
    ## 539  3277           SOUTHWEST               DENVER   805-877-3887
    ## 540   556              ALASKA              SEATTLE (739) 710-2966
    ## 541   810              ALASKA            BALTIMORE   819 732 4132
    ## 542   481              ALASKA             PORTLAND   367 221 9710
    ## 543   350              ALASKA               NEWARK   361 154 1789
    ## 544  1789              UNITED       RALEIGH-DURHAM   680 488 1182
    ## 545  1256         UNITED INTL         TOKYO-NARITA   928 638 1186
    ## 546  2763      CATHAY PACIFIC            HONG KONG   588-693-9875
    ## 547  1442              UNITED             PORTLAND   681-308-7915
    ## 548  1229      ANA ALL NIPPON         TOKYO-NARITA   783 647 8490
    ## 549   969           LUFTHANSA               MUNICH   897 847 0632
    ## 550  1025          AIR CANADA            VANCOUVER   150 952 4453
    ## 551  1926      AIR FRANCE/KLM      PARIS-DE GAULLE   322-884-3020
    ## 552  2093            AMERICAN         PHILADELPHIA   176-313-5403
    ## 553  2812               DELTA          LOS ANGELES   487 109 4196
    ## 554   334           SOUTHWEST              PHOENIX (477) 182-4689
    ## 555   343              ALASKA            SAN DIEGO   544 382 8289
    ## 556   196            AMERICAN          LOS ANGELES   781 543 7456
    ## 557   337           SOUTHWEST              PHOENIX   911 829 6476
    ## 558  2682              UNITED       CHICAGO-O'HARE (525) 362-5532
    ## 559  1799              ALASKA       RALEIGH-DURHAM   517 986 3426
    ## 560  1951      AIR FRANCE/KLM      PARIS-DE GAULLE   838 220 5397
    ## 561  1673            EMIRATES                DUBAI (687) 887-6766
    ## 562  1813         UNITED INTL      LONDON HEATHROW   179 163 0902
    ## 563  1929      AIR FRANCE/KLM      PARIS-DE GAULLE   539 137 8983
    ## 564  1736          AIR CANADA              TORONTO   733-154-0094
    ## 565  2282                 WOW            REYKJAVIK   639 881 3693
    ## 566  2788                 WOW            REYKJAVIK   291-830-3017
    ## 567  2941    TURKISH AIRLINES             ISTANBUL (637) 100-0509
    ## 568  1653     VIRGIN ATLANTIC      LONDON HEATHROW   750 520 0167
    ## 569  2186              QANTAS               SYDNEY   676 485 8963
    ## 570  1719      AIR FRANCE/KLM      PARIS-DE GAULLE   135 566 5090
    ## 571  1440              UNITED             PORTLAND   337 260 4996
    ## 572  1343         UNITED INTL               KANSAI   371 185 2377
    ## 573   874            AMERICAN     DALLAS-FT. WORTH   280 461 1386
    ## 574  3144              UNITED            SAN DIEGO (603) 149-7268
    ## 575  1313             JETBLUE         NEW YORK-JFK (364) 792-5553
    ## 576  2530             JETBLUE               BOSTON   496-429-1314
    ## 577  2699           SOUTHWEST               DENVER   459 671 4698
    ## 578   545            EMIRATES                DUBAI   486-268-3312
    ## 579  1785              UNITED       RALEIGH-DURHAM   497-518-4050
    ## 580  2896               DELTA       SALT LAKE CITY (535) 685-8273
    ## 581  2323              QANTAS               SYDNEY   859 495 4050
    ## 582   826              ALASKA            NASHVILLE (826) 738-8316
    ## 583   694      AIR FRANCE/KLM      PARIS-DE GAULLE   724-134-3870
    ## 584   660         UNITED INTL               MUNICH (554) 269-8937
    ## 585   822              ALASKA            NASHVILLE   125-578-4253
    ## 586  1234           SOUTHWEST            LAS VEGAS   614 800 2861
    ## 587   982           LUFTHANSA               MUNICH   487-232-4449
    ## 588  1515               DELTA          LOS ANGELES (298) 135-0900
    ## 589  3097              UNITED              SPOKANE (392) 183-7831
    ## 590  2462              UNITED       CHICAGO-O'HARE (606) 596-1029
    ## 591  1538               DELTA         NEW YORK-JFK (384) 953-4795
    ## 592  2330               DELTA MINNEAPOLIS-ST. PAUL (855) 811-8811
    ## 593  2297              QANTAS               SYDNEY   253-374-7102
    ## 594  2005      AIR FRANCE/KLM      PARIS-DE GAULLE (419) 295-9580
    ## 595  1459              UNITED             PORTLAND   802 102 8345
    ## 596  1318         UNITED INTL      LONDON HEATHROW   417 393 0050
    ## 597  3178            AMERICAN     DALLAS-FT. WORTH   787-624-8443
    ## 598   320           SOUTHWEST          LOS ANGELES (919) 486-4251
    ## 599   605              ALASKA WASHINGTON DC-DULLES   341 824 5322
    ## 600  1590         UNITED INTL            HONG KONG   415 551 1608
    ## 601   292           SOUTHWEST          LOS ANGELES (392) 495-7961
    ## 602  1635          KOREAN AIR                SEOUL   473-238-3324
    ## 603  2820               DELTA          LOS ANGELES (506) 760-3043
    ## 604  2068            AMERICAN         PHILADELPHIA   876-834-0624
    ## 605  2387         UNITED INTL              BEIJING   919 611 6170
    ## 606  3121              ALASKA               NEWARK (146) 699-3488
    ## 607  1032          AIR CANADA            VANCOUVER   261 434 7760
    ## 608  2255              UNITED          LOS ANGELES   617 310 2684
    ## 609   555            EMIRATES                DUBAI   182-535-3412
    ## 610   718      CATHAY PACIFIC            HONG KONG   506 129 1694
    ## 611  2748      AIR FRANCE/KLM      PARIS-DE GAULLE (302) 339-0791
    ## 612   264          AIR CANADA              TORONTO (446) 229-4342
    ## 613  2317              QANTAS               SYDNEY (249) 602-6985
    ## 614  2076             JETBLUE           LONG BEACH (150) 905-6938
    ## 615  2177            FRONTIER               DENVER   313 990 8823
    ## 616  3076 PHILIPPINE AIRLINES               MANILA   656-941-5355
    ## 617   827              ALASKA              SEATTLE (116) 689-6617
    ## 618  1980      CATHAY PACIFIC            HONG KONG   955 324 5981
    ## 619   432              UNITED             HONOLULU   175 808 2189
    ## 620  2318              QANTAS               SYDNEY (896) 993-8555
    ## 621  3256              UNITED            SAN DIEGO   105-687-6500
    ## 622  1571               DELTA              DETROIT   757 524 2964
    ## 623  3268              UNITED            LAS VEGAS   201 374 2424
    ## 624  2636             JETBLUE         NEW YORK-JFK (466) 912-8401
    ## 625  2327               DELTA MINNEAPOLIS-ST. PAUL   766 112 6143
    ## 626  1164           SOUTHWEST          LOS ANGELES   783-463-4865
    ## 627  2950           SOUTHWEST            SAN DIEGO   853-803-9900
    ## 628  1670            EMIRATES                DUBAI (347) 851-5388
    ## 629   700      AIR FRANCE/KLM      PARIS-DE GAULLE   992 114 6973
    ## 630  1369         UNITED INTL               KANSAI   316-212-7309
    ## 631  2264              UNITED          LOS ANGELES   301 672 1092
    ## 632  2205      CATHAY PACIFIC            HONG KONG   795 137 0201
    ## 633  1383         UNITED INTL             SHANGHAI   381-883-5497
    ## 634  2762               DELTA              ATLANTA   100-531-4642
    ## 635  1034          AIR CANADA            VANCOUVER   994 923 6634
    ## 636  1156           SOUTHWEST            SAN DIEGO   920 355 8404
    ## 637  1336         UNITED INTL               KANSAI (441) 445-6532
    ## 638  2522              ALASKA       SALT LAKE CITY   325 795 2455
    ## 639  1814              ALASKA       RALEIGH-DURHAM   593 829 6250
    ## 640  3263              UNITED            SAN DIEGO   566-482-9004
    ## 641  1288         UNITED INTL      LONDON HEATHROW   542 537 6770
    ## 642  3214              UNITED        SANTA BARBARA   716 191 1741
    ## 643  1614     VIRGIN ATLANTIC      LONDON HEATHROW   491-727-7162
    ## 644  2471              ALASKA       SALT LAKE CITY   167-336-5660
    ## 645  2308              QANTAS               SYDNEY   358 831 0725
    ## 646  2544             JETBLUE           LONG BEACH   432 979 7292
    ## 647  2561             JETBLUE           LONG BEACH (205) 382-5599
    ## 648   669              UNITED               NEWARK (208) 794-9612
    ## 649  3086                COPA          PANAMA CITY   728 662 3934
    ## 650  2987            AMERICAN            CHARLOTTE   380-918-8572
    ## 651  3124                COPA          PANAMA CITY (905) 742-3525
    ## 652  1263         UNITED INTL         TOKYO-NARITA   151 434 6989
    ## 653  2928    TURKISH AIRLINES             ISTANBUL   755 544 2629
    ## 654  3129              ALASKA               NEWARK   633 181 4494
    ## 655  1773            AMERICAN       CHICAGO-O'HARE   346 706 5964
    ## 656   682              UNITED               NEWARK   688 690 2184
    ## 657   491              UNITED               DENVER   618 717 1697
    ## 658   857              ALASKA             PORTLAND   185-321-6877
    ## 659  1738          AIR CANADA              TORONTO (147) 535-3529
    ## 660  1698              ALASKA            SANTA ANA (152) 912-4118
    ## 661   520              ALASKA      PUERTO VALLARTA   726 943 7486
    ## 662  1035          AIR CANADA            VANCOUVER (634) 521-4714
    ## 663  1916      AIR FRANCE/KLM      PARIS-DE GAULLE   670-248-0186
    ## 664   939          AIR CANADA              TORONTO (121) 509-7306
    ## 665  1461              ALASKA         NEW YORK-JFK (105) 635-5212
    ## 666  2908    TURKISH AIRLINES             ISTANBUL (732) 168-0110
    ## 667  1612     VIRGIN ATLANTIC      LONDON HEATHROW   364 834 3150
    ## 668  1220           SOUTHWEST            LAS VEGAS   176 508 2778
    ## 669  2472              UNITED       CHICAGO-O'HARE   120 941 0833
    ## 670   284              UNITED            BALTIMORE   670 902 3199
    ## 671  1412              UNITED          KANSAS CITY (214) 250-8756
    ## 672  3240            AMERICAN              PHOENIX (119) 975-8484
    ## 673  1897           LUFTHANSA               MUNICH   297 484 3285
    ## 674  1660            EMIRATES                DUBAI (489) 534-6272
    ## 675   688      AIR FRANCE/KLM      PARIS-DE GAULLE (610) 716-5732
    ## 676  1102               DELTA MINNEAPOLIS-ST. PAUL (456) 925-4236
    ## 677  2213      CATHAY PACIFIC            HONG KONG   743-103-7645
    ## 678  1551               DELTA         NEW YORK-JFK   432-281-3682
    ## 679  1298             JETBLUE         NEW YORK-JFK (167) 144-9470
    ## 680  1139           SOUTHWEST            SAN DIEGO   648 685 6188
    ## 681  2936           SOUTHWEST            SAN DIEGO   548 191 4898
    ## 682   275              UNITED          LOS ANGELES   288 110 9483
    ## 683  1134           SOUTHWEST            SAN DIEGO   946-558-5801
    ## 684  3212            AMERICAN          LOS ANGELES   388 744 9637
    ## 685  1657            EMIRATES                DUBAI   506 463 9129
    ## 686  2049     BRITISH AIRWAYS      LONDON HEATHROW (848) 149-5208
    ## 687   789            AMERICAN          LOS ANGELES (970) 908-2298
    ## 688  2804           SOUTHWEST              PHOENIX (843) 120-5653
    ## 689  1690      CATHAY PACIFIC            HONG KONG   306 394 8640
    ## 690  1756           LUFTHANSA               MUNICH   170-641-3537
    ## 691  2133          AIR CANADA            VANCOUVER   860 723 5066
    ## 692   429              UNITED             HONOLULU   814-895-6610
    ## 693   815              ALASKA             PORTLAND (139) 727-9901
    ## 694   960          AIR CANADA            VANCOUVER   598 735 8557
    ## 695  2797                 WOW            REYKJAVIK   593 895 6761
    ## 696  3252              UNITED            SAN DIEGO (817) 824-3849
    ## 697   865            AMERICAN     DALLAS-FT. WORTH   508 484 9738
    ## 698   362              ALASKA               NEWARK   719 489 4724
    ## 699   577              ALASKA              SEATTLE   503-671-4901
    ## 700  2144          AIR CANADA            VANCOUVER   275 649 8183
    ## 701  3044              UNITED             PORTLAND   968 130 7012
    ## 702  2358             JETBLUE       FT. LAUDERDALE   290 367 6676
    ## 703  1617     VIRGIN ATLANTIC      LONDON HEATHROW   499 766 9941
    ## 704   848            AMERICAN     DALLAS-FT. WORTH   538-393-2243
    ## 705  2605              UNITED       CHICAGO-O'HARE (540) 362-7136
    ## 706  2012      AIR FRANCE/KLM      PARIS-DE GAULLE (802) 910-1742
    ## 707  2196          AER LINGUS               DUBLIN   845 544 4748
    ## 708  2203      CATHAY PACIFIC            HONG KONG   784-458-8425
    ## 709  3120                COPA          PANAMA CITY (365) 217-0634
    ## 710  1941      AIR FRANCE/KLM      PARIS-DE GAULLE   708 500 2758
    ## 711  3149               DELTA       SALT LAKE CITY (594) 797-7729
    ## 712   743              ALASKA         NEW YORK-JFK   982 555 9504
    ## 713  1646     VIRGIN ATLANTIC      LONDON HEATHROW   477-307-3338
    ## 714  1648     VIRGIN ATLANTIC      LONDON HEATHROW   744-301-1148
    ## 715   917          AIR CANADA              TORONTO   389 484 8888
    ## 716  1495            FRONTIER               DENVER   739 303 6128
    ## 717  1728          AIR CANADA              TORONTO   175 905 9962
    ## 718  1946      AIR FRANCE/KLM      PARIS-DE GAULLE (900) 462-1379
    ## 719   722      AIR FRANCE/KLM      PARIS-DE GAULLE   878-636-2294
    ## 720  1520            INTERJET          GUADALAJARA (998) 692-1900
    ## 721   892              ALASKA       RALEIGH-DURHAM   994 421 8642
    ## 722  1865              ALASKA              KAHULUI   304-225-5895
    ## 723  2499              UNITED               DENVER   931-522-5498
    ## 724  3035              UNITED             PORTLAND   838 898 2275
    ## 725  1868              ALASKA              KAHULUI   718 187 7125
    ## 726  2347               DELTA MINNEAPOLIS-ST. PAUL   943 561 8955
    ## 727  1497            FRONTIER               DENVER   341 261 6456
    ## 728  2776                 WOW            REYKJAVIK (507) 483-3618
    ## 729   709      CATHAY PACIFIC            HONG KONG   826 266 0205
    ## 730   328              UNITED              BURBANK (380) 449-7849
    ## 731  2819               DELTA          LOS ANGELES   589-975-0198
    ## 732   557            EMIRATES                DUBAI   657 680 8781
    ## 733  1021          AIR CANADA            VANCOUVER   501-668-7869
    ## 734  2085             JETBLUE           LONG BEACH   224 365 8299
    ## 735  1246         UNITED INTL         TOKYO-NARITA   747 588 1968
    ## 736   766              ALASKA         NEW YORK-JFK (706) 836-7047
    ## 737  2888              UNITED               AUSTIN (698) 462-6742
    ## 738  1267         UNITED INTL         TOKYO-NARITA   748 446 1257
    ## 739  2882             EVA AIR               TAIPEI   525-552-4162
    ## 740  1961      CATHAY PACIFIC            HONG KONG   635-714-8302
    ## 741  2653             JETBLUE               BOSTON   114 668 2834
    ## 742  2217      CATHAY PACIFIC            HONG KONG   404 788 2855
    ## 743  3028              UNITED             PORTLAND   673 292 2444
    ## 744   761              ALASKA         NEW YORK-JFK   136 788 1426
    ## 745   338              ALASKA            SAN DIEGO   564 677 3934
    ## 746   685      AIR FRANCE/KLM      PARIS-DE GAULLE   213 981 7762
    ## 747  1909              UNITED               BOSTON   453-556-0852
    ## 748   894              ALASKA       RALEIGH-DURHAM   423 593 4483
    ## 749  2208      CATHAY PACIFIC            HONG KONG   820 609 7454
    ## 750  1779              UNITED       RALEIGH-DURHAM   131-641-1331
    ## 751   711      AIR FRANCE/KLM      PARIS-DE GAULLE   824 540 9579
    ## 752  2483              UNITED               DENVER (753) 726-0123
    ## 753  1062          AIR CANADA              CALGARY   739 737 9041
    ## 754   817              ALASKA             PORTLAND   609-332-7370
    ## 755  1987      AIR FRANCE/KLM            AMSTERDAM   426-182-1365
    ## 756  1672            EMIRATES                DUBAI (347) 782-5787
    ## 757  3161              UNITED            SAN DIEGO   898-210-6218
    ## 758  2194      CATHAY PACIFIC            HONG KONG   938 394 0411
    ## 759  3169               DELTA          LOS ANGELES   904 844 1759
    ## 760  1633          KOREAN AIR                SEOUL   658-861-4306
    ## 761   834              ALASKA              SEATTLE (716) 184-1232
    ## 762  2392         UNITED INTL              BEIJING   380-105-1757
    ## 763  2074             JETBLUE           LONG BEACH (969) 555-0453
    ## 764  3112              UNITED              SPOKANE   249 452 4370
    ## 765   642              ALASKA WASHINGTON DC-DULLES   443 384 8253
    ## 766  1342         UNITED INTL               KANSAI   711 289 2247
    ## 767  1341         UNITED INTL             SHANGHAI   343-973-0193
    ## 768  3092                COPA          PANAMA CITY   367 650 3720
    ## 769  2895                 WOW            REYKJAVIK (265) 286-5671
    ## 770   691      AIR FRANCE/KLM      PARIS-DE GAULLE   148 630 8560
    ## 771   525              ALASKA      PUERTO VALLARTA   413-727-2672
    ## 772   724      CATHAY PACIFIC            HONG KONG (162) 332-5838
    ## 773   381              ALASKA    SAN JOSE DEL CABO (229) 604-7790
    ## 774  1760           LUFTHANSA               MUNICH (892) 301-0333
    ## 775  1707      AIR FRANCE/KLM      PARIS-DE GAULLE   148 501 5084
    ## 776  1624          KOREAN AIR                SEOUL   564 780 8272
    ## 777  1943      AIR FRANCE/KLM      PARIS-DE GAULLE   524 190 0899
    ## 778  1120               DELTA MINNEAPOLIS-ST. PAUL   463 792 2782
    ## 779  2202          AER LINGUS               DUBLIN (331) 472-8624
    ## 780  3290           SOUTHWEST               DENVER   522-286-5318
    ## 781   927          AIR CANADA              TORONTO   310 719 4550
    ## 782  2572             JETBLUE           LONG BEACH   314-360-4288
    ## 783   308              UNITED            SAN DIEGO (301) 534-5754
    ## 784  1068          AIR CANADA              CALGARY   822 271 5719
    ## 785   344           LUFTHANSA            FRANKFURT (341) 473-0639
    ## 786  1729          AIR CANADA              TORONTO (835) 882-3693
    ## 787  2702              UNITED              ORLANDO   465-550-6610
    ## 788  3057 PHILIPPINE AIRLINES               MANILA   388 100 1482
    ## 789   599             JETBLUE               BOSTON (589) 194-0523
    ## 790  2352               DELTA MINNEAPOLIS-ST. PAUL   982 842 4913
    ## 791  1570         UNITED INTL            HONG KONG   671 913 4563
    ## 792  3118              ALASKA               NEWARK   144 468 5864
    ## 793  2766               DELTA              ATLANTA   194 344 4039
    ## 794   706      CATHAY PACIFIC            HONG KONG   331 747 5714
    ## 795  1911      AIR FRANCE/KLM      PARIS-DE GAULLE   221-190-1449
    ## 796   704      AIR FRANCE/KLM      PARIS-DE GAULLE (322) 843-0185
    ## 797  2567             JETBLUE           LONG BEACH   676-614-9095
    ## 798  3127               DELTA       SALT LAKE CITY (190) 975-2514
    ## 799  1250       CHINA EASTERN             SHANGHAI (909) 382-3774
    ## 800  2663             JETBLUE               BOSTON   956 257 9319
    ## 801  2080            AMERICAN         PHILADELPHIA   362 145 8268
    ## 802   229            AMERICAN          LOS ANGELES   570 727 3998
    ## 803  1787              UNITED       RALEIGH-DURHAM   225 964 9193
    ## 804  2942           SOUTHWEST            SAN DIEGO   227 419 9482
    ## 805  2749      AIR FRANCE/KLM      PARIS-DE GAULLE   949 360 7605
    ## 806  2691              UNITED              ORLANDO (347) 896-3463
    ## 807   287              UNITED            BALTIMORE   794 939 9735
    ## 808   909          AIR CANADA              TORONTO   413 754 3034
    ## 809  1225           SOUTHWEST            LAS VEGAS   806 730 0459
    ## 810  1077          AIR CANADA              CALGARY   791 195 8909
    ## 811   427              UNITED             HONOLULU   691-318-3535
    ## 812   832              ALASKA              SEATTLE   852-386-6029
    ## 813   707      AIR FRANCE/KLM      PARIS-DE GAULLE   380 682 7795
    ## 814  2849              UNITED              SEATTLE (355) 550-1392
    ## 815  2422     AIR NEW ZEALAND             AUCKLAND   247 586 4579
    ## 816  1639     VIRGIN ATLANTIC      LONDON HEATHROW   235 257 1041
    ## 817  1557               DELTA         NEW YORK-JFK (705) 456-1905
    ## 818  1998              UNITED            BALTIMORE   106 756 2785
    ## 819  2329            AMERICAN       CHICAGO-O'HARE (836) 207-8419
    ## 820   997           LUFTHANSA               MUNICH (306) 552-1875
    ## 821  1906           LUFTHANSA               MUNICH   729-102-7511
    ## 822  1995              UNITED            BALTIMORE   795 583 0958
    ## 823  2961           SOUTHWEST          LOS ANGELES   700-431-3918
    ## 824   422              UNITED         INDIANAPOLIS   929 632 1068
    ## 825  2135          AIR CANADA            VANCOUVER   763 906 2495
    ## 826  2858              UNITED              SEATTLE   469 976 6796
    ## 827  1217           SOUTHWEST            LAS VEGAS   887-657-4143
    ## 828  1086          AIR CANADA              CALGARY   574-438-5329
    ## 829   996           LUFTHANSA               MUNICH   319 127 9518
    ## 830  1214           SOUTHWEST            LAS VEGAS   429 960 9710
    ## 831  1204           SOUTHWEST            LAS VEGAS   419 646 0299
    ## 832  1186           SOUTHWEST            LAS VEGAS   192 343 8515
    ## 833  2872              UNITED WASHINGTON DC-DULLES   521 336 8581
    ## 834   974           LUFTHANSA               MUNICH   776 367 6109
    ## 835  2675              UNITED       CHICAGO-O'HARE   470 367 1392
    ## 836   347              ALASKA            SAN DIEGO (944) 189-7555
    ## 837   633              ALASKA WASHINGTON DC-DULLES   998-931-4783
    ## 838   613              ALASKA WASHINGTON DC-DULLES   362-178-6307
    ## 839  1580               DELTA              DETROIT (458) 404-9558
    ## 840  3291           SOUTHWEST               DENVER   212 286 7936
    ## 841   268          AIR CANADA              TORONTO (481) 522-1039
    ## 842   446              UNITED             HONOLULU (376) 611-4588
    ## 843   805            AMERICAN          LOS ANGELES   936 193 9690
    ## 844  1664            EMIRATES                DUBAI (641) 544-6549
    ## 845  2025     BRITISH AIRWAYS      LONDON HEATHROW   797-870-7818
    ## 846  1691      CATHAY PACIFIC            HONG KONG   693 907 5353
    ## 847  1860              UNITED          LOS ANGELES   332 973 4943
    ## 848  2809               DELTA          LOS ANGELES   929 622 9077
    ## 849   423              UNITED             HONOLULU   649-379-5361
    ## 850  2276              UNITED          LOS ANGELES (572) 748-6932
    ## 851  2747      AIR FRANCE/KLM      PARIS-DE GAULLE   395-892-5646
    ## 852  2155      CATHAY PACIFIC            HONG KONG   221-628-9561
    ## 853   199            AMERICAN          LOS ANGELES (227) 801-6148
    ## 854   561              ALASKA              SEATTLE   549-649-1864
    ## 855  2938    TURKISH AIRLINES             ISTANBUL   342 941 0439
    ## 856  3151               DELTA       SALT LAKE CITY   701 390 9814
    ## 857  2017     BRITISH AIRWAYS      LONDON HEATHROW (519) 573-6576
    ## 858   567              ALASKA              SEATTLE   919-342-0230
    ## 859   607             JETBLUE               BOSTON   364-759-2705
    ## 860  1533              ALASKA          LOS ANGELES   949 543 7906
    ## 861  2932           SOUTHWEST            SAN DIEGO   942 732 6403
    ## 862  1889  SINGAPORE AIRLINES            SINGAPORE (900) 586-1787
    ## 863  2818               DELTA          LOS ANGELES   308-607-9855
    ## 864  2931           SOUTHWEST            SAN DIEGO   764 645 5740
    ## 865  1974              UNITED            BALTIMORE   472-337-8838
    ## 866  2013              UNITED               NEWARK   791 847 7278
    ## 867  3250              UNITED            SAN DIEGO   128 805 3828
    ## 868   202            AMERICAN          LOS ANGELES   365-832-0674
    ## 869  2176            FRONTIER               DENVER   123-282-3494
    ## 870  3273              UNITED            LAS VEGAS   285 424 4318
    ## 871  3275              UNITED            LAS VEGAS   452 352 1387
    ## 872  1115               DELTA MINNEAPOLIS-ST. PAUL   129-377-8159
    ## 873  2915               DELTA              SEATTLE   222 143 3131
    ## 874  1099               DELTA MINNEAPOLIS-ST. PAUL   162-451-0594
    ## 875  1710      AIR FRANCE/KLM      PARIS-DE GAULLE (239) 325-5321
    ## 876   263          AIR CANADA              TORONTO   436-422-6171
    ## 877  3184            AMERICAN     DALLAS-FT. WORTH   605 284 4260
    ## 878  2053              UNITED               NEWARK   929-102-5905
    ## 879  2871              UNITED WASHINGTON DC-DULLES   847 507 8268
    ## 880  2672             JETBLUE               BOSTON   452-811-8088
    ## 881  2975            AMERICAN            CHARLOTTE   799 143 1677
    ## 882  2949           SOUTHWEST          LOS ANGELES   196 756 4555
    ## 883  1727          AIR CANADA              TORONTO   119-444-0817
    ## 884  1015          AIR CANADA            VANCOUVER   885 454 0883
    ## 885  2481              UNITED       CHICAGO-O'HARE   945-998-0444
    ## 886  1080          AIR CANADA              CALGARY (367) 897-7969
    ## 887  1535               DELTA         NEW YORK-JFK   163 241 9321
    ## 888  2953           SOUTHWEST            SAN DIEGO   594-176-5811
    ## 889   431              UNITED         INDIANAPOLIS (621) 874-9973
    ## 890  2355              UNITED               AUSTIN   332 963 4103
    ## 891  1827         UNITED INTL      LONDON HEATHROW   389 318 3975
    ## 892  2634             JETBLUE         NEW YORK-JFK   894-593-7953
    ## 893  1895           LUFTHANSA               MUNICH   561-266-7842
    ## 894  2029     BRITISH AIRWAYS      LONDON HEATHROW   354-958-8052
    ## 895  2140          AIR CANADA            VANCOUVER   151 921 2775
    ## 896  2994            AMERICAN                MIAMI   901 140 3759
    ## 897   898          AIR CANADA              TORONTO (426) 342-7378
    ## 898  1418              UNITED            LAS VEGAS   333-520-4811
    ## 899   643              ALASKA          NEW ORLEANS (765) 191-1797
    ## 900  2426              UNITED             HONOLULU   850 914 9348
    ## 901  2613              UNITED       CHICAGO-O'HARE   246 272 9019
    ## 902  1574         UNITED INTL            HONG KONG (400) 250-0871
    ## 903  3055 PHILIPPINE AIRLINES               MANILA   504 419 9191
    ## 904  2169            FRONTIER               DENVER (434) 725-0561
    ## 905  1559               DELTA         NEW YORK-JFK   231 863 7554
    ## 906  2365             JETBLUE       FT. LAUDERDALE (969) 207-3261
    ## 907  2769               DELTA              ATLANTA   939 253 9048
    ## 908   512              UNITED              KAHULUI   879-154-4494
    ## 909  1811         UNITED INTL      LONDON HEATHROW   577 786 2546
    ## 910  1577         UNITED INTL            HONG KONG (994) 688-3259
    ## 911  2403         UNITED INTL               SYDNEY   841-717-4447
    ## 912  2388         UNITED INTL               SYDNEY   397-353-6309
    ## 913  1741          AIR CANADA              TORONTO   558 191 4548
    ## 914  1293         UNITED INTL      LONDON HEATHROW   905 768 2297
    ## 915  3287           SOUTHWEST               DENVER   348 522 2051
    ## 916  1857              ALASKA              KAHULUI   307-323-6861
    ## 917  2951           SOUTHWEST            SAN DIEGO   663 886 2487
    ## 918  2469              UNITED               DENVER   274 944 6097
    ## 919  2655             JETBLUE               BOSTON (494) 308-3048
    ## 920  2713               DELTA          LOS ANGELES   328 307 0875
    ## 921  1070          AIR CANADA              CALGARY   673 524 3504
    ## 922  2657             JETBLUE               BOSTON   934 721 0615
    ## 923  3085              UNITED              SPOKANE   102-957-6486
    ## 924  3009            AMERICAN                MIAMI (715) 288-8832
    ## 925  3081                COPA          PANAMA CITY       925 8846
    ## 926   210            AMERICAN          LOS ANGELES   637 281 4111
    ## 927  3142               DELTA       SALT LAKE CITY (594) 448-6242
    ## 928  2893                 WOW            REYKJAVIK   335 802 2651
    ## 929  2725               DELTA          LOS ANGELES   560 699 9908
    ## 930  1798              ALASKA       RALEIGH-DURHAM   451 163 0102
    ## 931  2608          AEROMEXICO          MEXICO CITY   613 800 0835
    ## 932   352           LUFTHANSA            FRANKFURT   370 453 5800
    ## 933  1171           SOUTHWEST          LOS ANGELES   192 507 5411
    ## 934  1339         UNITED INTL               KANSAI   182-227-4838
    ## 935  1791              UNITED       RALEIGH-DURHAM   647 126 2332
    ## 936  1905           LUFTHANSA               MUNICH   606 125 6957
    ## 937  2238          AER LINGUS               DUBLIN   728-404-5558
    ## 938  3193      CATHAY PACIFIC            HONG KONG   506 812 6052
    ## 939   985           LUFTHANSA               MUNICH   427-665-3475
    ## 940  1259         UNITED INTL         TOKYO-NARITA   808 739 7162
    ## 941  1893           LUFTHANSA               MUNICH   375-978-3305
    ## 942  2738               DELTA          LOS ANGELES   181-708-2089
    ## 943  1619     VIRGIN ATLANTIC      LONDON HEATHROW (802) 810-5574
    ## 944   448              UNITED             HONOLULU   242-540-4234
    ## 945   655         UNITED INTL               MUNICH   105 116 9695
    ## 946  1508        HAWAIIAN AIR             HONOLULU (867) 891-0871
    ## 947  1510        HAWAIIAN AIR             HONOLULU   945 144 7892
    ## 948  3231              ALASKA             PORTLAND   531 895 6695
    ## 949  2564             JETBLUE           LONG BEACH (533) 213-4368
    ## 950  1965      CATHAY PACIFIC            HONG KONG   894 810 2674
    ## 951  1817         UNITED INTL      LONDON HEATHROW   943 812 6349
    ## 952  1282      CHINA SOUTHERN                WUHAN   476 168 4235
    ## 953  1816         UNITED INTL      LONDON HEATHROW   931 385 6757
    ## 954   763          AIR CANADA              TORONTO (324) 188-6781
    ## 955  1609     VIRGIN ATLANTIC      LONDON HEATHROW   100 378 8095
    ## 956  1802         UNITED INTL      LONDON HEATHROW   459 572 0244
    ## 957  2595             JETBLUE               BOSTON   397-362-5469
    ## 958  1921      AIR FRANCE/KLM      PARIS-DE GAULLE (102) 928-7959
    ## 959  1387         UNITED INTL      PARIS-DE GAULLE (439) 568-6611
    ## 960  1480              ALASKA         NEW YORK-JFK (767) 205-0604
    ## 961  1743      CATHAY PACIFIC            HONG KONG (890) 548-9219
    ## 962  2757      AIR FRANCE/KLM      PARIS-DE GAULLE   938 982 5585
    ## 963  1488            FRONTIER               DENVER   769-472-2992
    ## 964  2204          AER LINGUS               DUBLIN   190 204 1154
    ## 965  2508              UNITED               NEWARK (649) 925-8489
    ## 966  2889              UNITED               AUSTIN   321 616 1013
    ## 967  2277              UNITED          BAKERSFIELD (156) 837-4491
    ## 968  1595           SOUTHWEST          LOS ANGELES   178-232-0815
    ## 969  2099         UNITED INTL          MEXICO CITY (882) 304-9032
    ## 970  1445              UNITED             PORTLAND   828 549 6666
    ## 971  2230          AER LINGUS               DUBLIN   280 544 4554
    ## 972  2629             JETBLUE         NEW YORK-JFK   183 208 5054
    ## 973  3172            AMERICAN     DALLAS-FT. WORTH (971) 548-6611
    ## 974  1920      AIR FRANCE/KLM      PARIS-DE GAULLE (828) 153-5819
    ## 975  1924      AIR FRANCE/KLM      PARIS-DE GAULLE   203 448 1522
    ## 976  2142          AIR CANADA            VANCOUVER   900-871-9056
    ## 977  2755      AIR FRANCE/KLM      PARIS-DE GAULLE   406-167-1379
    ## 978  1986      AIR FRANCE/KLM            AMSTERDAM (906) 850-9192
    ## 979   529            EMIRATES                DUBAI (859) 777-8245
    ## 980  3157              UNITED            SAN DIEGO   641-635-8466
    ## 981  1782              UNITED       RALEIGH-DURHAM   807-671-6158
    ## 982  3068              UNITED              SPOKANE (589) 270-7518
    ## 983  1455              UNITED             PORTLAND   768 529 8051
    ## 984   488              ALASKA             PORTLAND   220 660 0306
    ## 985  2680              UNITED              ORLANDO   928-179-7556
    ## 986  3065              UNITED              SPOKANE   153 756 0278
    ## 987  3254              UNITED            SAN DIEGO   273 829 9197
    ## 988   331              ALASKA            SAN DIEGO   269 463 0911
    ## 989  3284           SOUTHWEST               DENVER   696 984 8826
    ## 990  2321           SOUTHWEST          LOS ANGELES   905-903-5258
    ## 991  2448             JETBLUE               BOSTON   267 332 4709
    ## 992  3038              UNITED             PORTLAND (146) 129-5118
    ## 993  1634          KOREAN AIR                SEOUL (927) 747-9822
    ## 994  2120             JETBLUE           LONG BEACH   534 216 6666
    ## 995  2877             EVA AIR               TAIPEI (481) 479-7013
    ## 996  1254       CHINA EASTERN             SHANGHAI   971 175 2968
    ## 997  1600  SINGAPORE AIRLINES            SINGAPORE (716) 777-3762
    ## 998  2778              UNITED               AUSTIN   274-863-3205
    ## 999  2562             JETBLUE               BOSTON (217) 589-0596
    ## 1000 1714      AIR FRANCE/KLM      PARIS-DE GAULLE   928 445 5474
    ## 1001 2619            INTERJET          GUADALAJARA   858 990 5153
    ## 1002 1486            FRONTIER               DENVER   731-813-2043
    ## 1003 1803              ALASKA       RALEIGH-DURHAM   563-732-6802
    ## 1004 2494              UNITED               DENVER   145 725 4021
    ## 1005 2489              UNITED               DENVER   931 311 5801
    ## 1006 2161            FRONTIER               DENVER (637) 782-6989
    ## 1007 1460              UNITED             PORTLAND   172 990 3485
    ## 1008 3090                COPA          PANAMA CITY   872 325 4341
    ## 1009 1019          AIR CANADA            VANCOUVER (359) 803-9809
    ## 1010  693      CATHAY PACIFIC            HONG KONG   152 790 8238
    ## 1011    9           SOUTHWEST          LOS ANGELES   330 561 9257
    ## 1012 3073 PHILIPPINE AIRLINES               MANILA   437 420 7546
    ## 1013 2258                 WOW            REYKJAVIK   495 632 4027
    ## 1014 2867              UNITED WASHINGTON DC-DULLES (416) 788-2844
    ## 1015  477              UNITED               DENVER   311-305-4367
    ## 1016  677      AIR FRANCE/KLM      PARIS-DE GAULLE   817-400-0481
    ## 1017 2674             JETBLUE               BOSTON   430 723 1079
    ## 1018 2707               DELTA          LOS ANGELES (729) 609-4819
    ## 1019  992           LUFTHANSA               MUNICH (201) 737-4409
    ## 1020 2395         UNITED INTL              BEIJING (137) 611-3694
    ## 1021 2091            AMERICAN         PHILADELPHIA   226 490 8696
    ## 1022 1540               DELTA         NEW YORK-JFK   123 570 8640
    ## 1023 1744      CATHAY PACIFIC            HONG KONG   665 803 2453
    ## 1024 3219              ALASKA             PORTLAND (812) 869-6263
    ## 1025 2870              UNITED WASHINGTON DC-DULLES   639 132 6386
    ## 1026 1377         UNITED INTL      PARIS-DE GAULLE (194) 198-0504
    ## 1027 1809              ALASKA       RALEIGH-DURHAM   437 886 0753
    ## 1028 2104             JETBLUE           LONG BEACH   626 756 5089
    ## 1029 2925    TURKISH AIRLINES             ISTANBUL (299) 137-6993
    ## 1030 3189               DELTA          LOS ANGELES   714 950 3364
    ## 1031 2190      CATHAY PACIFIC            HONG KONG   653-786-5985
    ## 1032 1969      CATHAY PACIFIC            HONG KONG   518 286 5956
    ## 1033 3255              UNITED            SAN DIEGO   194 960 2145
    ## 1034  692      CATHAY PACIFIC            HONG KONG   362-136-1153
    ## 1035 3116              ALASKA               NEWARK   376-456-0697
    ## 1036 2704           SOUTHWEST               DENVER   657 832 1189
    ## 1037 1097               DELTA MINNEAPOLIS-ST. PAUL   962-918-6117
    ## 1038  733      AIR FRANCE/KLM      PARIS-DE GAULLE   692 929 3592
    ## 1039 2368              UNITED               AUSTIN   805-877-3887
    ## 1040 2708      AIR FRANCE/KLM      PARIS-DE GAULLE (739) 710-2966
    ## 1041 2733               DELTA          LOS ANGELES   819 732 4132
    ## 1042 1170               DELTA         NEW YORK-JFK   367 221 9710
    ## 1043 1786              UNITED       RALEIGH-DURHAM   361 154 1789
    ## 1044  623              ALASKA WASHINGTON DC-DULLES   680 488 1182
    ## 1045 3170            AMERICAN     DALLAS-FT. WORTH   928 638 1186
    ## 1046 1309             JETBLUE         NEW YORK-JFK   588-693-9875
    ## 1047 3283           SOUTHWEST               DENVER   681-308-7915
    ## 1048 3234              ALASKA             PORTLAND   783 647 8490
    ## 1049 3195      CATHAY PACIFIC            HONG KONG   897 847 0632
    ## 1050 2731               DELTA          LOS ANGELES   150 952 4453
    ## 1051 2946           SOUTHWEST            SAN DIEGO   322-884-3020
    ## 1052  428              UNITED         INDIANAPOLIS   176-313-5403
    ## 1053 2432     AIR NEW ZEALAND             AUCKLAND   487 109 4196
    ## 1054 3141              ALASKA               NEWARK (477) 182-4689
    ## 1055 2109         UNITED INTL          MEXICO CITY   544 382 8289
    ## 1056 1708      AIR FRANCE/KLM      PARIS-DE GAULLE   781 543 7456
    ## 1057 2924    TURKISH AIRLINES             ISTANBUL   911 829 6476
    ## 1058 1432              UNITED            LAS VEGAS (525) 362-5532
    ## 1059 3155              UNITED            SAN DIEGO   517 986 3426
    ## 1060  492              ALASKA             PORTLAND   838 220 5397
    ## 1061 1891  SINGAPORE AIRLINES            SINGAPORE (687) 887-6766
    ## 1062 2773                 WOW            REYKJAVIK   179 163 0902
    ## 1063  283              UNITED            BALTIMORE   539 137 8983
    ## 1064 2768                 WOW            REYKJAVIK   733-154-0094
    ## 1065 1862              ALASKA              KAHULUI   639 881 3693
    ## 1066 2218          AER LINGUS               DUBLIN   291-830-3017
    ## 1067 1433              UNITED             PORTLAND (637) 100-0509
    ## 1068 1231      ANA ALL NIPPON         TOKYO-NARITA   750 520 0167
    ## 1069  806              ALASKA            BALTIMORE   676 485 8963
    ## 1070 1056          AIR CANADA              CALGARY   135 566 5090
    ## 1071 2861              UNITED              SEATTLE   337 260 4996
    ## 1072 2016              UNITED               NEWARK   371 185 2377
    ## 1073 1209           SOUTHWEST            LAS VEGAS   280 461 1386
    ## 1074 2687              UNITED              ORLANDO (603) 149-7268
    ## 1075 1325             JETBLUE         NEW YORK-JFK (364) 792-5553
    ## 1076  912          AIR CANADA              TORONTO   496-429-1314
    ## 1077  562            EMIRATES                DUBAI   459 671 4698
    ## 1078 1871              ALASKA              KAHULUI   486-268-3312
    ## 1079  888              ALASKA       RALEIGH-DURHAM   497-518-4050
    ## 1080 2287                 WOW            REYKJAVIK (535) 685-8273
    ## 1081 1944      AIR FRANCE/KLM      PARIS-DE GAULLE   859 495 4050
    ## 1082 2007      AIR FRANCE/KLM      PARIS-DE GAULLE (826) 738-8316
    ## 1083  267          AIR CANADA              TORONTO   724-134-3870
    ## 1084 2420     AIR NEW ZEALAND             AUCKLAND (554) 269-8937
    ## 1085  627              ALASKA               NEWARK   125-578-4253
    ## 1086 3148              UNITED            SAN DIEGO   614 800 2861
    ## 1087 2758               DELTA              ATLANTA   487-232-4449
    ## 1088 1304             JETBLUE         NEW YORK-JFK (298) 135-0900
    ## 1089 2643             JETBLUE         NEW YORK-JFK (392) 183-7831
    ## 1090 1992      AIR FRANCE/KLM            AMSTERDAM (606) 596-1029
    ## 1091  353              ALASKA               NEWARK (384) 953-4795
    ## 1092  698      CATHAY PACIFIC            HONG KONG (855) 811-8811
    ## 1093 1628          KOREAN AIR                SEOUL   253-374-7102
    ## 1094 2011      AIR FRANCE/KLM      PARIS-DE GAULLE (419) 295-9580
    ## 1095 1925      AIR FRANCE/KLM      PARIS-DE GAULLE   802 102 8345
    ## 1096 2852              UNITED              SEATTLE   417 393 0050
    ## 1097 1783              UNITED       RALEIGH-DURHAM   787-624-8443
    ## 1098 3125              ALASKA               NEWARK (919) 486-4251
    ## 1099  921          AIR CANADA              TORONTO   341 824 5322
    ## 1100 3267              UNITED            LAS VEGAS   415 551 1608
    ## 1101 3229            AMERICAN              PHOENIX (392) 495-7961
    ## 1102  489              ALASKA             PORTLAND   473-238-3324
    ## 1103 1733          AIR CANADA              TORONTO (506) 760-3043
    ## 1104 2137          AIR CANADA            VANCOUVER   876-834-0624
    ## 1105  262          AIR CANADA              TORONTO   919 611 6170
    ## 1106 2999            AMERICAN                MIAMI (146) 699-3488
    ## 1107 2992            AMERICAN                MIAMI   261 434 7760
    ## 1108 3062 PHILIPPINE AIRLINES               MANILA   617 310 2684
    ## 1109 3078 PHILIPPINE AIRLINES               MANILA   182-535-3412
    ## 1110 2009      AIR FRANCE/KLM      PARIS-DE GAULLE   506 129 1694
    ## 1111 3197      CATHAY PACIFIC            HONG KONG (302) 339-0791
    ## 1112  437              UNITED             HONOLULU (446) 229-4342
    ## 1113 1869              ALASKA              KAHULUI (249) 602-6985
    ## 1114  550            EMIRATES                DUBAI (150) 905-6938
    ## 1115 1315             JETBLUE         NEW YORK-JFK   313 990 8823
    ## 1116 1321             JETBLUE         NEW YORK-JFK   656-941-5355
    ## 1117 3114                COPA          PANAMA CITY (116) 689-6617
    ## 1118 1902           LUFTHANSA               MUNICH   955 324 5981
    ## 1119 2688           SOUTHWEST               DENVER   175 808 2189
    ## 1120 2876             EVA AIR               TAIPEI (896) 993-8555
    ## 1121  371           LUFTHANSA            FRANKFURT   105-687-6500
    ## 1122 1735          AIR CANADA              TORONTO   757 524 2964
    ## 1123 2042     BRITISH AIRWAYS      LONDON HEATHROW   201 374 2424
    ## 1124 2597             JETBLUE               BOSTON (466) 912-8401
    ## 1125 2033     BRITISH AIRWAYS      LONDON HEATHROW   766 112 6143
    ## 1126 1739          AIR CANADA              TORONTO   783-463-4865
    ## 1127  734      CATHAY PACIFIC            HONG KONG   853-803-9900
    ## 1128  811              ALASKA             PORTLAND (347) 851-5388
    ## 1129  376              ALASKA    SAN JOSE DEL CABO   992 114 6973
    ## 1130  522              ALASKA      PUERTO VALLARTA   316-212-7309
    ## 1131 3095              UNITED              SPOKANE   301 672 1092
    ## 1132 1074          AIR CANADA              CALGARY   795 137 0201
    ## 1133  855              ALASKA             PORTLAND   381-883-5497
    ## 1134 2106             JETBLUE           LONG BEACH   100-531-4642
    ## 1135 3150              UNITED            SAN DIEGO   994 923 6634
    ## 1136 3154               DELTA       SALT LAKE CITY   920 355 8404
    ## 1137 3194               DELTA          LOS ANGELES (441) 445-6532
    ## 1138 1836              ALASKA       RALEIGH-DURHAM   325 795 2455
    ## 1139 1076          AIR CANADA              CALGARY   593 829 6250
    ## 1140  774            AMERICAN          LOS ANGELES   566-482-9004
    ## 1141  218            AMERICAN          LOS ANGELES   542 537 6770
    ## 1142  305           SOUTHWEST          LOS ANGELES   716 191 1741
    ## 1143 2616            INTERJET          GUADALAJARA   491-727-7162
    ## 1144  934          AIR CANADA              TORONTO   167-336-5660
    ## 1145 2268              UNITED          LOS ANGELES   358 831 0725
    ## 1146  727      CATHAY PACIFIC            HONG KONG   432 979 7292
    ## 1147 2160      CATHAY PACIFIC            HONG KONG (205) 382-5599
    ## 1148 1228           SOUTHWEST            LAS VEGAS (208) 794-9612
    ## 1149 1082          AIR CANADA              CALGARY   728 662 3934
    ## 1150 2632             JETBLUE         NEW YORK-JFK   380-918-8572
    ## 1151 2791                 WOW            REYKJAVIK (905) 742-3525
    ## 1152 2781                 WOW            REYKJAVIK   151 434 6989
    ## 1153 1114           SOUTHWEST            SAN DIEGO   755 544 2629
    ## 1154 3173            AMERICAN     DALLAS-FT. WORTH   633 181 4494
    ## 1155  259          AIR CANADA              TORONTO   346 706 5964
    ## 1156  258          AIR CANADA              TORONTO   688 690 2184
    ## 1157 3288           SOUTHWEST               DENVER   618 717 1697
    ## 1158 2808               DELTA          LOS ANGELES   185-321-6877
    ## 1159 1985              UNITED            BALTIMORE (147) 535-3529
    ## 1160 1147           SOUTHWEST            SAN DIEGO (152) 912-4118
    ## 1161  976           LUFTHANSA               MUNICH   726 943 7486
    ## 1162 2267              UNITED          BAKERSFIELD (634) 521-4714
    ## 1163 1153           SOUTHWEST            SAN DIEGO   670-248-0186
    ## 1164 2496              UNITED               DENVER (121) 509-7306
    ## 1165 2806               DELTA          LOS ANGELES (105) 635-5212
    ## 1166 1771            AMERICAN       CHICAGO-O'HARE (732) 168-0110
    ## 1167 2810               DELTA          LOS ANGELES   364 834 3150
    ## 1168 2911               DELTA              SEATTLE   176 508 2778
    ## 1169  223            AMERICAN          LOS ANGELES   120 941 0833
    ## 1170 2372              UNITED               AUSTIN   670 902 3199
    ## 1171 2131          AIR CANADA            VANCOUVER (214) 250-8756
    ## 1172 1421              UNITED            LAS VEGAS (119) 975-8484
    ## 1173 2831              UNITED              BURBANK   297 484 3285
    ## 1174 1179               DELTA         NEW YORK-JFK (489) 534-6272
    ## 1175  695      CATHAY PACIFIC            HONG KONG (610) 716-5732
    ## 1176 1705      AIR FRANCE/KLM      PARIS-DE GAULLE (456) 925-4236
    ## 1177 1530             JETBLUE         NEW YORK-JFK   743-103-7645
    ## 1178 2848              UNITED              SEATTLE   432-281-3682
    ## 1179 2265              UNITED          LOS ANGELES (167) 144-9470
    ## 1180 2635             JETBLUE         NEW YORK-JFK   648 685 6188
    ## 1181 1205           SOUTHWEST            LAS VEGAS   548 191 4898
    ## 1182  406              UNITED         HOUSTON-BUSH   288 110 9483
    ## 1183 2792               DELTA       SALT LAKE CITY   946-558-5801
    ## 1184 2693           SOUTHWEST               DENVER   388 744 9637
    ## 1185  193            AMERICAN          LOS ANGELES   506 463 9129
    ## 1186  824              ALASKA            NASHVILLE (848) 149-5208
    ## 1187 1922      AIR FRANCE/KLM      PARIS-DE GAULLE (970) 908-2298
    ## 1188 1821              ALASKA       RALEIGH-DURHAM (843) 120-5653
    ## 1189 2622             JETBLUE               BOSTON   306 394 8640
    ## 1190 1010          AIR CANADA            VANCOUVER   170-641-3537
    ## 1191 2679              UNITED       CHICAGO-O'HARE   860 723 5066
    ## 1192  254          AIR CANADA              TORONTO   814-895-6610
    ## 1193 2415     AIR NEW ZEALAND             AUCKLAND (139) 727-9901
    ## 1194  968           LUFTHANSA               MUNICH   598 735 8557
    ## 1195  812              ALASKA            BALTIMORE   593 895 6761
    ## 1196 1688      CATHAY PACIFIC            HONG KONG (817) 824-3849
    ## 1197 2084            AMERICAN         PHILADELPHIA   508 484 9738
    ## 1198 3213            AMERICAN          LOS ANGELES   719 489 4724
    ## 1199  247              UNITED            NASHVILLE   503-671-4901
    ## 1200  771              ALASKA         NEW YORK-JFK   275 649 8183
    ## 1201 1952              UNITED               BOSTON   968 130 7012
    ## 1202 2761              UNITED               AUSTIN   290 367 6676
    ## 1203  388              ALASKA          LOS ANGELES   499 766 9941
    ## 1204 2493              ALASKA       SALT LAKE CITY   538-393-2243
    ## 1205 2113             JETBLUE           LONG BEACH (540) 362-7136
    ## 1206  581             JETBLUE               BOSTON (802) 910-1742
    ## 1207 2945           SOUTHWEST          LOS ANGELES   845 544 4748
    ## 1208  382              ALASKA          LOS ANGELES   784-458-8425
    ## 1209  358              ALASKA               NEWARK (365) 217-0634
    ## 1210  915          AIR CANADA              TORONTO   708 500 2758
    ## 1211 1950      AIR FRANCE/KLM      PARIS-DE GAULLE (594) 797-7729
    ## 1212 2102         UNITED INTL          MEXICO CITY   982 555 9504
    ## 1213 3185               DELTA          LOS ANGELES   477-307-3338
    ## 1214  584             JETBLUE               BOSTON   744-301-1148
    ## 1215 2064              UNITED               NEWARK   389 484 8888
    ## 1216  285              UNITED            BALTIMORE   739 303 6128
    ## 1217 1123               DELTA MINNEAPOLIS-ST. PAUL   175 905 9962
    ## 1218 2934           SOUTHWEST            SAN DIEGO (900) 462-1379
    ## 1219 2827              UNITED              BURBANK   878-636-2294
    ## 1220  257          AIR CANADA              TORONTO (998) 692-1900
    ## 1221 2374              UNITED               AUSTIN   994 421 8642
    ## 1222  850            AMERICAN     DALLAS-FT. WORTH   304-225-5895
    ## 1223  194            AMERICAN          LOS ANGELES   931-522-5498
    ## 1224  306              UNITED            SAN DIEGO   838 898 2275
    ## 1225  458              UNITED     DALLAS-FT. WORTH   718 187 7125
    ## 1226  701      CATHAY PACIFIC            HONG KONG   943 561 8955
    ## 1227 2006      AIR FRANCE/KLM      PARIS-DE GAULLE   341 261 6456
    ## 1228 2624              UNITED       CHICAGO-O'HARE (507) 483-3618
    ## 1229 2617          AEROMEXICO          MEXICO CITY   826 266 0205
    ## 1230 1676      AIR FRANCE/KLM      PARIS-DE GAULLE (380) 449-7849
    ## 1231 1643     VIRGIN ATLANTIC      LONDON HEATHROW   589-975-0198
    ## 1232 2668              UNITED       RALEIGH-DURHAM   657 680 8781
    ## 1233 2772              UNITED               AUSTIN   501-668-7869
    ## 1234 2357              UNITED               AUSTIN   224 365 8299
    ## 1235 2479              UNITED       CHICAGO-O'HARE   747 588 1968
    ## 1236 3146               DELTA       SALT LAKE CITY (706) 836-7047
    ## 1237 3237            AMERICAN              PHOENIX (698) 462-6742
    ## 1238 2780               DELTA              ATLANTA   748 446 1257
    ## 1239 2333            AMERICAN       CHICAGO-O'HARE   525-552-4162
    ## 1240 2342              UNITED               AUSTIN   635-714-8302
    ## 1241 2359              UNITED               AUSTIN   114 668 2834
    ## 1242 3099              ALASKA               NEWARK   404 788 2855
    ## 1243 2310            AMERICAN       CHICAGO-O'HARE   673 292 2444
    ## 1244  200            AMERICAN          LOS ANGELES   136 788 1426
    ## 1245 2977            AMERICAN            CHARLOTTE   564 677 3934
    ## 1246 1300         UNITED INTL      LONDON HEATHROW   213 981 7762
    ## 1247 2526             JETBLUE           LONG BEACH   453-556-0852
    ## 1248  238            AMERICAN          LOS ANGELES   423 593 4483
    ## 1249 1122           SOUTHWEST            SAN DIEGO   820 609 7454
    ## 1250 1532             JETBLUE         NEW YORK-JFK   131-641-1331
    ## 1251 1877              UNITED               BOSTON   824 540 9579
    ## 1252 2332               DELTA MINNEAPOLIS-ST. PAUL (753) 726-0123
    ## 1253 2390         UNITED INTL               SYDNEY   739 737 9041
    ## 1254 2817               DELTA          LOS ANGELES   609-332-7370
    ## 1255 3182            AMERICAN     DALLAS-FT. WORTH   426-182-1365
    ## 1256 3281           SOUTHWEST               DENVER (347) 782-5787
    ## 1257  225            AMERICAN          LOS ANGELES   898-210-6218
    ## 1258 1175               DELTA         NEW YORK-JFK   938 394 0411
    ## 1259 1457              UNITED             PORTLAND   904 844 1759
    ## 1260 1757           LUFTHANSA               MUNICH   658-861-4306
    ## 1261  315              UNITED            SAN DIEGO (716) 184-1232
    ## 1262  274              UNITED          LOS ANGELES   380-105-1757
    ## 1263  465              UNITED               DENVER (969) 555-0453
    ## 1264 2873             EVA AIR               TAIPEI   249 452 4370
    ## 1265 1436              UNITED            LAS VEGAS   443 384 8253
    ## 1266  412              UNITED         HOUSTON-BUSH   711 289 2247
    ## 1267 1565         UNITED INTL            HONG KONG   343-973-0193
    ## 1268 2718      AIR FRANCE/KLM      PARIS-DE GAULLE   367 650 3720
    ## 1269 3218            AMERICAN          LOS ANGELES (265) 286-5671
    ## 1270 1930      AIR FRANCE/KLM      PARIS-DE GAULLE   148 630 8560
    ## 1271  867            AMERICAN     DALLAS-FT. WORTH   413-727-2672
    ## 1272  340           SOUTHWEST              PHOENIX           1623
    ## 1273 1629          KOREAN AIR                SEOUL (229) 604-7790
    ## 1274 1201           SOUTHWEST            LAS VEGAS (892) 301-0333
    ## 1275 2868              UNITED WASHINGTON DC-DULLES   148 501 5084
    ## 1276 1165               DELTA         NEW YORK-JFK   564 780 8272
    ## 1277  622              ALASKA               NEWARK   524 190 0899
    ## 1278  658         UNITED INTL               MUNICH   463 792 2782
    ## 1279  505              ALASKA             PORTLAND (331) 472-8624
    ## 1280  564            EMIRATES                DUBAI   522-286-5318
    ## 1281 3139              ALASKA               NEWARK   310 719 4550
    ## 1282 3220            AMERICAN          LOS ANGELES   314-360-4288
    ## 1283  528              ALASKA      PUERTO VALLARTA (301) 534-5754
    ## 1284 1560               DELTA              ATLANTA   822 271 5719
    ## 1285 1937      AIR FRANCE/KLM      PARIS-DE GAULLE (341) 473-0639
    ## 1286 2444              UNITED             HONOLULU (835) 882-3693
    ## 1287 2553             JETBLUE               BOSTON   465-550-6610
    ## 1288 2771               DELTA              ATLANTA   388 100 1482
    ## 1289 3022              UNITED             PORTLAND (589) 194-0523
    ## 1290  739      CATHAY PACIFIC            HONG KONG   982 842 4913
    ## 1291 2796                 WOW            REYKJAVIK   671 913 4563
    ## 1292 1316         UNITED INTL      LONDON HEATHROW   144 468 5864
    ## 1293 2429     AIR NEW ZEALAND             AUCKLAND   194 344 4039
    ## 1294 1932      AIR FRANCE/KLM      PARIS-DE GAULLE   331 747 5714
    ## 1295 2086            AMERICAN         PHILADELPHIA   221-190-1449
    ## 1296 2360              UNITED               AUSTIN (322) 843-0185
    ## 1297  390              ALASKA          LOS ANGELES   676-614-9095
    ## 1298 1278         UNITED INTL      LONDON HEATHROW (190) 975-2514
    ## 1299 1949      AIR FRANCE/KLM      PARIS-DE GAULLE (909) 382-3774
    ## 1300 1626          KOREAN AIR                SEOUL   956 257 9319
    ## 1301 3241            AMERICAN              PHOENIX   362 145 8268
    ## 1302 3230              ALASKA             PORTLAND   570 727 3998
    ## 1303  514              UNITED              KAHULUI   225 964 9193
    ## 1304  365              ALASKA    SAN JOSE DEL CABO   227 419 9482
    ## 1305 2280                 WOW            REYKJAVIK   949 360 7605
    ## 1306 1936              UNITED               BOSTON (347) 896-3463
    ## 1307  425              UNITED             HONOLULU   794 939 9735
    ## 1308 2981            AMERICAN            CHARLOTTE   413 754 3034
    ## 1309 2022              UNITED               NEWARK   806 730 0459
    ## 1310 1674            EMIRATES                DUBAI   791 195 8909
    ## 1311  374              ALASKA    SAN JOSE DEL CABO   691-318-3535
    ## 1312  592             JETBLUE               BOSTON   852-386-6029
    ## 1313 1732          AIR CANADA              TORONTO   380 682 7795
    ## 1314 1976              UNITED            BALTIMORE (355) 550-1392
    ## 1315 2801                 WOW            REYKJAVIK   247 586 4579
    ## 1316 2805           SOUTHWEST              PHOENIX   235 257 1041
    ## 1317   12           SOUTHWEST          LOS ANGELES (705) 456-1905
    ## 1318  579             JETBLUE               BOSTON   106 756 2785
    ## 1319 1140           SOUTHWEST          LOS ANGELES (836) 207-8419
    ## 1320 1652     VIRGIN ATLANTIC      LONDON HEATHROW (306) 552-1875
    ## 1321 1907           LUFTHANSA               MUNICH   729-102-7511
    ## 1322 2536             JETBLUE               BOSTON   795 583 0958
    ## 1323 2735               DELTA          LOS ANGELES   700-431-3918
    ## 1324 1939      AIR FRANCE/KLM      PARIS-DE GAULLE   929 632 1068
    ## 1325 1567         UNITED INTL            HONG KONG   763 906 2495
    ## 1326 2542             JETBLUE           LONG BEACH   469 976 6796
    ## 1327  501              UNITED              KAHULUI   887-657-4143
    ## 1328 1207           SOUTHWEST            LAS VEGAS   574-438-5329
    ## 1329 1281         UNITED INTL      LONDON HEATHROW   319 127 9518
    ## 1330 2058              UNITED               NEWARK   429 960 9710
    ## 1331  674      AIR FRANCE/KLM      PARIS-DE GAULLE   419 646 0299
    ## 1332  339           SOUTHWEST              PHOENIX   192 343 8515
    ## 1333 2546             JETBLUE               BOSTON   521 336 8581
    ## 1334 1597           SOUTHWEST          LOS ANGELES   776 367 6109
    ## 1335 1213           SOUTHWEST            LAS VEGAS   470 367 1392
    ## 1336 2449             JETBLUE               BOSTON (944) 189-7555
    ## 1337 1016          AIR CANADA            VANCOUVER   998-931-4783
    ## 1338 1347         UNITED INTL             SHANGHAI   362-178-6307
    ## 1339 1645     VIRGIN ATLANTIC      LONDON HEATHROW (458) 404-9558
    ## 1340 2286              QANTAS               SYDNEY   212 286 7936
    ## 1341 2883             EVA AIR               TAIPEI (481) 522-1039
    ## 1342 3098                COPA          PANAMA CITY (376) 611-4588
    ## 1343 3238              ALASKA             PORTLAND   936 193 9690
    ## 1344 1552               DELTA         NEW YORK-JFK (641) 544-6549
    ## 1345 2927    TURKISH AIRLINES             ISTANBUL   797-870-7818
    ## 1346 2962            AMERICAN         NEW YORK-JFK   693 907 5353
    ## 1347 1143           SOUTHWEST          LOS ANGELES   332 973 4943
    ## 1348 2341               DELTA MINNEAPOLIS-ST. PAUL   929 622 9077
    ## 1349 2291              QANTAS               SYDNEY   649-379-5361
    ## 1350 2790                 WOW            REYKJAVIK (572) 748-6932
    ## 1351  444              UNITED             HONOLULU   395-892-5646
    ## 1352 1223           SOUTHWEST            LAS VEGAS   221-628-9561
    ## 1353  946          AIR CANADA            VANCOUVER (227) 801-6148
    ## 1354 2734      AIR FRANCE/KLM      PARIS-DE GAULLE   549-649-1864
    ## 1355 2437     AIR NEW ZEALAND             AUCKLAND   342 941 0439
    ## 1356  703      CATHAY PACIFIC            HONG KONG   701 390 9814
    ## 1357 1611     VIRGIN ATLANTIC      LONDON HEATHROW (519) 573-6576
    ## 1358 1812              ALASKA       RALEIGH-DURHAM   919-342-0230
    ## 1359 2840              UNITED       CHICAGO-O'HARE   364-759-2705
    ## 1360  785         UNITED INTL    SAN JOSE DEL CABO   949 543 7906
    ## 1361  409              UNITED         HOUSTON-BUSH   942 732 6403
    ## 1362  608             JETBLUE               BOSTON (900) 586-1787
    ## 1363 2386         UNITED INTL               SYDNEY   308-607-9855
    ## 1364 3069 PHILIPPINE AIRLINES               MANILA   764 645 5740
    ## 1365 3183               DELTA          LOS ANGELES   472-337-8838
    ## 1366 1511        HAWAIIAN AIR             HONOLULU   791 847 7278
    ## 1367 1763           LUFTHANSA               MUNICH   128 805 3828
    ## 1368 1884  SINGAPORE AIRLINES            SINGAPORE   365-832-0674
    ## 1369 2813               DELTA          LOS ANGELES   123-282-3494
    ## 1370  843              ALASKA              SEATTLE   285 424 4318
    ## 1371  356           LUFTHANSA            FRANKFURT   452 352 1387
    ## 1372  195            AMERICAN          LOS ANGELES   129-377-8159
    ## 1373  288              UNITED            BALTIMORE   222 143 3131
    ## 1374 1596  SINGAPORE AIRLINES            SINGAPORE   162-451-0594
    ## 1375 1915      AIR FRANCE/KLM      PARIS-DE GAULLE (239) 325-5321
    ## 1376 2207          AER LINGUS               DUBLIN   436-422-6171
    ## 1377  792            AMERICAN          LOS ANGELES   605 284 4260
    ## 1378 1438              UNITED             PORTLAND   929-102-5905
    ## 1379 1576               DELTA              DETROIT   847 507 8268
    ## 1380 2538             JETBLUE               BOSTON   452-811-8088
    ## 1381 2465              UNITED               DENVER   799 143 1677
    ## 1382 3279           SOUTHWEST               DENVER   196 756 4555
    ## 1383 1423              UNITED          KANSAS CITY   119-444-0817
    ## 1384  625              ALASKA               NEWARK   885 454 0883
    ## 1385 3059 PHILIPPINE AIRLINES               MANILA   945-998-0444
    ## 1386  462              ALASKA            LAS VEGAS (367) 897-7969
    ## 1387  731      CATHAY PACIFIC            HONG KONG   163 241 9321
    ## 1388 1498        HAWAIIAN AIR             HONOLULU   594-176-5811
    ## 1389 1642     VIRGIN ATLANTIC      LONDON HEATHROW (621) 874-9973
    ## 1390 2018     BRITISH AIRWAYS      LONDON HEATHROW   332 963 4103
    ## 1391 3064                COPA          PANAMA CITY   389 318 3975
    ## 1392  511              ALASKA             PORTLAND   894-593-7953
    ## 1393 1587               DELTA       SALT LAKE CITY   561-266-7842
    ## 1394 2587             JETBLUE               BOSTON   354-958-8052
    ## 1395  631              ALASKA WASHINGTON DC-DULLES   151 921 2775
    ## 1396 1468             AVIANCA         SAN SALVADOR   901 140 3759
    ## 1397  666         UNITED INTL               MUNICH (426) 342-7378
    ## 1398  441              UNITED             HONOLULU   333-520-4811
    ## 1399 2786                 WOW            REYKJAVIK (765) 191-1797
    ## 1400 1524               DELTA          LOS ANGELES   850 914 9348
    ## 1401 2799                 WOW            REYKJAVIK   246 272 9019
    ## 1402 2200          AER LINGUS               DUBLIN (400) 250-0871
    ## 1403 2243           SOUTHWEST              PHOENIX   504 419 9191
    ## 1404  752          AIR CANADA              TORONTO (434) 725-0561
    ## 1405 1473             AVIANCA         SAN SALVADOR   231 863 7554
    ## 1406 1661            EMIRATES                DUBAI (969) 207-3261
    ## 1407 1563               DELTA              ATLANTA   939 253 9048
    ## 1408 1711      AIR FRANCE/KLM      PARIS-DE GAULLE   879-154-4494
    ## 1409 2424              UNITED             HONOLULU   577 786 2546
    ## 1410 2517              UNITED               NEWARK (994) 688-3259
    ## 1411 2696              UNITED              ORLANDO   841-717-4447
    ## 1412 2965            AMERICAN            CHARLOTTE   397-353-6309
    ## 1413  769          AIR CANADA              TORONTO   558 191 4548
    ## 1414  980           LUFTHANSA               MUNICH   905 768 2297
    ## 1415 1111               DELTA MINNEAPOLIS-ST. PAUL   348 522 2051
    ## 1416 2003      AIR FRANCE/KLM      PARIS-DE GAULLE   307-323-6861
    ## 1417 2095             JETBLUE           LONG BEACH   663 886 2487
    ## 1418 2447             JETBLUE               BOSTON   274 944 6097
    ## 1419 2726      AIR FRANCE/KLM      PARIS-DE GAULLE (494) 308-3048
    ## 1420 3104              UNITED              SPOKANE   328 307 0875
    ## 1421 3105                COPA          PANAMA CITY   673 524 3504
    ## 1422 2794                 WOW            REYKJAVIK   934 721 0615
    ## 1423 1933      AIR FRANCE/KLM      PARIS-DE GAULLE   102-957-6486
    ## 1424 1666            EMIRATES                DUBAI (715) 288-8832
    ## 1425 2739              UNITED              KAHULUI   794 925 8846
    ## 1426 2032              UNITED               NEWARK   637 281 4111
    ## 1427 1398         UNITED INTL             SHANGHAI (594) 448-6242
    ## 1428  729      AIR FRANCE/KLM      PARIS-DE GAULLE   335 802 2651
    ## 1429  838              ALASKA              SEATTLE   560 699 9908
    ## 1430 2847              UNITED              SEATTLE   451 163 0102
    ## 1431 2464              UNITED             HONOLULU   613 800 0835
    ## 1432  245              UNITED            NASHVILLE   370 453 5800
    ## 1433 2859              UNITED              SEATTLE   192 507 5411
    ## 1434 2428     AIR NEW ZEALAND             AUCKLAND   182-227-4838
    ## 1435 3080              UNITED              SPOKANE   647 126 2332
    ## 1436  530              ALASKA      PUERTO VALLARTA   606 125 6957
    ## 1437 1572         UNITED INTL            HONG KONG   728-404-5558
    ## 1438 2285              QANTAS               SYDNEY   506 812 6052
    ## 1439 2585             JETBLUE           LONG BEACH   427-665-3475
    ## 1440 2241      CATHAY PACIFIC            HONG KONG   808 739 7162
    ## 1441 2545             JETBLUE           LONG BEACH   375-978-3305
    ## 1442 3093              UNITED              SPOKANE   181-708-2089
    ## 1443 2376             JETBLUE       FT. LAUDERDALE (802) 810-5574
    ## 1444 2540             JETBLUE           LONG BEACH   242-540-4234
    ## 1445 3018              UNITED             PORTLAND   105 116 9695
    ## 1446 1890              UNITED               BOSTON (867) 891-0871
    ## 1447 3289           SOUTHWEST               DENVER   945 144 7892
    ## 1448 1581               DELTA              DETROIT   531 895 6695
    ## 1449 1702      AIR FRANCE/KLM      PARIS-DE GAULLE (533) 213-4368
    ## 1450 1182               DELTA         NEW YORK-JFK   894 810 2674
    ## 1451 2968            AMERICAN            CHARLOTTE   943 812 6349
    ## 1452 1248       CHINA EASTERN             SHANGHAI   476 168 4235
    ## 1453 1526             JETBLUE         NEW YORK-JFK   931 385 6757
    ## 1454 1464              ALASKA         NEW YORK-JFK (324) 188-6781
    ## 1455 1651     VIRGIN ATLANTIC      LONDON HEATHROW   100 378 8095
    ## 1456  341              ALASKA            SAN DIEGO   459 572 0244
    ## 1457 1472              ALASKA         NEW YORK-JFK   397-362-5469
    ## 1458  227            AMERICAN          LOS ANGELES (102) 928-7959
    ## 1459 1872              ALASKA              KAHULUI (439) 568-6611
    ## 1460 2024              UNITED               NEWARK (767) 205-0604
    ## 1461 2127            FRONTIER               DENVER (890) 548-9219
    ## 1462 3286           SOUTHWEST               DENVER   938 982 5585
    ## 1463  332           SOUTHWEST              PHOENIX   769-472-2992
    ## 1464 1030          AIR CANADA            VANCOUVER   190 204 1154
    ## 1465  497              UNITED               DENVER (649) 925-8489
    ## 1466 3051 PHILIPPINE AIRLINES               MANILA   321 616 1013
    ## 1467  809              ALASKA             PORTLAND (156) 837-4491
    ## 1468 1599  SINGAPORE AIRLINES            SINGAPORE   178-232-0815
    ## 1469 2644             JETBLUE         NEW YORK-JFK (882) 304-9032
    ## 1470  738      CATHAY PACIFIC            HONG KONG   828 549 6666
    ## 1471 1221           SOUTHWEST            LAS VEGAS   280 544 4554
    ## 1472 1790              UNITED       RALEIGH-DURHAM   183 208 5054
    ## 1473 1979      CATHAY PACIFIC            HONG KONG (971) 548-6611
    ## 1474 2638             JETBLUE         NEW YORK-JFK (828) 153-5819
    ## 1475 1720          AIR CANADA              TORONTO   203 448 1522
    ## 1476 3033              UNITED             PORTLAND   900-871-9056
    ## 1477 2560             JETBLUE           LONG BEACH   406-167-1379
    ## 1478 3071 PHILIPPINE AIRLINES               MANILA (906) 850-9192
    ## 1479 1543           AIR CHINA              BEIJING (859) 777-8245
    ## 1480 1238       CHINA EASTERN             SHANGHAI   641-635-8466
    ## 1481 1731          AIR CANADA              TORONTO   807-671-6158
    ## 1482 1968      CATHAY PACIFIC            HONG KONG (589) 270-7518
    ## 1483 2274              UNITED          LOS ANGELES   768 529 8051
    ## 1484 2431     AIR NEW ZEALAND             AUCKLAND   220 660 0306
    ## 1485 2541             JETBLUE           LONG BEACH   928-179-7556
    ## 1486 1138           SOUTHWEST          LOS ANGELES   153 756 0278
    ## 1487 2234           SOUTHWEST            SANTA ANA   273 829 9197
    ## 1488 1503        HAWAIIAN AIR             HONOLULU   269 463 0911
    ## 1489 2742      AIR FRANCE/KLM      PARIS-DE GAULLE   696 984 8826
    ## 1490 2235      CATHAY PACIFIC            HONG KONG   905-903-5258
    ## 1491 1157           SOUTHWEST          LOS ANGELES   267 332 4709
    ## 1492 2210          AER LINGUS               DUBLIN (146) 129-5118
    ## 1493 1359         UNITED INTL               KANSAI (927) 747-9822
    ## 1494 1334         UNITED INTL             SHANGHAI   534 216 6666
    ## 1495 1900              UNITED               BOSTON (481) 479-7013
    ## 1496 2175            FRONTIER               DENVER   971 175 2968
    ## 1497 1038          AIR CANADA            VANCOUVER (716) 777-3762
    ## 1498 2566             JETBLUE           LONG BEACH   274-863-3205
    ## 1499 2984            AMERICAN            CHARLOTTE (217) 589-0596
    ## 1500 2092             JETBLUE           LONG BEACH   928 445 5474
    ## 1501 2578             JETBLUE           LONG BEACH   858 990 5153
    ## 1502 3137              ALASKA               NEWARK   731-813-2043
    ## 1503 1476              ALASKA         NEW YORK-JFK   563-732-6802
    ## 1504 2647             JETBLUE               BOSTON   145 725 4021
    ## 1505  289           SOUTHWEST          LOS ANGELES   931 311 5801
    ## 1506 1126               DELTA MINNEAPOLIS-ST. PAUL (637) 782-6989
    ## 1507  784            AMERICAN          LOS ANGELES   172 990 3485
    ## 1508 1106               DELTA MINNEAPOLIS-ST. PAUL   872 325 4341
    ## 1509 2631             JETBLUE         NEW YORK-JFK (359) 803-9809
    ## 1510 3036            AMERICAN     DALLAS-FT. WORTH   152 790 8238
    ## 1511 1502        HAWAIIAN AIR             HONOLULU   330 561 9257
    ## 1512 1854           SOUTHWEST            SAN DIEGO   437 420 7546
    ## 1513  201            AMERICAN          LOS ANGELES   495 632 4027
    ## 1514  907          AIR CANADA              TORONTO (416) 788-2844
    ## 1515 2952           SOUTHWEST          LOS ANGELES   311-305-4367
    ## 1516 3166            AMERICAN     DALLAS-FT. WORTH   817-400-0481
    ## 1517  443              UNITED         INDIANAPOLIS   430 723 1079
    ## 1518  527            EMIRATES                DUBAI (729) 609-4819
    ## 1519 2559             JETBLUE               BOSTON (201) 737-4409
    ## 1520 2599          AEROMEXICO          MEXICO CITY (137) 611-3694
    ## 1521 3190      CATHAY PACIFIC            HONG KONG   226 490 8696
    ## 1522 1904              UNITED               BOSTON   123 570 8640
    ## 1523 1128               DELTA MINNEAPOLIS-ST. PAUL        665-803
    ## 1524 2153            FRONTIER               DENVER (812) 869-6263
    ## 1525 1997              UNITED            BALTIMORE   639 132 6386
    ## 1526 1777              UNITED       RALEIGH-DURHAM (194) 198-0504
    ## 1527 2159       CHINA EASTERN              QINGDAO   437 886 0753
    ## 1528 2063            AMERICAN         PHILADELPHIA   626 756 5089
    ## 1529  999           LUFTHANSA               MUNICH (299) 137-6993
    ## 1530 3168            AMERICAN     DALLAS-FT. WORTH   714 950 3364
    ## 1531  587              ALASKA WASHINGTON DC-DULLES   653-786-5985
    ## 1532 1396         UNITED INTL             SHANGHAI   518 286 5956
    ## 1533 1737          AIR CANADA              TORONTO   194 960 2145
    ## 1534 2439     AIR NEW ZEALAND             AUCKLAND   362-136-1153
    ## 1535  602              ALASKA WASHINGTON DC-DULLES   376-456-0697
    ## 1536 2571             JETBLUE           LONG BEACH   657 832 1189
    ## 1537 2370              UNITED               AUSTIN   962-918-6117
    ## 1538  524            EMIRATES                DUBAI   692 929 3592
    ## 1539 2219           SOUTHWEST              PHOENIX   805-877-3887
    ## 1540  383              ALASKA          LOS ANGELES (739) 710-2966
    ## 1541 1928      AIR FRANCE/KLM      PARIS-DE GAULLE   819 732 4132
    ## 1542  930          AIR CANADA              TORONTO   367 221 9710
    ## 1543 2245           SOUTHWEST              PHOENIX   361 154 1789
    ## 1544 1012          AIR CANADA            VANCOUVER   680 488 1182
    ## 1545 1717      AIR FRANCE/KLM      PARIS-DE GAULLE   928 638 1186
    ## 1546 1331         UNITED INTL               KANSAI   588-693-9875
    ## 1547 1647     VIRGIN ATLANTIC      LONDON HEATHROW   681-308-7915
    ## 1548 1870              UNITED          LOS ANGELES   783 647 8490
    ## 1549 1089               DELTA MINNEAPOLIS-ST. PAUL   897 847 0632
    ## 1550  786         UNITED INTL    SAN JOSE DEL CABO   150 952 4453
    ## 1551 2507              UNITED       CHICAGO-O'HARE   322-884-3020
    ## 1552 1796              ALASKA       RALEIGH-DURHAM   176-313-5403
    ## 1553 1963      CATHAY PACIFIC            HONG KONG   487 109 4196
    ## 1554 2150            FRONTIER               DENVER (477) 182-4689
    ## 1555  403              UNITED              PHOENIX   544 382 8289
    ## 1556  461              UNITED     DALLAS-FT. WORTH   781 543 7456
    ## 1557  804              ALASKA            BALTIMORE   911 829 6476
    ## 1558 3202      CATHAY PACIFIC            HONG KONG (525) 362-5532
    ## 1559 2979            AMERICAN            CHARLOTTE   517 986 3426
    ## 1560  905              ALASKA       RALEIGH-DURHAM   838 220 5397
    ## 1561  542            EMIRATES                DUBAI (687) 887-6766
    ## 1562 1730          AIR CANADA              TORONTO   179 163 0902
    ## 1563  470              UNITED               DENVER   539 137 8983
    ## 1564 1501        HAWAIIAN AIR             HONOLULU   733-154-0094
    ## 1565 1202           SOUTHWEST            LAS VEGAS   639 881 3693
    ## 1566 1380         UNITED INTL      PARIS-DE GAULLE   291-830-3017
    ## 1567 1346         UNITED INTL               KANSAI (637) 100-0509
    ## 1568    8           SOUTHWEST          LOS ANGELES   750 520 0167
    ## 1569  626              ALASKA WASHINGTON DC-DULLES   676 485 8963
    ## 1570 1801              ALASKA       RALEIGH-DURHAM   135 566 5090
    ## 1571 2247                 WOW            REYKJAVIK   337 260 4996
    ## 1572  873              ALASKA              SEATTLE   371 185 2377
    ## 1573  554            EMIRATES                DUBAI   280 461 1386
    ## 1574  697      AIR FRANCE/KLM      PARIS-DE GAULLE (603) 149-7268
    ## 1575  836              ALASKA              SEATTLE (364) 792-5553
    ## 1576 1918      AIR FRANCE/KLM      PARIS-DE GAULLE   496-429-1314
    ## 1577 2838              UNITED       CHICAGO-O'HARE   459 671 4698
    ## 1578 2879             EVA AIR               TAIPEI   486-268-3312
    ## 1579 1839              ALASKA       RALEIGH-DURHAM   497-518-4050
    ## 1580  445              UNITED         INDIANAPOLIS (535) 685-8273
    ## 1581 1393         UNITED INTL             SHANGHAI   859 495 4050
    ## 1582  928              ALASKA       RALEIGH-DURHAM (826) 738-8316
    ## 1583 1852           SOUTHWEST            SAN DIEGO   724-134-3870
    ## 1584  559              ALASKA              SEATTLE (554) 269-8937
    ## 1585 2136      CATHAY PACIFIC            HONG KONG   125-578-4253
    ## 1586 2660             JETBLUE               BOSTON   614 800 2861
    ## 1587  612             JETBLUE               BOSTON   487-232-4449
    ## 1588 2529             JETBLUE           LONG BEACH (298) 135-0900
    ## 1589  835              ALASKA            NASHVILLE (392) 183-7831
    ## 1590 1002           LUFTHANSA               MUNICH (606) 596-1029
    ## 1591  861            AMERICAN     DALLAS-FT. WORTH (384) 953-4795
    ## 1592 2662              UNITED       RALEIGH-DURHAM (855) 811-8811
    ## 1593 1620     VIRGIN ATLANTIC      LONDON HEATHROW   253-374-7102
    ## 1594 3126              ALASKA               NEWARK (419) 295-9580
    ## 1595 2916    TURKISH AIRLINES             ISTANBUL   802 102 8345
    ## 1596 3242            AMERICAN              PHOENIX   417 393 0050
    ## 1597  829              ALASKA              SEATTLE   787-624-8443
    ## 1598 1822         UNITED INTL      LONDON HEATHROW (919) 486-4251
    ## 1599 3077              UNITED              SPOKANE   341 824 5322
    ## 1600 2278                 WOW            REYKJAVIK   415 551 1608
    ## 1601 2556             JETBLUE           LONG BEACH (392) 495-7961
    ## 1602 2935    TURKISH AIRLINES             ISTANBUL   473-238-3324
    ## 1603 2266                 WOW            REYKJAVIK (506) 760-3043
    ## 1604 2044     BRITISH AIRWAYS      LONDON HEATHROW   876-834-0624
    ## 1605 1747           LUFTHANSA               MUNICH   919 611 6170
    ## 1606 2301              QANTAS               SYDNEY (146) 699-3488
    ## 1607  380              ALASKA          LOS ANGELES   261 434 7760
    ## 1608 2445             JETBLUE               BOSTON   617 310 2684
    ## 1609 1408              UNITED          KANSAS CITY   182-535-3412
    ## 1610 2346              UNITED               AUSTIN   506 129 1694
    ## 1611  558            EMIRATES                DUBAI (302) 339-0791
    ## 1612 1919      AIR FRANCE/KLM      PARIS-DE GAULLE (446) 229-4342
    ## 1613 1382         UNITED INTL      PARIS-DE GAULLE (249) 602-6985
    ## 1614  808              ALASKA            BALTIMORE (150) 905-6938
    ## 1615 2488              ALASKA       SALT LAKE CITY   313 990 8823
    ## 1616 2913    TURKISH AIRLINES             ISTANBUL   656-941-5355
    ## 1617 3140               DELTA       SALT LAKE CITY (116) 689-6617
    ## 1618 2320              QANTAS               SYDNEY   955 324 5981
    ## 1619  355              ALASKA               NEWARK   175 808 2189
    ## 1620 2212          AER LINGUS               DUBLIN (896) 993-8555
    ## 1621 2416     AIR NEW ZEALAND             AUCKLAND   105-687-6500
    ## 1622 2863              UNITED              SEATTLE   757 524 2964
    ## 1623 1881  SINGAPORE AIRLINES            SINGAPORE   201 374 2424
    ## 1624 2579             JETBLUE           LONG BEACH (466) 912-8401
    ## 1625  508              UNITED              KAHULUI   766 112 6143
    ## 1626 2878             EVA AIR               TAIPEI   783-463-4865
    ## 1627 3109              UNITED              SPOKANE   853-803-9900
    ## 1628  363           LUFTHANSA            FRANKFURT (347) 851-5388
    ## 1629 3067 PHILIPPINE AIRLINES               MANILA   992 114 6973
    ## 1630  818              ALASKA            BALTIMORE   316-212-7309
    ## 1631 2523              UNITED               NEWARK   301 672 1092
    ## 1632 1216           SOUTHWEST            LAS VEGAS   795 137 0201
    ## 1633 1659            EMIRATES                DUBAI   381-883-5497
    ## 1634  548            EMIRATES                DUBAI   100-531-4642
    ## 1635 2206          AER LINGUS               DUBLIN   994 923 6634
    ## 1636 2026     BRITISH AIRWAYS      LONDON HEATHROW   920 355 8404
    ## 1637  416              UNITED         HOUSTON-BUSH (441) 445-6532
    ## 1638 1547           AIR CHINA              BEIJING   325 795 2455
    ## 1639 2552             JETBLUE           LONG BEACH   593 829 6250
    ## 1640 2476              UNITED       CHICAGO-O'HARE   566-482-9004
    ## 1641 2441              UNITED             HONOLULU   542 537 6770
    ## 1642 2512              UNITED       CHICAGO-O'HARE   716 191 1741
    ## 1643 1753           LUFTHANSA               MUNICH   491-727-7162
    ## 1644  351           LUFTHANSA            FRANKFURT   167-336-5660
    ## 1645 2923               DELTA              SEATTLE   358 831 0725
    ## 1646  880              ALASKA              SEATTLE   432 979 7292
    ## 1647 3222              ALASKA             PORTLAND (205) 382-5599
    ## 1648 2671              UNITED       RALEIGH-DURHAM (208) 794-9612
    ## 1649  586             JETBLUE               BOSTON   728 662 3934
    ## 1650 1166           SOUTHWEST          LOS ANGELES   380-918-8572
    ## 1651 2306            AMERICAN       CHICAGO-O'HARE (905) 742-3525
    ## 1652  984           LUFTHANSA               MUNICH   151 434 6989
    ## 1653 2089            AMERICAN         PHILADELPHIA   755 544 2629
    ## 1654 3096              ALASKA               NEWARK   633 181 4494
    ## 1655 3031            AMERICAN     DALLAS-FT. WORTH   346 706 5964
    ## 1656 2027              UNITED               NEWARK   688 690 2184
    ## 1657  359           LUFTHANSA            FRANKFURT   618 717 1697
    ## 1658 1892  SINGAPORE AIRLINES            SINGAPORE   185-321-6877
    ## 1659  216            AMERICAN          LOS ANGELES (147) 535-3529
    ## 1660 1768            AMERICAN       CHICAGO-O'HARE (152) 912-4118
    ## 1661 1888  SINGAPORE AIRLINES            SINGAPORE   726 943 7486
    ## 1662 3156               DELTA       SALT LAKE CITY (634) 521-4714
    ## 1663 2719               DELTA          LOS ANGELES   670-248-0186
    ## 1664 2756               DELTA              ATLANTA (121) 509-7306
    ## 1665  214            AMERICAN          LOS ANGELES (105) 635-5212
    ## 1666  212            AMERICAN          LOS ANGELES (732) 168-0110
    ## 1667 2983            AMERICAN            CHARLOTTE   364 834 3150
    ## 1668 1350         UNITED INTL             SHANGHAI   176 508 2778
    ## 1669  493              UNITED               DENVER   120 941 0833
    ## 1670 1887              UNITED               BOSTON   670 902 3199
    ## 1671 2824              UNITED              BURBANK (214) 250-8756
    ## 1672 2348              UNITED               AUSTIN (119) 975-8484
    ## 1673 1750           LUFTHANSA               MUNICH   297 484 3285
    ## 1674 3245            AMERICAN              PHOENIX (489) 534-6272
    ## 1675 3270              UNITED            LAS VEGAS (610) 716-5732
    ## 1676 1118           SOUTHWEST            SAN DIEGO (456) 925-4236
    ## 1677 1142           SOUTHWEST            SAN DIEGO   743-103-7645
    ## 1678 1823              ALASKA       RALEIGH-DURHAM   432-281-3682
    ## 1679 2096             JETBLUE           LONG BEACH (167) 144-9470
    ## 1680 2473              UNITED               DENVER   648 685 6188
    ## 1681 2514              UNITED       CHICAGO-O'HARE   548 191 4898
    ## 1682 3282           SOUTHWEST               DENVER   288 110 9483
    ## 1683  878              ALASKA              SEATTLE   946-558-5801
    ## 1684 2315            AMERICAN       CHICAGO-O'HARE   388 744 9637
    ## 1685 2353              UNITED               AUSTIN   506 463 9129
    ## 1686 1618     VIRGIN ATLANTIC      LONDON HEATHROW (848) 149-5208
    ## 1687 2485              UNITED               DENVER (970) 908-2298
    ## 1688 2787               DELTA              ATLANTA (843) 120-5653
    ## 1689 2504              ALASKA       SALT LAKE CITY   306 394 8640
    ## 1690 2028              UNITED               NEWARK   170-641-3537
    ## 1691  366           LUFTHANSA            FRANKFURT   860 723 5066
    ## 1692 1679      CATHAY PACIFIC            HONG KONG   814-895-6610
    ## 1693 2880             EVA AIR               TAIPEI (139) 727-9901
    ## 1694  385              ALASKA          LOS ANGELES   598 735 8557
    ## 1695  569              ALASKA              SEATTLE   593 895 6761
    ## 1696  941          AIR CANADA            VANCOUVER (817) 824-3849
    ## 1697 1291         UNITED INTL      LONDON HEATHROW   508 484 9738
    ## 1698 1671            EMIRATES                DUBAI   719 489 4724
    ## 1699 2782               DELTA              ATLANTA   503-671-4901
    ## 1700 2850              UNITED              SEATTLE   275 649 8183
    ## 1701 3192               DELTA          LOS ANGELES   968 130 7012
    ## 1702 3274              UNITED            LAS VEGAS   290 367 6676
    ## 1703 2697              UNITED       CHICAGO-O'HARE   499 766 9941
    ## 1704 1684      CATHAY PACIFIC            HONG KONG   538-393-2243
    ## 1705 1999              UNITED            BALTIMORE (540) 362-7136
    ## 1706 1312         UNITED INTL      LONDON HEATHROW (802) 910-1742
    ## 1707 3132              UNITED            SAN DIEGO   845 544 4748
    ## 1708 2563              UNITED       CHICAGO-O'HARE   784-458-8425
    ## 1709 2690              UNITED       CHICAGO-O'HARE (365) 217-0634
    ## 1710  314              UNITED            SAN DIEGO   708 500 2758
    ## 1711 2830              UNITED              BURBANK (594) 797-7729
    ## 1712 1467              ALASKA         NEW YORK-JFK   982 555 9504
    ## 1713 2568             JETBLUE           LONG BEACH   477-307-3338
    ## 1714 2570              UNITED       CHICAGO-O'HARE   744-301-1148
    ## 1715 1437              UNITED            LAS VEGAS   389 484 8888
    ## 1716 1725          AIR CANADA              TORONTO   739 303 6128
    ## 1717 1668            EMIRATES                DUBAI   175 905 9962
    ## 1718 2646             JETBLUE         NEW YORK-JFK (900) 462-1379
    ## 1719 2079             JETBLUE           LONG BEACH   878-636-2294
    ## 1720  593             JETBLUE               BOSTON (998) 692-1900
    ## 1721 2539             JETBLUE           LONG BEACH   994 421 8642
    ## 1722  802            AMERICAN          LOS ANGELES   304-225-5895
    ## 1723 2851              UNITED              SEATTLE   931-522-5498
    ## 1724 1036          AIR CANADA            VANCOUVER   838 898 2275
    ## 1725 2795               DELTA       SALT LAKE CITY   718 187 7125
    ## 1726 1972              UNITED            BALTIMORE   943 561 8955
    ## 1727 2535             JETBLUE           LONG BEACH   341 261 6456
    ## 1728 2591             JETBLUE               BOSTON (507) 483-3618
    ## 1729  604             JETBLUE               BOSTON   826 266 0205
    ## 1730 2303              UNITED          SAN ANTONIO (380) 449-7849
    ## 1731 1675      AIR FRANCE/KLM      PARIS-DE GAULLE   589-975-0198
    ## 1732  404              UNITED              PHOENIX   657 680 8781
    ## 1733  357           LUFTHANSA            FRANKFURT   501-668-7869
    ## 1734  858            AMERICAN     DALLAS-FT. WORTH   224 365 8299
    ## 1735  933          AIR CANADA              TORONTO   747 588 1968
    ## 1736 2495              UNITED       CHICAGO-O'HARE (706) 836-7047
    ## 1737 2829              UNITED              BURBANK (698) 462-6742
    ## 1738 3159              UNITED            SAN DIEGO   748 446 1257
    ## 1739  537            EMIRATES                DUBAI   525-552-4162
    ## 1740  762          AIR CANADA              TORONTO   635-714-8302
    ## 1741 1548               DELTA         NEW YORK-JFK   114 668 2834
    ## 1742 2163            FRONTIER               DENVER   404 788 2855
    ## 1743 2466              UNITED       CHICAGO-O'HARE   673 292 2444
    ## 1744  407              UNITED              PHOENIX   136 788 1426
    ## 1745 1389         UNITED INTL             SHANGHAI   564 677 3934
    ## 1746  456              UNITED     DALLAS-FT. WORTH   213 981 7762
    ## 1747 1364         UNITED INTL               KANSAI   453-556-0852
    ## 1748 3280           SOUTHWEST               DENVER   423 593 4483
    ## 1749  851            AMERICAN     DALLAS-FT. WORTH   820 609 7454
    ## 1750  619              ALASKA               NEWARK   131-641-1331
    ## 1751 1818         UNITED INTL      LONDON HEATHROW   824 540 9579
    ## 1752  768              ALASKA         NEW YORK-JFK (753) 726-0123
    ## 1753 1964      CATHAY PACIFIC            HONG KONG   739 737 9041
    ## 1754 2271              UNITED          LOS ANGELES   609-332-7370
    ## 1755  472              UNITED               DENVER   426-182-1365
    ## 1756  644              ALASKA WASHINGTON DC-DULLES (347) 782-5787
    ## 1757  417              UNITED              PHOENIX   898-210-6218
    ## 1758  467              UNITED               DENVER   938 394 0411
    ## 1759 1310         UNITED INTL      LONDON HEATHROW   904 844 1759
    ## 1760 2633              UNITED       CHICAGO-O'HARE   658-861-4306
    ## 1761  378              ALASKA    SAN JOSE DEL CABO (716) 184-1232
    ## 1762 2201           SOUTHWEST            LAS VEGAS   380-105-1757
    ## 1763 3221            AMERICAN          LOS ANGELES (969) 555-0453
    ## 1764 1491        HAWAIIAN AIR             HONOLULU   249 452 4370
    ## 1765 2228          AER LINGUS               DUBLIN   443 384 8253
    ## 1766 2470              UNITED             HONOLULU   711 289 2247
    ## 1767 2419     AIR NEW ZEALAND             AUCKLAND   343-973-0193
    ## 1768 2767              UNITED               AUSTIN   367 650 3720
    ## 1769 2639              UNITED       CHICAGO-O'HARE (265) 286-5671
    ## 1770  301           SOUTHWEST          LOS ANGELES   148 630 8560
    ## 1771 2972            AMERICAN            CHARLOTTE   413-727-2672
    ## 1772 1631          KOREAN AIR                SEOUL (162) 332-5838
    ## 1773 1994              UNITED            BALTIMORE (229) 604-7790
    ## 1774  310              UNITED WASHINGTON DC-DULLES (892) 301-0333
    ## 1775  333              ALASKA            SAN DIEGO   148 501 5084
    ## 1776 1465            FRONTIER               DENVER   564 780 8272
    ## 1777  482              UNITED               DENVER   524 190 0899
    ## 1778 1701      AIR FRANCE/KLM      PARIS-DE GAULLE   463 792 2782
    ## 1779  439              UNITED             HONOLULU (331) 472-8624
    ## 1780 1697              ALASKA            SANTA ANA   522-286-5318
    ## 1781 1829              ALASKA       RALEIGH-DURHAM   310 719 4550
    ## 1782 1146           SOUTHWEST          LOS ANGELES   314-360-4288
    ## 1783 2641              UNITED       CHICAGO-O'HARE (301) 534-5754
    ## 1784 3004            AMERICAN                MIAMI   822 271 5719
    ## 1785 2800               DELTA       SALT LAKE CITY (341) 473-0639
    ## 1786 3052            AMERICAN                MIAMI (835) 882-3693
    ## 1787  234            AMERICAN          LOS ANGELES   465-550-6610
    ## 1788 1190           SOUTHWEST            LAS VEGAS   388 100 1482
    ## 1789 1361         UNITED INTL               KANSAI (589) 194-0523
    ## 1790 1861              ALASKA              KAHULUI   982 842 4913
    ## 1791 2555             JETBLUE           LONG BEACH   671 913 4563
    ## 1792 1054          AIR CANADA              CALGARY   144 468 5864
    ## 1793 1057          AIR CANADA              CALGARY   194 344 4039
    ## 1794 2785                 WOW            REYKJAVIK   331 747 5714
    ## 1795  777         UNITED INTL    SAN JOSE DEL CABO   221-190-1449
    ## 1796  606             JETBLUE               BOSTON (322) 843-0185
    ## 1797  307           SOUTHWEST          LOS ANGELES   676-614-9095
    ## 1798 1394         UNITED INTL      PARIS-DE GAULLE (190) 975-2514
    ## 1799 2611          AEROMEXICO          MEXICO CITY (909) 382-3774
    ## 1800 1636          KOREAN AIR                SEOUL   956 257 9319
    ## 1801 2115         UNITED INTL          MEXICO CITY   362 145 8268
    ## 1802  236            AMERICAN          LOS ANGELES   570 727 3998
    ## 1803 1399         UNITED INTL      PARIS-DE GAULLE   225 964 9193
    ## 1804 1947      AIR FRANCE/KLM      PARIS-DE GAULLE   227 419 9482
    ## 1805  424              UNITED         INDIANAPOLIS   949 360 7605
    ## 1806 3094                COPA          PANAMA CITY (347) 896-3463
    ## 1807 3253              UNITED            SAN DIEGO   794 939 9735
    ## 1808  459              ALASKA            LAS VEGAS   413 754 3034
    ## 1809 2783                 WOW            REYKJAVIK   806 730 0459
    ## 1810  671              UNITED               NEWARK   791 195 8909
    ## 1811  779            AMERICAN          LOS ANGELES   691-318-3535
    ## 1812 1258       CHINA EASTERN             SHANGHAI   852-386-6029
    ## 1813 1804         UNITED INTL      LONDON HEATHROW   380 682 7795
    ## 1814 1934      AIR FRANCE/KLM      PARIS-DE GAULLE (355) 550-1392
    ## 1815 2071            AMERICAN         PHILADELPHIA   247 586 4579
    ## 1816  418              UNITED             HONOLULU   235 257 1041
    ## 1817 1475              ALASKA         NEW YORK-JFK (705) 456-1905
    ## 1818 2260                 WOW            REYKJAVIK   106 756 2785
    ## 1819 1742      CATHAY PACIFIC            HONG KONG (836) 207-8419
    ## 1820 1940      AIR FRANCE/KLM      PARIS-DE GAULLE (306) 552-1875
    ## 1821 2114         UNITED INTL          MEXICO CITY   729-102-7511
    ## 1822 1125           SOUTHWEST            SAN DIEGO   795 583 0958
    ## 1823 1867              UNITED          LOS ANGELES   700-431-3918
    ## 1824 1960      CATHAY PACIFIC            HONG KONG   929 632 1068
    ## 1825 2313              QANTAS               SYDNEY   763 906 2495
    ## 1826  756              ALASKA         NEW YORK-JFK   469 976 6796
    ## 1827  957              ALASKA         NEW YORK-JFK   887-657-4143
    ## 1828 2295              QANTAS               SYDNEY   574-438-5329
    ## 1829 2930    TURKISH AIRLINES             ISTANBUL   319 127 9518
    ## 1830 2990            AMERICAN                MIAMI   429 960 9710
    ## 1831 1815         UNITED INTL      LONDON HEATHROW   419 646 0299
    ## 1832 2061              UNITED               NEWARK   192 343 8515
    ## 1833 1561               DELTA         NEW YORK-JFK   521 336 8581
    ## 1834 3265              UNITED            SAN DIEGO   776 367 6109
    ## 1835 1381         UNITED INTL             SHANGHAI   470 367 1392
    ## 1836  991           LUFTHANSA               MUNICH (944) 189-7555
    ## 1837  484              ALASKA             PORTLAND   998-931-4783
    ## 1838 2110             JETBLUE           LONG BEACH   362-178-6307
    ## 1839 2369             JETBLUE       FT. LAUDERDALE (458) 404-9558
    ## 1840 2393         UNITED INTL               SYDNEY   212 286 7936
    ## 1841  395              ALASKA          LOS ANGELES (481) 522-1039
    ## 1842 1456              ALASKA         NEW YORK-JFK (376) 611-4588
    ## 1843 1910      AIR FRANCE/KLM      PARIS-DE GAULLE   936 193 9690
    ## 1844 1625          KOREAN AIR                SEOUL (641) 544-6549
    ## 1845 1494        HAWAIIAN AIR             HONOLULU   797-870-7818
    ## 1846 3226              ALASKA             PORTLAND   693 907 5353
    ## 1847 1938              UNITED               BOSTON   332 973 4943
    ## 1848 1303         UNITED INTL      LONDON HEATHROW   929 622 9077
    ## 1849 3191               DELTA          LOS ANGELES   649-379-5361
    ## 1850 1013          AIR CANADA            VANCOUVER (572) 748-6932
    ## 1851 1663            EMIRATES                DUBAI   395-892-5646
    ## 1852 1136           SOUTHWEST          LOS ANGELES   221-628-9561
    ## 1853 1713      AIR FRANCE/KLM      PARIS-DE GAULLE (227) 801-6148
    ## 1854 1185               DELTA         NEW YORK-JFK   549-649-1864
    ## 1855 1402         UNITED INTL      PARIS-DE GAULLE   342 941 0439
    ## 1856 1649     VIRGIN ATLANTIC      LONDON HEATHROW   701 390 9814
    ## 1857 1956              UNITED               BOSTON (519) 573-6576
    ## 1858 1988      AIR FRANCE/KLM            AMSTERDAM   919-342-0230
    ## 1859 2220           SOUTHWEST              PHOENIX   364-759-2705
    ## 1860 2312            AMERICAN       CHICAGO-O'HARE   949 543 7906
    ## 1861 2614            INTERJET          GUADALAJARA   942 732 6403
    ## 1862 2798               DELTA       SALT LAKE CITY (900) 586-1787
    ## 1863 2525              UNITED               NEWARK   308-607-9855
    ## 1864 1496        HAWAIIAN AIR             HONOLULU   764 645 5740
    ## 1865 1640     VIRGIN ATLANTIC      LONDON HEATHROW   472-337-8838
    ## 1866 2724              UNITED              KAHULUI   791 847 7278
    ## 1867 3131              ALASKA               NEWARK   128 805 3828
    ## 1868 1285      CHINA SOUTHERN                WUHAN   365-832-0674
    ## 1869 2881             EVA AIR               TAIPEI   123-282-3494
    ## 1870  807            AMERICAN          LOS ANGELES   285 424 4318
    ## 1871 3147               DELTA       SALT LAKE CITY   452 352 1387
    ## 1872 2225          AER LINGUS               DUBLIN   129-377-8159
    ## 1873 1935      AIR FRANCE/KLM      PARIS-DE GAULLE   222 143 3131
    ## 1874 1843           SOUTHWEST            SAN DIEGO   162-451-0594
    ## 1875  361           LUFTHANSA            FRANKFURT (239) 325-5321
    ## 1876  454              UNITED     DALLAS-FT. WORTH   436-422-6171
    ## 1877  565              ALASKA              SEATTLE   605 284 4260
    ## 1878  799              ALASKA            BALTIMORE   929-102-5905
    ## 1879 1001           LUFTHANSA               MUNICH   847 507 8268
    ## 1880 2237          AER LINGUS               DUBLIN   452-811-8088
    ## 1881 2841              UNITED       CHICAGO-O'HARE   799 143 1677
    ## 1882 2898                 WOW            REYKJAVIK   196 756 4555
    ## 1883 3110              ALASKA               NEWARK   119-444-0817
    ## 1884 3133               DELTA       SALT LAKE CITY   885 454 0883
    ## 1885  926              ALASKA       RALEIGH-DURHAM   945-998-0444
    ## 1886 1322         UNITED INTL      LONDON HEATHROW (367) 897-7969
    ## 1887 1463              ALASKA         NEW YORK-JFK   163 241 9321
    ## 1888 1917      AIR FRANCE/KLM      PARIS-DE GAULLE   594-176-5811
    ## 1889 1709      AIR FRANCE/KLM      PARIS-DE GAULLE (621) 874-9973
    ## 1890 1927      AIR FRANCE/KLM      PARIS-DE GAULLE   332 963 4103
    ## 1891 1522            INTERJET          GUADALAJARA   389 318 3975
    ## 1892 3210            AMERICAN          LOS ANGELES   894-593-7953
    ## 1893 2737              UNITED              KAHULUI   561-266-7842
    ## 1894 1390         UNITED INTL      PARIS-DE GAULLE   354-958-8052
    ## 1895 1366         UNITED INTL               KANSAI   151 921 2775
    ## 1896 2256              UNITED          BAKERSFIELD   901 140 3759
    ## 1897 2364              UNITED               AUSTIN (426) 342-7378
    ## 1898  433              UNITED             HONOLULU   333-520-4811
    ## 1899 3075              UNITED              SPOKANE (765) 191-1797
    ## 1900  813              ALASKA             PORTLAND   850 914 9348
    ## 1901 1375         UNITED INTL      PARIS-DE GAULLE   246 272 9019
    ## 1902  758              ALASKA         NEW YORK-JFK (400) 250-0871
    ## 1903  434              UNITED         INDIANAPOLIS   504 419 9191
    ## 1904 1689      CATHAY PACIFIC            HONG KONG (434) 725-0561
    ## 1905 2293              QANTAS               SYDNEY   231 863 7554
    ## 1906 1762           LUFTHANSA               MUNICH (969) 207-3261
    ## 1907 1667            EMIRATES                DUBAI   939 253 9048
    ## 1908 2124             JETBLUE           LONG BEACH   879-154-4494
    ## 1909 2685              UNITED       CHICAGO-O'HARE   577 786 2546
    ## 1910 2100             JETBLUE           LONG BEACH (994) 688-3259
    ## 1911  787            AMERICAN          LOS ANGELES   841-717-4447
    ## 1912 2130      CATHAY PACIFIC            HONG KONG   397-353-6309
    ## 1913 1351         UNITED INTL               KANSAI   558 191 4548
    ## 1914 1191           SOUTHWEST            LAS VEGAS   905 768 2297
    ## 1915 1332         UNITED INTL             SHANGHAI   348 522 2051
    ## 1916 2625              UNITED       CHICAGO-O'HARE   307-323-6861
    ## 1917  663         UNITED INTL               MUNICH   663 886 2487
    ## 1918 2478              UNITED               DENVER   274 944 6097
    ## 1919 1512               DELTA          LOS ANGELES (494) 308-3048
    ## 1920 1218           SOUTHWEST            LAS VEGAS   328 307 0875
    ## 1921 1885  SINGAPORE AIRLINES            SINGAPORE   673 524 3504
    ## 1922 2558             JETBLUE           LONG BEACH   934 721 0615
    ## 1923 2121         UNITED INTL          MEXICO CITY   102-957-6486
    ## 1924 2875             EVA AIR               TAIPEI (715) 288-8832
    ## 1925  767          AIR CANADA              TORONTO   794 925 8846
    ## 1926 1826              ALASKA       RALEIGH-DURHAM   637 281 4111
    ## 1927  414              UNITED         HOUSTON-BUSH (594) 448-6242
    ## 1928 1876  SINGAPORE AIRLINES            SINGAPORE   335 802 2651
    ## 1929 2589            INTERJET          GUADALAJARA   560 699 9908
    ## 1930 2658          AEROMEXICO          MEXICO CITY   451 163 0102
    ## 1931 2584             JETBLUE               BOSTON   613 800 0835
    ## 1932 2604            INTERJET          GUADALAJARA   370 453 5800
    ## 1933  723      CATHAY PACIFIC            HONG KONG   192 507 5411
    ## 1934 2185              QANTAS               SYDNEY   182-227-4838
    ## 1935 2516              UNITED       CHICAGO-O'HARE   647 126 2332
    ## 1936 2510              UNITED       CHICAGO-O'HARE   606 125 6957
    ## 1937  300              UNITED            SAN DIEGO   728-404-5558
    ## 1938 2627              UNITED       CHICAGO-O'HARE   506 812 6052
    ## 1939 1734          AIR CANADA              TORONTO   427-665-3475
    ## 1940 2463              UNITED               DENVER   808 739 7162
    ## 1941 2326              QANTAS               SYDNEY   375-978-3305
    ## 1942 2361             JETBLUE       FT. LAUDERDALE   181-708-2089
    ## 1943 2314              UNITED          SAN ANTONIO (802) 810-5574
    ## 1944  509              ALASKA             PORTLAND   242-540-4234
    ## 1945 1417              UNITED          KANSAS CITY   105 116 9695
    ## 1946 1162           SOUTHWEST          LOS ANGELES (867) 891-0871
    ## 1947 2246           SOUTHWEST              PHOENIX   945 144 7892
    ## 1948  875              ALASKA              SEATTLE   531 895 6695
    ## 1949 1296         UNITED INTL      LONDON HEATHROW (533) 213-4368
    ## 1950 2531              UNITED               NEWARK   894 810 2674
    ## 1951 2752      AIR FRANCE/KLM      PARIS-DE GAULLE   943 812 6349
    ## 1952 2166            FRONTIER               DENVER   476 168 4235
    ## 1953 2118         UNITED INTL          MEXICO CITY   931 385 6757
    ## 1954 2711              UNITED        SANTA BARBARA (324) 188-6781
    ## 1955 2722              UNITED       CHICAGO-O'HARE   100 378 8095
    ## 1956 2503              UNITED               NEWARK   459 572 0244
    ## 1957 3088              UNITED              SPOKANE   397-362-5469
    ## 1958 2098             JETBLUE           LONG BEACH (102) 928-7959
    ## 1959 2664          AEROMEXICO          MEXICO CITY (439) 568-6611
    ## 1960 2302            AMERICAN       CHICAGO-O'HARE (767) 205-0604
    ## 1961  713      AIR FRANCE/KLM      PARIS-DE GAULLE (890) 548-9219
    ## 1962 1856              ALASKA              KAHULUI   938 982 5585
    ## 1963 1641     VIRGIN ATLANTIC      LONDON HEATHROW   769-472-2992
    ## 1964 2712           SOUTHWEST               DENVER   190 204 1154
    ## 1965 3199      CATHAY PACIFIC            HONG KONG (649) 925-8489
    ## 1966  764              ALASKA         NEW YORK-JFK   321 616 1013
    ## 1967 3072              UNITED              SPOKANE (156) 837-4491
    ## 1968 1519               DELTA          LOS ANGELES   178-232-0815
    ## 1969 2066            AMERICAN         PHILADELPHIA (882) 304-9032
    ## 1970 2116         UNITED INTL          MEXICO CITY   828 549 6666
    ## 1971 1252         UNITED INTL         TOKYO-NARITA   280 544 4554
    ## 1972 1662            EMIRATES                DUBAI   183 208 5054
    ## 1973 2123             JETBLUE           LONG BEACH (971) 548-6611
    ## 1974 2065              UNITED               NEWARK (828) 153-5819
    ## 1975 2948           SOUTHWEST            SAN DIEGO   203 448 1522
    ## 1976 2705              UNITED        SANTA BARBARA   900-871-9056
    ## 1977  716      CATHAY PACIFIC            HONG KONG   406-167-1379
    ## 1978 3236              ALASKA             PORTLAND (906) 850-9192
    ## 1979 1765            AMERICAN       CHICAGO-O'HARE (859) 777-8245
    ## 1980  533              ALASKA      PUERTO VALLARTA   641-635-8466
    ## 1981 3276           SOUTHWEST               DENVER   807-671-6158
    ## 1982  790         UNITED INTL    SAN JOSE DEL CABO (589) 270-7518
    ## 1983 2105         UNITED INTL          MEXICO CITY   768 529 8051
    ## 1984 1819              ALASKA       RALEIGH-DURHAM   220 660 0306
    ## 1985  504              UNITED              KAHULUI   928-179-7556
    ## 1986 2069            AMERICAN         PHILADELPHIA   153 756 0278
    ## 1987  255          AIR CANADA              TORONTO   273 829 9197
    ## 1988  919          AIR CANADA              TORONTO   269 463 0911
    ## 1989 1712      AIR FRANCE/KLM      PARIS-DE GAULLE   696 984 8826
    ## 1990 3167               DELTA          LOS ANGELES   905-903-5258
    ## 1991 1575         UNITED INTL            HONG KONG   267 332 4709
    ## 1992  708      AIR FRANCE/KLM      PARIS-DE GAULLE (146) 129-5118
    ## 1993 2775               DELTA              ATLANTA (927) 747-9822
    ## 1994  856            AMERICAN     DALLAS-FT. WORTH   534 216 6666
    ## 1995  923              ALASKA       RALEIGH-DURHAM (481) 479-7013
    ## 1996  389              ALASKA         INDIANAPOLIS   971 175 2968
    ## 1997 2940    TURKISH AIRLINES             ISTANBUL (716) 777-3762
    ## 1998  261          AIR CANADA              TORONTO   274-863-3205
    ## 1999 2825              UNITED              BURBANK (217) 589-0596
    ## 2000  281              UNITED            BALTIMORE   928 445 5474
    ## 2001  536              ALASKA      PUERTO VALLARTA   858 990 5153
    ## 2002 2440     AIR NEW ZEALAND             AUCKLAND   731-813-2043
    ## 2003 2839              UNITED       CHICAGO-O'HARE   563-732-6802
    ## 2004 3211              UNITED        SANTA BARBARA   145 725 4021
    ## 2005  793              ALASKA            BALTIMORE   931 311 5801
    ## 2006 2860              UNITED              SEATTLE (637) 782-6989
    ## 2007 2399         UNITED INTL              BEIJING   172 990 3485
    ## 2008 1245       CHINA EASTERN             SHANGHAI   872 325 4341
    ## 2009 2146            FRONTIER               DENVER (359) 803-9809
    ## 2010 2337               DELTA MINNEAPOLIS-ST. PAUL   152 790 8238
    ## 2011 2505              UNITED       CHICAGO-O'HARE   330 561 9257
    ## 2012 2580            INTERJET          GUADALAJARA   437 420 7546
    ## 2013  198            AMERICAN          LOS ANGELES   495 632 4027
    ## 2014 2474              ALASKA       SALT LAKE CITY (416) 788-2844
    ## 2015 2506              ALASKA       SALT LAKE CITY   311-305-4367
    ## 2016 2349              UNITED               AUSTIN   817-400-0481
    ## 2017 2651             JETBLUE               BOSTON   430 723 1079
    ## 2018 3216            AMERICAN          LOS ANGELES (729) 609-4819
    ## 2019 3164            AMERICAN     DALLAS-FT. WORTH (201) 737-4409
    ## 2020 1261       CHINA EASTERN             SHANGHAI (137) 611-3694
    ## 2021 2887                 WOW            REYKJAVIK   226 490 8696
    ## 2022 1528             JETBLUE         NEW YORK-JFK   123 570 8640
    ## 2023  781            AMERICAN          LOS ANGELES   665 803 2453
    ## 2024  348           LUFTHANSA            FRANKFURT (812) 869-6263
    ## 2025 1699      AIR FRANCE/KLM      PARIS-DE GAULLE   639 132 6386
    ## 2026  646              ALASKA WASHINGTON DC-DULLES (194) 198-0504
    ## 2027 2970            AMERICAN            CHARLOTTE   437 886 0753
    ## 2028 3188      CATHAY PACIFIC            HONG KONG   626 756 5089
    ## 2029 1430              UNITED            LAS VEGAS (299) 137-6993
    ## 2030  629              ALASKA WASHINGTON DC-DULLES   714 950 3364
    ## 2031 1568         UNITED INTL            HONG KONG   653-786-5985
    ## 2032 2966            AMERICAN            CHARLOTTE   518 286 5956
    ## 2033 3053 PHILIPPINE AIRLINES               MANILA   194 960 2145
    ## 2034 2270                 WOW            REYKJAVIK   362-136-1153
    ## 2035 3177               DELTA          LOS ANGELES   376-456-0697
    ## 2036 1778              UNITED       RALEIGH-DURHAM   657 832 1189
    ## 2037 2157            FRONTIER               DENVER   962-918-6117
    ## 2038 2784               DELTA              ATLANTA   692 929 3592
    ## 2039 1948              UNITED               BOSTON   805-877-3887
    ## 2040 2050     BRITISH AIRWAYS      LONDON HEATHROW (739) 710-2966
    ## 2041 2650             JETBLUE               BOSTON   819 732 4132
    ## 2042 2170            FRONTIER               DENVER   367 221 9710
    ## 2043 1715      AIR FRANCE/KLM      PARIS-DE GAULLE   361 154 1789
    ## 2044  719      AIR FRANCE/KLM      PARIS-DE GAULLE   680 488 1182
    ## 2045 2921    TURKISH AIRLINES             ISTANBUL   928 638 1186
    ## 2046 3103              ALASKA               NEWARK   588-693-9875
    ## 2047  839              ALASKA              SEATTLE   681-308-7915
    ## 2048 3007            AMERICAN                MIAMI   783 647 8490
    ## 2049  250              UNITED            NASHVILLE   897 847 0632
    ## 2050 1774            AMERICAN       CHICAGO-O'HARE   150 952 4453
    ## 2051  408              UNITED              PHOENIX   322-884-3020
    ## 2052 1630          KOREAN AIR                SEOUL   176-313-5403
    ## 2053 2350               DELTA MINNEAPOLIS-ST. PAUL   487 109 4196
    ## 2054 1517               DELTA          LOS ANGELES (477) 182-4689
    ## 2055 1292             JETBLUE         NEW YORK-JFK   544 382 8289
    ## 2056 2378             JETBLUE       FT. LAUDERDALE   781 543 7456
    ## 2057 1253       CHINA EASTERN             SHANGHAI   911 829 6476
    ## 2058 2037              UNITED               NEWARK (525) 362-5532
    ## 2059 1721          AIR CANADA              TORONTO   517 986 3426
    ## 2060 2087             JETBLUE           LONG BEACH   838 220 5397
    ## 2061 2684              UNITED              ORLANDO (687) 887-6766
    ## 2062  440              UNITED         INDIANAPOLIS   179 163 0902
    ## 2063 2119         UNITED INTL          MEXICO CITY   539 137 8983
    ## 2064 1716      AIR FRANCE/KLM      PARIS-DE GAULLE   733-154-0094
    ## 2065 3215              UNITED        SANTA BARBARA   639 881 3693
    ## 2066 1531              ALASKA          LOS ANGELES   291-830-3017
    ## 2067 2351              UNITED               AUSTIN (637) 100-0509
    ## 2068 3203              UNITED      ONTARIO (CALIF)   750 520 0167
    ## 2069  487              UNITED               DENVER   676 485 8963
    ## 2070 1788              UNITED       RALEIGH-DURHAM   135 566 5090
    ## 2071 1687      CATHAY PACIFIC            HONG KONG   337 260 4996
    ## 2072 1129           SOUTHWEST            SAN DIEGO   371 185 2377
    ## 2073 2618              UNITED       CHICAGO-O'HARE   280 461 1386
    ## 2074 2216           SOUTHWEST              PHOENIX (603) 149-7268
    ## 2075  549              ALASKA              SEATTLE (364) 792-5553
    ## 2076  782         UNITED INTL    SAN JOSE DEL CABO   496-429-1314
    ## 2077 3261              UNITED            SAN DIEGO   459 671 4698
    ## 2078 1425              UNITED            LAS VEGAS   486-268-3312
    ## 2079 1828         UNITED INTL      LONDON HEATHROW   497-518-4050
    ## 2080  967           LUFTHANSA               MUNICH (535) 685-8273
    ## 2081 2665              UNITED       RALEIGH-DURHAM   859 495 4050
    ## 2082 2010              UNITED               NEWARK (826) 738-8316
    ## 2083 1409              UNITED          KANSAS CITY   724-134-3870
    ## 2084 2461              UNITED               DENVER (554) 269-8937
    ## 2085  883              ALASKA              SEATTLE   125-578-4253
    ## 2086 2060            AMERICAN         PHILADELPHIA   614 800 2861
    ## 2087 2090             JETBLUE           LONG BEACH   487-232-4449
    ## 2088  979           LUFTHANSA               MUNICH (298) 135-0900
    ## 2089  854              ALASKA             PORTLAND (392) 183-7831
    ## 2090 1637          KOREAN AIR                SEOUL (606) 596-1029
    ## 2091 3239            AMERICAN              PHOENIX (384) 953-4795
    ## 2092  816              ALASKA            BALTIMORE (855) 811-8811
    ## 2093 1154           SOUTHWEST          LOS ANGELES   253-374-7102
    ## 2094 1145           SOUTHWEST            SAN DIEGO (419) 295-9580
    ## 2095 3179               DELTA          LOS ANGELES   802 102 8345
    ## 2096 1542               DELTA         NEW YORK-JFK   417 393 0050
    ## 2097  853            AMERICAN     DALLAS-FT. WORTH   787-624-8443
    ## 2098 2075            AMERICAN         PHILADELPHIA (919) 486-4251
    ## 2099  877              ALASKA              SEATTLE   341 824 5322
    ## 2100 2856              UNITED              SEATTLE   415 551 1608
    ## 2101 1993              UNITED            BALTIMORE (392) 495-7961
    ## 2102 2081             JETBLUE           LONG BEACH   473-238-3324
    ## 2103 2598              UNITED       CHICAGO-O'HARE (506) 760-3043
    ## 2104  590              ALASKA WASHINGTON DC-DULLES   876-834-0624
    ## 2105 3056            AMERICAN                MIAMI   919 611 6170
    ## 2106 2832              UNITED              BURBANK (146) 699-3488
    ## 2107 3153              UNITED            SAN DIEGO   261 434 7760
    ## 2108  396              ALASKA         INDIANAPOLIS   617 310 2684
    ## 2109 2656              UNITED       RALEIGH-DURHAM   182-535-3412
    ## 2110  460              UNITED     DALLAS-FT. WORTH   506 129 1694
    ## 2111  679              UNITED               NEWARK (302) 339-0791
    ## 2112 1772            AMERICAN       CHICAGO-O'HARE (446) 229-4342
    ## 2113 2052              UNITED               NEWARK (249) 602-6985
    ## 2114 2821               DELTA          LOS ANGELES (150) 905-6938
    ## 2115 3223            AMERICAN          LOS ANGELES   313 990 8823
    ## 2116 3130               DELTA       SALT LAKE CITY   656-941-5355
    ## 2117 2502              UNITED       CHICAGO-O'HARE (116) 689-6617
    ## 2118  637              ALASKA          NEW ORLEANS   955 324 5981
    ## 2119 2826              UNITED              BURBANK   175 808 2189
    ## 2120 1084          AIR CANADA              CALGARY (896) 993-8555
    ## 2121 2947           SOUTHWEST          LOS ANGELES   105-687-6500
    ## 2122 3160               DELTA          LOS ANGELES   757 524 2964
    ## 2123 2865              UNITED WASHINGTON DC-DULLES   201 374 2424
    ## 2124  955          AIR CANADA            VANCOUVER (466) 912-8401
    ## 2125 2815               DELTA          LOS ANGELES   766 112 6143
    ## 2126 2955           SOUTHWEST          LOS ANGELES   783-463-4865
    ## 2127 1593           SOUTHWEST          LOS ANGELES   853-803-9900
    ## 2128 2257              UNITED          LOS ANGELES (347) 851-5388
    ## 2129 2373             JETBLUE       FT. LAUDERDALE   992 114 6973
    ## 2130 2823              UNITED              BURBANK   316-212-7309
    ## 2131 3034            AMERICAN     DALLAS-FT. WORTH   301 672 1092
    ## 2132 1989              UNITED            BALTIMORE   795 137 0201
    ## 2133 3271              UNITED            LAS VEGAS   381-883-5497
    ## 2134 2770              UNITED               AUSTIN   100-531-4642
    ## 2135 1344         UNITED INTL             SHANGHAI   994 923 6634
    ## 2136  309              UNITED            SAN DIEGO   920 355 8404
    ## 2137  801              ALASKA            BALTIMORE (441) 445-6532
    ## 2138  312              UNITED            SAN DIEGO   325 795 2455
    ## 2139 2167            FRONTIER               DENVER   593 829 6250
    ## 2140  447              UNITED         INDIANAPOLIS   566-482-9004
    ## 2141 2854              UNITED              SEATTLE   542 537 6770
    ## 2142  676              UNITED               NEWARK   716 191 1741
    ## 2143 2901               DELTA       SALT LAKE CITY   491-727-7162
    ## 2144 1450              UNITED            LAS VEGAS   167-336-5660
    ## 2145 2335            AMERICAN       CHICAGO-O'HARE   358 831 0725
    ## 2146 2475              UNITED               DENVER   432 979 7292
    ## 2147 2581             JETBLUE               BOSTON (205) 382-5599
    ## 2148 2760               DELTA              ATLANTA (208) 794-9612
    ## 2149 2822               DELTA          LOS ANGELES   728 662 3934
    ## 2150 1894           LUFTHANSA               MUNICH   380-918-8572
    ## 2151 1323         UNITED INTL      LONDON HEATHROW (905) 742-3525
    ## 2152 1781              UNITED       RALEIGH-DURHAM   151 434 6989
    ## 2153 2750               DELTA              ATLANTA   755 544 2629
    ## 2154 2833              UNITED              BURBANK   633 181 4494
    ## 2155  273              UNITED          LOS ANGELES   346 706 5964
    ## 2156 2828              UNITED              BURBANK   688 690 2184
    ## 2157  329           SOUTHWEST              PHOENIX   618 717 1697
    ## 2158 1061          AIR CANADA              CALGARY   185-321-6877
    ## 2159   13           SOUTHWEST          LOS ANGELES (147) 535-3529
    ## 2160 2576             JETBLUE           LONG BEACH (152) 912-4118
    ## 2161 2261              UNITED          LOS ANGELES   726 943 7486
    ## 2162 2528             JETBLUE           LONG BEACH (634) 521-4714
    ## 2163 1151           SOUTHWEST          LOS ANGELES   670-248-0186
    ## 2164 2532             JETBLUE           LONG BEACH (121) 509-7306
    ## 2165 3128              UNITED            SAN DIEGO (105) 635-5212
    ## 2166   14           SOUTHWEST          LOS ANGELES (732) 168-0110
    ## 2167  543            EMIRATES                DUBAI   364 834 3150
    ## 2168  405              UNITED         HOUSTON-BUSH   176 508 2778
    ## 2169  634              ALASKA               NEWARK   120 941 0833
    ## 2170 1174               DELTA         NEW YORK-JFK   670 902 3199
    ## 2171 1537               DELTA         NEW YORK-JFK (214) 250-8756
    ## 2172 2304            AMERICAN       CHICAGO-O'HARE (119) 975-8484
    ## 2173 2709              UNITED        SANTA BARBARA   297 484 3285
    ## 2174  430              UNITED         INDIANAPOLIS (489) 534-6272
    ## 2175  800            AMERICAN          LOS ANGELES (610) 716-5732
    ## 2176  871              ALASKA              SEATTLE (456) 925-4236
    ## 2177 1544               DELTA         NEW YORK-JFK   743-103-7645
    ## 2178 2056              UNITED               NEWARK   432-281-3682
    ## 2179 2300            AMERICAN       CHICAGO-O'HARE (167) 144-9470
    ## 2180 2459              UNITED               DENVER   648 685 6188
    ## 2181 2467              UNITED               DENVER   548 191 4898
    ## 2182 3026              UNITED             PORTLAND   288 110 9483
    ## 2183 3247            AMERICAN              PHOENIX   946-558-5801
    ## 2184 3249            AMERICAN              PHOENIX   388 744 9637
    ## 2185  364           LUFTHANSA            FRANKFURT   506 463 9129
    ## 2186 1913      AIR FRANCE/KLM      PARIS-DE GAULLE (848) 149-5208
    ## 2187 2423     AIR NEW ZEALAND             AUCKLAND (970) 908-2298
    ## 2188 2430              UNITED             HONOLULU (843) 120-5653
    ## 2189 1385         UNITED INTL             SHANGHAI   306 394 8640
    ## 2190  759          AIR CANADA              TORONTO   170-641-3537
    ## 2191 1879              UNITED               BOSTON   860 723 5066
    ## 2192  597              ALASKA WASHINGTON DC-DULLES   814-895-6610
    ## 2193 1726          AIR CANADA              TORONTO (139) 727-9901
    ## 2194  583              ALASKA              SEATTLE   598 735 8557
    ## 2195  994           LUFTHANSA               MUNICH   593 895 6761
    ## 2196 3003            AMERICAN                MIAMI (817) 824-3849
    ## 2197 2989            AMERICAN                MIAMI   508 484 9738
    ## 2198 1474              ALASKA         NEW YORK-JFK   719 489 4724
    ## 2199 2917               DELTA              SEATTLE   503-671-4901
    ## 2200 1970      CATHAY PACIFIC            HONG KONG   275 649 8183
    ## 2201  553              ALASKA              SEATTLE   968 130 7012
    ## 2202 1627          KOREAN AIR                SEOUL   290 367 6676
    ## 2203 1650     VIRGIN ATLANTIC      LONDON HEATHROW   499 766 9941
    ## 2204 3006            AMERICAN                MIAMI   538-393-2243
    ## 2205 1493            FRONTIER               DENVER (540) 362-7136
    ## 2206 1592           SOUTHWEST          LOS ANGELES (802) 910-1742
    ## 2207  989           LUFTHANSA               MUNICH   845 544 4748
    ## 2208 1357         UNITED INTL             SHANGHAI   784-458-8425
    ## 2209 1873              UNITED               BOSTON (365) 217-0634
    ## 2210  755          AIR CANADA              TORONTO   708 500 2758
    ## 2211 2853              UNITED              SEATTLE (594) 797-7729
    ## 2212  349              ALASKA            SAN DIEGO   982 555 9504
    ## 2213 2174            FRONTIER               DENVER   477-307-3338
    ## 2214 3050            AMERICAN          LOS ANGELES   744-301-1148
    ## 2215 2577            INTERJET          GUADALAJARA   389 484 8888
    ## 2216 1745          AIR CANADA              TORONTO   739 303 6128
    ## 2217 2103             JETBLUE           LONG BEACH   175 905 9962
    ## 2218 2774                 WOW            REYKJAVIK (900) 462-1379
    ## 2219  379              ALASKA    SAN JOSE DEL CABO   878-636-2294
    ## 2220  455              ALASKA            LAS VEGAS (998) 692-1900
    ## 2221 2279              UNITED          LOS ANGELES   994 421 8642
    ## 2222 2637              UNITED       CHICAGO-O'HARE   304-225-5895
    ## 2223 2779                 WOW            REYKJAVIK   931-522-5498
    ## 2224 3134              ALASKA               NEWARK   838 898 2275
    ## 2225 2223          AER LINGUS               DUBLIN   718 187 7125
    ## 2226 2382         UNITED INTL              BEIJING   943 561 8955
    ## 2227 1059          AIR CANADA              CALGARY   341 261 6456
    ## 2228  884              ALASKA              SEATTLE (507) 483-3618
    ## 2229 1066          AIR CANADA              CALGARY   826 266 0205
    ## 2230 1793              UNITED       RALEIGH-DURHAM (380) 449-7849
    ## 2231 2273                 WOW            REYKJAVIK   589-975-0198
    ## 2232 2886              UNITED               AUSTIN   657 680 8781
    ## 2233 3117              UNITED              SPOKANE   501-668-7869
    ## 2234 2414     AIR NEW ZEALAND             AUCKLAND   224 365 8299
    ## 2235  534            EMIRATES                DUBAI   747 588 1968
    ## 2236  411              UNITED              PHOENIX (706) 836-7047
    ## 2237 1665            EMIRATES                DUBAI (698) 462-6742
    ## 2238  948          AIR CANADA            VANCOUVER   748 446 1257
    ## 2239 2484              UNITED       CHICAGO-O'HARE   525-552-4162
    ## 2240 1446              UNITED            LAS VEGAS   635-714-8302
    ## 2241 2008              UNITED               NEWARK   114 668 2834
    ## 2242 2729      AIR FRANCE/KLM      PARIS-DE GAULLE   404 788 2855
    ## 2243  833              ALASKA            NASHVILLE   673 292 2444
    ## 2244 2072             JETBLUE           LONG BEACH   136 788 1426
    ## 2245 2396         UNITED INTL               SYDNEY   564 677 3934
    ## 2246 1397         UNITED INTL      PARIS-DE GAULLE   213 981 7762
    ## 2247 1991              UNITED            BALTIMORE   453-556-0852
    ## 2248 3205      CATHAY PACIFIC            HONG KONG   423 593 4483
    ## 2249 2078            AMERICAN         PHILADELPHIA   820 609 7454
    ## 2250 1324         UNITED INTL      LONDON HEATHROW   131-641-1331
    ## 2251 2933    TURKISH AIRLINES             ISTANBUL   824 540 9579
    ## 2252 1638          KOREAN AIR                SEOUL (753) 726-0123
    ## 2253 3227            AMERICAN              PHOENIX   739 737 9041
    ## 2254  640              ALASKA          NEW ORLEANS   609-332-7370
    ## 2255 2197          AER LINGUS               DUBLIN   426-182-1365
    ## 2256  397              ALASKA          LOS ANGELES (347) 782-5787
    ## 2257  864            AMERICAN     DALLAS-FT. WORTH   898-210-6218
    ## 2258  324           SOUTHWEST          LOS ANGELES   938 394 0411
    ## 2259  730      AIR FRANCE/KLM      PARIS-DE GAULLE   904 844 1759
    ## 2260 1863              ALASKA              KAHULUI   658-861-4306
    ## 2261 1677      AIR FRANCE/KLM      PARIS-DE GAULLE (716) 184-1232
    ## 2262  896              ALASKA       RALEIGH-DURHAM   380-105-1757
    ## 2263 1384         UNITED INTL      PARIS-DE GAULLE (969) 555-0453
    ## 2264 2094            AMERICAN         PHILADELPHIA   249 452 4370
    ## 2265 2910    TURKISH AIRLINES             ISTANBUL   443 384 8253
    ## 2266 1882  SINGAPORE AIRLINES            SINGAPORE   711 289 2247
    ## 2267 1023          AIR CANADA            VANCOUVER   343-973-0193
    ## 2268  336              ALASKA            SAN DIEGO   367 650 3720
    ## 2269 1242         UNITED INTL         TOKYO-NARITA (265) 286-5671
    ## 2270 2164            FRONTIER               DENVER   148 630 8560
    ## 2271 3186               DELTA          LOS ANGELES   413-727-2672
    ## 2272 1807         UNITED INTL      LONDON HEATHROW (162) 332-5838
    ## 2273 1505        HAWAIIAN AIR             HONOLULU (229) 604-7790
    ## 2274  367              ALASKA    SAN JOSE DEL CABO (892) 301-0333
    ## 2275  311              UNITED            SAN DIEGO   148 501 5084
    ## 2276 1119           SOUTHWEST            SAN DIEGO   564 780 8272
    ## 2277 1834              ALASKA       RALEIGH-DURHAM   524 190 0899
    ## 2278  897              ALASKA       RALEIGH-DURHAM   463 792 2782
    ## 2279 1286         UNITED INTL      LONDON HEATHROW (331) 472-8624
    ## 2280 1449              UNITED            LAS VEGAS   522-286-5318
    ## 2281 1694              ALASKA            SANTA ANA   310 719 4550
    ## 2282 2710      AIR FRANCE/KLM      PARIS-DE GAULLE   314-360-4288
    ## 2283  317              UNITED            SAN DIEGO (301) 534-5754
    ## 2284 1955      AIR FRANCE/KLM      PARIS-DE GAULLE   822 271 5719
    ## 2285 1886  SINGAPORE AIRLINES            SINGAPORE (341) 473-0639
    ## 2286 2534             JETBLUE               BOSTON (835) 882-3693
    ## 2287 1953      AIR FRANCE/KLM      PARIS-DE GAULLE   465-550-6610
    ## 2288 1820         UNITED INTL      LONDON HEATHROW   388 100 1482
    ## 2289 2954           SOUTHWEST          LOS ANGELES (589) 194-0523
    ## 2290 2527             JETBLUE               BOSTON   982 842 4913
    ## 2291 2681           SOUTHWEST               DENVER   671 913 4563
    ## 2292 1621     VIRGIN ATLANTIC      LONDON HEATHROW   144 468 5864
    ## 2293 1749           LUFTHANSA               MUNICH   194 344 4039
    ## 2294 1755           LUFTHANSA               MUNICH   331 747 5714
    ## 2295 2648             JETBLUE               BOSTON   221-190-1449
    ## 2296 1959              UNITED               BOSTON (322) 843-0185
    ## 2297 2706      AIR FRANCE/KLM      PARIS-DE GAULLE   676-614-9095
    ## 2298 1553           AIR CHINA              BEIJING (190) 975-2514
    ## 2299 2694              UNITED       CHICAGO-O'HARE (909) 382-3774
    ## 2300  269          AIR CANADA              TORONTO   956 257 9319
    ## 2301  321           SOUTHWEST          LOS ANGELES   362 145 8268
    ## 2302 2890                 WOW            REYKJAVIK   570 727 3998
    ## 2303 1513               DELTA          LOS ANGELES   225 964 9193
    ## 2304 1682      CATHAY PACIFIC            HONG KONG   227 419 9482
    ## 2305 2609             JETBLUE               BOSTON   949 360 7605
    ## 2306    2           SOUTHWEST            SAN DIEGO (347) 896-3463
    ## 2307 1484            FRONTIER               DENVER   794 939 9735
    ## 2308 1598  SINGAPORE AIRLINES            SINGAPORE   413 754 3034
    ## 2309 1188           SOUTHWEST            LAS VEGAS   806 730 0459
    ## 2310 1971              UNITED            BALTIMORE   791 195 8909
    ## 2311 1236           SOUTHWEST            LAS VEGAS   691-318-3535
    ## 2312 1615     VIRGIN ATLANTIC      LONDON HEATHROW   852-386-6029
    ## 2313 2371             JETBLUE       FT. LAUDERDALE   380 682 7795
    ## 2314  868            AMERICAN     DALLAS-FT. WORTH (355) 550-1392
    ## 2315  595              ALASKA WASHINGTON DC-DULLES   247 586 4579
    ## 2316  572              ALASKA              SEATTLE   235 257 1041
    ## 2317 1413              UNITED            LAS VEGAS (705) 456-1905
    ## 2318 1591         UNITED INTL            HONG KONG   106 756 2785
    ## 2319 3266              UNITED            LAS VEGAS (836) 207-8419
    ## 2320 2492              UNITED               DENVER (306) 552-1875
    ## 2321 1931      AIR FRANCE/KLM      PARIS-DE GAULLE   729-102-7511
    ## 2322  496              ALASKA             PORTLAND   795 583 0958
    ## 2323  951          AIR CANADA            VANCOUVER   700-431-3918
    ## 2324  720      CATHAY PACIFIC            HONG KONG   929 632 1068
    ## 2325 1945              UNITED               BOSTON   763 906 2495
    ## 2326  410              UNITED         HOUSTON-BUSH   469 976 6796
    ## 2327  638              ALASKA WASHINGTON DC-DULLES   887-657-4143
    ## 2328  197            AMERICAN          LOS ANGELES   574-438-5329
    ## 2329 2606             JETBLUE               BOSTON   319 127 9518
    ## 2330 2727              UNITED              KAHULUI   429 960 9710
    ## 2331 2866              UNITED WASHINGTON DC-DULLES   419 646 0299
    ## 2332  373              ALASKA    SAN JOSE DEL CABO          38515
    ## 2333  574              ALASKA              SEATTLE   521 336 8581
    ## 2334  630              ALASKA               NEWARK   776 367 6109
    ## 2335 1150           SOUTHWEST            SAN DIEGO   470 367 1392
    ## 2336 1172               DELTA         NEW YORK-JFK (944) 189-7555
    ## 2337 1232           SOUTHWEST            LAS VEGAS   998-931-4783
    ## 2338 1260       CHINA EASTERN             SHANGHAI   362-178-6307
    ## 2339 1401         UNITED INTL      PARIS-DE GAULLE (458) 404-9558
    ## 2340 1444              UNITED            LAS VEGAS   212 286 7936
    ## 2341 1703      AIR FRANCE/KLM      PARIS-DE GAULLE (481) 522-1039
    ## 2342 1767            AMERICAN       CHICAGO-O'HARE (376) 611-4588
    ## 2343 1780              UNITED       RALEIGH-DURHAM   936 193 9690
    ## 2344 2019              UNITED               NEWARK (641) 544-6549
    ## 2345 2054              UNITED               NEWARK   797-870-7818
    ## 2346 2125         UNITED INTL          MEXICO CITY   693 907 5353
    ## 2347 2583             JETBLUE           LONG BEACH   332 973 4943
    ## 2348 2601             JETBLUE               BOSTON   929 622 9077
    ## 2349 2642             JETBLUE         NEW YORK-JFK   649-379-5361
    ## 2350 2695      AIR FRANCE/KLM      PARIS-DE GAULLE (572) 748-6932
    ## 2351 2960            AMERICAN         NEW YORK-JFK   395-892-5646
    ## 2352 2997            AMERICAN                MIAMI   221-628-9561
    ## 2353 3001            AMERICAN                MIAMI (227) 801-6148
    ## 2354 3049            AMERICAN          LOS ANGELES   549-649-1864
    ## 2355 1724          AIR CANADA              TORONTO   342 941 0439
    ## 2356  831              ALASKA            NASHVILLE   701 390 9814
    ## 2357 2816               DELTA          LOS ANGELES (519) 573-6576
    ## 2358 2855              UNITED              SEATTLE   919-342-0230
    ## 2359 1211           SOUTHWEST            LAS VEGAS   364-759-2705
    ## 2360 3054            AMERICAN                MIAMI   949 543 7906
    ## 2361 1227           SOUTHWEST            LAS VEGAS   942 732 6403
    ## 2362 1506        HAWAIIAN AIR             HONOLULU (900) 586-1787
    ## 2363 2443             JETBLUE               BOSTON   308-607-9855
    ## 2364 1754           LUFTHANSA               MUNICH   764 645 5740
    ## 2365  345              ALASKA            SAN DIEGO   472-337-8838
    ## 2366  552              ALASKA              SEATTLE   791 847 7278
    ## 2367 1681      CATHAY PACIFIC            HONG KONG   128 805 3828
    ## 2368 3029            AMERICAN     DALLAS-FT. WORTH   365-832-0674
    ## 2369 1435              UNITED             PORTLAND   123-282-3494
    ## 2370  746              ALASKA         NEW YORK-JFK   285 424 4318
    ## 2371 1824         UNITED INTL      LONDON HEATHROW   452 352 1387
    ## 2372 2477              UNITED             HONOLULU   129-377-8159
    ## 2373 2807               DELTA          LOS ANGELES   222 143 3131
    ## 2374 1616     VIRGIN ATLANTIC      LONDON HEATHROW   162-451-0594
    ## 2375 3196               DELTA          LOS ANGELES (239) 325-5321
    ## 2376  563              ALASKA              SEATTLE   436-422-6171
    ## 2377 2575              UNITED       CHICAGO-O'HARE   605 284 4260
    ## 2378  457              ALASKA            LAS VEGAS   929-102-5905
    ## 2379 2741              UNITED              KAHULUI   847 507 8268
    ## 2380  972           LUFTHANSA               MUNICH   452-811-8088
    ## 2381 2446              UNITED             HONOLULU   799 143 1677
    ## 2382  747              ALASKA         NEW YORK-JFK   196 756 4555
    ## 2383  354           LUFTHANSA            FRANKFURT   119-444-0817
    ## 2384  678      AIR FRANCE/KLM      PARIS-DE GAULLE   885 454 0883
    ## 2385 1301             JETBLUE         NEW YORK-JFK   945-998-0444
    ## 2386 1608     VIRGIN ATLANTIC      LONDON HEATHROW (367) 897-7969
    ## 2387 1622     VIRGIN ATLANTIC      LONDON HEATHROW   163 241 9321
    ## 2388 1878  SINGAPORE AIRLINES            SINGAPORE   594-176-5811
    ## 2389 2427     AIR NEW ZEALAND             AUCKLAND (621) 874-9973
    ## 2390 3176               DELTA          LOS ANGELES   332 963 4103
    ## 2391  386              ALASKA          LOS ANGELES   389 318 3975
    ## 2392  398              ALASKA         INDIANAPOLIS   894-593-7953
    ## 2393  675              UNITED               NEWARK   561-266-7842
    ## 2394  944          AIR CANADA            VANCOUVER   354-958-8052
    ## 2395 1284         UNITED INTL      LONDON HEATHROW   151 921 2775
    ## 2396 1307         UNITED INTL      LONDON HEATHROW   901 140 3759
    ## 2397 1407              UNITED            LAS VEGAS (426) 342-7378
    ## 2398 1466              ALASKA         NEW YORK-JFK   333-520-4811
    ## 2399 1610     VIRGIN ATLANTIC      LONDON HEATHROW (765) 191-1797
    ## 2400 1654            EMIRATES                DUBAI   850 914 9348
    ## 2401 1683      CATHAY PACIFIC            HONG KONG   246 272 9019
    ## 2402 1800         UNITED INTL      LONDON HEATHROW (400) 250-0871
    ## 2403 1806              ALASKA       RALEIGH-DURHAM   504 419 9191
    ## 2404 1923      AIR FRANCE/KLM      PARIS-DE GAULLE (434) 725-0561
    ## 2405 1954      AIR FRANCE/KLM      PARIS-DE GAULLE   231 863 7554
    ## 2406 2173            FRONTIER               DENVER (969) 207-3261
    ## 2407 2275                 WOW            REYKJAVIK   939 253 9048
    ## 2408 2397         UNITED INTL              BEIJING   879-154-4494
    ## 2409 2498              UNITED               NEWARK   577 786 2546
    ## 2410 2600            INTERJET          GUADALAJARA (994) 688-3259
    ## 2411 2607            INTERJET          GUADALAJARA   841-717-4447
    ## 2412 2793               DELTA       SALT LAKE CITY   397-353-6309
    ## 2413 2814               DELTA          LOS ANGELES   558 191 4548
    ## 2414 2897               DELTA       SALT LAKE CITY   905 768 2297
    ## 2415 2996            AMERICAN                MIAMI   348 522 2051
    ## 2416 3111                COPA          PANAMA CITY   307-323-6861
    ## 2417 3145               DELTA       SALT LAKE CITY   663 886 2487
    ## 2418 3228              ALASKA             PORTLAND   274 944 6097
    ## 2419 1623     VIRGIN ATLANTIC      LONDON HEATHROW (494) 308-3048
    ## 2420 2610            INTERJET          GUADALAJARA   328 307 0875
    ## 2421 2126         UNITED INTL          MEXICO CITY   673 524 3504
    ## 2422  540              ALASKA      PUERTO VALLARTA   934 721 0615
    ## 2423 1901           LUFTHANSA               MUNICH   102-957-6486
    ## 2424 3084                COPA          PANAMA CITY (715) 288-8832
    ## 2425  828              ALASKA            NASHVILLE   794 925 8846
    ## 2426 2417     AIR NEW ZEALAND             AUCKLAND   637 281 4111
    ## 2427 2899               DELTA       SALT LAKE CITY (594) 448-6242
    ## 2428 3135              UNITED            SAN DIEGO   335 802 2651
    ## 2429  421              UNITED             HONOLULU   560 699 9908
    ## 2430  772         UNITED INTL    SAN JOSE DEL CABO   451 163 0102
    ## 2431  442              UNITED             HONOLULU   613 800 0835
    ## 2432  760              ALASKA         NEW YORK-JFK   370 453 5800
    ## 2433 1354         UNITED INTL               KANSAI   192 507 5411
    ## 2434 2435              UNITED             HONOLULU   182-227-4838
    ## 2435 2259              UNITED          LOS ANGELES   647 126 2332
    ## 2436  393              ALASKA         INDIANAPOLIS   606 125 6957
    ## 2437 1419              UNITED          KANSAS CITY   728-404-5558
    ## 2438 2398         UNITED INTL               SYDNEY   506 812 6052
    ## 2439 3233              ALASKA             PORTLAND   427-665-3475
    ## 2440 2043              UNITED               NEWARK   808 739 7162
    ## 2441 2438              UNITED             HONOLULU   375-978-3305
    ## 2442 1338         UNITED INTL               KANSAI   181-708-2089
    ## 2443  463              ALASKA            LAS VEGAS (802) 810-5574
    ## 2444 2107         UNITED INTL          MEXICO CITY   242-540-4234
    ## 2445 2753               DELTA              ATLANTA   105 116 9695
    ## 2446 2309              UNITED          SAN ANTONIO (867) 891-0871
    ## 2447 2730              UNITED              KAHULUI   945 144 7892
    ## 2448 2744              UNITED              KAHULUI   531 895 6695
    ## 2449 2864              UNITED              SEATTLE (533) 213-4368
    ## 2450  495              ALASKA             PORTLAND   894 810 2674
    ## 2451 2433              UNITED             HONOLULU   943 812 6349
    ## 2452 2678              UNITED              ORLANDO   476 168 4235
    ## 2453 2714              UNITED              KAHULUI   931 385 6757
    ## 2454 2593              UNITED       CHICAGO-O'HARE (324) 188-6781
    ## 2455  449              UNITED         INDIANAPOLIS   100 378 8095
    ## 2456  596             JETBLUE               BOSTON   459 572 0244
    ## 2457 1117               DELTA MINNEAPOLIS-ST. PAUL   397-362-5469
    ## 2458 1847           SOUTHWEST            SAN DIEGO (102) 928-7959
    ## 2459 2141            FRONTIER               DENVER (439) 568-6611
    ## 2460  298              UNITED            SAN DIEGO (767) 205-0604
    ## 2461 2603             JETBLUE               BOSTON (890) 548-9219
    ## 2462 2384         UNITED INTL              BEIJING   938 982 5585
    ## 2463 1832              ALASKA       RALEIGH-DURHAM   769-472-2992
    ## 2464 1299         UNITED INTL      LONDON HEATHROW   190 204 1154
    ## 2465 2040              UNITED               NEWARK (649) 925-8489
    ## 2466 3014              UNITED             PORTLAND   321 616 1013
    ## 2467 2862              UNITED              SEATTLE (156) 837-4491
    ## 2468  436              UNITED         INDIANAPOLIS   178-232-0815
    ## 2469  415              UNITED              PHOENIX (882) 304-9032
    ## 2470 3292           SOUTHWEST               DENVER   828 549 6666
    ## 2471 2549             JETBLUE           LONG BEACH   280 544 4554
    ## 2472 1349         UNITED INTL               KANSAI   183 208 5054
    ## 2473 3248            AMERICAN              PHOENIX (971) 548-6611
    ## 2474  715      CATHAY PACIFIC            HONG KONG (828) 153-5819
    ## 2475 2759      AIR FRANCE/KLM      PARIS-DE GAULLE   203 448 1522
    ## 2476 3123              ALASKA               NEWARK   900-871-9056
    ## 2477 2547             JETBLUE           LONG BEACH   406-167-1379
    ## 2478 2574             JETBLUE           LONG BEACH (906) 850-9192
    ## 2479 3108                COPA          PANAMA CITY (859) 777-8245
    ## 2480 1177               DELTA         NEW YORK-JFK   641-635-8466
    ## 2481 1805         UNITED INTL      LONDON HEATHROW   807-671-6158
    ## 2482  500              ALASKA             PORTLAND (589) 270-7518
    ## 2483 1215           SOUTHWEST            LAS VEGAS   768 529 8051
    ## 2484 1092               DELTA MINNEAPOLIS-ST. PAUL   220 660 0306
    ## 2485  335           SOUTHWEST              PHOENIX   928-179-7556
    ## 2486 1469              ALASKA         NEW YORK-JFK   153 756 0278
    ## 2487 2745      AIR FRANCE/KLM      PARIS-DE GAULLE   273 829 9197
    ## 2488 3224              ALASKA             PORTLAND   269 463 0911
    ## 2489 2004              UNITED               NEWARK   696 984 8826
    ## 2490  943          AIR CANADA            VANCOUVER   905-903-5258
    ## 2491 1573               DELTA              DETROIT   267 332 4709
    ## 2492 1295             JETBLUE         NEW YORK-JFK (146) 129-5118
    ## 2493  736      CATHAY PACIFIC            HONG KONG (927) 747-9822
    ## 2494 1305         UNITED INTL      LONDON HEATHROW   534 216 6666
    ## 2495 1482            FRONTIER               DENVER (481) 479-7013
    ## 2496 2973            AMERICAN            CHARLOTTE   971 175 2968
    ## 2497  392              ALASKA          LOS ANGELES (716) 777-3762
    ## 2498 1160           SOUTHWEST          LOS ANGELES   274-863-3205
    ## 2499 2594            INTERJET          GUADALAJARA (217) 589-0596
    ## 2500 3063 PHILIPPINE AIRLINES               MANILA   928 445 5474
    ## 2501 3243            AMERICAN              PHOENIX   858 990 5153
    ## 2502   10           SOUTHWEST          LOS ANGELES   731-813-2043
    ## 2503  513              ALASKA             PORTLAND   563-732-6802
    ## 2504  610             JETBLUE               BOSTON   145 725 4021
    ## 2505 1017          AIR CANADA            VANCOUVER   931 311 5801
    ## 2506 1131               DELTA MINNEAPOLIS-ST. PAUL (637) 782-6989
    ## 2507 2233          AER LINGUS               DUBLIN   172 990 3485
    ## 2508  391              ALASKA         INDIANAPOLIS   872 325 4341
    ## 2509  641              ALASKA WASHINGTON DC-DULLES (359) 803-9809
    ## 2510 2394         UNITED INTL              BEIJING   152 790 8238
    ## 2511  560            EMIRATES                DUBAI   330 561 9257
    ## 2512 1977      CATHAY PACIFIC            HONG KONG   437 420 7546
    ## 2513 1200           SOUTHWEST            LAS VEGAS   495 632 4027
    ## 2514 2083             JETBLUE           LONG BEACH (416) 788-2844
    ## 2515 2316              UNITED          SAN ANTONIO   311-305-4367
    ## 2516 2692      AIR FRANCE/KLM      PARIS-DE GAULLE   817-400-0481
    ## 2517 3048 PHILIPPINE AIRLINES               MANILA   430 723 1079
    ## 2518  580              ALASKA              SEATTLE (729) 609-4819
    ## 2519  859            AMERICAN     DALLAS-FT. WORTH (201) 737-4409
    ## 2520 1578               DELTA              DETROIT (137) 611-3694
    ## 2521  420              UNITED         INDIANAPOLIS   226 490 8696
    ## 2522  652              ALASKA WASHINGTON DC-DULLES   123 570 8640
    ## 2523 1490            FRONTIER               DENVER   665 803 2453
    ## 2524 2715               DELTA          LOS ANGELES (812) 869-6263
    ## 2525 3136              ALASKA               NEWARK   639 132 6386
    ## 2526  749              ALASKA         NEW YORK-JFK (194) 198-0504
    ## 2527  256          AIR CANADA              TORONTO   437 886 0753
    ## 2528 1479             AVIANCA         SAN SALVADOR   626 756 5089
    ## 2529 3171               DELTA          LOS ANGELES (299) 137-6993
    ## 2530 1448              UNITED             PORTLAND   714 950 3364
    ## 2531 1784              UNITED       RALEIGH-DURHAM   653-786-5985
    ## 2532 1481             AVIANCA         SAN SALVADOR   518 286 5956
    ## 2533  795              ALASKA            BALTIMORE   194 960 2145
    ## 2534 1148           SOUTHWEST          LOS ANGELES   362-136-1153
    ## 2535 3037            AMERICAN     DALLAS-FT. WORTH   376-456-0697
    ## 2536 1477             AVIANCA         SAN SALVADOR   657 832 1189
    ## 2537 3278           SOUTHWEST               DENVER   962-918-6117
    ## 2538  721      CATHAY PACIFIC            HONG KONG   692 929 3592
    ## 2539 2147      CATHAY PACIFIC            HONG KONG   805-877-3887
    ## 2540 1105               DELTA MINNEAPOLIS-ST. PAUL (739) 710-2966
    ## 2541 3175            AMERICAN     DALLAS-FT. WORTH   819 732 4132
    ## 2542 1792              UNITED       RALEIGH-DURHAM   367 221 9710
    ## 2543 2339               DELTA MINNEAPOLIS-ST. PAUL   361 154 1789
    ## 2544  803              ALASKA            BALTIMORE   680 488 1182
    ## 2545  876            AMERICAN     DALLAS-FT. WORTH   928 638 1186
    ## 2546 1110           SOUTHWEST            SAN DIEGO   588-693-9875
    ## 2547 1113               DELTA MINNEAPOLIS-ST. PAUL   681-308-7915
    ## 2548 1180           SOUTHWEST            LAS VEGAS   783 647 8490
    ## 2549 1499            FRONTIER               DENVER   897 847 0632
    ## 2550 2154          AIR CANADA            VANCOUVER   150 952 4453
    ## 2551 2362             JETBLUE       FT. LAUDERDALE   322-884-3020
    ## 2552 2509              ALASKA       SALT LAKE CITY   176-313-5403
    ## 2553 2623            INTERJET          GUADALAJARA   487 109 4196
    ## 2554 1908              UNITED               BOSTON (477) 182-4689
    ## 2555 2151      CATHAY PACIFIC            HONG KONG   544 382 8289
    ## 2556 2188              QANTAS               SYDNEY   781 543 7456
    ## 2557 3174               DELTA          LOS ANGELES   911 829 6476
    ## 2558  438              UNITED         INDIANAPOLIS (525) 362-5532
    ## 2559 1883              UNITED               BOSTON   517 986 3426
    ## 2560 3107              UNITED              SPOKANE   838 220 5397
    ## 2561 2108             JETBLUE           LONG BEACH (687) 887-6766
    ## 2562 1859              ALASKA              KAHULUI   179 163 0902
    ## 2563 2345               DELTA MINNEAPOLIS-ST. PAUL   539 137 8983
    ## 2564 1669            EMIRATES                DUBAI   733-154-0094
    ## 2565 3246            AMERICAN              PHOENIX   639 881 3693
    ## 2566 1874            AMERICAN       CHICAGO-O'HARE   291-830-3017
    ## 2567  521            EMIRATES                DUBAI (637) 100-0509
    ## 2568  600              ALASKA WASHINGTON DC-DULLES   750 520 0167
    ## 2569 2311              QANTAS               SYDNEY   676 485 8963
    ## 2570 3119                COPA          PANAMA CITY   135 566 5090
    ## 2571 3079                COPA          PANAMA CITY   337 260 4996
    ## 2572  814              ALASKA            BALTIMORE   371 185 2377
    ## 2573    7           SOUTHWEST          LOS ANGELES   280 461 1386
    ## 2574  290           SOUTHWEST          LOS ANGELES (603) 149-7268
    ## 2575  319           SOUTHWEST          LOS ANGELES (364) 792-5553
    ## 2576  377              ALASKA          LOS ANGELES   496-429-1314
    ## 2577  485              UNITED               DENVER   459 671 4698
    ## 2578  765          AIR CANADA              TORONTO   486-268-3312
    ## 2579 1761           LUFTHANSA               MUNICH   497-518-4050
    ## 2580 1797              ALASKA       RALEIGH-DURHAM (535) 685-8273
    ## 2581 1966              UNITED            BALTIMORE   859 495 4050
    ## 2582 2143            FRONTIER               DENVER (826) 738-8316
    ## 2583 2554             JETBLUE           LONG BEACH   724-134-3870
    ## 2584 2588             JETBLUE           LONG BEACH (554) 269-8937
    ## 2585 2976            AMERICAN            CHARLOTTE   125-578-4253
    ## 2586 3244            AMERICAN              PHOENIX   614 800 2861
    ## 2587 2367             JETBLUE       FT. LAUDERDALE   487-232-4449
    ## 2588 1899           LUFTHANSA               MUNICH (298) 135-0900
    ## 2589 1483             AVIANCA         SAN SALVADOR (392) 183-7831
    ## 2590 1391         UNITED INTL             SHANGHAI (606) 596-1029
    ## 2591 3042              UNITED             PORTLAND (384) 953-4795
    ## 2592 1996              UNITED            BALTIMORE (855) 811-8811
    ## 2593 1549           AIR CHINA              BEIJING   253-374-7102
    ## 2594 2497              UNITED       CHICAGO-O'HARE (419) 295-9580
    ## 2595 2620             JETBLUE               BOSTON   802 102 8345
    ## 2596 2343              UNITED               AUSTIN   417 393 0050
    ## 2597 2645              UNITED       CHICAGO-O'HARE   787-624-8443
    ## 2598 2551             JETBLUE               BOSTON (919) 486-4251
    ## 2599 1249         UNITED INTL         TOKYO-NARITA   341 824 5322
    ## 2600  260          AIR CANADA              TORONTO   415 551 1608
    ## 2601 2906    TURKISH AIRLINES             ISTANBUL (392) 495-7961
    ## 2602 3260              UNITED            SAN DIEGO   473-238-3324
    ## 2603 1850           SOUTHWEST            SAN DIEGO (506) 760-3043
    ## 2604 3115              UNITED              SPOKANE   876-834-0624
    ## 2605 2907               DELTA              SEATTLE   919 611 6170
    ## 2606 1704      AIR FRANCE/KLM      PARIS-DE GAULLE (146) 699-3488
    ## 2607 1769            AMERICAN       CHICAGO-O'HARE   261 434 7760
    ## 2608  796            AMERICAN          LOS ANGELES   617 310 2684
    ## 2609  249              UNITED            NASHVILLE   182-535-3412
    ## 2610  282              UNITED            BALTIMORE   506 129 1694
    ## 2611 2334               DELTA MINNEAPOLIS-ST. PAUL (302) 339-0791
    ## 2612 1386         UNITED INTL             SHANGHAI (446) 229-4342
    ## 2613 3285           SOUTHWEST               DENVER (249) 602-6985
    ## 2614 2073            AMERICAN         PHILADELPHIA (150) 905-6938
    ## 2615 2728               DELTA          LOS ANGELES   313 990 8823
    ## 2616 1766            AMERICAN       CHICAGO-O'HARE   656-941-5355
    ## 2617 2944           SOUTHWEST            SAN DIEGO (116) 689-6617
    ## 2618  712      CATHAY PACIFIC            HONG KONG   955 324 5981
    ## 2619 2666             JETBLUE               BOSTON   175 808 2189
    ## 2620 2513              UNITED               NEWARK (896) 993-8555
    ## 2621 1169           SOUTHWEST          LOS ANGELES   105-687-6500
    ## 2622 1680      CATHAY PACIFIC            HONG KONG   757 524 2964
    ## 2623 2701              UNITED       CHICAGO-O'HARE   201 374 2424
    ## 2624 1319             JETBLUE         NEW YORK-JFK (466) 912-8401
    ## 2625  394              ALASKA          LOS ANGELES   766 112 6143
    ## 2626  628              ALASKA WASHINGTON DC-DULLES   783-463-4865
    ## 2627 1685      CATHAY PACIFIC            HONG KONG   853-803-9900
    ## 2628  426              UNITED         INDIANAPOLIS (347) 851-5388
    ## 2629 2148            FRONTIER               DENVER   992 114 6973
    ## 2630  900              ALASKA       RALEIGH-DURHAM   316-212-7309
    ## 2631  903              ALASKA       RALEIGH-DURHAM   301 672 1092
    ## 2632  588             JETBLUE               BOSTON   795 137 0201
    ## 2633  369              ALASKA    SAN JOSE DEL CABO   381-883-5497
    ## 2634 2565            INTERJET          GUADALAJARA   100-531-4642
    ## 2635 1983              UNITED            BALTIMORE   994 923 6634
    ## 2636 3025              UNITED             PORTLAND   920 355 8404
    ## 2637 1290             JETBLUE         NEW YORK-JFK (441) 445-6532
    ## 2638  689      CATHAY PACIFIC            HONG KONG   325 795 2455
    ## 2639 2837              UNITED       CHICAGO-O'HARE   593 829 6250
    ## 2640 2857              UNITED              SEATTLE   566-482-9004
    ## 2641  208            AMERICAN          LOS ANGELES   542 537 6770
    ## 2642 1601  SINGAPORE AIRLINES            SINGAPORE   716 191 1741
    ## 2643 2582              UNITED       CHICAGO-O'HARE   491-727-7162
    ## 2644 2720      AIR FRANCE/KLM      PARIS-DE GAULLE   167-336-5660
    ## 2645  399              ALASKA          LOS ANGELES   358 831 0725
    ## 2646 2452              UNITED               DENVER   432 979 7292
    ## 2647  650              ALASKA WASHINGTON DC-DULLES (205) 382-5599
    ## 2648 2518              ALASKA       SALT LAKE CITY (208) 794-9612
    ## 2649 1692              ALASKA            SANTA ANA   728 662 3934
    ## 2650  571            EMIRATES                DUBAI   380-918-8572
    ## 2651  717      AIR FRANCE/KLM      PARIS-DE GAULLE (905) 742-3525
    ## 2652 2963            AMERICAN            CHARLOTTE   151 434 6989
    ## 2653 2912               DELTA              SEATTLE   755 544 2629
    ## 2654 1655            EMIRATES                DUBAI   633 181 4494
    ## 2655 1700      AIR FRANCE/KLM      PARIS-DE GAULLE   346 706 5964
    ## 2656  870              ALASKA              SEATTLE   688 690 2184
    ## 2657  945          AIR CANADA              TORONTO   618 717 1697
    ## 2658 2586              UNITED       CHICAGO-O'HARE   185-321-6877
    ## 2659 2232           SOUTHWEST            SANTA ANA (147) 535-3529
    ## 2660 2400         UNITED INTL              BEIJING (152) 912-4118
    ## 2661  299           SOUTHWEST          LOS ANGELES   726 943 7486
    ## 2662  452              UNITED     DALLAS-FT. WORTH (634) 521-4714
    ## 2663 1613     VIRGIN ATLANTIC      LONDON HEATHROW   670-248-0186
    ## 2664 2515              ALASKA       SALT LAKE CITY (121) 509-7306
    ## 2665  568            EMIRATES                DUBAI (105) 635-5212
    ## 2666 2269              UNITED          LOS ANGELES (732) 168-0110
    ## 2667  313              UNITED WASHINGTON DC-DULLES   364 834 3150
    ## 2668  821              ALASKA              SEATTLE   176 508 2778
    ## 2669  860              ALASKA             PORTLAND   120 941 0833
    ## 2670 1251       CHINA EASTERN             SHANGHAI   670 902 3199
    ## 2671 1554               DELTA         NEW YORK-JFK (214) 250-8756
    ## 2672 1556               DELTA         NEW YORK-JFK (119) 975-8484
    ## 2673 1656            EMIRATES                DUBAI   297 484 3285
    ## 2674 2661          AEROMEXICO          MEXICO CITY (489) 534-6272
    ## 2675 2842              UNITED       CHICAGO-O'HARE (610) 716-5732
    ## 2676 3162              UNITED            SAN DIEGO (456) 925-4236
    ## 2677 2129         UNITED INTL          MEXICO CITY   743-103-7645
    ## 2678  686      CATHAY PACIFIC            HONG KONG   432-281-3682
    ## 2679 2168            FRONTIER               DENVER (167) 144-9470
    ## 2680 3217              ALASKA             PORTLAND   648 685 6188
    ## 2681  653         UNITED INTL               MUNICH   548 191 4898
    ## 2682 2626              UNITED       CHICAGO-O'HARE   288 110 9483
    ## 2683 1962              UNITED               BOSTON   946-558-5801
    ## 2684 2723      AIR FRANCE/KLM      PARIS-DE GAULLE   388 744 9637
    ## 2685 2754      AIR FRANCE/KLM      PARIS-DE GAULLE   506 463 9129
    ## 2686 3016              UNITED             PORTLAND (848) 149-5208
    ## 2687 1604     VIRGIN ATLANTIC      LONDON HEATHROW (970) 908-2298
    ## 2688 2402         UNITED INTL              BEIJING (843) 120-5653
    ## 2689 1693              ALASKA            SANTA ANA   306 394 8640
    ## 2690  368           LUFTHANSA            FRANKFURT   170-641-3537
    ## 2691 2401         UNITED INTL               SYDNEY   860 723 5066
    ## 2692  672      AIR FRANCE/KLM      PARIS-DE GAULLE   814-895-6610
    ## 2693 2667          AEROMEXICO          MEXICO CITY (139) 727-9901
    ## 2694 3259              UNITED            SAN DIEGO   598 735 8557
    ## 2695 2048              UNITED               NEWARK   593 895 6761
    ## 2696 2743               DELTA              ATLANTA (817) 824-3849
    ## 2697  532            EMIRATES                DUBAI   508 484 9738
    ## 2698  862              ALASKA             PORTLAND   719 489 4724
    ## 2699 2519              UNITED       CHICAGO-O'HARE   503-671-4901
    ## 2700 2630              UNITED       CHICAGO-O'HARE   275 649 8183
    ## 2701 2436     AIR NEW ZEALAND             AUCKLAND   968 130 7012
    ## 2702 1632          KOREAN AIR                SEOUL   290 367 6676
    ## 2703  276              UNITED          LOS ANGELES   499 766 9941
    ## 2704 1158           SOUTHWEST            SAN DIEGO   538-393-2243
    ## 2705 1224           SOUTHWEST            LAS VEGAS (540) 362-7136
    ## 2706 2178       CHINA EASTERN              QINGDAO (802) 910-1742
    ## 2707 2222           SOUTHWEST              PHOENIX   845 544 4748
    ## 2708  450              UNITED         INDIANAPOLIS   784-458-8425
    ## 2709 3251              UNITED            SAN DIEGO (365) 217-0634
    ## 2710  451              ALASKA            LAS VEGAS   708 500 2758
    ## 2711  372           LUFTHANSA            FRANKFURT (594) 797-7729
    ## 2712 1071          AIR CANADA              CALGARY   982 555 9504
    ## 2713 2015     BRITISH AIRWAYS      LONDON HEATHROW   477-307-3338
    ## 2714 2520              ALASKA       SALT LAKE CITY   744-301-1148
    ## 2715 2184              QANTAS               SYDNEY   389 484 8888
    ## 2716 2165            FRONTIER               DENVER   739 303 6128
    ## 2717  823              ALASKA              SEATTLE   175 905 9962
    ## 2718 1453              UNITED             PORTLAND (900) 462-1379
    ## 2719 3187               DELTA          LOS ANGELES   878-636-2294
    ## 2720 2324              QANTAS               SYDNEY (998) 692-1900
    ## 2721 2717              UNITED              KAHULUI   994 421 8642
    ## 2722 1518            INTERJET          GUADALAJARA   304-225-5895
    ## 2723 3030              UNITED             PORTLAND   931-522-5498
    ## 2724 1065          AIR CANADA              CALGARY   838 898 2275
    ## 2725  757          AIR CANADA              TORONTO   718 187 7125
    ## 2726 1644     VIRGIN ATLANTIC      LONDON HEATHROW   943 561 8955
    ## 2727 2959            AMERICAN         NEW YORK-JFK   341 261 6456
    ## 2728 1132           SOUTHWEST            SAN DIEGO (507) 483-3618
    ## 2729  936          AIR CANADA              TORONTO   826 266 0205
    ## 2730  566            EMIRATES                DUBAI (380) 449-7849
    ## 2731 2922               DELTA              SEATTLE   589-975-0198
    ## 2732 1244       CHINA EASTERN             SHANGHAI   657 680 8781
    ## 2733 2596          AEROMEXICO          MEXICO CITY   501-668-7869
    ## 2734 1052          AIR CANADA              CALGARY   224 365 8299
    ## 2735  931              ALASKA       RALEIGH-DURHAM   747 588 1968
    ## 2736 3100                COPA          PANAMA CITY (706) 836-7047
    ## 2737 1411              UNITED            LAS VEGAS (698) 462-6742
    ## 2738  726      AIR FRANCE/KLM      PARIS-DE GAULLE   748 446 1257
    ## 2739  435              UNITED             HONOLULU   525-552-4162
    ## 2740 1605     VIRGIN ATLANTIC      LONDON HEATHROW   635-714-8302
    ## 2741 2263                 WOW            REYKJAVIK   114 668 2834
    ## 2742  825              ALASKA              SEATTLE   404 788 2855
    ## 2743 1752           LUFTHANSA               MUNICH   673 292 2444
    ## 2744 2964            AMERICAN            CHARLOTTE   136 788 1426
    ## 2745 1534              ALASKA          LOS ANGELES   564 677 3934
    ## 2746 2480              UNITED               DENVER   213 981 7762
    ## 2747 1405              UNITED          KANSAS CITY   453-556-0852
    ## 2748 2669          AEROMEXICO          MEXICO CITY   423 593 4483
    ## 2749 1606     VIRGIN ATLANTIC      LONDON HEATHROW   820 609 7454
    ## 2750  539            EMIRATES                DUBAI   131-641-1331
    ## 2751  570              ALASKA              SEATTLE   824 540 9579
    ## 2752 3083 PHILIPPINE AIRLINES               MANILA (753) 726-0123
    ## 2753 1875            AMERICAN       CHICAGO-O'HARE   739 737 9041
    ## 2754 2919    TURKISH AIRLINES             ISTANBUL   609-332-7370
    ## 2755 1333         UNITED INTL               KANSAI   426-182-1365
    ## 2756 3113              ALASKA               NEWARK (347) 782-5787
    ## 2757 2045              UNITED               NEWARK   898-210-6218
    ## 2758 3257              UNITED            SAN DIEGO   938 394 0411
    ## 2759  609              ALASKA WASHINGTON DC-DULLES   904 844 1759
    ## 2760 1912      AIR FRANCE/KLM      PARIS-DE GAULLE   658-861-4306
    ## 2761 3082              UNITED              SPOKANE (716) 184-1232
    ## 2762 1978              UNITED            BALTIMORE   380-105-1757
    ## 2763 2162       CHINA EASTERN              QINGDAO (969) 555-0453
    ## 2764 1550           AIR CHINA              BEIJING   249 452 4370
    ## 2765 2307            AMERICAN       CHICAGO-O'HARE   443 384 8253
    ## 2766 1492            FRONTIER               DENVER   711 289 2247
    ## 2767 1521               DELTA          LOS ANGELES   343-973-0193
    ## 2768 1722          AIR CANADA              TORONTO   367 650 3720
    ## 2769 1279      CHINA SOUTHERN                WUHAN (265) 286-5671
    ## 2770 2487              UNITED       CHICAGO-O'HARE   148 630 8560
    ## 2771 2902               DELTA              SEATTLE   413-727-2672
    ## 2772 2958           SOUTHWEST          LOS ANGELES (162) 332-5838
    ## 2773 3225            AMERICAN              PHOENIX (229) 604-7790
    ## 2774  474              UNITED               DENVER (892) 301-0333
    ## 2775 1746           LUFTHANSA               MUNICH   148 501 5084
    ## 2776 3101              ALASKA               NEWARK   564 780 8272
    ## 2777 1199           SOUTHWEST            LAS VEGAS   524 190 0899
    ## 2778 2884             EVA AIR               TAIPEI   463 792 2782
    ## 2779 1775              UNITED       RALEIGH-DURHAM (331) 472-8624
    ## 2780 3005              UNITED            NASHVILLE   522-286-5318
    ## 2781 2974              UNITED          LOS ANGELES   310 719 4550
    ## 2782 3017              ALASKA      PUERTO VALLARTA   314-360-4288
    ## 2783 2993              UNITED            BALTIMORE (301) 534-5754
    ## 2784 2986              UNITED             PORTLAND   822 271 5719
    ## 2785 2998              UNITED              BURBANK (341) 473-0639
    ## 2786 3027              ALASKA              SEATTLE (835) 882-3693
    ## 2787 3008               DELTA         NEW YORK-JFK   465-550-6610
    ## 2788 2980              UNITED            NASHVILLE   388 100 1482
    ## 2789 3011              ALASKA WASHINGTON DC-DULLES (589) 194-0523
    ## 2790 2995              ALASKA         NEW YORK-JFK   982 842 4913
    ## 2791 3013              UNITED               AUSTIN   671 913 4563
    ## 2792 2985              UNITED          LOS ANGELES   144 468 5864
    ## 2793 2978           SOUTHWEST              PHOENIX   194 344 4039
    ## 2794 2967              UNITED             HONOLULU   331 747 5714
    ## 2795 3015              UNITED WASHINGTON DC-DULLES   221-190-1449
    ## 2796 3000               DELTA MINNEAPOLIS-ST. PAUL (322) 843-0185
    ## 2797 2991              UNITED              KAHULUI   676-614-9095
    ## 2798 2969              UNITED             HONOLULU (190) 975-2514
    ## 2799 3020              ALASKA              SEATTLE (909) 382-3774
    ## 2800 2988            FRONTIER               DENVER   956 257 9319
    ## 2801 3021              ALASKA              SEATTLE   362 145 8268
    ## 2802 3002               DELTA MINNEAPOLIS-ST. PAUL   570 727 3998
    ## 2803 2982           SOUTHWEST            SANTA ANA   225 964 9193
    ## 2804 3024              ALASKA              SEATTLE   227 419 9482
    ## 2805 9001              UNITED               AUSTIN   949 360 7605
    ## 2806 2971              UNITED       CHICAGO-O'HARE (347) 896-3463
    ## 2807 9002              UNITED               NEWARK   794 939 9735
    ## 2808 9004          AIR CANADA            VANCOUVER   413 754 3034
    ## 2809 9003              ALASKA             PORTLAND   806 730 0459
    ##      phone_no_parens  phone_clean
    ## 1       858 990 5153 858 990 5153
    ## 2       731-813-2043 731 813 2043
    ## 3       563-732-6802 563 732 6802
    ## 4       145 725 4021 145 725 4021
    ## 5       931 311 5801 931 311 5801
    ## 6       637 782-6989 637 782 6989
    ## 7       172 990 3485 172 990 3485
    ## 8       872 325 4341 872 325 4341
    ## 9       359 803-9809 359 803 9809
    ## 10      152 790 8238 152 790 8238
    ## 11      330 561 9257 330 561 9257
    ## 12      437 420 7546 437 420 7546
    ## 13      495 632 4027 495 632 4027
    ## 14      416 788-2844 416 788 2844
    ## 15      311-305-4367 311 305 4367
    ## 16      817-400-0481 817 400 0481
    ## 17      430 723 1079 430 723 1079
    ## 18      729 609-4819 729 609 4819
    ## 19      201 737-4409 201 737 4409
    ## 20      137 611-3694 137 611 3694
    ## 21      226 490 8696 226 490 8696
    ## 22      123 570 8640 123 570 8640
    ## 23      665 803 2453 665 803 2453
    ## 24      812 869-6263 812 869 6263
    ## 25      639 132 6386 639 132 6386
    ## 26      194 198-0504 194 198 0504
    ## 27      437 886 0753 437 886 0753
    ## 28      626 756 5089 626 756 5089
    ## 29      299 137-6993 299 137 6993
    ## 30      714 950 3364 714 950 3364
    ## 31      653-786-5985 653 786 5985
    ## 32      518 286 5956 518 286 5956
    ## 33      194 960 2145 194 960 2145
    ## 34      362-136-1153 362 136 1153
    ## 35      376-456-0697 376 456 0697
    ## 36      657 832 1189 657 832 1189
    ## 37      962-918-6117 962 918 6117
    ## 38      692 929 3592 692 929 3592
    ## 39      805-877-3887 805 877 3887
    ## 40      739 710-2966 739 710 2966
    ## 41      819 732 4132 819 732 4132
    ## 42      367 221 9710 367 221 9710
    ## 43      361 154 1789 361 154 1789
    ## 44      680 488 1182 680 488 1182
    ## 45      928 638 1186 928 638 1186
    ## 46      588-693-9875 588 693 9875
    ## 47      681-308-7915 681 308 7915
    ## 48      783 647 8490 783 647 8490
    ## 49      897 847 0632 897 847 0632
    ## 50      150 952 4453 150 952 4453
    ## 51      322-884-3020 322 884 3020
    ## 52      176-313-5403 176 313 5403
    ## 53      487 109 4196 487 109 4196
    ## 54      477 182-4689 477 182 4689
    ## 55      544 382 8289 544 382 8289
    ## 56      781 543 7456 781 543 7456
    ## 57      911 829 6476 911 829 6476
    ## 58      525 362-5532 525 362 5532
    ## 59      517 986 3426 517 986 3426
    ## 60      838 220 5397 838 220 5397
    ## 61      687 887-6766 687 887 6766
    ## 62      179 163 0902 179 163 0902
    ## 63      539 137 8983 539 137 8983
    ## 64      733-154-0094 733 154 0094
    ## 65      639 881 3693 639 881 3693
    ## 66      291-830-3017 291 830 3017
    ## 67      637 100-0509 637 100 0509
    ## 68      750 520 0167 750 520 0167
    ## 69      676 485 8963 676 485 8963
    ## 70      135 566 5090 135 566 5090
    ## 71      337 260 4996 337 260 4996
    ## 72      371 185 2377 371 185 2377
    ## 73      280 461 1386 280 461 1386
    ## 74      603 149-7268 603 149 7268
    ## 75      364 792-5553 364 792 5553
    ## 76      496-429-1314 496 429 1314
    ## 77      459 671 4698 459 671 4698
    ## 78      486-268-3312 486 268 3312
    ## 79      497-518-4050 497 518 4050
    ## 80      535 685-8273 535 685 8273
    ## 81      859 495 4050 859 495 4050
    ## 82      826 738-8316 826 738 8316
    ## 83      724-134-3870 724 134 3870
    ## 84      554 269-8937 554 269 8937
    ## 85      125-578-4253 125 578 4253
    ## 86      614 800 2861 614 800 2861
    ## 87      487-232-4449 487 232 4449
    ## 88      298 135-0900 298 135 0900
    ## 89      392 183-7831 392 183 7831
    ## 90      606 596-1029 606 596 1029
    ## 91      384 953-4795 384 953 4795
    ## 92      855 811-8811 855 811 8811
    ## 93      253-374-7102 253 374 7102
    ## 94      419 295-9580 419 295 9580
    ## 95      802 102 8345 802 102 8345
    ## 96      417 393 0050 417 393 0050
    ## 97      787-624-8443 787 624 8443
    ## 98      919 486-4251 919 486 4251
    ## 99      341 824 5322 341 824 5322
    ## 100     415 551 1608 415 551 1608
    ## 101     392 495-7961 392 495 7961
    ## 102     473-238-3324 473 238 3324
    ## 103     506 760-3043 506 760 3043
    ## 104     876-834-0624 876 834 0624
    ## 105     919 611 6170 919 611 6170
    ## 106     146 699-3488 146 699 3488
    ## 107     261 434 7760 261 434 7760
    ## 108     617 310 2684 617 310 2684
    ## 109     182-535-3412 182 535 3412
    ## 110     506 129 1694 506 129 1694
    ## 111     302 339-0791 302 339 0791
    ## 112     446 229-4342 446 229 4342
    ## 113     249 602-6985 249 602 6985
    ## 114     150 905-6938 150 905 6938
    ## 115     313 990 8823 313 990 8823
    ## 116     656-941-5355 656 941 5355
    ## 117     116 689-6617 116 689 6617
    ## 118     955 324 5981 955 324 5981
    ## 119     175 808 2189 175 808 2189
    ## 120     896 993-8555 896 993 8555
    ## 121     105-687-6500 105 687 6500
    ## 122     757 524 2964 757 524 2964
    ## 123     201 374 2424 201 374 2424
    ## 124     466 912-8401 466 912 8401
    ## 125     766 112 6143 766 112 6143
    ## 126     783-463-4865 783 463 4865
    ## 127     853-803-9900 853 803 9900
    ## 128     347 851-5388 347 851 5388
    ## 129     992 114 6973 992 114 6973
    ## 130     316-212-7309 316 212 7309
    ## 131     301 672 1092 301 672 1092
    ## 132     795 137 0201 795 137 0201
    ## 133     381-883-5497 381 883 5497
    ## 134     100-531-4642 100 531 4642
    ## 135     994 923 6634 994 923 6634
    ## 136     920 355 8404 920 355 8404
    ## 137     441 445-6532 441 445 6532
    ## 138     325 795 2455 325 795 2455
    ## 139     593 829 6250 593 829 6250
    ## 140     566-482-9004 566 482 9004
    ## 141     542 537 6770 542 537 6770
    ## 142     716 191 1741 716 191 1741
    ## 143     491-727-7162 491 727 7162
    ## 144     167-336-5660 167 336 5660
    ## 145     358 831 0725 358 831 0725
    ## 146     432 979 7292 432 979 7292
    ## 147     205 382-5599 205 382 5599
    ## 148     208 794-9612 208 794 9612
    ## 149     728 662 3934 728 662 3934
    ## 150     380-918-8572 380 918 8572
    ## 151     905 742-3525 905 742 3525
    ## 152     151 434 6989 151 434 6989
    ## 153     755 544 2629 755 544 2629
    ## 154     633 181 4494 633 181 4494
    ## 155     346 706 5964 346 706 5964
    ## 156     688 690 2184 688 690 2184
    ## 157     618 717 1697 618 717 1697
    ## 158     185-321-6877 185 321 6877
    ## 159     147 535-3529 147 535 3529
    ## 160     152 912-4118 152 912 4118
    ## 161     726 943 7486 726 943 7486
    ## 162     634 521-4714 634 521 4714
    ## 163     670-248-0186 670 248 0186
    ## 164     121 509-7306 121 509 7306
    ## 165     105 635-5212 105 635 5212
    ## 166     732 168-0110 732 168 0110
    ## 167     364 834 3150 364 834 3150
    ## 168     176 508 2778 176 508 2778
    ## 169     120 941 0833 120 941 0833
    ## 170     670 902 3199 670 902 3199
    ## 171     214 250-8756 214 250 8756
    ## 172     119 975-8484 119 975 8484
    ## 173     297 484 3285 297 484 3285
    ## 174     489 534-6272 489 534 6272
    ## 175     610 716-5732 610 716 5732
    ## 176     456 925-4236 456 925 4236
    ## 177     743-103-7645 743 103 7645
    ## 178     432-281-3682 432 281 3682
    ## 179     167 144-9470 167 144 9470
    ## 180     648 685 6188 648 685 6188
    ## 181     548 191 4898 548 191 4898
    ## 182     288 110 9483 288 110 9483
    ## 183     946-558-5801 946 558 5801
    ## 184     388 744 9637 388 744 9637
    ## 185     506 463 9129 506 463 9129
    ## 186     848 149-5208 848 149 5208
    ## 187     970 908-2298 970 908 2298
    ## 188     843 120-5653 843 120 5653
    ## 189     306 394 8640 306 394 8640
    ## 190     170-641-3537 170 641 3537
    ## 191     860 723 5066 860 723 5066
    ## 192     814-895-6610 814 895 6610
    ## 193     139 727-9901 139 727 9901
    ## 194     598 735 8557 598 735 8557
    ## 195     593 895 6761 593 895 6761
    ## 196     817 824-3849 817 824 3849
    ## 197     508 484 9738 508 484 9738
    ## 198     719 489 4724 719 489 4724
    ## 199     503-671-4901 503 671 4901
    ## 200     275 649 8183 275 649 8183
    ## 201     968 130 7012 968 130 7012
    ## 202     290 367 6676 290 367 6676
    ## 203     499 766 9941 499 766 9941
    ## 204     538-393-2243 538 393 2243
    ## 205     540 362-7136 540 362 7136
    ## 206     802 910-1742 802 910 1742
    ## 207     845 544 4748 845 544 4748
    ## 208     784-458-8425 784 458 8425
    ## 209     365 217-0634 365 217 0634
    ## 210     708 500 2758 708 500 2758
    ## 211     594 797-7729 594 797 7729
    ## 212     982 555 9504 982 555 9504
    ## 213     477-307-3338 477 307 3338
    ## 214     744-301-1148 744 301 1148
    ## 215     389 484 8888 389 484 8888
    ## 216     739 303 6128 739 303 6128
    ## 217     175 905 9962 175 905 9962
    ## 218     900 462-1379 900 462 1379
    ## 219     878-636-2294 878 636 2294
    ## 220     998 692-1900 998 692 1900
    ## 221     994 421 8642 994 421 8642
    ## 222     304-225-5895 304 225 5895
    ## 223     931-522-5498 931 522 5498
    ## 224     838 898 2275 838 898 2275
    ## 225     718 187 7125 718 187 7125
    ## 226     943 561 8955 943 561 8955
    ## 227     341 261 6456 341 261 6456
    ## 228     507 483-3618 507 483 3618
    ## 229     826 266 0205 826 266 0205
    ## 230     380 449-7849 380 449 7849
    ## 231     589-975-0198 589 975 0198
    ## 232     657 680 8781 657 680 8781
    ## 233     501-668-7869 501 668 7869
    ## 234     224 365 8299 224 365 8299
    ## 235     747 588 1968 747 588 1968
    ## 236     706 836-7047 706 836 7047
    ## 237     698 462-6742 698 462 6742
    ## 238     748 446 1257 748 446 1257
    ## 239     525-552-4162 525 552 4162
    ## 240     635-714-8302 635 714 8302
    ## 241     114 668 2834 114 668 2834
    ## 242     404 788 2855 404 788 2855
    ## 243     673 292 2444 673 292 2444
    ## 244     136 788 1426 136 788 1426
    ## 245     564 677 3934 564 677 3934
    ## 246     213 981 7762 213 981 7762
    ## 247     453-556-0852 453 556 0852
    ## 248     423 593 4483 423 593 4483
    ## 249     820 609 7454 820 609 7454
    ## 250     131-641-1331 131 641 1331
    ## 251     824 540 9579 824 540 9579
    ## 252     753 726-0123 753 726 0123
    ## 253     739 737 9041 739 737 9041
    ## 254     609-332-7370 609 332 7370
    ## 255     426-182-1365 426 182 1365
    ## 256     347 782-5787 347 782 5787
    ## 257     898-210-6218 898 210 6218
    ## 258     938 394 0411 938 394 0411
    ## 259     904 844 1759 904 844 1759
    ## 260     658-861-4306 658 861 4306
    ## 261     716 184-1232 716 184 1232
    ## 262     380-105-1757 380 105 1757
    ## 263     969 555-0453 969 555 0453
    ## 264     249 452 4370 249 452 4370
    ## 265     443 384 8253 443 384 8253
    ## 266     711 289 2247 711 289 2247
    ## 267     343-973-0193 343 973 0193
    ## 268     367 650 3720 367 650 3720
    ## 269     265 286-5671 265 286 5671
    ## 270     148 630 8560 148 630 8560
    ## 271     413-727-2672 413 727 2672
    ## 272     162 332-5838 162 332 5838
    ## 273     229 604-7790 229 604 7790
    ## 274     892 301-0333 892 301 0333
    ## 275     148 501 5084 148 501 5084
    ## 276     564 780 8272 564 780 8272
    ## 277     524 190 0899 524 190 0899
    ## 278     463 792 2782 463 792 2782
    ## 279     331 472-8624 331 472 8624
    ## 280     522-286-5318 522 286 5318
    ## 281     310 719 4550 310 719 4550
    ## 282     314-360-4288 314 360 4288
    ## 283     301 534-5754 301 534 5754
    ## 284     822 271 5719 822 271 5719
    ## 285     341 473-0639 341 473 0639
    ## 286     835 882-3693 835 882 3693
    ## 287     465-550-6610 465 550 6610
    ## 288     388 100 1482 388 100 1482
    ## 289     589 194-0523 589 194 0523
    ## 290     982 842 4913 982 842 4913
    ## 291     671 913 4563 671 913 4563
    ## 292     144 468 5864 144 468 5864
    ## 293     194 344 4039 194 344 4039
    ## 294     331 747 5714 331 747 5714
    ## 295     221-190-1449 221 190 1449
    ## 296     322 843-0185 322 843 0185
    ## 297     676-614-9095 676 614 9095
    ## 298     190 975-2514 190 975 2514
    ## 299     909 382-3774 909 382 3774
    ## 300     956 257 9319 956 257 9319
    ## 301     362 145 8268 362 145 8268
    ## 302     570 727 3998 570 727 3998
    ## 303     225 964 9193 225 964 9193
    ## 304     227 419 9482 227 419 9482
    ## 305     949 360 7605 949 360 7605
    ## 306     347 896-3463 347 896 3463
    ## 307     794 939 9735 794 939 9735
    ## 308     413 754 3034 413 754 3034
    ## 309     806 730 0459 806 730 0459
    ## 310     791 195 8909 791 195 8909
    ## 311     691-318-3535 691 318 3535
    ## 312     852-386-6029 852 386 6029
    ## 313     380 682 7795 380 682 7795
    ## 314     355 550-1392 355 550 1392
    ## 315     247 586 4579 247 586 4579
    ## 316     235 257 1041 235 257 1041
    ## 317     705 456-1905 705 456 1905
    ## 318     106 756 2785 106 756 2785
    ## 319     836 207-8419 836 207 8419
    ## 320     306 552-1875 306 552 1875
    ## 321     729-102-7511 729 102 7511
    ## 322     795 583 0958 795 583 0958
    ## 323     700-431-3918 700 431 3918
    ## 324     929 632 1068 929 632 1068
    ## 325     763 906 2495 763 906 2495
    ## 326     469 976 6796 469 976 6796
    ## 327     887-657-4143 887 657 4143
    ## 328     574-438-5329 574 438 5329
    ## 329     319 127 9518 319 127 9518
    ## 330     429 960 9710 429 960 9710
    ## 331     419 646 0299 419 646 0299
    ## 332     192 343 8515 192 343 8515
    ## 333     521 336 8581 521 336 8581
    ## 334     776 367 6109 776 367 6109
    ## 335     470 367 1392 470 367 1392
    ## 336     944 189-7555 944 189 7555
    ## 337     998-931-4783 998 931 4783
    ## 338     362-178-6307 362 178 6307
    ## 339     458 404-9558 458 404 9558
    ## 340     212 286 7936 212 286 7936
    ## 341     481 522-1039 481 522 1039
    ## 342     376 611-4588 376 611 4588
    ## 343     936 193 9690 936 193 9690
    ## 344     641 544-6549 641 544 6549
    ## 345     797-870-7818 797 870 7818
    ## 346     693 907 5353 693 907 5353
    ## 347     332 973 4943 332 973 4943
    ## 348     929 622 9077 929 622 9077
    ## 349     649-379-5361 649 379 5361
    ## 350     572 748-6932 572 748 6932
    ## 351     395-892-5646 395 892 5646
    ## 352     221-628-9561 221 628 9561
    ## 353     227 801-6148 227 801 6148
    ## 354     549-649-1864 549 649 1864
    ## 355     342 941 0439 342 941 0439
    ## 356     701 390 9814 701 390 9814
    ## 357     519 573-6576 519 573 6576
    ## 358     919-342-0230 919 342 0230
    ## 359     364-759-2705 364 759 2705
    ## 360     949 543 7906 949 543 7906
    ## 361     942 732 6403 942 732 6403
    ## 362     900 586-1787 900 586 1787
    ## 363     308-607-9855 308 607 9855
    ## 364     764 645 5740 764 645 5740
    ## 365     472-337-8838 472 337 8838
    ## 366     791 847 7278 791 847 7278
    ## 367     128 805 3828 128 805 3828
    ## 368     365-832-0674 365 832 0674
    ## 369     123-282-3494 123 282 3494
    ## 370     285 424 4318 285 424 4318
    ## 371     452 352 1387 452 352 1387
    ## 372     129-377-8159 129 377 8159
    ## 373     222 143 3131 222 143 3131
    ## 374     162-451-0594 162 451 0594
    ## 375     239 325-5321 239 325 5321
    ## 376     436-422-6171 436 422 6171
    ## 377     605 284 4260 605 284 4260
    ## 378     929-102-5905 929 102 5905
    ## 379     847 507 8268 847 507 8268
    ## 380     452-811-8088 452 811 8088
    ## 381     799 143 1677 799 143 1677
    ## 382     196 756 4555 196 756 4555
    ## 383     119-444-0817 119 444 0817
    ## 384     885 454 0883 885 454 0883
    ## 385     945-998-0444 945 998 0444
    ## 386     367 897-7969 367 897 7969
    ## 387     163 241 9321 163 241 9321
    ## 388     594-176-5811 594 176 5811
    ## 389     621 874-9973 621 874 9973
    ## 390     332 963 4103 332 963 4103
    ## 391     389 318 3975 389 318 3975
    ## 392     894-593-7953 894 593 7953
    ## 393     561-266-7842 561 266 7842
    ## 394     354-958-8052 354 958 8052
    ## 395     151 921 2775 151 921 2775
    ## 396     901 140 3759 901 140 3759
    ## 397     426 342-7378 426 342 7378
    ## 398     333-520-4811 333 520 4811
    ## 399     765 191-1797 765 191 1797
    ## 400     850 914 9348 850 914 9348
    ## 401     246 272 9019 246 272 9019
    ## 402     400 250-0871 400 250 0871
    ## 403     504 419 9191 504 419 9191
    ## 404     434 725-0561 434 725 0561
    ## 405     231 863 7554 231 863 7554
    ## 406     969 207-3261 969 207 3261
    ## 407     939 253 9048 939 253 9048
    ## 408     879-154-4494 879 154 4494
    ## 409     577 786 2546 577 786 2546
    ## 410     994 688-3259 994 688 3259
    ## 411     841-717-4447 841 717 4447
    ## 412     397-353-6309 397 353 6309
    ## 413     558 191 4548 558 191 4548
    ## 414     905 768 2297 905 768 2297
    ## 415     348 522 2051 348 522 2051
    ## 416     307-323-6861 307 323 6861
    ## 417     663 886 2487 663 886 2487
    ## 418     274 944 6097 274 944 6097
    ## 419     494 308-3048 494 308 3048
    ## 420     328 307 0875 328 307 0875
    ## 421     673 524 3504 673 524 3504
    ## 422     934 721 0615 934 721 0615
    ## 423     102-957-6486 102 957 6486
    ## 424     715 288-8832 715 288 8832
    ## 425     794 925 8846 794 925 8846
    ## 426     637 281 4111 637 281 4111
    ## 427     594 448-6242 594 448 6242
    ## 428     335 802 2651 335 802 2651
    ## 429     560 699 9908 560 699 9908
    ## 430     451 163 0102 451 163 0102
    ## 431     613 800 0835 613 800 0835
    ## 432     370 453 5800 370 453 5800
    ## 433     192 507 5411 192 507 5411
    ## 434     182-227-4838 182 227 4838
    ## 435     647 126 2332 647 126 2332
    ## 436     606 125 6957 606 125 6957
    ## 437     728-404-5558 728 404 5558
    ## 438     506 812 6052 506 812 6052
    ## 439     427-665-3475 427 665 3475
    ## 440     808 739 7162 808 739 7162
    ## 441     375-978-3305 375 978 3305
    ## 442     181-708-2089 181 708 2089
    ## 443     802 810-5574 802 810 5574
    ## 444     242-540-4234 242 540 4234
    ## 445     105 116 9695 105 116 9695
    ## 446     867 891-0871 867 891 0871
    ## 447     945 144 7892 945 144 7892
    ## 448     531 895 6695 531 895 6695
    ## 449     533 213-4368 533 213 4368
    ## 450     894 810 2674 894 810 2674
    ## 451     943 812 6349 943 812 6349
    ## 452     476 168 4235 476 168 4235
    ## 453     931 385 6757 931 385 6757
    ## 454     324 188-6781 324 188 6781
    ## 455     100 378 8095 100 378 8095
    ## 456           0244-5       0244 5
    ## 457     397-362-5469 397 362 5469
    ## 458     102 928-7959 102 928 7959
    ## 459     439 568-6611 439 568 6611
    ## 460     767 205-0604 767 205 0604
    ## 461     890 548-9219 890 548 9219
    ## 462     938 982 5585 938 982 5585
    ## 463     769-472-2992 769 472 2992
    ## 464     190 204 1154 190 204 1154
    ## 465     649 925-8489 649 925 8489
    ## 466     321 616 1013 321 616 1013
    ## 467     156 837-4491 156 837 4491
    ## 468     178-232-0815 178 232 0815
    ## 469     882 304-9032 882 304 9032
    ## 470     828 549 6666 828 549 6666
    ## 471     280 544 4554 280 544 4554
    ## 472     183 208 5054 183 208 5054
    ## 473     971 548-6611 971 548 6611
    ## 474     828 153-5819 828 153 5819
    ## 475     203 448 1522 203 448 1522
    ## 476     900-871-9056 900 871 9056
    ## 477     406-167-1379 406 167 1379
    ## 478     906 850-9192 906 850 9192
    ## 479     859 777-8245 859 777 8245
    ## 480     641-635-8466 641 635 8466
    ## 481     807-671-6158 807 671 6158
    ## 482     589 270-7518 589 270 7518
    ## 483     768 529 8051 768 529 8051
    ## 484     220 660 0306 220 660 0306
    ## 485     928-179-7556 928 179 7556
    ## 486     153 756 0278 153 756 0278
    ## 487     273 829 9197 273 829 9197
    ## 488     269 463 0911 269 463 0911
    ## 489     696 984 8826 696 984 8826
    ## 490     905-903-5258 905 903 5258
    ## 491     267 332 4709 267 332 4709
    ## 492     146 129-5118 146 129 5118
    ## 493     927 747-9822 927 747 9822
    ## 494     534 216 6666 534 216 6666
    ## 495     481 479-7013 481 479 7013
    ## 496     971 175 2968 971 175 2968
    ## 497     716 777-3762 716 777 3762
    ## 498     274-863-3205 274 863 3205
    ## 499     217 589-0596 217 589 0596
    ## 500     928 445 5474 928 445 5474
    ## 501     858 990 5153 858 990 5153
    ## 502     731-813-2043 731 813 2043
    ## 503     563-732-6802 563 732 6802
    ## 504     145 725 4021 145 725 4021
    ## 505     931 311 5801 931 311 5801
    ## 506     637 782-6989 637 782 6989
    ## 507     172 990 3485 172 990 3485
    ## 508     872 325 4341 872 325 4341
    ## 509     359 803-9809 359 803 9809
    ## 510     152 790 8238 152 790 8238
    ## 511     330 561 9257 330 561 9257
    ## 512     437 420 7546 437 420 7546
    ## 513     495 632 4027 495 632 4027
    ## 514     416 788-2844 416 788 2844
    ## 515     311-305-4367 311 305 4367
    ## 516     817-400-0481 817 400 0481
    ## 517     430 723 1079 430 723 1079
    ## 518     729 609-4819 729 609 4819
    ## 519     201 737-4409 201 737 4409
    ## 520     137 611-3694 137 611 3694
    ## 521     226 490 8696 226 490 8696
    ## 522     123 570 8640 123 570 8640
    ## 523     665 803 2453 665 803 2453
    ## 524     812 869-6263 812 869 6263
    ## 525     639 132 6386 639 132 6386
    ## 526     194 198-0504 194 198 0504
    ## 527     437 886 0753 437 886 0753
    ## 528     626 756 5089 626 756 5089
    ## 529     299 137-6993 299 137 6993
    ## 530     714 950 3364 714 950 3364
    ## 531     653-786-5985 653 786 5985
    ## 532     518 286 5956 518 286 5956
    ## 533     194 960 2145 194 960 2145
    ## 534     362-136-1153 362 136 1153
    ## 535     376-456-0697 376 456 0697
    ## 536     657 832 1189 657 832 1189
    ## 537     962-918-6117 962 918 6117
    ## 538     692 929 3592 692 929 3592
    ## 539     805-877-3887 805 877 3887
    ## 540     739 710-2966 739 710 2966
    ## 541     819 732 4132 819 732 4132
    ## 542     367 221 9710 367 221 9710
    ## 543     361 154 1789 361 154 1789
    ## 544     680 488 1182 680 488 1182
    ## 545     928 638 1186 928 638 1186
    ## 546     588-693-9875 588 693 9875
    ## 547     681-308-7915 681 308 7915
    ## 548     783 647 8490 783 647 8490
    ## 549     897 847 0632 897 847 0632
    ## 550     150 952 4453 150 952 4453
    ## 551     322-884-3020 322 884 3020
    ## 552     176-313-5403 176 313 5403
    ## 553     487 109 4196 487 109 4196
    ## 554     477 182-4689 477 182 4689
    ## 555     544 382 8289 544 382 8289
    ## 556     781 543 7456 781 543 7456
    ## 557     911 829 6476 911 829 6476
    ## 558     525 362-5532 525 362 5532
    ## 559     517 986 3426 517 986 3426
    ## 560     838 220 5397 838 220 5397
    ## 561     687 887-6766 687 887 6766
    ## 562     179 163 0902 179 163 0902
    ## 563     539 137 8983 539 137 8983
    ## 564     733-154-0094 733 154 0094
    ## 565     639 881 3693 639 881 3693
    ## 566     291-830-3017 291 830 3017
    ## 567     637 100-0509 637 100 0509
    ## 568     750 520 0167 750 520 0167
    ## 569     676 485 8963 676 485 8963
    ## 570     135 566 5090 135 566 5090
    ## 571     337 260 4996 337 260 4996
    ## 572     371 185 2377 371 185 2377
    ## 573     280 461 1386 280 461 1386
    ## 574     603 149-7268 603 149 7268
    ## 575     364 792-5553 364 792 5553
    ## 576     496-429-1314 496 429 1314
    ## 577     459 671 4698 459 671 4698
    ## 578     486-268-3312 486 268 3312
    ## 579     497-518-4050 497 518 4050
    ## 580     535 685-8273 535 685 8273
    ## 581     859 495 4050 859 495 4050
    ## 582     826 738-8316 826 738 8316
    ## 583     724-134-3870 724 134 3870
    ## 584     554 269-8937 554 269 8937
    ## 585     125-578-4253 125 578 4253
    ## 586     614 800 2861 614 800 2861
    ## 587     487-232-4449 487 232 4449
    ## 588     298 135-0900 298 135 0900
    ## 589     392 183-7831 392 183 7831
    ## 590     606 596-1029 606 596 1029
    ## 591     384 953-4795 384 953 4795
    ## 592     855 811-8811 855 811 8811
    ## 593     253-374-7102 253 374 7102
    ## 594     419 295-9580 419 295 9580
    ## 595     802 102 8345 802 102 8345
    ## 596     417 393 0050 417 393 0050
    ## 597     787-624-8443 787 624 8443
    ## 598     919 486-4251 919 486 4251
    ## 599     341 824 5322 341 824 5322
    ## 600     415 551 1608 415 551 1608
    ## 601     392 495-7961 392 495 7961
    ## 602     473-238-3324 473 238 3324
    ## 603     506 760-3043 506 760 3043
    ## 604     876-834-0624 876 834 0624
    ## 605     919 611 6170 919 611 6170
    ## 606     146 699-3488 146 699 3488
    ## 607     261 434 7760 261 434 7760
    ## 608     617 310 2684 617 310 2684
    ## 609     182-535-3412 182 535 3412
    ## 610     506 129 1694 506 129 1694
    ## 611     302 339-0791 302 339 0791
    ## 612     446 229-4342 446 229 4342
    ## 613     249 602-6985 249 602 6985
    ## 614     150 905-6938 150 905 6938
    ## 615     313 990 8823 313 990 8823
    ## 616     656-941-5355 656 941 5355
    ## 617     116 689-6617 116 689 6617
    ## 618     955 324 5981 955 324 5981
    ## 619     175 808 2189 175 808 2189
    ## 620     896 993-8555 896 993 8555
    ## 621     105-687-6500 105 687 6500
    ## 622     757 524 2964 757 524 2964
    ## 623     201 374 2424 201 374 2424
    ## 624     466 912-8401 466 912 8401
    ## 625     766 112 6143 766 112 6143
    ## 626     783-463-4865 783 463 4865
    ## 627     853-803-9900 853 803 9900
    ## 628     347 851-5388 347 851 5388
    ## 629     992 114 6973 992 114 6973
    ## 630     316-212-7309 316 212 7309
    ## 631     301 672 1092 301 672 1092
    ## 632     795 137 0201 795 137 0201
    ## 633     381-883-5497 381 883 5497
    ## 634     100-531-4642 100 531 4642
    ## 635     994 923 6634 994 923 6634
    ## 636     920 355 8404 920 355 8404
    ## 637     441 445-6532 441 445 6532
    ## 638     325 795 2455 325 795 2455
    ## 639     593 829 6250 593 829 6250
    ## 640     566-482-9004 566 482 9004
    ## 641     542 537 6770 542 537 6770
    ## 642     716 191 1741 716 191 1741
    ## 643     491-727-7162 491 727 7162
    ## 644     167-336-5660 167 336 5660
    ## 645     358 831 0725 358 831 0725
    ## 646     432 979 7292 432 979 7292
    ## 647     205 382-5599 205 382 5599
    ## 648     208 794-9612 208 794 9612
    ## 649     728 662 3934 728 662 3934
    ## 650     380-918-8572 380 918 8572
    ## 651     905 742-3525 905 742 3525
    ## 652     151 434 6989 151 434 6989
    ## 653     755 544 2629 755 544 2629
    ## 654     633 181 4494 633 181 4494
    ## 655     346 706 5964 346 706 5964
    ## 656     688 690 2184 688 690 2184
    ## 657     618 717 1697 618 717 1697
    ## 658     185-321-6877 185 321 6877
    ## 659     147 535-3529 147 535 3529
    ## 660     152 912-4118 152 912 4118
    ## 661     726 943 7486 726 943 7486
    ## 662     634 521-4714 634 521 4714
    ## 663     670-248-0186 670 248 0186
    ## 664     121 509-7306 121 509 7306
    ## 665     105 635-5212 105 635 5212
    ## 666     732 168-0110 732 168 0110
    ## 667     364 834 3150 364 834 3150
    ## 668     176 508 2778 176 508 2778
    ## 669     120 941 0833 120 941 0833
    ## 670     670 902 3199 670 902 3199
    ## 671     214 250-8756 214 250 8756
    ## 672     119 975-8484 119 975 8484
    ## 673     297 484 3285 297 484 3285
    ## 674     489 534-6272 489 534 6272
    ## 675     610 716-5732 610 716 5732
    ## 676     456 925-4236 456 925 4236
    ## 677     743-103-7645 743 103 7645
    ## 678     432-281-3682 432 281 3682
    ## 679     167 144-9470 167 144 9470
    ## 680     648 685 6188 648 685 6188
    ## 681     548 191 4898 548 191 4898
    ## 682     288 110 9483 288 110 9483
    ## 683     946-558-5801 946 558 5801
    ## 684     388 744 9637 388 744 9637
    ## 685     506 463 9129 506 463 9129
    ## 686     848 149-5208 848 149 5208
    ## 687     970 908-2298 970 908 2298
    ## 688     843 120-5653 843 120 5653
    ## 689     306 394 8640 306 394 8640
    ## 690     170-641-3537 170 641 3537
    ## 691     860 723 5066 860 723 5066
    ## 692     814-895-6610 814 895 6610
    ## 693     139 727-9901 139 727 9901
    ## 694     598 735 8557 598 735 8557
    ## 695     593 895 6761 593 895 6761
    ## 696     817 824-3849 817 824 3849
    ## 697     508 484 9738 508 484 9738
    ## 698     719 489 4724 719 489 4724
    ## 699     503-671-4901 503 671 4901
    ## 700     275 649 8183 275 649 8183
    ## 701     968 130 7012 968 130 7012
    ## 702     290 367 6676 290 367 6676
    ## 703     499 766 9941 499 766 9941
    ## 704     538-393-2243 538 393 2243
    ## 705     540 362-7136 540 362 7136
    ## 706     802 910-1742 802 910 1742
    ## 707     845 544 4748 845 544 4748
    ## 708     784-458-8425 784 458 8425
    ## 709     365 217-0634 365 217 0634
    ## 710     708 500 2758 708 500 2758
    ## 711     594 797-7729 594 797 7729
    ## 712     982 555 9504 982 555 9504
    ## 713     477-307-3338 477 307 3338
    ## 714     744-301-1148 744 301 1148
    ## 715     389 484 8888 389 484 8888
    ## 716     739 303 6128 739 303 6128
    ## 717     175 905 9962 175 905 9962
    ## 718     900 462-1379 900 462 1379
    ## 719     878-636-2294 878 636 2294
    ## 720     998 692-1900 998 692 1900
    ## 721     994 421 8642 994 421 8642
    ## 722     304-225-5895 304 225 5895
    ## 723     931-522-5498 931 522 5498
    ## 724     838 898 2275 838 898 2275
    ## 725     718 187 7125 718 187 7125
    ## 726     943 561 8955 943 561 8955
    ## 727     341 261 6456 341 261 6456
    ## 728     507 483-3618 507 483 3618
    ## 729     826 266 0205 826 266 0205
    ## 730     380 449-7849 380 449 7849
    ## 731     589-975-0198 589 975 0198
    ## 732     657 680 8781 657 680 8781
    ## 733     501-668-7869 501 668 7869
    ## 734     224 365 8299 224 365 8299
    ## 735     747 588 1968 747 588 1968
    ## 736     706 836-7047 706 836 7047
    ## 737     698 462-6742 698 462 6742
    ## 738     748 446 1257 748 446 1257
    ## 739     525-552-4162 525 552 4162
    ## 740     635-714-8302 635 714 8302
    ## 741     114 668 2834 114 668 2834
    ## 742     404 788 2855 404 788 2855
    ## 743     673 292 2444 673 292 2444
    ## 744     136 788 1426 136 788 1426
    ## 745     564 677 3934 564 677 3934
    ## 746     213 981 7762 213 981 7762
    ## 747     453-556-0852 453 556 0852
    ## 748     423 593 4483 423 593 4483
    ## 749     820 609 7454 820 609 7454
    ## 750     131-641-1331 131 641 1331
    ## 751     824 540 9579 824 540 9579
    ## 752     753 726-0123 753 726 0123
    ## 753     739 737 9041 739 737 9041
    ## 754     609-332-7370 609 332 7370
    ## 755     426-182-1365 426 182 1365
    ## 756     347 782-5787 347 782 5787
    ## 757     898-210-6218 898 210 6218
    ## 758     938 394 0411 938 394 0411
    ## 759     904 844 1759 904 844 1759
    ## 760     658-861-4306 658 861 4306
    ## 761     716 184-1232 716 184 1232
    ## 762     380-105-1757 380 105 1757
    ## 763     969 555-0453 969 555 0453
    ## 764     249 452 4370 249 452 4370
    ## 765     443 384 8253 443 384 8253
    ## 766     711 289 2247 711 289 2247
    ## 767     343-973-0193 343 973 0193
    ## 768     367 650 3720 367 650 3720
    ## 769     265 286-5671 265 286 5671
    ## 770     148 630 8560 148 630 8560
    ## 771     413-727-2672 413 727 2672
    ## 772     162 332-5838 162 332 5838
    ## 773     229 604-7790 229 604 7790
    ## 774     892 301-0333 892 301 0333
    ## 775     148 501 5084 148 501 5084
    ## 776     564 780 8272 564 780 8272
    ## 777     524 190 0899 524 190 0899
    ## 778     463 792 2782 463 792 2782
    ## 779     331 472-8624 331 472 8624
    ## 780     522-286-5318 522 286 5318
    ## 781     310 719 4550 310 719 4550
    ## 782     314-360-4288 314 360 4288
    ## 783     301 534-5754 301 534 5754
    ## 784     822 271 5719 822 271 5719
    ## 785     341 473-0639 341 473 0639
    ## 786     835 882-3693 835 882 3693
    ## 787     465-550-6610 465 550 6610
    ## 788     388 100 1482 388 100 1482
    ## 789     589 194-0523 589 194 0523
    ## 790     982 842 4913 982 842 4913
    ## 791     671 913 4563 671 913 4563
    ## 792     144 468 5864 144 468 5864
    ## 793     194 344 4039 194 344 4039
    ## 794     331 747 5714 331 747 5714
    ## 795     221-190-1449 221 190 1449
    ## 796     322 843-0185 322 843 0185
    ## 797     676-614-9095 676 614 9095
    ## 798     190 975-2514 190 975 2514
    ## 799     909 382-3774 909 382 3774
    ## 800     956 257 9319 956 257 9319
    ## 801     362 145 8268 362 145 8268
    ## 802     570 727 3998 570 727 3998
    ## 803     225 964 9193 225 964 9193
    ## 804     227 419 9482 227 419 9482
    ## 805     949 360 7605 949 360 7605
    ## 806     347 896-3463 347 896 3463
    ## 807     794 939 9735 794 939 9735
    ## 808     413 754 3034 413 754 3034
    ## 809     806 730 0459 806 730 0459
    ## 810     791 195 8909 791 195 8909
    ## 811     691-318-3535 691 318 3535
    ## 812     852-386-6029 852 386 6029
    ## 813     380 682 7795 380 682 7795
    ## 814     355 550-1392 355 550 1392
    ## 815     247 586 4579 247 586 4579
    ## 816     235 257 1041 235 257 1041
    ## 817     705 456-1905 705 456 1905
    ## 818     106 756 2785 106 756 2785
    ## 819     836 207-8419 836 207 8419
    ## 820     306 552-1875 306 552 1875
    ## 821     729-102-7511 729 102 7511
    ## 822     795 583 0958 795 583 0958
    ## 823     700-431-3918 700 431 3918
    ## 824     929 632 1068 929 632 1068
    ## 825     763 906 2495 763 906 2495
    ## 826     469 976 6796 469 976 6796
    ## 827     887-657-4143 887 657 4143
    ## 828     574-438-5329 574 438 5329
    ## 829     319 127 9518 319 127 9518
    ## 830     429 960 9710 429 960 9710
    ## 831     419 646 0299 419 646 0299
    ## 832     192 343 8515 192 343 8515
    ## 833     521 336 8581 521 336 8581
    ## 834     776 367 6109 776 367 6109
    ## 835     470 367 1392 470 367 1392
    ## 836     944 189-7555 944 189 7555
    ## 837     998-931-4783 998 931 4783
    ## 838     362-178-6307 362 178 6307
    ## 839     458 404-9558 458 404 9558
    ## 840     212 286 7936 212 286 7936
    ## 841     481 522-1039 481 522 1039
    ## 842     376 611-4588 376 611 4588
    ## 843     936 193 9690 936 193 9690
    ## 844     641 544-6549 641 544 6549
    ## 845     797-870-7818 797 870 7818
    ## 846     693 907 5353 693 907 5353
    ## 847     332 973 4943 332 973 4943
    ## 848     929 622 9077 929 622 9077
    ## 849     649-379-5361 649 379 5361
    ## 850     572 748-6932 572 748 6932
    ## 851     395-892-5646 395 892 5646
    ## 852     221-628-9561 221 628 9561
    ## 853     227 801-6148 227 801 6148
    ## 854     549-649-1864 549 649 1864
    ## 855     342 941 0439 342 941 0439
    ## 856     701 390 9814 701 390 9814
    ## 857     519 573-6576 519 573 6576
    ## 858     919-342-0230 919 342 0230
    ## 859     364-759-2705 364 759 2705
    ## 860     949 543 7906 949 543 7906
    ## 861     942 732 6403 942 732 6403
    ## 862     900 586-1787 900 586 1787
    ## 863     308-607-9855 308 607 9855
    ## 864     764 645 5740 764 645 5740
    ## 865     472-337-8838 472 337 8838
    ## 866     791 847 7278 791 847 7278
    ## 867     128 805 3828 128 805 3828
    ## 868     365-832-0674 365 832 0674
    ## 869     123-282-3494 123 282 3494
    ## 870     285 424 4318 285 424 4318
    ## 871     452 352 1387 452 352 1387
    ## 872     129-377-8159 129 377 8159
    ## 873     222 143 3131 222 143 3131
    ## 874     162-451-0594 162 451 0594
    ## 875     239 325-5321 239 325 5321
    ## 876     436-422-6171 436 422 6171
    ## 877     605 284 4260 605 284 4260
    ## 878     929-102-5905 929 102 5905
    ## 879     847 507 8268 847 507 8268
    ## 880     452-811-8088 452 811 8088
    ## 881     799 143 1677 799 143 1677
    ## 882     196 756 4555 196 756 4555
    ## 883     119-444-0817 119 444 0817
    ## 884     885 454 0883 885 454 0883
    ## 885     945-998-0444 945 998 0444
    ## 886     367 897-7969 367 897 7969
    ## 887     163 241 9321 163 241 9321
    ## 888     594-176-5811 594 176 5811
    ## 889     621 874-9973 621 874 9973
    ## 890     332 963 4103 332 963 4103
    ## 891     389 318 3975 389 318 3975
    ## 892     894-593-7953 894 593 7953
    ## 893     561-266-7842 561 266 7842
    ## 894     354-958-8052 354 958 8052
    ## 895     151 921 2775 151 921 2775
    ## 896     901 140 3759 901 140 3759
    ## 897     426 342-7378 426 342 7378
    ## 898     333-520-4811 333 520 4811
    ## 899     765 191-1797 765 191 1797
    ## 900     850 914 9348 850 914 9348
    ## 901     246 272 9019 246 272 9019
    ## 902     400 250-0871 400 250 0871
    ## 903     504 419 9191 504 419 9191
    ## 904     434 725-0561 434 725 0561
    ## 905     231 863 7554 231 863 7554
    ## 906     969 207-3261 969 207 3261
    ## 907     939 253 9048 939 253 9048
    ## 908     879-154-4494 879 154 4494
    ## 909     577 786 2546 577 786 2546
    ## 910     994 688-3259 994 688 3259
    ## 911     841-717-4447 841 717 4447
    ## 912     397-353-6309 397 353 6309
    ## 913     558 191 4548 558 191 4548
    ## 914     905 768 2297 905 768 2297
    ## 915     348 522 2051 348 522 2051
    ## 916     307-323-6861 307 323 6861
    ## 917     663 886 2487 663 886 2487
    ## 918     274 944 6097 274 944 6097
    ## 919     494 308-3048 494 308 3048
    ## 920     328 307 0875 328 307 0875
    ## 921     673 524 3504 673 524 3504
    ## 922     934 721 0615 934 721 0615
    ## 923     102-957-6486 102 957 6486
    ## 924     715 288-8832 715 288 8832
    ## 925         925 8846     925 8846
    ## 926     637 281 4111 637 281 4111
    ## 927     594 448-6242 594 448 6242
    ## 928     335 802 2651 335 802 2651
    ## 929     560 699 9908 560 699 9908
    ## 930     451 163 0102 451 163 0102
    ## 931     613 800 0835 613 800 0835
    ## 932     370 453 5800 370 453 5800
    ## 933     192 507 5411 192 507 5411
    ## 934     182-227-4838 182 227 4838
    ## 935     647 126 2332 647 126 2332
    ## 936     606 125 6957 606 125 6957
    ## 937     728-404-5558 728 404 5558
    ## 938     506 812 6052 506 812 6052
    ## 939     427-665-3475 427 665 3475
    ## 940     808 739 7162 808 739 7162
    ## 941     375-978-3305 375 978 3305
    ## 942     181-708-2089 181 708 2089
    ## 943     802 810-5574 802 810 5574
    ## 944     242-540-4234 242 540 4234
    ## 945     105 116 9695 105 116 9695
    ## 946     867 891-0871 867 891 0871
    ## 947     945 144 7892 945 144 7892
    ## 948     531 895 6695 531 895 6695
    ## 949     533 213-4368 533 213 4368
    ## 950     894 810 2674 894 810 2674
    ## 951     943 812 6349 943 812 6349
    ## 952     476 168 4235 476 168 4235
    ## 953     931 385 6757 931 385 6757
    ## 954     324 188-6781 324 188 6781
    ## 955     100 378 8095 100 378 8095
    ## 956     459 572 0244 459 572 0244
    ## 957     397-362-5469 397 362 5469
    ## 958     102 928-7959 102 928 7959
    ## 959     439 568-6611 439 568 6611
    ## 960     767 205-0604 767 205 0604
    ## 961     890 548-9219 890 548 9219
    ## 962     938 982 5585 938 982 5585
    ## 963     769-472-2992 769 472 2992
    ## 964     190 204 1154 190 204 1154
    ## 965     649 925-8489 649 925 8489
    ## 966     321 616 1013 321 616 1013
    ## 967     156 837-4491 156 837 4491
    ## 968     178-232-0815 178 232 0815
    ## 969     882 304-9032 882 304 9032
    ## 970     828 549 6666 828 549 6666
    ## 971     280 544 4554 280 544 4554
    ## 972     183 208 5054 183 208 5054
    ## 973     971 548-6611 971 548 6611
    ## 974     828 153-5819 828 153 5819
    ## 975     203 448 1522 203 448 1522
    ## 976     900-871-9056 900 871 9056
    ## 977     406-167-1379 406 167 1379
    ## 978     906 850-9192 906 850 9192
    ## 979     859 777-8245 859 777 8245
    ## 980     641-635-8466 641 635 8466
    ## 981     807-671-6158 807 671 6158
    ## 982     589 270-7518 589 270 7518
    ## 983     768 529 8051 768 529 8051
    ## 984     220 660 0306 220 660 0306
    ## 985     928-179-7556 928 179 7556
    ## 986     153 756 0278 153 756 0278
    ## 987     273 829 9197 273 829 9197
    ## 988     269 463 0911 269 463 0911
    ## 989     696 984 8826 696 984 8826
    ## 990     905-903-5258 905 903 5258
    ## 991     267 332 4709 267 332 4709
    ## 992     146 129-5118 146 129 5118
    ## 993     927 747-9822 927 747 9822
    ## 994     534 216 6666 534 216 6666
    ## 995     481 479-7013 481 479 7013
    ## 996     971 175 2968 971 175 2968
    ## 997     716 777-3762 716 777 3762
    ## 998     274-863-3205 274 863 3205
    ## 999     217 589-0596 217 589 0596
    ## 1000    928 445 5474 928 445 5474
    ## 1001    858 990 5153 858 990 5153
    ## 1002    731-813-2043 731 813 2043
    ## 1003    563-732-6802 563 732 6802
    ## 1004    145 725 4021 145 725 4021
    ## 1005    931 311 5801 931 311 5801
    ## 1006    637 782-6989 637 782 6989
    ## 1007    172 990 3485 172 990 3485
    ## 1008    872 325 4341 872 325 4341
    ## 1009    359 803-9809 359 803 9809
    ## 1010    152 790 8238 152 790 8238
    ## 1011    330 561 9257 330 561 9257
    ## 1012    437 420 7546 437 420 7546
    ## 1013    495 632 4027 495 632 4027
    ## 1014    416 788-2844 416 788 2844
    ## 1015    311-305-4367 311 305 4367
    ## 1016    817-400-0481 817 400 0481
    ## 1017    430 723 1079 430 723 1079
    ## 1018    729 609-4819 729 609 4819
    ## 1019    201 737-4409 201 737 4409
    ## 1020    137 611-3694 137 611 3694
    ## 1021    226 490 8696 226 490 8696
    ## 1022    123 570 8640 123 570 8640
    ## 1023    665 803 2453 665 803 2453
    ## 1024    812 869-6263 812 869 6263
    ## 1025    639 132 6386 639 132 6386
    ## 1026    194 198-0504 194 198 0504
    ## 1027    437 886 0753 437 886 0753
    ## 1028    626 756 5089 626 756 5089
    ## 1029    299 137-6993 299 137 6993
    ## 1030    714 950 3364 714 950 3364
    ## 1031    653-786-5985 653 786 5985
    ## 1032    518 286 5956 518 286 5956
    ## 1033    194 960 2145 194 960 2145
    ## 1034    362-136-1153 362 136 1153
    ## 1035    376-456-0697 376 456 0697
    ## 1036    657 832 1189 657 832 1189
    ## 1037    962-918-6117 962 918 6117
    ## 1038    692 929 3592 692 929 3592
    ## 1039    805-877-3887 805 877 3887
    ## 1040    739 710-2966 739 710 2966
    ## 1041    819 732 4132 819 732 4132
    ## 1042    367 221 9710 367 221 9710
    ## 1043    361 154 1789 361 154 1789
    ## 1044    680 488 1182 680 488 1182
    ## 1045    928 638 1186 928 638 1186
    ## 1046    588-693-9875 588 693 9875
    ## 1047    681-308-7915 681 308 7915
    ## 1048    783 647 8490 783 647 8490
    ## 1049    897 847 0632 897 847 0632
    ## 1050    150 952 4453 150 952 4453
    ## 1051    322-884-3020 322 884 3020
    ## 1052    176-313-5403 176 313 5403
    ## 1053    487 109 4196 487 109 4196
    ## 1054    477 182-4689 477 182 4689
    ## 1055    544 382 8289 544 382 8289
    ## 1056    781 543 7456 781 543 7456
    ## 1057    911 829 6476 911 829 6476
    ## 1058    525 362-5532 525 362 5532
    ## 1059    517 986 3426 517 986 3426
    ## 1060    838 220 5397 838 220 5397
    ## 1061    687 887-6766 687 887 6766
    ## 1062    179 163 0902 179 163 0902
    ## 1063    539 137 8983 539 137 8983
    ## 1064    733-154-0094 733 154 0094
    ## 1065    639 881 3693 639 881 3693
    ## 1066    291-830-3017 291 830 3017
    ## 1067    637 100-0509 637 100 0509
    ## 1068    750 520 0167 750 520 0167
    ## 1069    676 485 8963 676 485 8963
    ## 1070    135 566 5090 135 566 5090
    ## 1071    337 260 4996 337 260 4996
    ## 1072    371 185 2377 371 185 2377
    ## 1073    280 461 1386 280 461 1386
    ## 1074    603 149-7268 603 149 7268
    ## 1075    364 792-5553 364 792 5553
    ## 1076    496-429-1314 496 429 1314
    ## 1077    459 671 4698 459 671 4698
    ## 1078    486-268-3312 486 268 3312
    ## 1079    497-518-4050 497 518 4050
    ## 1080    535 685-8273 535 685 8273
    ## 1081    859 495 4050 859 495 4050
    ## 1082    826 738-8316 826 738 8316
    ## 1083    724-134-3870 724 134 3870
    ## 1084    554 269-8937 554 269 8937
    ## 1085    125-578-4253 125 578 4253
    ## 1086    614 800 2861 614 800 2861
    ## 1087    487-232-4449 487 232 4449
    ## 1088    298 135-0900 298 135 0900
    ## 1089    392 183-7831 392 183 7831
    ## 1090    606 596-1029 606 596 1029
    ## 1091    384 953-4795 384 953 4795
    ## 1092    855 811-8811 855 811 8811
    ## 1093    253-374-7102 253 374 7102
    ## 1094    419 295-9580 419 295 9580
    ## 1095    802 102 8345 802 102 8345
    ## 1096    417 393 0050 417 393 0050
    ## 1097    787-624-8443 787 624 8443
    ## 1098    919 486-4251 919 486 4251
    ## 1099    341 824 5322 341 824 5322
    ## 1100    415 551 1608 415 551 1608
    ## 1101    392 495-7961 392 495 7961
    ## 1102    473-238-3324 473 238 3324
    ## 1103    506 760-3043 506 760 3043
    ## 1104    876-834-0624 876 834 0624
    ## 1105    919 611 6170 919 611 6170
    ## 1106    146 699-3488 146 699 3488
    ## 1107    261 434 7760 261 434 7760
    ## 1108    617 310 2684 617 310 2684
    ## 1109    182-535-3412 182 535 3412
    ## 1110    506 129 1694 506 129 1694
    ## 1111    302 339-0791 302 339 0791
    ## 1112    446 229-4342 446 229 4342
    ## 1113    249 602-6985 249 602 6985
    ## 1114    150 905-6938 150 905 6938
    ## 1115    313 990 8823 313 990 8823
    ## 1116    656-941-5355 656 941 5355
    ## 1117    116 689-6617 116 689 6617
    ## 1118    955 324 5981 955 324 5981
    ## 1119    175 808 2189 175 808 2189
    ## 1120    896 993-8555 896 993 8555
    ## 1121    105-687-6500 105 687 6500
    ## 1122    757 524 2964 757 524 2964
    ## 1123    201 374 2424 201 374 2424
    ## 1124    466 912-8401 466 912 8401
    ## 1125    766 112 6143 766 112 6143
    ## 1126    783-463-4865 783 463 4865
    ## 1127    853-803-9900 853 803 9900
    ## 1128    347 851-5388 347 851 5388
    ## 1129    992 114 6973 992 114 6973
    ## 1130    316-212-7309 316 212 7309
    ## 1131    301 672 1092 301 672 1092
    ## 1132    795 137 0201 795 137 0201
    ## 1133    381-883-5497 381 883 5497
    ## 1134    100-531-4642 100 531 4642
    ## 1135    994 923 6634 994 923 6634
    ## 1136    920 355 8404 920 355 8404
    ## 1137    441 445-6532 441 445 6532
    ## 1138    325 795 2455 325 795 2455
    ## 1139    593 829 6250 593 829 6250
    ## 1140    566-482-9004 566 482 9004
    ## 1141    542 537 6770 542 537 6770
    ## 1142    716 191 1741 716 191 1741
    ## 1143    491-727-7162 491 727 7162
    ## 1144    167-336-5660 167 336 5660
    ## 1145    358 831 0725 358 831 0725
    ## 1146    432 979 7292 432 979 7292
    ## 1147    205 382-5599 205 382 5599
    ## 1148    208 794-9612 208 794 9612
    ## 1149    728 662 3934 728 662 3934
    ## 1150    380-918-8572 380 918 8572
    ## 1151    905 742-3525 905 742 3525
    ## 1152    151 434 6989 151 434 6989
    ## 1153    755 544 2629 755 544 2629
    ## 1154    633 181 4494 633 181 4494
    ## 1155    346 706 5964 346 706 5964
    ## 1156    688 690 2184 688 690 2184
    ## 1157    618 717 1697 618 717 1697
    ## 1158    185-321-6877 185 321 6877
    ## 1159    147 535-3529 147 535 3529
    ## 1160    152 912-4118 152 912 4118
    ## 1161    726 943 7486 726 943 7486
    ## 1162    634 521-4714 634 521 4714
    ## 1163    670-248-0186 670 248 0186
    ## 1164    121 509-7306 121 509 7306
    ## 1165    105 635-5212 105 635 5212
    ## 1166    732 168-0110 732 168 0110
    ## 1167    364 834 3150 364 834 3150
    ## 1168    176 508 2778 176 508 2778
    ## 1169    120 941 0833 120 941 0833
    ## 1170    670 902 3199 670 902 3199
    ## 1171    214 250-8756 214 250 8756
    ## 1172    119 975-8484 119 975 8484
    ## 1173    297 484 3285 297 484 3285
    ## 1174    489 534-6272 489 534 6272
    ## 1175    610 716-5732 610 716 5732
    ## 1176    456 925-4236 456 925 4236
    ## 1177    743-103-7645 743 103 7645
    ## 1178    432-281-3682 432 281 3682
    ## 1179    167 144-9470 167 144 9470
    ## 1180    648 685 6188 648 685 6188
    ## 1181    548 191 4898 548 191 4898
    ## 1182    288 110 9483 288 110 9483
    ## 1183    946-558-5801 946 558 5801
    ## 1184    388 744 9637 388 744 9637
    ## 1185    506 463 9129 506 463 9129
    ## 1186    848 149-5208 848 149 5208
    ## 1187    970 908-2298 970 908 2298
    ## 1188    843 120-5653 843 120 5653
    ## 1189    306 394 8640 306 394 8640
    ## 1190    170-641-3537 170 641 3537
    ## 1191    860 723 5066 860 723 5066
    ## 1192    814-895-6610 814 895 6610
    ## 1193    139 727-9901 139 727 9901
    ## 1194    598 735 8557 598 735 8557
    ## 1195    593 895 6761 593 895 6761
    ## 1196    817 824-3849 817 824 3849
    ## 1197    508 484 9738 508 484 9738
    ## 1198    719 489 4724 719 489 4724
    ## 1199    503-671-4901 503 671 4901
    ## 1200    275 649 8183 275 649 8183
    ## 1201    968 130 7012 968 130 7012
    ## 1202    290 367 6676 290 367 6676
    ## 1203    499 766 9941 499 766 9941
    ## 1204    538-393-2243 538 393 2243
    ## 1205    540 362-7136 540 362 7136
    ## 1206    802 910-1742 802 910 1742
    ## 1207    845 544 4748 845 544 4748
    ## 1208    784-458-8425 784 458 8425
    ## 1209    365 217-0634 365 217 0634
    ## 1210    708 500 2758 708 500 2758
    ## 1211    594 797-7729 594 797 7729
    ## 1212    982 555 9504 982 555 9504
    ## 1213    477-307-3338 477 307 3338
    ## 1214    744-301-1148 744 301 1148
    ## 1215    389 484 8888 389 484 8888
    ## 1216    739 303 6128 739 303 6128
    ## 1217    175 905 9962 175 905 9962
    ## 1218    900 462-1379 900 462 1379
    ## 1219    878-636-2294 878 636 2294
    ## 1220    998 692-1900 998 692 1900
    ## 1221    994 421 8642 994 421 8642
    ## 1222    304-225-5895 304 225 5895
    ## 1223    931-522-5498 931 522 5498
    ## 1224    838 898 2275 838 898 2275
    ## 1225    718 187 7125 718 187 7125
    ## 1226    943 561 8955 943 561 8955
    ## 1227    341 261 6456 341 261 6456
    ## 1228    507 483-3618 507 483 3618
    ## 1229    826 266 0205 826 266 0205
    ## 1230    380 449-7849 380 449 7849
    ## 1231    589-975-0198 589 975 0198
    ## 1232    657 680 8781 657 680 8781
    ## 1233    501-668-7869 501 668 7869
    ## 1234    224 365 8299 224 365 8299
    ## 1235    747 588 1968 747 588 1968
    ## 1236    706 836-7047 706 836 7047
    ## 1237    698 462-6742 698 462 6742
    ## 1238    748 446 1257 748 446 1257
    ## 1239    525-552-4162 525 552 4162
    ## 1240    635-714-8302 635 714 8302
    ## 1241    114 668 2834 114 668 2834
    ## 1242    404 788 2855 404 788 2855
    ## 1243    673 292 2444 673 292 2444
    ## 1244    136 788 1426 136 788 1426
    ## 1245    564 677 3934 564 677 3934
    ## 1246    213 981 7762 213 981 7762
    ## 1247    453-556-0852 453 556 0852
    ## 1248    423 593 4483 423 593 4483
    ## 1249    820 609 7454 820 609 7454
    ## 1250    131-641-1331 131 641 1331
    ## 1251    824 540 9579 824 540 9579
    ## 1252    753 726-0123 753 726 0123
    ## 1253    739 737 9041 739 737 9041
    ## 1254    609-332-7370 609 332 7370
    ## 1255    426-182-1365 426 182 1365
    ## 1256    347 782-5787 347 782 5787
    ## 1257    898-210-6218 898 210 6218
    ## 1258    938 394 0411 938 394 0411
    ## 1259    904 844 1759 904 844 1759
    ## 1260    658-861-4306 658 861 4306
    ## 1261    716 184-1232 716 184 1232
    ## 1262    380-105-1757 380 105 1757
    ## 1263    969 555-0453 969 555 0453
    ## 1264    249 452 4370 249 452 4370
    ## 1265    443 384 8253 443 384 8253
    ## 1266    711 289 2247 711 289 2247
    ## 1267    343-973-0193 343 973 0193
    ## 1268    367 650 3720 367 650 3720
    ## 1269    265 286-5671 265 286 5671
    ## 1270    148 630 8560 148 630 8560
    ## 1271    413-727-2672 413 727 2672
    ## 1272            1623         1623
    ## 1273    229 604-7790 229 604 7790
    ## 1274    892 301-0333 892 301 0333
    ## 1275    148 501 5084 148 501 5084
    ## 1276    564 780 8272 564 780 8272
    ## 1277    524 190 0899 524 190 0899
    ## 1278    463 792 2782 463 792 2782
    ## 1279    331 472-8624 331 472 8624
    ## 1280    522-286-5318 522 286 5318
    ## 1281    310 719 4550 310 719 4550
    ## 1282    314-360-4288 314 360 4288
    ## 1283    301 534-5754 301 534 5754
    ## 1284    822 271 5719 822 271 5719
    ## 1285    341 473-0639 341 473 0639
    ## 1286    835 882-3693 835 882 3693
    ## 1287    465-550-6610 465 550 6610
    ## 1288    388 100 1482 388 100 1482
    ## 1289    589 194-0523 589 194 0523
    ## 1290    982 842 4913 982 842 4913
    ## 1291    671 913 4563 671 913 4563
    ## 1292    144 468 5864 144 468 5864
    ## 1293    194 344 4039 194 344 4039
    ## 1294    331 747 5714 331 747 5714
    ## 1295    221-190-1449 221 190 1449
    ## 1296    322 843-0185 322 843 0185
    ## 1297    676-614-9095 676 614 9095
    ## 1298    190 975-2514 190 975 2514
    ## 1299    909 382-3774 909 382 3774
    ## 1300    956 257 9319 956 257 9319
    ## 1301    362 145 8268 362 145 8268
    ## 1302    570 727 3998 570 727 3998
    ## 1303    225 964 9193 225 964 9193
    ## 1304    227 419 9482 227 419 9482
    ## 1305    949 360 7605 949 360 7605
    ## 1306    347 896-3463 347 896 3463
    ## 1307    794 939 9735 794 939 9735
    ## 1308    413 754 3034 413 754 3034
    ## 1309    806 730 0459 806 730 0459
    ## 1310    791 195 8909 791 195 8909
    ## 1311    691-318-3535 691 318 3535
    ## 1312    852-386-6029 852 386 6029
    ## 1313    380 682 7795 380 682 7795
    ## 1314    355 550-1392 355 550 1392
    ## 1315    247 586 4579 247 586 4579
    ## 1316    235 257 1041 235 257 1041
    ## 1317    705 456-1905 705 456 1905
    ## 1318    106 756 2785 106 756 2785
    ## 1319    836 207-8419 836 207 8419
    ## 1320    306 552-1875 306 552 1875
    ## 1321    729-102-7511 729 102 7511
    ## 1322    795 583 0958 795 583 0958
    ## 1323    700-431-3918 700 431 3918
    ## 1324    929 632 1068 929 632 1068
    ## 1325    763 906 2495 763 906 2495
    ## 1326    469 976 6796 469 976 6796
    ## 1327    887-657-4143 887 657 4143
    ## 1328    574-438-5329 574 438 5329
    ## 1329    319 127 9518 319 127 9518
    ## 1330    429 960 9710 429 960 9710
    ## 1331    419 646 0299 419 646 0299
    ## 1332    192 343 8515 192 343 8515
    ## 1333    521 336 8581 521 336 8581
    ## 1334    776 367 6109 776 367 6109
    ## 1335    470 367 1392 470 367 1392
    ## 1336    944 189-7555 944 189 7555
    ## 1337    998-931-4783 998 931 4783
    ## 1338    362-178-6307 362 178 6307
    ## 1339    458 404-9558 458 404 9558
    ## 1340    212 286 7936 212 286 7936
    ## 1341    481 522-1039 481 522 1039
    ## 1342    376 611-4588 376 611 4588
    ## 1343    936 193 9690 936 193 9690
    ## 1344    641 544-6549 641 544 6549
    ## 1345    797-870-7818 797 870 7818
    ## 1346    693 907 5353 693 907 5353
    ## 1347    332 973 4943 332 973 4943
    ## 1348    929 622 9077 929 622 9077
    ## 1349    649-379-5361 649 379 5361
    ## 1350    572 748-6932 572 748 6932
    ## 1351    395-892-5646 395 892 5646
    ## 1352    221-628-9561 221 628 9561
    ## 1353    227 801-6148 227 801 6148
    ## 1354    549-649-1864 549 649 1864
    ## 1355    342 941 0439 342 941 0439
    ## 1356    701 390 9814 701 390 9814
    ## 1357    519 573-6576 519 573 6576
    ## 1358    919-342-0230 919 342 0230
    ## 1359    364-759-2705 364 759 2705
    ## 1360    949 543 7906 949 543 7906
    ## 1361    942 732 6403 942 732 6403
    ## 1362    900 586-1787 900 586 1787
    ## 1363    308-607-9855 308 607 9855
    ## 1364    764 645 5740 764 645 5740
    ## 1365    472-337-8838 472 337 8838
    ## 1366    791 847 7278 791 847 7278
    ## 1367    128 805 3828 128 805 3828
    ## 1368    365-832-0674 365 832 0674
    ## 1369    123-282-3494 123 282 3494
    ## 1370    285 424 4318 285 424 4318
    ## 1371    452 352 1387 452 352 1387
    ## 1372    129-377-8159 129 377 8159
    ## 1373    222 143 3131 222 143 3131
    ## 1374    162-451-0594 162 451 0594
    ## 1375    239 325-5321 239 325 5321
    ## 1376    436-422-6171 436 422 6171
    ## 1377    605 284 4260 605 284 4260
    ## 1378    929-102-5905 929 102 5905
    ## 1379    847 507 8268 847 507 8268
    ## 1380    452-811-8088 452 811 8088
    ## 1381    799 143 1677 799 143 1677
    ## 1382    196 756 4555 196 756 4555
    ## 1383    119-444-0817 119 444 0817
    ## 1384    885 454 0883 885 454 0883
    ## 1385    945-998-0444 945 998 0444
    ## 1386    367 897-7969 367 897 7969
    ## 1387    163 241 9321 163 241 9321
    ## 1388    594-176-5811 594 176 5811
    ## 1389    621 874-9973 621 874 9973
    ## 1390    332 963 4103 332 963 4103
    ## 1391    389 318 3975 389 318 3975
    ## 1392    894-593-7953 894 593 7953
    ## 1393    561-266-7842 561 266 7842
    ## 1394    354-958-8052 354 958 8052
    ## 1395    151 921 2775 151 921 2775
    ## 1396    901 140 3759 901 140 3759
    ## 1397    426 342-7378 426 342 7378
    ## 1398    333-520-4811 333 520 4811
    ## 1399    765 191-1797 765 191 1797
    ## 1400    850 914 9348 850 914 9348
    ## 1401    246 272 9019 246 272 9019
    ## 1402    400 250-0871 400 250 0871
    ## 1403    504 419 9191 504 419 9191
    ## 1404    434 725-0561 434 725 0561
    ## 1405    231 863 7554 231 863 7554
    ## 1406    969 207-3261 969 207 3261
    ## 1407    939 253 9048 939 253 9048
    ## 1408    879-154-4494 879 154 4494
    ## 1409    577 786 2546 577 786 2546
    ## 1410    994 688-3259 994 688 3259
    ## 1411    841-717-4447 841 717 4447
    ## 1412    397-353-6309 397 353 6309
    ## 1413    558 191 4548 558 191 4548
    ## 1414    905 768 2297 905 768 2297
    ## 1415    348 522 2051 348 522 2051
    ## 1416    307-323-6861 307 323 6861
    ## 1417    663 886 2487 663 886 2487
    ## 1418    274 944 6097 274 944 6097
    ## 1419    494 308-3048 494 308 3048
    ## 1420    328 307 0875 328 307 0875
    ## 1421    673 524 3504 673 524 3504
    ## 1422    934 721 0615 934 721 0615
    ## 1423    102-957-6486 102 957 6486
    ## 1424    715 288-8832 715 288 8832
    ## 1425    794 925 8846 794 925 8846
    ## 1426    637 281 4111 637 281 4111
    ## 1427    594 448-6242 594 448 6242
    ## 1428    335 802 2651 335 802 2651
    ## 1429    560 699 9908 560 699 9908
    ## 1430    451 163 0102 451 163 0102
    ## 1431    613 800 0835 613 800 0835
    ## 1432    370 453 5800 370 453 5800
    ## 1433    192 507 5411 192 507 5411
    ## 1434    182-227-4838 182 227 4838
    ## 1435    647 126 2332 647 126 2332
    ## 1436    606 125 6957 606 125 6957
    ## 1437    728-404-5558 728 404 5558
    ## 1438    506 812 6052 506 812 6052
    ## 1439    427-665-3475 427 665 3475
    ## 1440    808 739 7162 808 739 7162
    ## 1441    375-978-3305 375 978 3305
    ## 1442    181-708-2089 181 708 2089
    ## 1443    802 810-5574 802 810 5574
    ## 1444    242-540-4234 242 540 4234
    ## 1445    105 116 9695 105 116 9695
    ## 1446    867 891-0871 867 891 0871
    ## 1447    945 144 7892 945 144 7892
    ## 1448    531 895 6695 531 895 6695
    ## 1449    533 213-4368 533 213 4368
    ## 1450    894 810 2674 894 810 2674
    ## 1451    943 812 6349 943 812 6349
    ## 1452    476 168 4235 476 168 4235
    ## 1453    931 385 6757 931 385 6757
    ## 1454    324 188-6781 324 188 6781
    ## 1455    100 378 8095 100 378 8095
    ## 1456    459 572 0244 459 572 0244
    ## 1457    397-362-5469 397 362 5469
    ## 1458    102 928-7959 102 928 7959
    ## 1459    439 568-6611 439 568 6611
    ## 1460    767 205-0604 767 205 0604
    ## 1461    890 548-9219 890 548 9219
    ## 1462    938 982 5585 938 982 5585
    ## 1463    769-472-2992 769 472 2992
    ## 1464    190 204 1154 190 204 1154
    ## 1465    649 925-8489 649 925 8489
    ## 1466    321 616 1013 321 616 1013
    ## 1467    156 837-4491 156 837 4491
    ## 1468    178-232-0815 178 232 0815
    ## 1469    882 304-9032 882 304 9032
    ## 1470    828 549 6666 828 549 6666
    ## 1471    280 544 4554 280 544 4554
    ## 1472    183 208 5054 183 208 5054
    ## 1473    971 548-6611 971 548 6611
    ## 1474    828 153-5819 828 153 5819
    ## 1475    203 448 1522 203 448 1522
    ## 1476    900-871-9056 900 871 9056
    ## 1477    406-167-1379 406 167 1379
    ## 1478    906 850-9192 906 850 9192
    ## 1479    859 777-8245 859 777 8245
    ## 1480    641-635-8466 641 635 8466
    ## 1481    807-671-6158 807 671 6158
    ## 1482    589 270-7518 589 270 7518
    ## 1483    768 529 8051 768 529 8051
    ## 1484    220 660 0306 220 660 0306
    ## 1485    928-179-7556 928 179 7556
    ## 1486    153 756 0278 153 756 0278
    ## 1487    273 829 9197 273 829 9197
    ## 1488    269 463 0911 269 463 0911
    ## 1489    696 984 8826 696 984 8826
    ## 1490    905-903-5258 905 903 5258
    ## 1491    267 332 4709 267 332 4709
    ## 1492    146 129-5118 146 129 5118
    ## 1493    927 747-9822 927 747 9822
    ## 1494    534 216 6666 534 216 6666
    ## 1495    481 479-7013 481 479 7013
    ## 1496    971 175 2968 971 175 2968
    ## 1497    716 777-3762 716 777 3762
    ## 1498    274-863-3205 274 863 3205
    ## 1499    217 589-0596 217 589 0596
    ## 1500    928 445 5474 928 445 5474
    ## 1501    858 990 5153 858 990 5153
    ## 1502    731-813-2043 731 813 2043
    ## 1503    563-732-6802 563 732 6802
    ## 1504    145 725 4021 145 725 4021
    ## 1505    931 311 5801 931 311 5801
    ## 1506    637 782-6989 637 782 6989
    ## 1507    172 990 3485 172 990 3485
    ## 1508    872 325 4341 872 325 4341
    ## 1509    359 803-9809 359 803 9809
    ## 1510    152 790 8238 152 790 8238
    ## 1511    330 561 9257 330 561 9257
    ## 1512    437 420 7546 437 420 7546
    ## 1513    495 632 4027 495 632 4027
    ## 1514    416 788-2844 416 788 2844
    ## 1515    311-305-4367 311 305 4367
    ## 1516    817-400-0481 817 400 0481
    ## 1517    430 723 1079 430 723 1079
    ## 1518    729 609-4819 729 609 4819
    ## 1519    201 737-4409 201 737 4409
    ## 1520    137 611-3694 137 611 3694
    ## 1521    226 490 8696 226 490 8696
    ## 1522    123 570 8640 123 570 8640
    ## 1523         665-803      665 803
    ## 1524    812 869-6263 812 869 6263
    ## 1525    639 132 6386 639 132 6386
    ## 1526    194 198-0504 194 198 0504
    ## 1527    437 886 0753 437 886 0753
    ## 1528    626 756 5089 626 756 5089
    ## 1529    299 137-6993 299 137 6993
    ## 1530    714 950 3364 714 950 3364
    ## 1531    653-786-5985 653 786 5985
    ## 1532    518 286 5956 518 286 5956
    ## 1533    194 960 2145 194 960 2145
    ## 1534    362-136-1153 362 136 1153
    ## 1535    376-456-0697 376 456 0697
    ## 1536    657 832 1189 657 832 1189
    ## 1537    962-918-6117 962 918 6117
    ## 1538    692 929 3592 692 929 3592
    ## 1539    805-877-3887 805 877 3887
    ## 1540    739 710-2966 739 710 2966
    ## 1541    819 732 4132 819 732 4132
    ## 1542    367 221 9710 367 221 9710
    ## 1543    361 154 1789 361 154 1789
    ## 1544    680 488 1182 680 488 1182
    ## 1545    928 638 1186 928 638 1186
    ## 1546    588-693-9875 588 693 9875
    ## 1547    681-308-7915 681 308 7915
    ## 1548    783 647 8490 783 647 8490
    ## 1549    897 847 0632 897 847 0632
    ## 1550    150 952 4453 150 952 4453
    ## 1551    322-884-3020 322 884 3020
    ## 1552    176-313-5403 176 313 5403
    ## 1553    487 109 4196 487 109 4196
    ## 1554    477 182-4689 477 182 4689
    ## 1555    544 382 8289 544 382 8289
    ## 1556    781 543 7456 781 543 7456
    ## 1557    911 829 6476 911 829 6476
    ## 1558    525 362-5532 525 362 5532
    ## 1559    517 986 3426 517 986 3426
    ## 1560    838 220 5397 838 220 5397
    ## 1561    687 887-6766 687 887 6766
    ## 1562    179 163 0902 179 163 0902
    ## 1563    539 137 8983 539 137 8983
    ## 1564    733-154-0094 733 154 0094
    ## 1565    639 881 3693 639 881 3693
    ## 1566    291-830-3017 291 830 3017
    ## 1567    637 100-0509 637 100 0509
    ## 1568    750 520 0167 750 520 0167
    ## 1569    676 485 8963 676 485 8963
    ## 1570    135 566 5090 135 566 5090
    ## 1571    337 260 4996 337 260 4996
    ## 1572    371 185 2377 371 185 2377
    ## 1573    280 461 1386 280 461 1386
    ## 1574    603 149-7268 603 149 7268
    ## 1575    364 792-5553 364 792 5553
    ## 1576    496-429-1314 496 429 1314
    ## 1577    459 671 4698 459 671 4698
    ## 1578    486-268-3312 486 268 3312
    ## 1579    497-518-4050 497 518 4050
    ## 1580    535 685-8273 535 685 8273
    ## 1581    859 495 4050 859 495 4050
    ## 1582    826 738-8316 826 738 8316
    ## 1583    724-134-3870 724 134 3870
    ## 1584    554 269-8937 554 269 8937
    ## 1585    125-578-4253 125 578 4253
    ## 1586    614 800 2861 614 800 2861
    ## 1587    487-232-4449 487 232 4449
    ## 1588    298 135-0900 298 135 0900
    ## 1589    392 183-7831 392 183 7831
    ## 1590    606 596-1029 606 596 1029
    ## 1591    384 953-4795 384 953 4795
    ## 1592    855 811-8811 855 811 8811
    ## 1593    253-374-7102 253 374 7102
    ## 1594    419 295-9580 419 295 9580
    ## 1595    802 102 8345 802 102 8345
    ## 1596    417 393 0050 417 393 0050
    ## 1597    787-624-8443 787 624 8443
    ## 1598    919 486-4251 919 486 4251
    ## 1599    341 824 5322 341 824 5322
    ## 1600    415 551 1608 415 551 1608
    ## 1601    392 495-7961 392 495 7961
    ## 1602    473-238-3324 473 238 3324
    ## 1603    506 760-3043 506 760 3043
    ## 1604    876-834-0624 876 834 0624
    ## 1605    919 611 6170 919 611 6170
    ## 1606    146 699-3488 146 699 3488
    ## 1607    261 434 7760 261 434 7760
    ## 1608    617 310 2684 617 310 2684
    ## 1609    182-535-3412 182 535 3412
    ## 1610    506 129 1694 506 129 1694
    ## 1611    302 339-0791 302 339 0791
    ## 1612    446 229-4342 446 229 4342
    ## 1613    249 602-6985 249 602 6985
    ## 1614    150 905-6938 150 905 6938
    ## 1615    313 990 8823 313 990 8823
    ## 1616    656-941-5355 656 941 5355
    ## 1617    116 689-6617 116 689 6617
    ## 1618    955 324 5981 955 324 5981
    ## 1619    175 808 2189 175 808 2189
    ## 1620    896 993-8555 896 993 8555
    ## 1621    105-687-6500 105 687 6500
    ## 1622    757 524 2964 757 524 2964
    ## 1623    201 374 2424 201 374 2424
    ## 1624    466 912-8401 466 912 8401
    ## 1625    766 112 6143 766 112 6143
    ## 1626    783-463-4865 783 463 4865
    ## 1627    853-803-9900 853 803 9900
    ## 1628    347 851-5388 347 851 5388
    ## 1629    992 114 6973 992 114 6973
    ## 1630    316-212-7309 316 212 7309
    ## 1631    301 672 1092 301 672 1092
    ## 1632    795 137 0201 795 137 0201
    ## 1633    381-883-5497 381 883 5497
    ## 1634    100-531-4642 100 531 4642
    ## 1635    994 923 6634 994 923 6634
    ## 1636    920 355 8404 920 355 8404
    ## 1637    441 445-6532 441 445 6532
    ## 1638    325 795 2455 325 795 2455
    ## 1639    593 829 6250 593 829 6250
    ## 1640    566-482-9004 566 482 9004
    ## 1641    542 537 6770 542 537 6770
    ## 1642    716 191 1741 716 191 1741
    ## 1643    491-727-7162 491 727 7162
    ## 1644    167-336-5660 167 336 5660
    ## 1645    358 831 0725 358 831 0725
    ## 1646    432 979 7292 432 979 7292
    ## 1647    205 382-5599 205 382 5599
    ## 1648    208 794-9612 208 794 9612
    ## 1649    728 662 3934 728 662 3934
    ## 1650    380-918-8572 380 918 8572
    ## 1651    905 742-3525 905 742 3525
    ## 1652    151 434 6989 151 434 6989
    ## 1653    755 544 2629 755 544 2629
    ## 1654    633 181 4494 633 181 4494
    ## 1655    346 706 5964 346 706 5964
    ## 1656    688 690 2184 688 690 2184
    ## 1657    618 717 1697 618 717 1697
    ## 1658    185-321-6877 185 321 6877
    ## 1659    147 535-3529 147 535 3529
    ## 1660    152 912-4118 152 912 4118
    ## 1661    726 943 7486 726 943 7486
    ## 1662    634 521-4714 634 521 4714
    ## 1663    670-248-0186 670 248 0186
    ## 1664    121 509-7306 121 509 7306
    ## 1665    105 635-5212 105 635 5212
    ## 1666    732 168-0110 732 168 0110
    ## 1667    364 834 3150 364 834 3150
    ## 1668    176 508 2778 176 508 2778
    ## 1669    120 941 0833 120 941 0833
    ## 1670    670 902 3199 670 902 3199
    ## 1671    214 250-8756 214 250 8756
    ## 1672    119 975-8484 119 975 8484
    ## 1673    297 484 3285 297 484 3285
    ## 1674    489 534-6272 489 534 6272
    ## 1675    610 716-5732 610 716 5732
    ## 1676    456 925-4236 456 925 4236
    ## 1677    743-103-7645 743 103 7645
    ## 1678    432-281-3682 432 281 3682
    ## 1679    167 144-9470 167 144 9470
    ## 1680    648 685 6188 648 685 6188
    ## 1681    548 191 4898 548 191 4898
    ## 1682    288 110 9483 288 110 9483
    ## 1683    946-558-5801 946 558 5801
    ## 1684    388 744 9637 388 744 9637
    ## 1685    506 463 9129 506 463 9129
    ## 1686    848 149-5208 848 149 5208
    ## 1687    970 908-2298 970 908 2298
    ## 1688    843 120-5653 843 120 5653
    ## 1689    306 394 8640 306 394 8640
    ## 1690    170-641-3537 170 641 3537
    ## 1691    860 723 5066 860 723 5066
    ## 1692    814-895-6610 814 895 6610
    ## 1693    139 727-9901 139 727 9901
    ## 1694    598 735 8557 598 735 8557
    ## 1695    593 895 6761 593 895 6761
    ## 1696    817 824-3849 817 824 3849
    ## 1697    508 484 9738 508 484 9738
    ## 1698    719 489 4724 719 489 4724
    ## 1699    503-671-4901 503 671 4901
    ## 1700    275 649 8183 275 649 8183
    ## 1701    968 130 7012 968 130 7012
    ## 1702    290 367 6676 290 367 6676
    ## 1703    499 766 9941 499 766 9941
    ## 1704    538-393-2243 538 393 2243
    ## 1705    540 362-7136 540 362 7136
    ## 1706    802 910-1742 802 910 1742
    ## 1707    845 544 4748 845 544 4748
    ## 1708    784-458-8425 784 458 8425
    ## 1709    365 217-0634 365 217 0634
    ## 1710    708 500 2758 708 500 2758
    ## 1711    594 797-7729 594 797 7729
    ## 1712    982 555 9504 982 555 9504
    ## 1713    477-307-3338 477 307 3338
    ## 1714    744-301-1148 744 301 1148
    ## 1715    389 484 8888 389 484 8888
    ## 1716    739 303 6128 739 303 6128
    ## 1717    175 905 9962 175 905 9962
    ## 1718    900 462-1379 900 462 1379
    ## 1719    878-636-2294 878 636 2294
    ## 1720    998 692-1900 998 692 1900
    ## 1721    994 421 8642 994 421 8642
    ## 1722    304-225-5895 304 225 5895
    ## 1723    931-522-5498 931 522 5498
    ## 1724    838 898 2275 838 898 2275
    ## 1725    718 187 7125 718 187 7125
    ## 1726    943 561 8955 943 561 8955
    ## 1727    341 261 6456 341 261 6456
    ## 1728    507 483-3618 507 483 3618
    ## 1729    826 266 0205 826 266 0205
    ## 1730    380 449-7849 380 449 7849
    ## 1731    589-975-0198 589 975 0198
    ## 1732    657 680 8781 657 680 8781
    ## 1733    501-668-7869 501 668 7869
    ## 1734    224 365 8299 224 365 8299
    ## 1735    747 588 1968 747 588 1968
    ## 1736    706 836-7047 706 836 7047
    ## 1737    698 462-6742 698 462 6742
    ## 1738    748 446 1257 748 446 1257
    ## 1739    525-552-4162 525 552 4162
    ## 1740    635-714-8302 635 714 8302
    ## 1741    114 668 2834 114 668 2834
    ## 1742    404 788 2855 404 788 2855
    ## 1743    673 292 2444 673 292 2444
    ## 1744    136 788 1426 136 788 1426
    ## 1745    564 677 3934 564 677 3934
    ## 1746    213 981 7762 213 981 7762
    ## 1747    453-556-0852 453 556 0852
    ## 1748    423 593 4483 423 593 4483
    ## 1749    820 609 7454 820 609 7454
    ## 1750    131-641-1331 131 641 1331
    ## 1751    824 540 9579 824 540 9579
    ## 1752    753 726-0123 753 726 0123
    ## 1753    739 737 9041 739 737 9041
    ## 1754    609-332-7370 609 332 7370
    ## 1755    426-182-1365 426 182 1365
    ## 1756    347 782-5787 347 782 5787
    ## 1757    898-210-6218 898 210 6218
    ## 1758    938 394 0411 938 394 0411
    ## 1759    904 844 1759 904 844 1759
    ## 1760    658-861-4306 658 861 4306
    ## 1761    716 184-1232 716 184 1232
    ## 1762    380-105-1757 380 105 1757
    ## 1763    969 555-0453 969 555 0453
    ## 1764    249 452 4370 249 452 4370
    ## 1765    443 384 8253 443 384 8253
    ## 1766    711 289 2247 711 289 2247
    ## 1767    343-973-0193 343 973 0193
    ## 1768    367 650 3720 367 650 3720
    ## 1769    265 286-5671 265 286 5671
    ## 1770    148 630 8560 148 630 8560
    ## 1771    413-727-2672 413 727 2672
    ## 1772    162 332-5838 162 332 5838
    ## 1773    229 604-7790 229 604 7790
    ## 1774    892 301-0333 892 301 0333
    ## 1775    148 501 5084 148 501 5084
    ## 1776    564 780 8272 564 780 8272
    ## 1777    524 190 0899 524 190 0899
    ## 1778    463 792 2782 463 792 2782
    ## 1779    331 472-8624 331 472 8624
    ## 1780    522-286-5318 522 286 5318
    ## 1781    310 719 4550 310 719 4550
    ## 1782    314-360-4288 314 360 4288
    ## 1783    301 534-5754 301 534 5754
    ## 1784    822 271 5719 822 271 5719
    ## 1785    341 473-0639 341 473 0639
    ## 1786    835 882-3693 835 882 3693
    ## 1787    465-550-6610 465 550 6610
    ## 1788    388 100 1482 388 100 1482
    ## 1789    589 194-0523 589 194 0523
    ## 1790    982 842 4913 982 842 4913
    ## 1791    671 913 4563 671 913 4563
    ## 1792    144 468 5864 144 468 5864
    ## 1793    194 344 4039 194 344 4039
    ## 1794    331 747 5714 331 747 5714
    ## 1795    221-190-1449 221 190 1449
    ## 1796    322 843-0185 322 843 0185
    ## 1797    676-614-9095 676 614 9095
    ## 1798    190 975-2514 190 975 2514
    ## 1799    909 382-3774 909 382 3774
    ## 1800    956 257 9319 956 257 9319
    ## 1801    362 145 8268 362 145 8268
    ## 1802    570 727 3998 570 727 3998
    ## 1803    225 964 9193 225 964 9193
    ## 1804    227 419 9482 227 419 9482
    ## 1805    949 360 7605 949 360 7605
    ## 1806    347 896-3463 347 896 3463
    ## 1807    794 939 9735 794 939 9735
    ## 1808    413 754 3034 413 754 3034
    ## 1809    806 730 0459 806 730 0459
    ## 1810    791 195 8909 791 195 8909
    ## 1811    691-318-3535 691 318 3535
    ## 1812    852-386-6029 852 386 6029
    ## 1813    380 682 7795 380 682 7795
    ## 1814    355 550-1392 355 550 1392
    ## 1815    247 586 4579 247 586 4579
    ## 1816    235 257 1041 235 257 1041
    ## 1817    705 456-1905 705 456 1905
    ## 1818    106 756 2785 106 756 2785
    ## 1819    836 207-8419 836 207 8419
    ## 1820    306 552-1875 306 552 1875
    ## 1821    729-102-7511 729 102 7511
    ## 1822    795 583 0958 795 583 0958
    ## 1823    700-431-3918 700 431 3918
    ## 1824    929 632 1068 929 632 1068
    ## 1825    763 906 2495 763 906 2495
    ## 1826    469 976 6796 469 976 6796
    ## 1827    887-657-4143 887 657 4143
    ## 1828    574-438-5329 574 438 5329
    ## 1829    319 127 9518 319 127 9518
    ## 1830    429 960 9710 429 960 9710
    ## 1831    419 646 0299 419 646 0299
    ## 1832    192 343 8515 192 343 8515
    ## 1833    521 336 8581 521 336 8581
    ## 1834    776 367 6109 776 367 6109
    ## 1835    470 367 1392 470 367 1392
    ## 1836    944 189-7555 944 189 7555
    ## 1837    998-931-4783 998 931 4783
    ## 1838    362-178-6307 362 178 6307
    ## 1839    458 404-9558 458 404 9558
    ## 1840    212 286 7936 212 286 7936
    ## 1841    481 522-1039 481 522 1039
    ## 1842    376 611-4588 376 611 4588
    ## 1843    936 193 9690 936 193 9690
    ## 1844    641 544-6549 641 544 6549
    ## 1845    797-870-7818 797 870 7818
    ## 1846    693 907 5353 693 907 5353
    ## 1847    332 973 4943 332 973 4943
    ## 1848    929 622 9077 929 622 9077
    ## 1849    649-379-5361 649 379 5361
    ## 1850    572 748-6932 572 748 6932
    ## 1851    395-892-5646 395 892 5646
    ## 1852    221-628-9561 221 628 9561
    ## 1853    227 801-6148 227 801 6148
    ## 1854    549-649-1864 549 649 1864
    ## 1855    342 941 0439 342 941 0439
    ## 1856    701 390 9814 701 390 9814
    ## 1857    519 573-6576 519 573 6576
    ## 1858    919-342-0230 919 342 0230
    ## 1859    364-759-2705 364 759 2705
    ## 1860    949 543 7906 949 543 7906
    ## 1861    942 732 6403 942 732 6403
    ## 1862    900 586-1787 900 586 1787
    ## 1863    308-607-9855 308 607 9855
    ## 1864    764 645 5740 764 645 5740
    ## 1865    472-337-8838 472 337 8838
    ## 1866    791 847 7278 791 847 7278
    ## 1867    128 805 3828 128 805 3828
    ## 1868    365-832-0674 365 832 0674
    ## 1869    123-282-3494 123 282 3494
    ## 1870    285 424 4318 285 424 4318
    ## 1871    452 352 1387 452 352 1387
    ## 1872    129-377-8159 129 377 8159
    ## 1873    222 143 3131 222 143 3131
    ## 1874    162-451-0594 162 451 0594
    ## 1875    239 325-5321 239 325 5321
    ## 1876    436-422-6171 436 422 6171
    ## 1877    605 284 4260 605 284 4260
    ## 1878    929-102-5905 929 102 5905
    ## 1879    847 507 8268 847 507 8268
    ## 1880    452-811-8088 452 811 8088
    ## 1881    799 143 1677 799 143 1677
    ## 1882    196 756 4555 196 756 4555
    ## 1883    119-444-0817 119 444 0817
    ## 1884    885 454 0883 885 454 0883
    ## 1885    945-998-0444 945 998 0444
    ## 1886    367 897-7969 367 897 7969
    ## 1887    163 241 9321 163 241 9321
    ## 1888    594-176-5811 594 176 5811
    ## 1889    621 874-9973 621 874 9973
    ## 1890    332 963 4103 332 963 4103
    ## 1891    389 318 3975 389 318 3975
    ## 1892    894-593-7953 894 593 7953
    ## 1893    561-266-7842 561 266 7842
    ## 1894    354-958-8052 354 958 8052
    ## 1895    151 921 2775 151 921 2775
    ## 1896    901 140 3759 901 140 3759
    ## 1897    426 342-7378 426 342 7378
    ## 1898    333-520-4811 333 520 4811
    ## 1899    765 191-1797 765 191 1797
    ## 1900    850 914 9348 850 914 9348
    ## 1901    246 272 9019 246 272 9019
    ## 1902    400 250-0871 400 250 0871
    ## 1903    504 419 9191 504 419 9191
    ## 1904    434 725-0561 434 725 0561
    ## 1905    231 863 7554 231 863 7554
    ## 1906    969 207-3261 969 207 3261
    ## 1907    939 253 9048 939 253 9048
    ## 1908    879-154-4494 879 154 4494
    ## 1909    577 786 2546 577 786 2546
    ## 1910    994 688-3259 994 688 3259
    ## 1911    841-717-4447 841 717 4447
    ## 1912    397-353-6309 397 353 6309
    ## 1913    558 191 4548 558 191 4548
    ## 1914    905 768 2297 905 768 2297
    ## 1915    348 522 2051 348 522 2051
    ## 1916    307-323-6861 307 323 6861
    ## 1917    663 886 2487 663 886 2487
    ## 1918    274 944 6097 274 944 6097
    ## 1919    494 308-3048 494 308 3048
    ## 1920    328 307 0875 328 307 0875
    ## 1921    673 524 3504 673 524 3504
    ## 1922    934 721 0615 934 721 0615
    ## 1923    102-957-6486 102 957 6486
    ## 1924    715 288-8832 715 288 8832
    ## 1925    794 925 8846 794 925 8846
    ## 1926    637 281 4111 637 281 4111
    ## 1927    594 448-6242 594 448 6242
    ## 1928    335 802 2651 335 802 2651
    ## 1929    560 699 9908 560 699 9908
    ## 1930    451 163 0102 451 163 0102
    ## 1931    613 800 0835 613 800 0835
    ## 1932    370 453 5800 370 453 5800
    ## 1933    192 507 5411 192 507 5411
    ## 1934    182-227-4838 182 227 4838
    ## 1935    647 126 2332 647 126 2332
    ## 1936    606 125 6957 606 125 6957
    ## 1937    728-404-5558 728 404 5558
    ## 1938    506 812 6052 506 812 6052
    ## 1939    427-665-3475 427 665 3475
    ## 1940    808 739 7162 808 739 7162
    ## 1941    375-978-3305 375 978 3305
    ## 1942    181-708-2089 181 708 2089
    ## 1943    802 810-5574 802 810 5574
    ## 1944    242-540-4234 242 540 4234
    ## 1945    105 116 9695 105 116 9695
    ## 1946    867 891-0871 867 891 0871
    ## 1947    945 144 7892 945 144 7892
    ## 1948    531 895 6695 531 895 6695
    ## 1949    533 213-4368 533 213 4368
    ## 1950    894 810 2674 894 810 2674
    ## 1951    943 812 6349 943 812 6349
    ## 1952    476 168 4235 476 168 4235
    ## 1953    931 385 6757 931 385 6757
    ## 1954    324 188-6781 324 188 6781
    ## 1955    100 378 8095 100 378 8095
    ## 1956    459 572 0244 459 572 0244
    ## 1957    397-362-5469 397 362 5469
    ## 1958    102 928-7959 102 928 7959
    ## 1959    439 568-6611 439 568 6611
    ## 1960    767 205-0604 767 205 0604
    ## 1961    890 548-9219 890 548 9219
    ## 1962    938 982 5585 938 982 5585
    ## 1963    769-472-2992 769 472 2992
    ## 1964    190 204 1154 190 204 1154
    ## 1965    649 925-8489 649 925 8489
    ## 1966    321 616 1013 321 616 1013
    ## 1967    156 837-4491 156 837 4491
    ## 1968    178-232-0815 178 232 0815
    ## 1969    882 304-9032 882 304 9032
    ## 1970    828 549 6666 828 549 6666
    ## 1971    280 544 4554 280 544 4554
    ## 1972    183 208 5054 183 208 5054
    ## 1973    971 548-6611 971 548 6611
    ## 1974    828 153-5819 828 153 5819
    ## 1975    203 448 1522 203 448 1522
    ## 1976    900-871-9056 900 871 9056
    ## 1977    406-167-1379 406 167 1379
    ## 1978    906 850-9192 906 850 9192
    ## 1979    859 777-8245 859 777 8245
    ## 1980    641-635-8466 641 635 8466
    ## 1981    807-671-6158 807 671 6158
    ## 1982    589 270-7518 589 270 7518
    ## 1983    768 529 8051 768 529 8051
    ## 1984    220 660 0306 220 660 0306
    ## 1985    928-179-7556 928 179 7556
    ## 1986    153 756 0278 153 756 0278
    ## 1987    273 829 9197 273 829 9197
    ## 1988    269 463 0911 269 463 0911
    ## 1989    696 984 8826 696 984 8826
    ## 1990    905-903-5258 905 903 5258
    ## 1991    267 332 4709 267 332 4709
    ## 1992    146 129-5118 146 129 5118
    ## 1993    927 747-9822 927 747 9822
    ## 1994    534 216 6666 534 216 6666
    ## 1995    481 479-7013 481 479 7013
    ## 1996    971 175 2968 971 175 2968
    ## 1997    716 777-3762 716 777 3762
    ## 1998    274-863-3205 274 863 3205
    ## 1999    217 589-0596 217 589 0596
    ## 2000    928 445 5474 928 445 5474
    ## 2001    858 990 5153 858 990 5153
    ## 2002    731-813-2043 731 813 2043
    ## 2003    563-732-6802 563 732 6802
    ## 2004    145 725 4021 145 725 4021
    ## 2005    931 311 5801 931 311 5801
    ## 2006    637 782-6989 637 782 6989
    ## 2007    172 990 3485 172 990 3485
    ## 2008    872 325 4341 872 325 4341
    ## 2009    359 803-9809 359 803 9809
    ## 2010    152 790 8238 152 790 8238
    ## 2011    330 561 9257 330 561 9257
    ## 2012    437 420 7546 437 420 7546
    ## 2013    495 632 4027 495 632 4027
    ## 2014    416 788-2844 416 788 2844
    ## 2015    311-305-4367 311 305 4367
    ## 2016    817-400-0481 817 400 0481
    ## 2017    430 723 1079 430 723 1079
    ## 2018    729 609-4819 729 609 4819
    ## 2019    201 737-4409 201 737 4409
    ## 2020    137 611-3694 137 611 3694
    ## 2021    226 490 8696 226 490 8696
    ## 2022    123 570 8640 123 570 8640
    ## 2023    665 803 2453 665 803 2453
    ## 2024    812 869-6263 812 869 6263
    ## 2025    639 132 6386 639 132 6386
    ## 2026    194 198-0504 194 198 0504
    ## 2027    437 886 0753 437 886 0753
    ## 2028    626 756 5089 626 756 5089
    ## 2029    299 137-6993 299 137 6993
    ## 2030    714 950 3364 714 950 3364
    ## 2031    653-786-5985 653 786 5985
    ## 2032    518 286 5956 518 286 5956
    ## 2033    194 960 2145 194 960 2145
    ## 2034    362-136-1153 362 136 1153
    ## 2035    376-456-0697 376 456 0697
    ## 2036    657 832 1189 657 832 1189
    ## 2037    962-918-6117 962 918 6117
    ## 2038    692 929 3592 692 929 3592
    ## 2039    805-877-3887 805 877 3887
    ## 2040    739 710-2966 739 710 2966
    ## 2041    819 732 4132 819 732 4132
    ## 2042    367 221 9710 367 221 9710
    ## 2043    361 154 1789 361 154 1789
    ## 2044    680 488 1182 680 488 1182
    ## 2045    928 638 1186 928 638 1186
    ## 2046    588-693-9875 588 693 9875
    ## 2047    681-308-7915 681 308 7915
    ## 2048    783 647 8490 783 647 8490
    ## 2049    897 847 0632 897 847 0632
    ## 2050    150 952 4453 150 952 4453
    ## 2051    322-884-3020 322 884 3020
    ## 2052    176-313-5403 176 313 5403
    ## 2053    487 109 4196 487 109 4196
    ## 2054    477 182-4689 477 182 4689
    ## 2055    544 382 8289 544 382 8289
    ## 2056    781 543 7456 781 543 7456
    ## 2057    911 829 6476 911 829 6476
    ## 2058    525 362-5532 525 362 5532
    ## 2059    517 986 3426 517 986 3426
    ## 2060    838 220 5397 838 220 5397
    ## 2061    687 887-6766 687 887 6766
    ## 2062    179 163 0902 179 163 0902
    ## 2063    539 137 8983 539 137 8983
    ## 2064    733-154-0094 733 154 0094
    ## 2065    639 881 3693 639 881 3693
    ## 2066    291-830-3017 291 830 3017
    ## 2067    637 100-0509 637 100 0509
    ## 2068    750 520 0167 750 520 0167
    ## 2069    676 485 8963 676 485 8963
    ## 2070    135 566 5090 135 566 5090
    ## 2071    337 260 4996 337 260 4996
    ## 2072    371 185 2377 371 185 2377
    ## 2073    280 461 1386 280 461 1386
    ## 2074    603 149-7268 603 149 7268
    ## 2075    364 792-5553 364 792 5553
    ## 2076    496-429-1314 496 429 1314
    ## 2077    459 671 4698 459 671 4698
    ## 2078    486-268-3312 486 268 3312
    ## 2079    497-518-4050 497 518 4050
    ## 2080    535 685-8273 535 685 8273
    ## 2081    859 495 4050 859 495 4050
    ## 2082    826 738-8316 826 738 8316
    ## 2083    724-134-3870 724 134 3870
    ## 2084    554 269-8937 554 269 8937
    ## 2085    125-578-4253 125 578 4253
    ## 2086    614 800 2861 614 800 2861
    ## 2087    487-232-4449 487 232 4449
    ## 2088    298 135-0900 298 135 0900
    ## 2089    392 183-7831 392 183 7831
    ## 2090    606 596-1029 606 596 1029
    ## 2091    384 953-4795 384 953 4795
    ## 2092    855 811-8811 855 811 8811
    ## 2093    253-374-7102 253 374 7102
    ## 2094    419 295-9580 419 295 9580
    ## 2095    802 102 8345 802 102 8345
    ## 2096    417 393 0050 417 393 0050
    ## 2097    787-624-8443 787 624 8443
    ## 2098    919 486-4251 919 486 4251
    ## 2099    341 824 5322 341 824 5322
    ## 2100    415 551 1608 415 551 1608
    ## 2101    392 495-7961 392 495 7961
    ## 2102    473-238-3324 473 238 3324
    ## 2103    506 760-3043 506 760 3043
    ## 2104    876-834-0624 876 834 0624
    ## 2105    919 611 6170 919 611 6170
    ## 2106    146 699-3488 146 699 3488
    ## 2107    261 434 7760 261 434 7760
    ## 2108    617 310 2684 617 310 2684
    ## 2109    182-535-3412 182 535 3412
    ## 2110    506 129 1694 506 129 1694
    ## 2111    302 339-0791 302 339 0791
    ## 2112    446 229-4342 446 229 4342
    ## 2113    249 602-6985 249 602 6985
    ## 2114    150 905-6938 150 905 6938
    ## 2115    313 990 8823 313 990 8823
    ## 2116    656-941-5355 656 941 5355
    ## 2117    116 689-6617 116 689 6617
    ## 2118    955 324 5981 955 324 5981
    ## 2119    175 808 2189 175 808 2189
    ## 2120    896 993-8555 896 993 8555
    ## 2121    105-687-6500 105 687 6500
    ## 2122    757 524 2964 757 524 2964
    ## 2123    201 374 2424 201 374 2424
    ## 2124    466 912-8401 466 912 8401
    ## 2125    766 112 6143 766 112 6143
    ## 2126    783-463-4865 783 463 4865
    ## 2127    853-803-9900 853 803 9900
    ## 2128    347 851-5388 347 851 5388
    ## 2129    992 114 6973 992 114 6973
    ## 2130    316-212-7309 316 212 7309
    ## 2131    301 672 1092 301 672 1092
    ## 2132    795 137 0201 795 137 0201
    ## 2133    381-883-5497 381 883 5497
    ## 2134    100-531-4642 100 531 4642
    ## 2135    994 923 6634 994 923 6634
    ## 2136    920 355 8404 920 355 8404
    ## 2137    441 445-6532 441 445 6532
    ## 2138    325 795 2455 325 795 2455
    ## 2139    593 829 6250 593 829 6250
    ## 2140    566-482-9004 566 482 9004
    ## 2141    542 537 6770 542 537 6770
    ## 2142    716 191 1741 716 191 1741
    ## 2143    491-727-7162 491 727 7162
    ## 2144    167-336-5660 167 336 5660
    ## 2145    358 831 0725 358 831 0725
    ## 2146    432 979 7292 432 979 7292
    ## 2147    205 382-5599 205 382 5599
    ## 2148    208 794-9612 208 794 9612
    ## 2149    728 662 3934 728 662 3934
    ## 2150    380-918-8572 380 918 8572
    ## 2151    905 742-3525 905 742 3525
    ## 2152    151 434 6989 151 434 6989
    ## 2153    755 544 2629 755 544 2629
    ## 2154    633 181 4494 633 181 4494
    ## 2155    346 706 5964 346 706 5964
    ## 2156    688 690 2184 688 690 2184
    ## 2157    618 717 1697 618 717 1697
    ## 2158    185-321-6877 185 321 6877
    ## 2159    147 535-3529 147 535 3529
    ## 2160    152 912-4118 152 912 4118
    ## 2161    726 943 7486 726 943 7486
    ## 2162    634 521-4714 634 521 4714
    ## 2163    670-248-0186 670 248 0186
    ## 2164    121 509-7306 121 509 7306
    ## 2165    105 635-5212 105 635 5212
    ## 2166    732 168-0110 732 168 0110
    ## 2167    364 834 3150 364 834 3150
    ## 2168    176 508 2778 176 508 2778
    ## 2169    120 941 0833 120 941 0833
    ## 2170    670 902 3199 670 902 3199
    ## 2171    214 250-8756 214 250 8756
    ## 2172    119 975-8484 119 975 8484
    ## 2173    297 484 3285 297 484 3285
    ## 2174    489 534-6272 489 534 6272
    ## 2175    610 716-5732 610 716 5732
    ## 2176    456 925-4236 456 925 4236
    ## 2177    743-103-7645 743 103 7645
    ## 2178    432-281-3682 432 281 3682
    ## 2179    167 144-9470 167 144 9470
    ## 2180    648 685 6188 648 685 6188
    ## 2181    548 191 4898 548 191 4898
    ## 2182    288 110 9483 288 110 9483
    ## 2183    946-558-5801 946 558 5801
    ## 2184    388 744 9637 388 744 9637
    ## 2185    506 463 9129 506 463 9129
    ## 2186    848 149-5208 848 149 5208
    ## 2187    970 908-2298 970 908 2298
    ## 2188    843 120-5653 843 120 5653
    ## 2189    306 394 8640 306 394 8640
    ## 2190    170-641-3537 170 641 3537
    ## 2191    860 723 5066 860 723 5066
    ## 2192    814-895-6610 814 895 6610
    ## 2193    139 727-9901 139 727 9901
    ## 2194    598 735 8557 598 735 8557
    ## 2195    593 895 6761 593 895 6761
    ## 2196    817 824-3849 817 824 3849
    ## 2197    508 484 9738 508 484 9738
    ## 2198    719 489 4724 719 489 4724
    ## 2199    503-671-4901 503 671 4901
    ## 2200    275 649 8183 275 649 8183
    ## 2201    968 130 7012 968 130 7012
    ## 2202    290 367 6676 290 367 6676
    ## 2203    499 766 9941 499 766 9941
    ## 2204    538-393-2243 538 393 2243
    ## 2205    540 362-7136 540 362 7136
    ## 2206    802 910-1742 802 910 1742
    ## 2207    845 544 4748 845 544 4748
    ## 2208    784-458-8425 784 458 8425
    ## 2209    365 217-0634 365 217 0634
    ## 2210    708 500 2758 708 500 2758
    ## 2211    594 797-7729 594 797 7729
    ## 2212    982 555 9504 982 555 9504
    ## 2213    477-307-3338 477 307 3338
    ## 2214    744-301-1148 744 301 1148
    ## 2215    389 484 8888 389 484 8888
    ## 2216    739 303 6128 739 303 6128
    ## 2217    175 905 9962 175 905 9962
    ## 2218    900 462-1379 900 462 1379
    ## 2219    878-636-2294 878 636 2294
    ## 2220    998 692-1900 998 692 1900
    ## 2221    994 421 8642 994 421 8642
    ## 2222    304-225-5895 304 225 5895
    ## 2223    931-522-5498 931 522 5498
    ## 2224    838 898 2275 838 898 2275
    ## 2225    718 187 7125 718 187 7125
    ## 2226    943 561 8955 943 561 8955
    ## 2227    341 261 6456 341 261 6456
    ## 2228    507 483-3618 507 483 3618
    ## 2229    826 266 0205 826 266 0205
    ## 2230    380 449-7849 380 449 7849
    ## 2231    589-975-0198 589 975 0198
    ## 2232    657 680 8781 657 680 8781
    ## 2233    501-668-7869 501 668 7869
    ## 2234    224 365 8299 224 365 8299
    ## 2235    747 588 1968 747 588 1968
    ## 2236    706 836-7047 706 836 7047
    ## 2237    698 462-6742 698 462 6742
    ## 2238    748 446 1257 748 446 1257
    ## 2239    525-552-4162 525 552 4162
    ## 2240    635-714-8302 635 714 8302
    ## 2241    114 668 2834 114 668 2834
    ## 2242    404 788 2855 404 788 2855
    ## 2243    673 292 2444 673 292 2444
    ## 2244    136 788 1426 136 788 1426
    ## 2245    564 677 3934 564 677 3934
    ## 2246    213 981 7762 213 981 7762
    ## 2247    453-556-0852 453 556 0852
    ## 2248    423 593 4483 423 593 4483
    ## 2249    820 609 7454 820 609 7454
    ## 2250    131-641-1331 131 641 1331
    ## 2251    824 540 9579 824 540 9579
    ## 2252    753 726-0123 753 726 0123
    ## 2253    739 737 9041 739 737 9041
    ## 2254    609-332-7370 609 332 7370
    ## 2255    426-182-1365 426 182 1365
    ## 2256    347 782-5787 347 782 5787
    ## 2257    898-210-6218 898 210 6218
    ## 2258    938 394 0411 938 394 0411
    ## 2259    904 844 1759 904 844 1759
    ## 2260    658-861-4306 658 861 4306
    ## 2261    716 184-1232 716 184 1232
    ## 2262    380-105-1757 380 105 1757
    ## 2263    969 555-0453 969 555 0453
    ## 2264    249 452 4370 249 452 4370
    ## 2265    443 384 8253 443 384 8253
    ## 2266    711 289 2247 711 289 2247
    ## 2267    343-973-0193 343 973 0193
    ## 2268    367 650 3720 367 650 3720
    ## 2269    265 286-5671 265 286 5671
    ## 2270    148 630 8560 148 630 8560
    ## 2271    413-727-2672 413 727 2672
    ## 2272    162 332-5838 162 332 5838
    ## 2273    229 604-7790 229 604 7790
    ## 2274    892 301-0333 892 301 0333
    ## 2275    148 501 5084 148 501 5084
    ## 2276    564 780 8272 564 780 8272
    ## 2277    524 190 0899 524 190 0899
    ## 2278    463 792 2782 463 792 2782
    ## 2279    331 472-8624 331 472 8624
    ## 2280    522-286-5318 522 286 5318
    ## 2281    310 719 4550 310 719 4550
    ## 2282    314-360-4288 314 360 4288
    ## 2283    301 534-5754 301 534 5754
    ## 2284    822 271 5719 822 271 5719
    ## 2285    341 473-0639 341 473 0639
    ## 2286    835 882-3693 835 882 3693
    ## 2287    465-550-6610 465 550 6610
    ## 2288    388 100 1482 388 100 1482
    ## 2289    589 194-0523 589 194 0523
    ## 2290    982 842 4913 982 842 4913
    ## 2291    671 913 4563 671 913 4563
    ## 2292    144 468 5864 144 468 5864
    ## 2293    194 344 4039 194 344 4039
    ## 2294    331 747 5714 331 747 5714
    ## 2295    221-190-1449 221 190 1449
    ## 2296    322 843-0185 322 843 0185
    ## 2297    676-614-9095 676 614 9095
    ## 2298    190 975-2514 190 975 2514
    ## 2299    909 382-3774 909 382 3774
    ## 2300    956 257 9319 956 257 9319
    ## 2301    362 145 8268 362 145 8268
    ## 2302    570 727 3998 570 727 3998
    ## 2303    225 964 9193 225 964 9193
    ## 2304    227 419 9482 227 419 9482
    ## 2305    949 360 7605 949 360 7605
    ## 2306    347 896-3463 347 896 3463
    ## 2307    794 939 9735 794 939 9735
    ## 2308    413 754 3034 413 754 3034
    ## 2309    806 730 0459 806 730 0459
    ## 2310    791 195 8909 791 195 8909
    ## 2311    691-318-3535 691 318 3535
    ## 2312    852-386-6029 852 386 6029
    ## 2313    380 682 7795 380 682 7795
    ## 2314    355 550-1392 355 550 1392
    ## 2315    247 586 4579 247 586 4579
    ## 2316    235 257 1041 235 257 1041
    ## 2317    705 456-1905 705 456 1905
    ## 2318    106 756 2785 106 756 2785
    ## 2319    836 207-8419 836 207 8419
    ## 2320    306 552-1875 306 552 1875
    ## 2321    729-102-7511 729 102 7511
    ## 2322    795 583 0958 795 583 0958
    ## 2323    700-431-3918 700 431 3918
    ## 2324    929 632 1068 929 632 1068
    ## 2325    763 906 2495 763 906 2495
    ## 2326    469 976 6796 469 976 6796
    ## 2327    887-657-4143 887 657 4143
    ## 2328    574-438-5329 574 438 5329
    ## 2329    319 127 9518 319 127 9518
    ## 2330    429 960 9710 429 960 9710
    ## 2331    419 646 0299 419 646 0299
    ## 2332           38515        38515
    ## 2333    521 336 8581 521 336 8581
    ## 2334    776 367 6109 776 367 6109
    ## 2335    470 367 1392 470 367 1392
    ## 2336    944 189-7555 944 189 7555
    ## 2337    998-931-4783 998 931 4783
    ## 2338    362-178-6307 362 178 6307
    ## 2339    458 404-9558 458 404 9558
    ## 2340    212 286 7936 212 286 7936
    ## 2341    481 522-1039 481 522 1039
    ## 2342    376 611-4588 376 611 4588
    ## 2343    936 193 9690 936 193 9690
    ## 2344    641 544-6549 641 544 6549
    ## 2345    797-870-7818 797 870 7818
    ## 2346    693 907 5353 693 907 5353
    ## 2347    332 973 4943 332 973 4943
    ## 2348    929 622 9077 929 622 9077
    ## 2349    649-379-5361 649 379 5361
    ## 2350    572 748-6932 572 748 6932
    ## 2351    395-892-5646 395 892 5646
    ## 2352    221-628-9561 221 628 9561
    ## 2353    227 801-6148 227 801 6148
    ## 2354    549-649-1864 549 649 1864
    ## 2355    342 941 0439 342 941 0439
    ## 2356    701 390 9814 701 390 9814
    ## 2357    519 573-6576 519 573 6576
    ## 2358    919-342-0230 919 342 0230
    ## 2359    364-759-2705 364 759 2705
    ## 2360    949 543 7906 949 543 7906
    ## 2361    942 732 6403 942 732 6403
    ## 2362    900 586-1787 900 586 1787
    ## 2363    308-607-9855 308 607 9855
    ## 2364    764 645 5740 764 645 5740
    ## 2365    472-337-8838 472 337 8838
    ## 2366    791 847 7278 791 847 7278
    ## 2367    128 805 3828 128 805 3828
    ## 2368    365-832-0674 365 832 0674
    ## 2369    123-282-3494 123 282 3494
    ## 2370    285 424 4318 285 424 4318
    ## 2371    452 352 1387 452 352 1387
    ## 2372    129-377-8159 129 377 8159
    ## 2373    222 143 3131 222 143 3131
    ## 2374    162-451-0594 162 451 0594
    ## 2375    239 325-5321 239 325 5321
    ## 2376    436-422-6171 436 422 6171
    ## 2377    605 284 4260 605 284 4260
    ## 2378    929-102-5905 929 102 5905
    ## 2379    847 507 8268 847 507 8268
    ## 2380    452-811-8088 452 811 8088
    ## 2381    799 143 1677 799 143 1677
    ## 2382    196 756 4555 196 756 4555
    ## 2383    119-444-0817 119 444 0817
    ## 2384    885 454 0883 885 454 0883
    ## 2385    945-998-0444 945 998 0444
    ## 2386    367 897-7969 367 897 7969
    ## 2387    163 241 9321 163 241 9321
    ## 2388    594-176-5811 594 176 5811
    ## 2389    621 874-9973 621 874 9973
    ## 2390    332 963 4103 332 963 4103
    ## 2391    389 318 3975 389 318 3975
    ## 2392    894-593-7953 894 593 7953
    ## 2393    561-266-7842 561 266 7842
    ## 2394    354-958-8052 354 958 8052
    ## 2395    151 921 2775 151 921 2775
    ## 2396    901 140 3759 901 140 3759
    ## 2397    426 342-7378 426 342 7378
    ## 2398    333-520-4811 333 520 4811
    ## 2399    765 191-1797 765 191 1797
    ## 2400    850 914 9348 850 914 9348
    ## 2401    246 272 9019 246 272 9019
    ## 2402    400 250-0871 400 250 0871
    ## 2403    504 419 9191 504 419 9191
    ## 2404    434 725-0561 434 725 0561
    ## 2405    231 863 7554 231 863 7554
    ## 2406    969 207-3261 969 207 3261
    ## 2407    939 253 9048 939 253 9048
    ## 2408    879-154-4494 879 154 4494
    ## 2409    577 786 2546 577 786 2546
    ## 2410    994 688-3259 994 688 3259
    ## 2411    841-717-4447 841 717 4447
    ## 2412    397-353-6309 397 353 6309
    ## 2413    558 191 4548 558 191 4548
    ## 2414    905 768 2297 905 768 2297
    ## 2415    348 522 2051 348 522 2051
    ## 2416    307-323-6861 307 323 6861
    ## 2417    663 886 2487 663 886 2487
    ## 2418    274 944 6097 274 944 6097
    ## 2419    494 308-3048 494 308 3048
    ## 2420    328 307 0875 328 307 0875
    ## 2421    673 524 3504 673 524 3504
    ## 2422    934 721 0615 934 721 0615
    ## 2423    102-957-6486 102 957 6486
    ## 2424    715 288-8832 715 288 8832
    ## 2425    794 925 8846 794 925 8846
    ## 2426    637 281 4111 637 281 4111
    ## 2427    594 448-6242 594 448 6242
    ## 2428    335 802 2651 335 802 2651
    ## 2429    560 699 9908 560 699 9908
    ## 2430    451 163 0102 451 163 0102
    ## 2431    613 800 0835 613 800 0835
    ## 2432    370 453 5800 370 453 5800
    ## 2433    192 507 5411 192 507 5411
    ## 2434    182-227-4838 182 227 4838
    ## 2435    647 126 2332 647 126 2332
    ## 2436    606 125 6957 606 125 6957
    ## 2437    728-404-5558 728 404 5558
    ## 2438    506 812 6052 506 812 6052
    ## 2439    427-665-3475 427 665 3475
    ## 2440    808 739 7162 808 739 7162
    ## 2441    375-978-3305 375 978 3305
    ## 2442    181-708-2089 181 708 2089
    ## 2443    802 810-5574 802 810 5574
    ## 2444    242-540-4234 242 540 4234
    ## 2445    105 116 9695 105 116 9695
    ## 2446    867 891-0871 867 891 0871
    ## 2447    945 144 7892 945 144 7892
    ## 2448    531 895 6695 531 895 6695
    ## 2449    533 213-4368 533 213 4368
    ## 2450    894 810 2674 894 810 2674
    ## 2451    943 812 6349 943 812 6349
    ## 2452    476 168 4235 476 168 4235
    ## 2453    931 385 6757 931 385 6757
    ## 2454    324 188-6781 324 188 6781
    ## 2455    100 378 8095 100 378 8095
    ## 2456    459 572 0244 459 572 0244
    ## 2457    397-362-5469 397 362 5469
    ## 2458    102 928-7959 102 928 7959
    ## 2459    439 568-6611 439 568 6611
    ## 2460    767 205-0604 767 205 0604
    ## 2461    890 548-9219 890 548 9219
    ## 2462    938 982 5585 938 982 5585
    ## 2463    769-472-2992 769 472 2992
    ## 2464    190 204 1154 190 204 1154
    ## 2465    649 925-8489 649 925 8489
    ## 2466    321 616 1013 321 616 1013
    ## 2467    156 837-4491 156 837 4491
    ## 2468    178-232-0815 178 232 0815
    ## 2469    882 304-9032 882 304 9032
    ## 2470    828 549 6666 828 549 6666
    ## 2471    280 544 4554 280 544 4554
    ## 2472    183 208 5054 183 208 5054
    ## 2473    971 548-6611 971 548 6611
    ## 2474    828 153-5819 828 153 5819
    ## 2475    203 448 1522 203 448 1522
    ## 2476    900-871-9056 900 871 9056
    ## 2477    406-167-1379 406 167 1379
    ## 2478    906 850-9192 906 850 9192
    ## 2479    859 777-8245 859 777 8245
    ## 2480    641-635-8466 641 635 8466
    ## 2481    807-671-6158 807 671 6158
    ## 2482    589 270-7518 589 270 7518
    ## 2483    768 529 8051 768 529 8051
    ## 2484    220 660 0306 220 660 0306
    ## 2485    928-179-7556 928 179 7556
    ## 2486    153 756 0278 153 756 0278
    ## 2487    273 829 9197 273 829 9197
    ## 2488    269 463 0911 269 463 0911
    ## 2489    696 984 8826 696 984 8826
    ## 2490    905-903-5258 905 903 5258
    ## 2491    267 332 4709 267 332 4709
    ## 2492    146 129-5118 146 129 5118
    ## 2493    927 747-9822 927 747 9822
    ## 2494    534 216 6666 534 216 6666
    ## 2495    481 479-7013 481 479 7013
    ## 2496    971 175 2968 971 175 2968
    ## 2497    716 777-3762 716 777 3762
    ## 2498    274-863-3205 274 863 3205
    ## 2499    217 589-0596 217 589 0596
    ## 2500    928 445 5474 928 445 5474
    ## 2501    858 990 5153 858 990 5153
    ## 2502    731-813-2043 731 813 2043
    ## 2503    563-732-6802 563 732 6802
    ## 2504    145 725 4021 145 725 4021
    ## 2505    931 311 5801 931 311 5801
    ## 2506    637 782-6989 637 782 6989
    ## 2507    172 990 3485 172 990 3485
    ## 2508    872 325 4341 872 325 4341
    ## 2509    359 803-9809 359 803 9809
    ## 2510    152 790 8238 152 790 8238
    ## 2511    330 561 9257 330 561 9257
    ## 2512    437 420 7546 437 420 7546
    ## 2513    495 632 4027 495 632 4027
    ## 2514    416 788-2844 416 788 2844
    ## 2515    311-305-4367 311 305 4367
    ## 2516    817-400-0481 817 400 0481
    ## 2517    430 723 1079 430 723 1079
    ## 2518    729 609-4819 729 609 4819
    ## 2519    201 737-4409 201 737 4409
    ## 2520    137 611-3694 137 611 3694
    ## 2521    226 490 8696 226 490 8696
    ## 2522    123 570 8640 123 570 8640
    ## 2523    665 803 2453 665 803 2453
    ## 2524    812 869-6263 812 869 6263
    ## 2525    639 132 6386 639 132 6386
    ## 2526    194 198-0504 194 198 0504
    ## 2527    437 886 0753 437 886 0753
    ## 2528    626 756 5089 626 756 5089
    ## 2529    299 137-6993 299 137 6993
    ## 2530    714 950 3364 714 950 3364
    ## 2531    653-786-5985 653 786 5985
    ## 2532    518 286 5956 518 286 5956
    ## 2533    194 960 2145 194 960 2145
    ## 2534    362-136-1153 362 136 1153
    ## 2535    376-456-0697 376 456 0697
    ## 2536    657 832 1189 657 832 1189
    ## 2537    962-918-6117 962 918 6117
    ## 2538    692 929 3592 692 929 3592
    ## 2539    805-877-3887 805 877 3887
    ## 2540    739 710-2966 739 710 2966
    ## 2541    819 732 4132 819 732 4132
    ## 2542    367 221 9710 367 221 9710
    ## 2543    361 154 1789 361 154 1789
    ## 2544    680 488 1182 680 488 1182
    ## 2545    928 638 1186 928 638 1186
    ## 2546    588-693-9875 588 693 9875
    ## 2547    681-308-7915 681 308 7915
    ## 2548    783 647 8490 783 647 8490
    ## 2549    897 847 0632 897 847 0632
    ## 2550    150 952 4453 150 952 4453
    ## 2551    322-884-3020 322 884 3020
    ## 2552    176-313-5403 176 313 5403
    ## 2553    487 109 4196 487 109 4196
    ## 2554    477 182-4689 477 182 4689
    ## 2555    544 382 8289 544 382 8289
    ## 2556    781 543 7456 781 543 7456
    ## 2557    911 829 6476 911 829 6476
    ## 2558    525 362-5532 525 362 5532
    ## 2559    517 986 3426 517 986 3426
    ## 2560    838 220 5397 838 220 5397
    ## 2561    687 887-6766 687 887 6766
    ## 2562    179 163 0902 179 163 0902
    ## 2563    539 137 8983 539 137 8983
    ## 2564    733-154-0094 733 154 0094
    ## 2565    639 881 3693 639 881 3693
    ## 2566    291-830-3017 291 830 3017
    ## 2567    637 100-0509 637 100 0509
    ## 2568    750 520 0167 750 520 0167
    ## 2569    676 485 8963 676 485 8963
    ## 2570    135 566 5090 135 566 5090
    ## 2571    337 260 4996 337 260 4996
    ## 2572    371 185 2377 371 185 2377
    ## 2573    280 461 1386 280 461 1386
    ## 2574    603 149-7268 603 149 7268
    ## 2575    364 792-5553 364 792 5553
    ## 2576    496-429-1314 496 429 1314
    ## 2577    459 671 4698 459 671 4698
    ## 2578    486-268-3312 486 268 3312
    ## 2579    497-518-4050 497 518 4050
    ## 2580    535 685-8273 535 685 8273
    ## 2581    859 495 4050 859 495 4050
    ## 2582    826 738-8316 826 738 8316
    ## 2583    724-134-3870 724 134 3870
    ## 2584    554 269-8937 554 269 8937
    ## 2585    125-578-4253 125 578 4253
    ## 2586    614 800 2861 614 800 2861
    ## 2587    487-232-4449 487 232 4449
    ## 2588    298 135-0900 298 135 0900
    ## 2589    392 183-7831 392 183 7831
    ## 2590    606 596-1029 606 596 1029
    ## 2591    384 953-4795 384 953 4795
    ## 2592    855 811-8811 855 811 8811
    ## 2593    253-374-7102 253 374 7102
    ## 2594    419 295-9580 419 295 9580
    ## 2595    802 102 8345 802 102 8345
    ## 2596    417 393 0050 417 393 0050
    ## 2597    787-624-8443 787 624 8443
    ## 2598    919 486-4251 919 486 4251
    ## 2599    341 824 5322 341 824 5322
    ## 2600    415 551 1608 415 551 1608
    ## 2601    392 495-7961 392 495 7961
    ## 2602    473-238-3324 473 238 3324
    ## 2603    506 760-3043 506 760 3043
    ## 2604    876-834-0624 876 834 0624
    ## 2605    919 611 6170 919 611 6170
    ## 2606    146 699-3488 146 699 3488
    ## 2607    261 434 7760 261 434 7760
    ## 2608    617 310 2684 617 310 2684
    ## 2609    182-535-3412 182 535 3412
    ## 2610    506 129 1694 506 129 1694
    ## 2611    302 339-0791 302 339 0791
    ## 2612    446 229-4342 446 229 4342
    ## 2613    249 602-6985 249 602 6985
    ## 2614    150 905-6938 150 905 6938
    ## 2615    313 990 8823 313 990 8823
    ## 2616    656-941-5355 656 941 5355
    ## 2617    116 689-6617 116 689 6617
    ## 2618    955 324 5981 955 324 5981
    ## 2619    175 808 2189 175 808 2189
    ## 2620    896 993-8555 896 993 8555
    ## 2621    105-687-6500 105 687 6500
    ## 2622    757 524 2964 757 524 2964
    ## 2623    201 374 2424 201 374 2424
    ## 2624    466 912-8401 466 912 8401
    ## 2625    766 112 6143 766 112 6143
    ## 2626    783-463-4865 783 463 4865
    ## 2627    853-803-9900 853 803 9900
    ## 2628    347 851-5388 347 851 5388
    ## 2629    992 114 6973 992 114 6973
    ## 2630    316-212-7309 316 212 7309
    ## 2631    301 672 1092 301 672 1092
    ## 2632    795 137 0201 795 137 0201
    ## 2633    381-883-5497 381 883 5497
    ## 2634    100-531-4642 100 531 4642
    ## 2635    994 923 6634 994 923 6634
    ## 2636    920 355 8404 920 355 8404
    ## 2637    441 445-6532 441 445 6532
    ## 2638    325 795 2455 325 795 2455
    ## 2639    593 829 6250 593 829 6250
    ## 2640    566-482-9004 566 482 9004
    ## 2641    542 537 6770 542 537 6770
    ## 2642    716 191 1741 716 191 1741
    ## 2643    491-727-7162 491 727 7162
    ## 2644    167-336-5660 167 336 5660
    ## 2645    358 831 0725 358 831 0725
    ## 2646    432 979 7292 432 979 7292
    ## 2647    205 382-5599 205 382 5599
    ## 2648    208 794-9612 208 794 9612
    ## 2649    728 662 3934 728 662 3934
    ## 2650    380-918-8572 380 918 8572
    ## 2651    905 742-3525 905 742 3525
    ## 2652    151 434 6989 151 434 6989
    ## 2653    755 544 2629 755 544 2629
    ## 2654    633 181 4494 633 181 4494
    ## 2655    346 706 5964 346 706 5964
    ## 2656    688 690 2184 688 690 2184
    ## 2657    618 717 1697 618 717 1697
    ## 2658    185-321-6877 185 321 6877
    ## 2659    147 535-3529 147 535 3529
    ## 2660    152 912-4118 152 912 4118
    ## 2661    726 943 7486 726 943 7486
    ## 2662    634 521-4714 634 521 4714
    ## 2663    670-248-0186 670 248 0186
    ## 2664    121 509-7306 121 509 7306
    ## 2665    105 635-5212 105 635 5212
    ## 2666    732 168-0110 732 168 0110
    ## 2667    364 834 3150 364 834 3150
    ## 2668    176 508 2778 176 508 2778
    ## 2669    120 941 0833 120 941 0833
    ## 2670    670 902 3199 670 902 3199
    ## 2671    214 250-8756 214 250 8756
    ## 2672    119 975-8484 119 975 8484
    ## 2673    297 484 3285 297 484 3285
    ## 2674    489 534-6272 489 534 6272
    ## 2675    610 716-5732 610 716 5732
    ## 2676    456 925-4236 456 925 4236
    ## 2677    743-103-7645 743 103 7645
    ## 2678    432-281-3682 432 281 3682
    ## 2679    167 144-9470 167 144 9470
    ## 2680    648 685 6188 648 685 6188
    ## 2681    548 191 4898 548 191 4898
    ## 2682    288 110 9483 288 110 9483
    ## 2683    946-558-5801 946 558 5801
    ## 2684    388 744 9637 388 744 9637
    ## 2685    506 463 9129 506 463 9129
    ## 2686    848 149-5208 848 149 5208
    ## 2687    970 908-2298 970 908 2298
    ## 2688    843 120-5653 843 120 5653
    ## 2689    306 394 8640 306 394 8640
    ## 2690    170-641-3537 170 641 3537
    ## 2691    860 723 5066 860 723 5066
    ## 2692    814-895-6610 814 895 6610
    ## 2693    139 727-9901 139 727 9901
    ## 2694    598 735 8557 598 735 8557
    ## 2695    593 895 6761 593 895 6761
    ## 2696    817 824-3849 817 824 3849
    ## 2697    508 484 9738 508 484 9738
    ## 2698    719 489 4724 719 489 4724
    ## 2699    503-671-4901 503 671 4901
    ## 2700    275 649 8183 275 649 8183
    ## 2701    968 130 7012 968 130 7012
    ## 2702    290 367 6676 290 367 6676
    ## 2703    499 766 9941 499 766 9941
    ## 2704    538-393-2243 538 393 2243
    ## 2705    540 362-7136 540 362 7136
    ## 2706    802 910-1742 802 910 1742
    ## 2707    845 544 4748 845 544 4748
    ## 2708    784-458-8425 784 458 8425
    ## 2709    365 217-0634 365 217 0634
    ## 2710    708 500 2758 708 500 2758
    ## 2711    594 797-7729 594 797 7729
    ## 2712    982 555 9504 982 555 9504
    ## 2713    477-307-3338 477 307 3338
    ## 2714    744-301-1148 744 301 1148
    ## 2715    389 484 8888 389 484 8888
    ## 2716    739 303 6128 739 303 6128
    ## 2717    175 905 9962 175 905 9962
    ## 2718    900 462-1379 900 462 1379
    ## 2719    878-636-2294 878 636 2294
    ## 2720    998 692-1900 998 692 1900
    ## 2721    994 421 8642 994 421 8642
    ## 2722    304-225-5895 304 225 5895
    ## 2723    931-522-5498 931 522 5498
    ## 2724    838 898 2275 838 898 2275
    ## 2725    718 187 7125 718 187 7125
    ## 2726    943 561 8955 943 561 8955
    ## 2727    341 261 6456 341 261 6456
    ## 2728    507 483-3618 507 483 3618
    ## 2729    826 266 0205 826 266 0205
    ## 2730    380 449-7849 380 449 7849
    ## 2731    589-975-0198 589 975 0198
    ## 2732    657 680 8781 657 680 8781
    ## 2733    501-668-7869 501 668 7869
    ## 2734    224 365 8299 224 365 8299
    ## 2735    747 588 1968 747 588 1968
    ## 2736    706 836-7047 706 836 7047
    ## 2737    698 462-6742 698 462 6742
    ## 2738    748 446 1257 748 446 1257
    ## 2739    525-552-4162 525 552 4162
    ## 2740    635-714-8302 635 714 8302
    ## 2741    114 668 2834 114 668 2834
    ## 2742    404 788 2855 404 788 2855
    ## 2743    673 292 2444 673 292 2444
    ## 2744    136 788 1426 136 788 1426
    ## 2745    564 677 3934 564 677 3934
    ## 2746    213 981 7762 213 981 7762
    ## 2747    453-556-0852 453 556 0852
    ## 2748    423 593 4483 423 593 4483
    ## 2749    820 609 7454 820 609 7454
    ## 2750    131-641-1331 131 641 1331
    ## 2751    824 540 9579 824 540 9579
    ## 2752    753 726-0123 753 726 0123
    ## 2753    739 737 9041 739 737 9041
    ## 2754    609-332-7370 609 332 7370
    ## 2755    426-182-1365 426 182 1365
    ## 2756    347 782-5787 347 782 5787
    ## 2757    898-210-6218 898 210 6218
    ## 2758    938 394 0411 938 394 0411
    ## 2759    904 844 1759 904 844 1759
    ## 2760    658-861-4306 658 861 4306
    ## 2761    716 184-1232 716 184 1232
    ## 2762    380-105-1757 380 105 1757
    ## 2763    969 555-0453 969 555 0453
    ## 2764    249 452 4370 249 452 4370
    ## 2765    443 384 8253 443 384 8253
    ## 2766    711 289 2247 711 289 2247
    ## 2767    343-973-0193 343 973 0193
    ## 2768    367 650 3720 367 650 3720
    ## 2769    265 286-5671 265 286 5671
    ## 2770    148 630 8560 148 630 8560
    ## 2771    413-727-2672 413 727 2672
    ## 2772    162 332-5838 162 332 5838
    ## 2773    229 604-7790 229 604 7790
    ## 2774    892 301-0333 892 301 0333
    ## 2775    148 501 5084 148 501 5084
    ## 2776    564 780 8272 564 780 8272
    ## 2777    524 190 0899 524 190 0899
    ## 2778    463 792 2782 463 792 2782
    ## 2779    331 472-8624 331 472 8624
    ## 2780    522-286-5318 522 286 5318
    ## 2781    310 719 4550 310 719 4550
    ## 2782    314-360-4288 314 360 4288
    ## 2783    301 534-5754 301 534 5754
    ## 2784    822 271 5719 822 271 5719
    ## 2785    341 473-0639 341 473 0639
    ## 2786    835 882-3693 835 882 3693
    ## 2787    465-550-6610 465 550 6610
    ## 2788    388 100 1482 388 100 1482
    ## 2789    589 194-0523 589 194 0523
    ## 2790    982 842 4913 982 842 4913
    ## 2791    671 913 4563 671 913 4563
    ## 2792    144 468 5864 144 468 5864
    ## 2793    194 344 4039 194 344 4039
    ## 2794    331 747 5714 331 747 5714
    ## 2795    221-190-1449 221 190 1449
    ## 2796    322 843-0185 322 843 0185
    ## 2797    676-614-9095 676 614 9095
    ## 2798    190 975-2514 190 975 2514
    ## 2799    909 382-3774 909 382 3774
    ## 2800    956 257 9319 956 257 9319
    ## 2801    362 145 8268 362 145 8268
    ## 2802    570 727 3998 570 727 3998
    ## 2803    225 964 9193 225 964 9193
    ## 2804    227 419 9482 227 419 9482
    ## 2805    949 360 7605 949 360 7605
    ## 2806    347 896-3463 347 896 3463
    ## 2807    794 939 9735 794 939 9735
    ## 2808    413 754 3034 413 754 3034
    ## 2809    806 730 0459 806 730 0459

### **`07-Invalid phone numbers`**

-   Examine the invalid `phone` numbers by filtering for numbers whose
    length is not equal to 12.

-   Remove the rows with invalid numbers by filtering for numbers with a
    length of exactly 12.

``` r
# Check out the invalid numbers
sfo_survey %>%
  filter(str_length(phone) != 12)
```

    ##       id             airline          destination          phone
    ## 1   3010            AMERICAN                MIAMI (637) 782-6989
    ## 2   2097         UNITED INTL          MEXICO CITY (359) 803-9809
    ## 3   1835    TURKISH AIRLINES             ISTANBUL (416) 788-2844
    ## 4    105              UNITED WASHINGTON DC-DULLES (729) 609-4819
    ## 5   1973      CATHAY PACIFIC            HONG KONG (201) 737-4409
    ## 6   2385         UNITED INTL               SYDNEY (137) 611-3694
    ## 7    517              UNITED       FT. LAUDERDALE (812) 869-6263
    ## 8   2885             EVA AIR               TAIPEI (194) 198-0504
    ## 9   2128            FRONTIER               DENVER (299) 137-6993
    ## 10  2132            FRONTIER               DENVER (739) 710-2966
    ## 11   519            EMIRATES                DUBAI (477) 182-4689
    ## 12   656              UNITED               NEWARK (525) 362-5532
    ## 13  2649           SOUTHWEST               DENVER (687) 887-6766
    ## 14  1049          AIR CANADA              CALGARY (637) 100-0509
    ## 15  3040            AMERICAN     DALLAS-FT. WORTH (603) 149-7268
    ## 16   844              ALASKA             PORTLAND (364) 792-5553
    ## 17  1042          AIR CANADA              CALGARY (535) 685-8273
    ## 18  2380         UNITED INTL               SYDNEY (826) 738-8316
    ## 19  2486              UNITED               NEWARK (554) 269-8937
    ## 20  1090               DELTA MINNEAPOLIS-ST. PAUL (298) 135-0900
    ## 21  2211           SOUTHWEST              PHOENIX (392) 183-7831
    ## 22  1555               DELTA              ATLANTA (606) 596-1029
    ## 23  2249              UNITED          BAKERSFIELD (384) 953-4795
    ## 24   879              ALASKA       RALEIGH-DURHAM (855) 811-8811
    ## 25  2956            AMERICAN         NEW YORK-JFK (419) 295-9580
    ## 26  1566               DELTA              DETROIT (919) 486-4251
    ## 27  1585               DELTA       SALT LAKE CITY (392) 495-7961
    ## 28  2670           SOUTHWEST               DENVER (506) 760-3043
    ## 29  3201              UNITED      ONTARIO (CALIF) (146) 699-3488
    ## 30  2765                 WOW            REYKJAVIK (302) 339-0791
    ## 31   272              UNITED          LOS ANGELES (446) 229-4342
    ## 32  1360         UNITED INTL      PARIS-DE GAULLE (249) 602-6985
    ## 33  3046            AMERICAN          LOS ANGELES (150) 905-6938
    ## 34  2673      AIR FRANCE/KLM      PARIS-DE GAULLE (116) 689-6617
    ## 35   750          AIR CANADA              TORONTO (896) 993-8555
    ## 36  1458            FRONTIER               DENVER (466) 912-8401
    ## 37  1487        HAWAIIAN AIR             HONOLULU (347) 851-5388
    ## 38  1454              UNITED            LAS VEGAS (441) 445-6532
    ## 39  2533             JETBLUE           LONG BEACH (205) 382-5599
    ## 40  2904    TURKISH AIRLINES             ISTANBUL (208) 794-9612
    ## 41  2193          AER LINGUS               DUBLIN (905) 742-3525
    ## 42  3087              ALASKA               NEWARK (147) 535-3529
    ## 43  2686      AIR FRANCE/KLM      PARIS-DE GAULLE (152) 912-4118
    ## 44   318              UNITED              BURBANK (634) 521-4714
    ## 45  2182              QANTAS               SYDNEY (121) 509-7306
    ## 46  2628             JETBLUE         NEW YORK-JFK (105) 635-5212
    ## 47  2521             JETBLUE           LONG BEACH (732) 168-0110
    ## 48  2034              UNITED               NEWARK (214) 250-8756
    ## 49   965           LUFTHANSA               MUNICH (119) 975-8484
    ## 50  1197           SOUTHWEST            LAS VEGAS (489) 534-6272
    ## 51   325              UNITED              BURBANK (610) 716-5732
    ## 52   846              ALASKA             PORTLAND (456) 925-4236
    ## 53  1588         UNITED INTL            HONG KONG (167) 144-9470
    ## 54  1851              ALASKA              KAHULUI (848) 149-5208
    ## 55  2226           SOUTHWEST            SANTA ANA (970) 908-2298
    ## 56  2248              UNITED          LOS ANGELES (843) 120-5653
    ## 57   297              UNITED            SAN DIEGO (139) 727-9901
    ## 58  2418              UNITED             HONOLULU (817) 824-3849
    ## 59  1327         UNITED INTL             SHANGHAI (540) 362-7136
    ## 60   664              UNITED               NEWARK (802) 910-1742
    ## 61  2292                 WOW            REYKJAVIK (365) 217-0634
    ## 62  1161               DELTA         NEW YORK-JFK (594) 797-7729
    ## 63  2383         UNITED INTL               SYDNEY (900) 462-1379
    ## 64   413              UNITED              PHOENIX (998) 692-1900
    ## 65  1558               DELTA              ATLANTA (507) 483-3618
    ## 66  3061                COPA          PANAMA CITY (380) 449-7849
    ## 67   741              ALASKA         NEW YORK-JFK (706) 836-7047
    ## 68  2299              UNITED          SAN ANTONIO (698) 462-6742
    ## 69  2659           SOUTHWEST               DENVER (753) 726-0123
    ## 70  2537              UNITED       CHICAGO-O'HARE (347) 782-5787
    ## 71  2453              UNITED               DENVER (716) 184-1232
    ## 72   748          AIR CANADA              TORONTO (969) 555-0453
    ## 73  1589           SOUTHWEST          LOS ANGELES (265) 286-5671
    ## 74  1855              ALASKA              KAHULUI (162) 332-5838
    ## 75  1178           SOUTHWEST            LAS VEGAS (229) 604-7790
    ## 76  1009          AIR CANADA            VANCOUVER (892) 301-0333
    ## 77  1853              ALASKA              KAHULUI (331) 472-8624
    ## 78  1235       CHINA EASTERN             SHANGHAI (301) 534-5754
    ## 79  2189          AER LINGUS               DUBLIN (341) 473-0639
    ## 80     5           SOUTHWEST          LOS ANGELES (835) 882-3693
    ## 81  2294              UNITED          SAN ANTONIO (589) 194-0523
    ## 82  1047          AIR CANADA              CALGARY (322) 843-0185
    ## 83  1586         UNITED INTL            HONG KONG (190) 975-2514
    ## 84  2621          AEROMEXICO          MEXICO CITY (909) 382-3774
    ## 85  1040          AIR CANADA              CALGARY (347) 896-3463
    ## 86   662              UNITED               NEWARK (355) 550-1392
    ## 87  2391         UNITED INTL              BEIJING (705) 456-1905
    ## 88  3058            AMERICAN                MIAMI (836) 207-8419
    ## 89  1100           SOUTHWEST            SAN DIEGO (306) 552-1875
    ## 90  2524             JETBLUE               BOSTON (944) 189-7555
    ## 91  3060            AMERICAN                MIAMI (458) 404-9558
    ## 92  3012              UNITED             PORTLAND (481) 522-1039
    ## 93  1485        HAWAIIAN AIR             HONOLULU (376) 611-4588
    ## 94  2000              UNITED               NEWARK (641) 544-6549
    ## 95  2482              ALASKA       SALT LAKE CITY (572) 748-6932
    ## 96  2031     BRITISH AIRWAYS      LONDON HEATHROW (227) 801-6148
    ## 97  3074                COPA          PANAMA CITY (519) 573-6576
    ## 98  3089              UNITED              SPOKANE (900) 586-1787
    ## 99   728      CATHAY PACIFIC            HONG KONG (239) 325-5321
    ## 100 2689      AIR FRANCE/KLM      PARIS-DE GAULLE (367) 897-7969
    ## 101 2602            INTERJET          GUADALAJARA (621) 874-9973
    ## 102 1416              UNITED            LAS VEGAS (426) 342-7378
    ## 103 2322              QANTAS               SYDNEY (765) 191-1797
    ## 104 1193           SOUTHWEST            LAS VEGAS (400) 250-0871
    ## 105 2939           SOUTHWEST            SAN DIEGO (434) 725-0561
    ## 106 2038     BRITISH AIRWAYS      LONDON HEATHROW (969) 207-3261
    ## 107  220            AMERICAN          LOS ANGELES (994) 688-3259
    ## 108 1678      CATHAY PACIFIC            HONG KONG (494) 308-3048
    ## 109 1073          AIR CANADA              CALGARY (715) 288-8832
    ## 110 2298              QANTAS               SYDNEY (594) 448-6242
    ## 111 3200      CATHAY PACIFIC            HONG KONG (802) 810-5574
    ## 112 1759           LUFTHANSA               MUNICH (867) 891-0871
    ## 113 3152               DELTA       SALT LAKE CITY (533) 213-4368
    ## 114 1328         UNITED INTL               KANSAI (324) 188-6781
    ## 115 2262              UNITED          BAKERSFIELD         0244-5
    ## 116 1990      AIR FRANCE/KLM            AMSTERDAM (102) 928-7959
    ## 117 1356         UNITED INTL               KANSAI (439) 568-6611
    ## 118 2511              ALASKA       SALT LAKE CITY (767) 205-0604
    ## 119 2468              ALASKA       SALT LAKE CITY (890) 548-9219
    ## 120  453              ALASKA            LAS VEGAS (649) 925-8489
    ## 121 2442             JETBLUE               BOSTON (156) 837-4491
    ## 122 2716           SOUTHWEST               DENVER (882) 304-9032
    ## 123  304              UNITED            SAN DIEGO (971) 548-6611
    ## 124 2250                 WOW            REYKJAVIK (828) 153-5819
    ## 125 2491              ALASKA       SALT LAKE CITY (906) 850-9192
    ## 126 1255         UNITED INTL         TOKYO-NARITA (859) 777-8245
    ## 127 2047     BRITISH AIRWAYS      LONDON HEATHROW (589) 270-7518
    ## 128 2172            FRONTIER               DENVER (146) 129-5118
    ## 129 1903           LUFTHANSA               MUNICH (927) 747-9822
    ## 130 2139            FRONTIER               DENVER (481) 479-7013
    ## 131 1404         UNITED INTL      PARIS-DE GAULLE (716) 777-3762
    ## 132 3138               DELTA       SALT LAKE CITY (217) 589-0596
    ## 133  841              ALASKA              SEATTLE (637) 782-6989
    ## 134 2272              UNITED          BAKERSFIELD (359) 803-9809
    ## 135   11           SOUTHWEST          LOS ANGELES (416) 788-2844
    ## 136 1167               DELTA         NEW YORK-JFK (729) 609-4819
    ## 137  302              UNITED            SAN DIEGO (201) 737-4409
    ## 138 1607     VIRGIN ATLANTIC      LONDON HEATHROW (137) 611-3694
    ## 139  911          AIR CANADA              TORONTO (812) 869-6263
    ## 140 1241       CHINA EASTERN             SHANGHAI (194) 198-0504
    ## 141 3023              UNITED             PORTLAND (299) 137-6993
    ## 142  556              ALASKA              SEATTLE (739) 710-2966
    ## 143  334           SOUTHWEST              PHOENIX (477) 182-4689
    ## 144 2682              UNITED       CHICAGO-O'HARE (525) 362-5532
    ## 145 1673            EMIRATES                DUBAI (687) 887-6766
    ## 146 2941    TURKISH AIRLINES             ISTANBUL (637) 100-0509
    ## 147 3144              UNITED            SAN DIEGO (603) 149-7268
    ## 148 1313             JETBLUE         NEW YORK-JFK (364) 792-5553
    ## 149 2896               DELTA       SALT LAKE CITY (535) 685-8273
    ## 150  826              ALASKA            NASHVILLE (826) 738-8316
    ## 151  660         UNITED INTL               MUNICH (554) 269-8937
    ## 152 1515               DELTA          LOS ANGELES (298) 135-0900
    ## 153 3097              UNITED              SPOKANE (392) 183-7831
    ## 154 2462              UNITED       CHICAGO-O'HARE (606) 596-1029
    ## 155 1538               DELTA         NEW YORK-JFK (384) 953-4795
    ## 156 2330               DELTA MINNEAPOLIS-ST. PAUL (855) 811-8811
    ## 157 2005      AIR FRANCE/KLM      PARIS-DE GAULLE (419) 295-9580
    ## 158  320           SOUTHWEST          LOS ANGELES (919) 486-4251
    ## 159  292           SOUTHWEST          LOS ANGELES (392) 495-7961
    ## 160 2820               DELTA          LOS ANGELES (506) 760-3043
    ## 161 3121              ALASKA               NEWARK (146) 699-3488
    ## 162 2748      AIR FRANCE/KLM      PARIS-DE GAULLE (302) 339-0791
    ## 163  264          AIR CANADA              TORONTO (446) 229-4342
    ## 164 2317              QANTAS               SYDNEY (249) 602-6985
    ## 165 2076             JETBLUE           LONG BEACH (150) 905-6938
    ## 166  827              ALASKA              SEATTLE (116) 689-6617
    ## 167 2318              QANTAS               SYDNEY (896) 993-8555
    ## 168 2636             JETBLUE         NEW YORK-JFK (466) 912-8401
    ## 169 1670            EMIRATES                DUBAI (347) 851-5388
    ## 170 1336         UNITED INTL               KANSAI (441) 445-6532
    ## 171 2561             JETBLUE           LONG BEACH (205) 382-5599
    ## 172  669              UNITED               NEWARK (208) 794-9612
    ## 173 3124                COPA          PANAMA CITY (905) 742-3525
    ## 174 1738          AIR CANADA              TORONTO (147) 535-3529
    ## 175 1698              ALASKA            SANTA ANA (152) 912-4118
    ## 176 1035          AIR CANADA            VANCOUVER (634) 521-4714
    ## 177  939          AIR CANADA              TORONTO (121) 509-7306
    ## 178 1461              ALASKA         NEW YORK-JFK (105) 635-5212
    ## 179 2908    TURKISH AIRLINES             ISTANBUL (732) 168-0110
    ## 180 1412              UNITED          KANSAS CITY (214) 250-8756
    ## 181 3240            AMERICAN              PHOENIX (119) 975-8484
    ## 182 1660            EMIRATES                DUBAI (489) 534-6272
    ## 183  688      AIR FRANCE/KLM      PARIS-DE GAULLE (610) 716-5732
    ## 184 1102               DELTA MINNEAPOLIS-ST. PAUL (456) 925-4236
    ## 185 1298             JETBLUE         NEW YORK-JFK (167) 144-9470
    ## 186 2049     BRITISH AIRWAYS      LONDON HEATHROW (848) 149-5208
    ## 187  789            AMERICAN          LOS ANGELES (970) 908-2298
    ## 188 2804           SOUTHWEST              PHOENIX (843) 120-5653
    ## 189  815              ALASKA             PORTLAND (139) 727-9901
    ## 190 3252              UNITED            SAN DIEGO (817) 824-3849
    ## 191 2605              UNITED       CHICAGO-O'HARE (540) 362-7136
    ## 192 2012      AIR FRANCE/KLM      PARIS-DE GAULLE (802) 910-1742
    ## 193 3120                COPA          PANAMA CITY (365) 217-0634
    ## 194 3149               DELTA       SALT LAKE CITY (594) 797-7729
    ## 195 1946      AIR FRANCE/KLM      PARIS-DE GAULLE (900) 462-1379
    ## 196 1520            INTERJET          GUADALAJARA (998) 692-1900
    ## 197 2776                 WOW            REYKJAVIK (507) 483-3618
    ## 198  328              UNITED              BURBANK (380) 449-7849
    ## 199  766              ALASKA         NEW YORK-JFK (706) 836-7047
    ## 200 2888              UNITED               AUSTIN (698) 462-6742
    ## 201 2483              UNITED               DENVER (753) 726-0123
    ## 202 1672            EMIRATES                DUBAI (347) 782-5787
    ## 203  834              ALASKA              SEATTLE (716) 184-1232
    ## 204 2074             JETBLUE           LONG BEACH (969) 555-0453
    ## 205 2895                 WOW            REYKJAVIK (265) 286-5671
    ## 206  724      CATHAY PACIFIC            HONG KONG (162) 332-5838
    ## 207  381              ALASKA    SAN JOSE DEL CABO (229) 604-7790
    ## 208 1760           LUFTHANSA               MUNICH (892) 301-0333
    ## 209 2202          AER LINGUS               DUBLIN (331) 472-8624
    ## 210  308              UNITED            SAN DIEGO (301) 534-5754
    ## 211  344           LUFTHANSA            FRANKFURT (341) 473-0639
    ## 212 1729          AIR CANADA              TORONTO (835) 882-3693
    ## 213  599             JETBLUE               BOSTON (589) 194-0523
    ## 214  704      AIR FRANCE/KLM      PARIS-DE GAULLE (322) 843-0185
    ## 215 3127               DELTA       SALT LAKE CITY (190) 975-2514
    ## 216 1250       CHINA EASTERN             SHANGHAI (909) 382-3774
    ## 217 2691              UNITED              ORLANDO (347) 896-3463
    ## 218 2849              UNITED              SEATTLE (355) 550-1392
    ## 219 1557               DELTA         NEW YORK-JFK (705) 456-1905
    ## 220 2329            AMERICAN       CHICAGO-O'HARE (836) 207-8419
    ## 221  997           LUFTHANSA               MUNICH (306) 552-1875
    ## 222  347              ALASKA            SAN DIEGO (944) 189-7555
    ## 223 1580               DELTA              DETROIT (458) 404-9558
    ## 224  268          AIR CANADA              TORONTO (481) 522-1039
    ## 225  446              UNITED             HONOLULU (376) 611-4588
    ## 226 1664            EMIRATES                DUBAI (641) 544-6549
    ## 227 2276              UNITED          LOS ANGELES (572) 748-6932
    ## 228  199            AMERICAN          LOS ANGELES (227) 801-6148
    ## 229 2017     BRITISH AIRWAYS      LONDON HEATHROW (519) 573-6576
    ## 230 1889  SINGAPORE AIRLINES            SINGAPORE (900) 586-1787
    ## 231 1710      AIR FRANCE/KLM      PARIS-DE GAULLE (239) 325-5321
    ## 232 1080          AIR CANADA              CALGARY (367) 897-7969
    ## 233  431              UNITED         INDIANAPOLIS (621) 874-9973
    ## 234  898          AIR CANADA              TORONTO (426) 342-7378
    ## 235  643              ALASKA          NEW ORLEANS (765) 191-1797
    ## 236 1574         UNITED INTL            HONG KONG (400) 250-0871
    ## 237 2169            FRONTIER               DENVER (434) 725-0561
    ## 238 2365             JETBLUE       FT. LAUDERDALE (969) 207-3261
    ## 239 1577         UNITED INTL            HONG KONG (994) 688-3259
    ## 240 2655             JETBLUE               BOSTON (494) 308-3048
    ## 241 3009            AMERICAN                MIAMI (715) 288-8832
    ## 242 3081                COPA          PANAMA CITY       925 8846
    ## 243 3142               DELTA       SALT LAKE CITY (594) 448-6242
    ## 244 1619     VIRGIN ATLANTIC      LONDON HEATHROW (802) 810-5574
    ## 245 1508        HAWAIIAN AIR             HONOLULU (867) 891-0871
    ## 246 2564             JETBLUE           LONG BEACH (533) 213-4368
    ## 247  763          AIR CANADA              TORONTO (324) 188-6781
    ## 248 1921      AIR FRANCE/KLM      PARIS-DE GAULLE (102) 928-7959
    ## 249 1387         UNITED INTL      PARIS-DE GAULLE (439) 568-6611
    ## 250 1480              ALASKA         NEW YORK-JFK (767) 205-0604
    ## 251 1743      CATHAY PACIFIC            HONG KONG (890) 548-9219
    ## 252 2508              UNITED               NEWARK (649) 925-8489
    ## 253 2277              UNITED          BAKERSFIELD (156) 837-4491
    ## 254 2099         UNITED INTL          MEXICO CITY (882) 304-9032
    ## 255 3172            AMERICAN     DALLAS-FT. WORTH (971) 548-6611
    ## 256 1920      AIR FRANCE/KLM      PARIS-DE GAULLE (828) 153-5819
    ## 257 1986      AIR FRANCE/KLM            AMSTERDAM (906) 850-9192
    ## 258  529            EMIRATES                DUBAI (859) 777-8245
    ## 259 3068              UNITED              SPOKANE (589) 270-7518
    ## 260 3038              UNITED             PORTLAND (146) 129-5118
    ## 261 1634          KOREAN AIR                SEOUL (927) 747-9822
    ## 262 2877             EVA AIR               TAIPEI (481) 479-7013
    ## 263 1600  SINGAPORE AIRLINES            SINGAPORE (716) 777-3762
    ## 264 2562             JETBLUE               BOSTON (217) 589-0596
    ## 265 2161            FRONTIER               DENVER (637) 782-6989
    ## 266 1019          AIR CANADA            VANCOUVER (359) 803-9809
    ## 267 2867              UNITED WASHINGTON DC-DULLES (416) 788-2844
    ## 268 2707               DELTA          LOS ANGELES (729) 609-4819
    ## 269  992           LUFTHANSA               MUNICH (201) 737-4409
    ## 270 2395         UNITED INTL              BEIJING (137) 611-3694
    ## 271 3219              ALASKA             PORTLAND (812) 869-6263
    ## 272 1377         UNITED INTL      PARIS-DE GAULLE (194) 198-0504
    ## 273 2925    TURKISH AIRLINES             ISTANBUL (299) 137-6993
    ## 274 2708      AIR FRANCE/KLM      PARIS-DE GAULLE (739) 710-2966
    ## 275 3141              ALASKA               NEWARK (477) 182-4689
    ## 276 1432              UNITED            LAS VEGAS (525) 362-5532
    ## 277 1891  SINGAPORE AIRLINES            SINGAPORE (687) 887-6766
    ## 278 1433              UNITED             PORTLAND (637) 100-0509
    ## 279 2687              UNITED              ORLANDO (603) 149-7268
    ## 280 1325             JETBLUE         NEW YORK-JFK (364) 792-5553
    ## 281 2287                 WOW            REYKJAVIK (535) 685-8273
    ## 282 2007      AIR FRANCE/KLM      PARIS-DE GAULLE (826) 738-8316
    ## 283 2420     AIR NEW ZEALAND             AUCKLAND (554) 269-8937
    ## 284 1304             JETBLUE         NEW YORK-JFK (298) 135-0900
    ## 285 2643             JETBLUE         NEW YORK-JFK (392) 183-7831
    ## 286 1992      AIR FRANCE/KLM            AMSTERDAM (606) 596-1029
    ## 287  353              ALASKA               NEWARK (384) 953-4795
    ## 288  698      CATHAY PACIFIC            HONG KONG (855) 811-8811
    ## 289 2011      AIR FRANCE/KLM      PARIS-DE GAULLE (419) 295-9580
    ## 290 3125              ALASKA               NEWARK (919) 486-4251
    ## 291 3229            AMERICAN              PHOENIX (392) 495-7961
    ## 292 1733          AIR CANADA              TORONTO (506) 760-3043
    ## 293 2999            AMERICAN                MIAMI (146) 699-3488
    ## 294 3197      CATHAY PACIFIC            HONG KONG (302) 339-0791
    ## 295  437              UNITED             HONOLULU (446) 229-4342
    ## 296 1869              ALASKA              KAHULUI (249) 602-6985
    ## 297  550            EMIRATES                DUBAI (150) 905-6938
    ## 298 3114                COPA          PANAMA CITY (116) 689-6617
    ## 299 2876             EVA AIR               TAIPEI (896) 993-8555
    ## 300 2597             JETBLUE               BOSTON (466) 912-8401
    ## 301  811              ALASKA             PORTLAND (347) 851-5388
    ## 302 3194               DELTA          LOS ANGELES (441) 445-6532
    ## 303 2160      CATHAY PACIFIC            HONG KONG (205) 382-5599
    ## 304 1228           SOUTHWEST            LAS VEGAS (208) 794-9612
    ## 305 2791                 WOW            REYKJAVIK (905) 742-3525
    ## 306 1985              UNITED            BALTIMORE (147) 535-3529
    ## 307 1147           SOUTHWEST            SAN DIEGO (152) 912-4118
    ## 308 2267              UNITED          BAKERSFIELD (634) 521-4714
    ## 309 2496              UNITED               DENVER (121) 509-7306
    ## 310 2806               DELTA          LOS ANGELES (105) 635-5212
    ## 311 1771            AMERICAN       CHICAGO-O'HARE (732) 168-0110
    ## 312 2131          AIR CANADA            VANCOUVER (214) 250-8756
    ## 313 1421              UNITED            LAS VEGAS (119) 975-8484
    ## 314 1179               DELTA         NEW YORK-JFK (489) 534-6272
    ## 315  695      CATHAY PACIFIC            HONG KONG (610) 716-5732
    ## 316 1705      AIR FRANCE/KLM      PARIS-DE GAULLE (456) 925-4236
    ## 317 2265              UNITED          LOS ANGELES (167) 144-9470
    ## 318  824              ALASKA            NASHVILLE (848) 149-5208
    ## 319 1922      AIR FRANCE/KLM      PARIS-DE GAULLE (970) 908-2298
    ## 320 1821              ALASKA       RALEIGH-DURHAM (843) 120-5653
    ## 321 2415     AIR NEW ZEALAND             AUCKLAND (139) 727-9901
    ## 322 1688      CATHAY PACIFIC            HONG KONG (817) 824-3849
    ## 323 2113             JETBLUE           LONG BEACH (540) 362-7136
    ## 324  581             JETBLUE               BOSTON (802) 910-1742
    ## 325  358              ALASKA               NEWARK (365) 217-0634
    ## 326 1950      AIR FRANCE/KLM      PARIS-DE GAULLE (594) 797-7729
    ## 327 2934           SOUTHWEST            SAN DIEGO (900) 462-1379
    ## 328  257          AIR CANADA              TORONTO (998) 692-1900
    ## 329 2624              UNITED       CHICAGO-O'HARE (507) 483-3618
    ## 330 1676      AIR FRANCE/KLM      PARIS-DE GAULLE (380) 449-7849
    ## 331 3146               DELTA       SALT LAKE CITY (706) 836-7047
    ## 332 3237            AMERICAN              PHOENIX (698) 462-6742
    ## 333 2332               DELTA MINNEAPOLIS-ST. PAUL (753) 726-0123
    ## 334 3281           SOUTHWEST               DENVER (347) 782-5787
    ## 335  315              UNITED            SAN DIEGO (716) 184-1232
    ## 336  465              UNITED               DENVER (969) 555-0453
    ## 337 3218            AMERICAN          LOS ANGELES (265) 286-5671
    ## 338  340           SOUTHWEST              PHOENIX           1623
    ## 339 1629          KOREAN AIR                SEOUL (229) 604-7790
    ## 340 1201           SOUTHWEST            LAS VEGAS (892) 301-0333
    ## 341  505              ALASKA             PORTLAND (331) 472-8624
    ## 342  528              ALASKA      PUERTO VALLARTA (301) 534-5754
    ## 343 1937      AIR FRANCE/KLM      PARIS-DE GAULLE (341) 473-0639
    ## 344 2444              UNITED             HONOLULU (835) 882-3693
    ## 345 3022              UNITED             PORTLAND (589) 194-0523
    ## 346 2360              UNITED               AUSTIN (322) 843-0185
    ## 347 1278         UNITED INTL      LONDON HEATHROW (190) 975-2514
    ## 348 1949      AIR FRANCE/KLM      PARIS-DE GAULLE (909) 382-3774
    ## 349 1936              UNITED               BOSTON (347) 896-3463
    ## 350 1976              UNITED            BALTIMORE (355) 550-1392
    ## 351   12           SOUTHWEST          LOS ANGELES (705) 456-1905
    ## 352 1140           SOUTHWEST          LOS ANGELES (836) 207-8419
    ## 353 1652     VIRGIN ATLANTIC      LONDON HEATHROW (306) 552-1875
    ## 354 2449             JETBLUE               BOSTON (944) 189-7555
    ## 355 1645     VIRGIN ATLANTIC      LONDON HEATHROW (458) 404-9558
    ## 356 2883             EVA AIR               TAIPEI (481) 522-1039
    ## 357 3098                COPA          PANAMA CITY (376) 611-4588
    ## 358 1552               DELTA         NEW YORK-JFK (641) 544-6549
    ## 359 2790                 WOW            REYKJAVIK (572) 748-6932
    ## 360  946          AIR CANADA            VANCOUVER (227) 801-6148
    ## 361 1611     VIRGIN ATLANTIC      LONDON HEATHROW (519) 573-6576
    ## 362  608             JETBLUE               BOSTON (900) 586-1787
    ## 363 1915      AIR FRANCE/KLM      PARIS-DE GAULLE (239) 325-5321
    ## 364  462              ALASKA            LAS VEGAS (367) 897-7969
    ## 365 1642     VIRGIN ATLANTIC      LONDON HEATHROW (621) 874-9973
    ## 366  666         UNITED INTL               MUNICH (426) 342-7378
    ## 367 2786                 WOW            REYKJAVIK (765) 191-1797
    ## 368 2200          AER LINGUS               DUBLIN (400) 250-0871
    ## 369  752          AIR CANADA              TORONTO (434) 725-0561
    ## 370 1661            EMIRATES                DUBAI (969) 207-3261
    ## 371 2517              UNITED               NEWARK (994) 688-3259
    ## 372 2726      AIR FRANCE/KLM      PARIS-DE GAULLE (494) 308-3048
    ## 373 1666            EMIRATES                DUBAI (715) 288-8832
    ## 374 1398         UNITED INTL             SHANGHAI (594) 448-6242
    ## 375 2376             JETBLUE       FT. LAUDERDALE (802) 810-5574
    ## 376 1890              UNITED               BOSTON (867) 891-0871
    ## 377 1702      AIR FRANCE/KLM      PARIS-DE GAULLE (533) 213-4368
    ## 378 1464              ALASKA         NEW YORK-JFK (324) 188-6781
    ## 379  227            AMERICAN          LOS ANGELES (102) 928-7959
    ## 380 1872              ALASKA              KAHULUI (439) 568-6611
    ## 381 2024              UNITED               NEWARK (767) 205-0604
    ## 382 2127            FRONTIER               DENVER (890) 548-9219
    ## 383  497              UNITED               DENVER (649) 925-8489
    ## 384  809              ALASKA             PORTLAND (156) 837-4491
    ## 385 2644             JETBLUE         NEW YORK-JFK (882) 304-9032
    ## 386 1979      CATHAY PACIFIC            HONG KONG (971) 548-6611
    ## 387 2638             JETBLUE         NEW YORK-JFK (828) 153-5819
    ## 388 3071 PHILIPPINE AIRLINES               MANILA (906) 850-9192
    ## 389 1543           AIR CHINA              BEIJING (859) 777-8245
    ## 390 1968      CATHAY PACIFIC            HONG KONG (589) 270-7518
    ## 391 2210          AER LINGUS               DUBLIN (146) 129-5118
    ## 392 1359         UNITED INTL               KANSAI (927) 747-9822
    ## 393 1900              UNITED               BOSTON (481) 479-7013
    ## 394 1038          AIR CANADA            VANCOUVER (716) 777-3762
    ## 395 2984            AMERICAN            CHARLOTTE (217) 589-0596
    ## 396 1126               DELTA MINNEAPOLIS-ST. PAUL (637) 782-6989
    ## 397 2631             JETBLUE         NEW YORK-JFK (359) 803-9809
    ## 398  907          AIR CANADA              TORONTO (416) 788-2844
    ## 399  527            EMIRATES                DUBAI (729) 609-4819
    ## 400 2559             JETBLUE               BOSTON (201) 737-4409
    ## 401 2599          AEROMEXICO          MEXICO CITY (137) 611-3694
    ## 402 1128               DELTA MINNEAPOLIS-ST. PAUL        665-803
    ## 403 2153            FRONTIER               DENVER (812) 869-6263
    ## 404 1777              UNITED       RALEIGH-DURHAM (194) 198-0504
    ## 405  999           LUFTHANSA               MUNICH (299) 137-6993
    ## 406  383              ALASKA          LOS ANGELES (739) 710-2966
    ## 407 2150            FRONTIER               DENVER (477) 182-4689
    ## 408 3202      CATHAY PACIFIC            HONG KONG (525) 362-5532
    ## 409  542            EMIRATES                DUBAI (687) 887-6766
    ## 410 1346         UNITED INTL               KANSAI (637) 100-0509
    ## 411  697      AIR FRANCE/KLM      PARIS-DE GAULLE (603) 149-7268
    ## 412  836              ALASKA              SEATTLE (364) 792-5553
    ## 413  445              UNITED         INDIANAPOLIS (535) 685-8273
    ## 414  928              ALASKA       RALEIGH-DURHAM (826) 738-8316
    ## 415  559              ALASKA              SEATTLE (554) 269-8937
    ## 416 2529             JETBLUE           LONG BEACH (298) 135-0900
    ## 417  835              ALASKA            NASHVILLE (392) 183-7831
    ## 418 1002           LUFTHANSA               MUNICH (606) 596-1029
    ## 419  861            AMERICAN     DALLAS-FT. WORTH (384) 953-4795
    ## 420 2662              UNITED       RALEIGH-DURHAM (855) 811-8811
    ## 421 3126              ALASKA               NEWARK (419) 295-9580
    ## 422 1822         UNITED INTL      LONDON HEATHROW (919) 486-4251
    ## 423 2556             JETBLUE           LONG BEACH (392) 495-7961
    ## 424 2266                 WOW            REYKJAVIK (506) 760-3043
    ## 425 2301              QANTAS               SYDNEY (146) 699-3488
    ## 426  558            EMIRATES                DUBAI (302) 339-0791
    ## 427 1919      AIR FRANCE/KLM      PARIS-DE GAULLE (446) 229-4342
    ## 428 1382         UNITED INTL      PARIS-DE GAULLE (249) 602-6985
    ## 429  808              ALASKA            BALTIMORE (150) 905-6938
    ## 430 3140               DELTA       SALT LAKE CITY (116) 689-6617
    ## 431 2212          AER LINGUS               DUBLIN (896) 993-8555
    ## 432 2579             JETBLUE           LONG BEACH (466) 912-8401
    ## 433  363           LUFTHANSA            FRANKFURT (347) 851-5388
    ## 434  416              UNITED         HOUSTON-BUSH (441) 445-6532
    ## 435 3222              ALASKA             PORTLAND (205) 382-5599
    ## 436 2671              UNITED       RALEIGH-DURHAM (208) 794-9612
    ## 437 2306            AMERICAN       CHICAGO-O'HARE (905) 742-3525
    ## 438  216            AMERICAN          LOS ANGELES (147) 535-3529
    ## 439 1768            AMERICAN       CHICAGO-O'HARE (152) 912-4118
    ## 440 3156               DELTA       SALT LAKE CITY (634) 521-4714
    ## 441 2756               DELTA              ATLANTA (121) 509-7306
    ## 442  214            AMERICAN          LOS ANGELES (105) 635-5212
    ## 443  212            AMERICAN          LOS ANGELES (732) 168-0110
    ## 444 2824              UNITED              BURBANK (214) 250-8756
    ## 445 2348              UNITED               AUSTIN (119) 975-8484
    ## 446 3245            AMERICAN              PHOENIX (489) 534-6272
    ## 447 3270              UNITED            LAS VEGAS (610) 716-5732
    ## 448 1118           SOUTHWEST            SAN DIEGO (456) 925-4236
    ## 449 2096             JETBLUE           LONG BEACH (167) 144-9470
    ## 450 1618     VIRGIN ATLANTIC      LONDON HEATHROW (848) 149-5208
    ## 451 2485              UNITED               DENVER (970) 908-2298
    ## 452 2787               DELTA              ATLANTA (843) 120-5653
    ## 453 2880             EVA AIR               TAIPEI (139) 727-9901
    ## 454  941          AIR CANADA            VANCOUVER (817) 824-3849
    ## 455 1999              UNITED            BALTIMORE (540) 362-7136
    ## 456 1312         UNITED INTL      LONDON HEATHROW (802) 910-1742
    ## 457 2690              UNITED       CHICAGO-O'HARE (365) 217-0634
    ## 458 2830              UNITED              BURBANK (594) 797-7729
    ## 459 2646             JETBLUE         NEW YORK-JFK (900) 462-1379
    ## 460  593             JETBLUE               BOSTON (998) 692-1900
    ## 461 2591             JETBLUE               BOSTON (507) 483-3618
    ## 462 2303              UNITED          SAN ANTONIO (380) 449-7849
    ## 463 2495              UNITED       CHICAGO-O'HARE (706) 836-7047
    ## 464 2829              UNITED              BURBANK (698) 462-6742
    ## 465  768              ALASKA         NEW YORK-JFK (753) 726-0123
    ## 466  644              ALASKA WASHINGTON DC-DULLES (347) 782-5787
    ## 467  378              ALASKA    SAN JOSE DEL CABO (716) 184-1232
    ## 468 3221            AMERICAN          LOS ANGELES (969) 555-0453
    ## 469 2639              UNITED       CHICAGO-O'HARE (265) 286-5671
    ## 470 1631          KOREAN AIR                SEOUL (162) 332-5838
    ## 471 1994              UNITED            BALTIMORE (229) 604-7790
    ## 472  310              UNITED WASHINGTON DC-DULLES (892) 301-0333
    ## 473  439              UNITED             HONOLULU (331) 472-8624
    ## 474 2641              UNITED       CHICAGO-O'HARE (301) 534-5754
    ## 475 2800               DELTA       SALT LAKE CITY (341) 473-0639
    ## 476 3052            AMERICAN                MIAMI (835) 882-3693
    ## 477 1361         UNITED INTL               KANSAI (589) 194-0523
    ## 478  606             JETBLUE               BOSTON (322) 843-0185
    ## 479 1394         UNITED INTL      PARIS-DE GAULLE (190) 975-2514
    ## 480 2611          AEROMEXICO          MEXICO CITY (909) 382-3774
    ## 481 3094                COPA          PANAMA CITY (347) 896-3463
    ## 482 1934      AIR FRANCE/KLM      PARIS-DE GAULLE (355) 550-1392
    ## 483 1475              ALASKA         NEW YORK-JFK (705) 456-1905
    ## 484 1742      CATHAY PACIFIC            HONG KONG (836) 207-8419
    ## 485 1940      AIR FRANCE/KLM      PARIS-DE GAULLE (306) 552-1875
    ## 486  991           LUFTHANSA               MUNICH (944) 189-7555
    ## 487 2369             JETBLUE       FT. LAUDERDALE (458) 404-9558
    ## 488  395              ALASKA          LOS ANGELES (481) 522-1039
    ## 489 1456              ALASKA         NEW YORK-JFK (376) 611-4588
    ## 490 1625          KOREAN AIR                SEOUL (641) 544-6549
    ## 491 1013          AIR CANADA            VANCOUVER (572) 748-6932
    ## 492 1713      AIR FRANCE/KLM      PARIS-DE GAULLE (227) 801-6148
    ## 493 1956              UNITED               BOSTON (519) 573-6576
    ## 494 2798               DELTA       SALT LAKE CITY (900) 586-1787
    ## 495  361           LUFTHANSA            FRANKFURT (239) 325-5321
    ## 496 1322         UNITED INTL      LONDON HEATHROW (367) 897-7969
    ## 497 1709      AIR FRANCE/KLM      PARIS-DE GAULLE (621) 874-9973
    ## 498 2364              UNITED               AUSTIN (426) 342-7378
    ## 499 3075              UNITED              SPOKANE (765) 191-1797
    ## 500  758              ALASKA         NEW YORK-JFK (400) 250-0871
    ## 501 1689      CATHAY PACIFIC            HONG KONG (434) 725-0561
    ## 502 1762           LUFTHANSA               MUNICH (969) 207-3261
    ## 503 2100             JETBLUE           LONG BEACH (994) 688-3259
    ## 504 1512               DELTA          LOS ANGELES (494) 308-3048
    ## 505 2875             EVA AIR               TAIPEI (715) 288-8832
    ## 506  414              UNITED         HOUSTON-BUSH (594) 448-6242
    ## 507 2314              UNITED          SAN ANTONIO (802) 810-5574
    ## 508 1162           SOUTHWEST          LOS ANGELES (867) 891-0871
    ## 509 1296         UNITED INTL      LONDON HEATHROW (533) 213-4368
    ## 510 2711              UNITED        SANTA BARBARA (324) 188-6781
    ## 511 2098             JETBLUE           LONG BEACH (102) 928-7959
    ## 512 2664          AEROMEXICO          MEXICO CITY (439) 568-6611
    ## 513 2302            AMERICAN       CHICAGO-O'HARE (767) 205-0604
    ## 514  713      AIR FRANCE/KLM      PARIS-DE GAULLE (890) 548-9219
    ## 515 3199      CATHAY PACIFIC            HONG KONG (649) 925-8489
    ## 516 3072              UNITED              SPOKANE (156) 837-4491
    ## 517 2066            AMERICAN         PHILADELPHIA (882) 304-9032
    ## 518 2123             JETBLUE           LONG BEACH (971) 548-6611
    ## 519 2065              UNITED               NEWARK (828) 153-5819
    ## 520 3236              ALASKA             PORTLAND (906) 850-9192
    ## 521 1765            AMERICAN       CHICAGO-O'HARE (859) 777-8245
    ## 522  790         UNITED INTL    SAN JOSE DEL CABO (589) 270-7518
    ## 523  708      AIR FRANCE/KLM      PARIS-DE GAULLE (146) 129-5118
    ## 524 2775               DELTA              ATLANTA (927) 747-9822
    ## 525  923              ALASKA       RALEIGH-DURHAM (481) 479-7013
    ## 526 2940    TURKISH AIRLINES             ISTANBUL (716) 777-3762
    ## 527 2825              UNITED              BURBANK (217) 589-0596
    ## 528 2860              UNITED              SEATTLE (637) 782-6989
    ## 529 2146            FRONTIER               DENVER (359) 803-9809
    ## 530 2474              ALASKA       SALT LAKE CITY (416) 788-2844
    ## 531 3216            AMERICAN          LOS ANGELES (729) 609-4819
    ## 532 3164            AMERICAN     DALLAS-FT. WORTH (201) 737-4409
    ## 533 1261       CHINA EASTERN             SHANGHAI (137) 611-3694
    ## 534  348           LUFTHANSA            FRANKFURT (812) 869-6263
    ## 535  646              ALASKA WASHINGTON DC-DULLES (194) 198-0504
    ## 536 1430              UNITED            LAS VEGAS (299) 137-6993
    ## 537 2050     BRITISH AIRWAYS      LONDON HEATHROW (739) 710-2966
    ## 538 1517               DELTA          LOS ANGELES (477) 182-4689
    ## 539 2037              UNITED               NEWARK (525) 362-5532
    ## 540 2684              UNITED              ORLANDO (687) 887-6766
    ## 541 2351              UNITED               AUSTIN (637) 100-0509
    ## 542 2216           SOUTHWEST              PHOENIX (603) 149-7268
    ## 543  549              ALASKA              SEATTLE (364) 792-5553
    ## 544  967           LUFTHANSA               MUNICH (535) 685-8273
    ## 545 2010              UNITED               NEWARK (826) 738-8316
    ## 546 2461              UNITED               DENVER (554) 269-8937
    ## 547  979           LUFTHANSA               MUNICH (298) 135-0900
    ## 548  854              ALASKA             PORTLAND (392) 183-7831
    ## 549 1637          KOREAN AIR                SEOUL (606) 596-1029
    ## 550 3239            AMERICAN              PHOENIX (384) 953-4795
    ## 551  816              ALASKA            BALTIMORE (855) 811-8811
    ## 552 1145           SOUTHWEST            SAN DIEGO (419) 295-9580
    ## 553 2075            AMERICAN         PHILADELPHIA (919) 486-4251
    ## 554 1993              UNITED            BALTIMORE (392) 495-7961
    ## 555 2598              UNITED       CHICAGO-O'HARE (506) 760-3043
    ## 556 2832              UNITED              BURBANK (146) 699-3488
    ## 557  679              UNITED               NEWARK (302) 339-0791
    ## 558 1772            AMERICAN       CHICAGO-O'HARE (446) 229-4342
    ## 559 2052              UNITED               NEWARK (249) 602-6985
    ## 560 2821               DELTA          LOS ANGELES (150) 905-6938
    ## 561 2502              UNITED       CHICAGO-O'HARE (116) 689-6617
    ## 562 1084          AIR CANADA              CALGARY (896) 993-8555
    ## 563  955          AIR CANADA            VANCOUVER (466) 912-8401
    ## 564 2257              UNITED          LOS ANGELES (347) 851-5388
    ## 565  801              ALASKA            BALTIMORE (441) 445-6532
    ## 566 2581             JETBLUE               BOSTON (205) 382-5599
    ## 567 2760               DELTA              ATLANTA (208) 794-9612
    ## 568 1323         UNITED INTL      LONDON HEATHROW (905) 742-3525
    ## 569   13           SOUTHWEST          LOS ANGELES (147) 535-3529
    ## 570 2576             JETBLUE           LONG BEACH (152) 912-4118
    ## 571 2528             JETBLUE           LONG BEACH (634) 521-4714
    ## 572 2532             JETBLUE           LONG BEACH (121) 509-7306
    ## 573 3128              UNITED            SAN DIEGO (105) 635-5212
    ## 574   14           SOUTHWEST          LOS ANGELES (732) 168-0110
    ## 575 1537               DELTA         NEW YORK-JFK (214) 250-8756
    ## 576 2304            AMERICAN       CHICAGO-O'HARE (119) 975-8484
    ## 577  430              UNITED         INDIANAPOLIS (489) 534-6272
    ## 578  800            AMERICAN          LOS ANGELES (610) 716-5732
    ## 579  871              ALASKA              SEATTLE (456) 925-4236
    ## 580 2300            AMERICAN       CHICAGO-O'HARE (167) 144-9470
    ## 581 1913      AIR FRANCE/KLM      PARIS-DE GAULLE (848) 149-5208
    ## 582 2423     AIR NEW ZEALAND             AUCKLAND (970) 908-2298
    ## 583 2430              UNITED             HONOLULU (843) 120-5653
    ## 584 1726          AIR CANADA              TORONTO (139) 727-9901
    ## 585 3003            AMERICAN                MIAMI (817) 824-3849
    ## 586 1493            FRONTIER               DENVER (540) 362-7136
    ## 587 1592           SOUTHWEST          LOS ANGELES (802) 910-1742
    ## 588 1873              UNITED               BOSTON (365) 217-0634
    ## 589 2853              UNITED              SEATTLE (594) 797-7729
    ## 590 2774                 WOW            REYKJAVIK (900) 462-1379
    ## 591  455              ALASKA            LAS VEGAS (998) 692-1900
    ## 592  884              ALASKA              SEATTLE (507) 483-3618
    ## 593 1793              UNITED       RALEIGH-DURHAM (380) 449-7849
    ## 594  411              UNITED              PHOENIX (706) 836-7047
    ## 595 1665            EMIRATES                DUBAI (698) 462-6742
    ## 596 1638          KOREAN AIR                SEOUL (753) 726-0123
    ## 597  397              ALASKA          LOS ANGELES (347) 782-5787
    ## 598 1677      AIR FRANCE/KLM      PARIS-DE GAULLE (716) 184-1232
    ## 599 1384         UNITED INTL      PARIS-DE GAULLE (969) 555-0453
    ## 600 1242         UNITED INTL         TOKYO-NARITA (265) 286-5671
    ## 601 1807         UNITED INTL      LONDON HEATHROW (162) 332-5838
    ## 602 1505        HAWAIIAN AIR             HONOLULU (229) 604-7790
    ## 603  367              ALASKA    SAN JOSE DEL CABO (892) 301-0333
    ## 604 1286         UNITED INTL      LONDON HEATHROW (331) 472-8624
    ## 605  317              UNITED            SAN DIEGO (301) 534-5754
    ## 606 1886  SINGAPORE AIRLINES            SINGAPORE (341) 473-0639
    ## 607 2534             JETBLUE               BOSTON (835) 882-3693
    ## 608 2954           SOUTHWEST          LOS ANGELES (589) 194-0523
    ## 609 1959              UNITED               BOSTON (322) 843-0185
    ## 610 1553           AIR CHINA              BEIJING (190) 975-2514
    ## 611 2694              UNITED       CHICAGO-O'HARE (909) 382-3774
    ## 612    2           SOUTHWEST            SAN DIEGO (347) 896-3463
    ## 613  868            AMERICAN     DALLAS-FT. WORTH (355) 550-1392
    ## 614 1413              UNITED            LAS VEGAS (705) 456-1905
    ## 615 3266              UNITED            LAS VEGAS (836) 207-8419
    ## 616 2492              UNITED               DENVER (306) 552-1875
    ## 617  373              ALASKA    SAN JOSE DEL CABO          38515
    ## 618 1172               DELTA         NEW YORK-JFK (944) 189-7555
    ## 619 1401         UNITED INTL      PARIS-DE GAULLE (458) 404-9558
    ## 620 1703      AIR FRANCE/KLM      PARIS-DE GAULLE (481) 522-1039
    ## 621 1767            AMERICAN       CHICAGO-O'HARE (376) 611-4588
    ## 622 2019              UNITED               NEWARK (641) 544-6549
    ## 623 2695      AIR FRANCE/KLM      PARIS-DE GAULLE (572) 748-6932
    ## 624 3001            AMERICAN                MIAMI (227) 801-6148
    ## 625 2816               DELTA          LOS ANGELES (519) 573-6576
    ## 626 1506        HAWAIIAN AIR             HONOLULU (900) 586-1787
    ## 627 3196               DELTA          LOS ANGELES (239) 325-5321
    ## 628 1608     VIRGIN ATLANTIC      LONDON HEATHROW (367) 897-7969
    ## 629 2427     AIR NEW ZEALAND             AUCKLAND (621) 874-9973
    ## 630 1407              UNITED            LAS VEGAS (426) 342-7378
    ## 631 1610     VIRGIN ATLANTIC      LONDON HEATHROW (765) 191-1797
    ## 632 1800         UNITED INTL      LONDON HEATHROW (400) 250-0871
    ## 633 1923      AIR FRANCE/KLM      PARIS-DE GAULLE (434) 725-0561
    ## 634 2173            FRONTIER               DENVER (969) 207-3261
    ## 635 2600            INTERJET          GUADALAJARA (994) 688-3259
    ## 636 1623     VIRGIN ATLANTIC      LONDON HEATHROW (494) 308-3048
    ## 637 3084                COPA          PANAMA CITY (715) 288-8832
    ## 638 2899               DELTA       SALT LAKE CITY (594) 448-6242
    ## 639  463              ALASKA            LAS VEGAS (802) 810-5574
    ## 640 2309              UNITED          SAN ANTONIO (867) 891-0871
    ## 641 2864              UNITED              SEATTLE (533) 213-4368
    ## 642 2593              UNITED       CHICAGO-O'HARE (324) 188-6781
    ## 643 1847           SOUTHWEST            SAN DIEGO (102) 928-7959
    ## 644 2141            FRONTIER               DENVER (439) 568-6611
    ## 645  298              UNITED            SAN DIEGO (767) 205-0604
    ## 646 2603             JETBLUE               BOSTON (890) 548-9219
    ## 647 2040              UNITED               NEWARK (649) 925-8489
    ## 648 2862              UNITED              SEATTLE (156) 837-4491
    ## 649  415              UNITED              PHOENIX (882) 304-9032
    ## 650 3248            AMERICAN              PHOENIX (971) 548-6611
    ## 651  715      CATHAY PACIFIC            HONG KONG (828) 153-5819
    ## 652 2574             JETBLUE           LONG BEACH (906) 850-9192
    ## 653 3108                COPA          PANAMA CITY (859) 777-8245
    ## 654  500              ALASKA             PORTLAND (589) 270-7518
    ## 655 1295             JETBLUE         NEW YORK-JFK (146) 129-5118
    ## 656  736      CATHAY PACIFIC            HONG KONG (927) 747-9822
    ## 657 1482            FRONTIER               DENVER (481) 479-7013
    ## 658  392              ALASKA          LOS ANGELES (716) 777-3762
    ## 659 2594            INTERJET          GUADALAJARA (217) 589-0596
    ## 660 1131               DELTA MINNEAPOLIS-ST. PAUL (637) 782-6989
    ## 661  641              ALASKA WASHINGTON DC-DULLES (359) 803-9809
    ## 662 2083             JETBLUE           LONG BEACH (416) 788-2844
    ## 663  580              ALASKA              SEATTLE (729) 609-4819
    ## 664  859            AMERICAN     DALLAS-FT. WORTH (201) 737-4409
    ## 665 1578               DELTA              DETROIT (137) 611-3694
    ## 666 2715               DELTA          LOS ANGELES (812) 869-6263
    ## 667  749              ALASKA         NEW YORK-JFK (194) 198-0504
    ## 668 3171               DELTA          LOS ANGELES (299) 137-6993
    ## 669 1105               DELTA MINNEAPOLIS-ST. PAUL (739) 710-2966
    ## 670 1908              UNITED               BOSTON (477) 182-4689
    ## 671  438              UNITED         INDIANAPOLIS (525) 362-5532
    ## 672 2108             JETBLUE           LONG BEACH (687) 887-6766
    ## 673  521            EMIRATES                DUBAI (637) 100-0509
    ## 674  290           SOUTHWEST          LOS ANGELES (603) 149-7268
    ## 675  319           SOUTHWEST          LOS ANGELES (364) 792-5553
    ## 676 1797              ALASKA       RALEIGH-DURHAM (535) 685-8273
    ## 677 2143            FRONTIER               DENVER (826) 738-8316
    ## 678 2588             JETBLUE           LONG BEACH (554) 269-8937
    ## 679 1899           LUFTHANSA               MUNICH (298) 135-0900
    ## 680 1483             AVIANCA         SAN SALVADOR (392) 183-7831
    ## 681 1391         UNITED INTL             SHANGHAI (606) 596-1029
    ## 682 3042              UNITED             PORTLAND (384) 953-4795
    ## 683 1996              UNITED            BALTIMORE (855) 811-8811
    ## 684 2497              UNITED       CHICAGO-O'HARE (419) 295-9580
    ## 685 2551             JETBLUE               BOSTON (919) 486-4251
    ## 686 2906    TURKISH AIRLINES             ISTANBUL (392) 495-7961
    ## 687 1850           SOUTHWEST            SAN DIEGO (506) 760-3043
    ## 688 1704      AIR FRANCE/KLM      PARIS-DE GAULLE (146) 699-3488
    ## 689 2334               DELTA MINNEAPOLIS-ST. PAUL (302) 339-0791
    ## 690 1386         UNITED INTL             SHANGHAI (446) 229-4342
    ## 691 3285           SOUTHWEST               DENVER (249) 602-6985
    ## 692 2073            AMERICAN         PHILADELPHIA (150) 905-6938
    ## 693 2944           SOUTHWEST            SAN DIEGO (116) 689-6617
    ## 694 2513              UNITED               NEWARK (896) 993-8555
    ## 695 1319             JETBLUE         NEW YORK-JFK (466) 912-8401
    ## 696  426              UNITED         INDIANAPOLIS (347) 851-5388
    ## 697 1290             JETBLUE         NEW YORK-JFK (441) 445-6532
    ## 698  650              ALASKA WASHINGTON DC-DULLES (205) 382-5599
    ## 699 2518              ALASKA       SALT LAKE CITY (208) 794-9612
    ## 700  717      AIR FRANCE/KLM      PARIS-DE GAULLE (905) 742-3525
    ## 701 2232           SOUTHWEST            SANTA ANA (147) 535-3529
    ## 702 2400         UNITED INTL              BEIJING (152) 912-4118
    ## 703  452              UNITED     DALLAS-FT. WORTH (634) 521-4714
    ## 704 2515              ALASKA       SALT LAKE CITY (121) 509-7306
    ## 705  568            EMIRATES                DUBAI (105) 635-5212
    ## 706 2269              UNITED          LOS ANGELES (732) 168-0110
    ## 707 1554               DELTA         NEW YORK-JFK (214) 250-8756
    ## 708 1556               DELTA         NEW YORK-JFK (119) 975-8484
    ## 709 2661          AEROMEXICO          MEXICO CITY (489) 534-6272
    ## 710 2842              UNITED       CHICAGO-O'HARE (610) 716-5732
    ## 711 3162              UNITED            SAN DIEGO (456) 925-4236
    ## 712 2168            FRONTIER               DENVER (167) 144-9470
    ## 713 3016              UNITED             PORTLAND (848) 149-5208
    ## 714 1604     VIRGIN ATLANTIC      LONDON HEATHROW (970) 908-2298
    ## 715 2402         UNITED INTL              BEIJING (843) 120-5653
    ## 716 2667          AEROMEXICO          MEXICO CITY (139) 727-9901
    ## 717 2743               DELTA              ATLANTA (817) 824-3849
    ## 718 1224           SOUTHWEST            LAS VEGAS (540) 362-7136
    ## 719 2178       CHINA EASTERN              QINGDAO (802) 910-1742
    ## 720 3251              UNITED            SAN DIEGO (365) 217-0634
    ## 721  372           LUFTHANSA            FRANKFURT (594) 797-7729
    ## 722 1453              UNITED             PORTLAND (900) 462-1379
    ## 723 2324              QANTAS               SYDNEY (998) 692-1900
    ## 724 1132           SOUTHWEST            SAN DIEGO (507) 483-3618
    ## 725  566            EMIRATES                DUBAI (380) 449-7849
    ## 726 3100                COPA          PANAMA CITY (706) 836-7047
    ## 727 1411              UNITED            LAS VEGAS (698) 462-6742
    ## 728 3083 PHILIPPINE AIRLINES               MANILA (753) 726-0123
    ## 729 3113              ALASKA               NEWARK (347) 782-5787
    ## 730 3082              UNITED              SPOKANE (716) 184-1232
    ## 731 2162       CHINA EASTERN              QINGDAO (969) 555-0453
    ## 732 1279      CHINA SOUTHERN                WUHAN (265) 286-5671
    ## 733 2958           SOUTHWEST          LOS ANGELES (162) 332-5838
    ## 734 3225            AMERICAN              PHOENIX (229) 604-7790
    ## 735  474              UNITED               DENVER (892) 301-0333
    ## 736 1775              UNITED       RALEIGH-DURHAM (331) 472-8624
    ## 737 2993              UNITED            BALTIMORE (301) 534-5754
    ## 738 2998              UNITED              BURBANK (341) 473-0639
    ## 739 3027              ALASKA              SEATTLE (835) 882-3693
    ## 740 3011              ALASKA WASHINGTON DC-DULLES (589) 194-0523
    ## 741 3000               DELTA MINNEAPOLIS-ST. PAUL (322) 843-0185
    ## 742 2969              UNITED             HONOLULU (190) 975-2514
    ## 743 3020              ALASKA              SEATTLE (909) 382-3774
    ## 744 2971              UNITED       CHICAGO-O'HARE (347) 896-3463

``` r
# Remove rows with invalid numbers
sfo_survey %>%
  filter(str_length(phone) == 12)
```

    ##        id             airline          destination        phone
    ## 1    1842    TURKISH AIRLINES             ISTANBUL 858 990 5153
    ## 2    1844    TURKISH AIRLINES             ISTANBUL 731-813-2043
    ## 3    1840    TURKISH AIRLINES             ISTANBUL 563-732-6802
    ## 4    1837    TURKISH AIRLINES             ISTANBUL 145 725 4021
    ## 5    1833    TURKISH AIRLINES             ISTANBUL 931 311 5801
    ## 6    1838    TURKISH AIRLINES             ISTANBUL 172 990 3485
    ## 7    1845    TURKISH AIRLINES             ISTANBUL 872 325 4341
    ## 8    1846    TURKISH AIRLINES             ISTANBUL 152 790 8238
    ## 9    1831    TURKISH AIRLINES             ISTANBUL 330 561 9257
    ## 10   1848    TURKISH AIRLINES             ISTANBUL 437 420 7546
    ## 11   1841    TURKISH AIRLINES             ISTANBUL 495 632 4027
    ## 12   1849    TURKISH AIRLINES             ISTANBUL 311-305-4367
    ## 13   2289              QANTAS               SYDNEY 817-400-0481
    ## 14    977           LUFTHANSA               MUNICH 430 723 1079
    ## 15   3209              UNITED        SANTA BARBARA 226 490 8696
    ## 16   1794              ALASKA       RALEIGH-DURHAM 123 570 8640
    ## 17    278              UNITED            BALTIMORE 665 803 2453
    ## 18   1265      CATHAY PACIFIC            HONG KONG 639 132 6386
    ## 19   2457              UNITED       CHICAGO-O'HARE 437 886 0753
    ## 20   3198              UNITED      ONTARIO (CALIF) 626 756 5089
    ## 21   2199           SOUTHWEST            LAS VEGAS 714 950 3364
    ## 22   1376         UNITED INTL             SHANGHAI 653-786-5985
    ## 23   2152          AIR CANADA            VANCOUVER 518 286 5956
    ## 24    621              ALASKA WASHINGTON DC-DULLES 194 960 2145
    ## 25      4           SOUTHWEST          LOS ANGELES 362-136-1153
    ## 26   1541           AIR CHINA              BEIJING 376-456-0697
    ## 27   2242                 WOW            REYKJAVIK 657 832 1189
    ## 28   1039          AIR CANADA              CALGARY 962-918-6117
    ## 29    475              ALASKA             PORTLAND 692 929 3592
    ## 30   2460              ALASKA       SALT LAKE CITY 805-877-3887
    ## 31   1594  SINGAPORE AIRLINES            SINGAPORE 819 732 4132
    ## 32   1536           AIR CHINA              BEIJING 367 221 9710
    ## 33   1582               DELTA       SALT LAKE CITY 361 154 1789
    ## 34    684      CATHAY PACIFIC            HONG KONG 680 488 1182
    ## 35    101              UNITED WASHINGTON DC-DULLES 928 638 1186
    ## 36   2455              ALASKA       SALT LAKE CITY 588-693-9875
    ## 37    294              UNITED            SAN DIEGO 681-308-7915
    ## 38   1005          AIR CANADA            VANCOUVER 783 647 8490
    ## 39    464              ALASKA             PORTLAND 897 847 0632
    ## 40   1583         UNITED INTL            HONG KONG 150 952 4453
    ## 41   1579         UNITED INTL            HONG KONG 322-884-3020
    ## 42   1269      CATHAY PACIFIC            HONG KONG 176-313-5403
    ## 43   2654          AEROMEXICO          MEXICO CITY 487 109 4196
    ## 44    863              ALASKA              SEATTLE 544 382 8289
    ## 45   1516            INTERJET          GUADALAJARA 781 543 7456
    ## 46    659              UNITED               NEWARK 911 829 6476
    ## 47   2929           SOUTHWEST            SAN DIEGO 517 986 3426
    ## 48    280              UNITED            BALTIMORE 838 220 5397
    ## 49   1500               DELTA          LOS ANGELES 179 163 0902
    ## 50   2450              UNITED       CHICAGO-O'HARE 539 137 8983
    ## 51   3045            AMERICAN     DALLAS-FT. WORTH 733-154-0094
    ## 52    614              ALASKA               NEWARK 639 881 3693
    ## 53    204            AMERICAN          LOS ANGELES 291-830-3017
    ## 54   2209           SOUTHWEST            LAS VEGAS 750 520 0167
    ## 55   1471             AVIANCA         SAN SALVADOR 676 485 8963
    ## 56   2296                 WOW            REYKJAVIK 135 566 5090
    ## 57   2412     AIR NEW ZEALAND             AUCKLAND 337 260 4996
    ## 58   2407     AIR NEW ZEALAND             AUCKLAND 371 185 2377
    ## 59   1514            INTERJET          GUADALAJARA 280 461 1386
    ## 60    881              ALASKA       RALEIGH-DURHAM 496-429-1314
    ## 61   2409     AIR NEW ZEALAND             AUCKLAND 459 671 4698
    ## 62   1527              ALASKA          LOS ANGELES 486-268-3312
    ## 63   1195           SOUTHWEST            LAS VEGAS 497-518-4050
    ## 64   2957            AMERICAN         NEW YORK-JFK 859 495 4050
    ## 65    102              UNITED WASHINGTON DC-DULLES 724-134-3870
    ## 66    342           LUFTHANSA            FRANKFURT 125-578-4253
    ## 67   1808         UNITED INTL      LONDON HEATHROW 614 800 2861
    ## 68    322              UNITED              BURBANK 487-232-4449
    ## 69    546              ALASKA              SEATTLE 253-374-7102
    ## 70   1439            FRONTIER               DENVER 802 102 8345
    ## 71    243              UNITED            NASHVILLE 417 393 0050
    ## 72    889          AIR CANADA              TORONTO 787-624-8443
    ## 73    104              UNITED WASHINGTON DC-DULLES 341 824 5322
    ## 74   2338              UNITED               AUSTIN 415 551 1608
    ## 75    326              UNITED              BURBANK 473-238-3324
    ## 76    869              ALASKA              SEATTLE 876-834-0624
    ## 77    279              UNITED            BALTIMORE 919 611 6170
    ## 78   1363         UNITED INTL      PARIS-DE GAULLE 261 434 7760
    ## 79   1365         UNITED INTL      PARIS-DE GAULLE 617 310 2684
    ## 80   1504               DELTA          LOS ANGELES 182-535-3412
    ## 81    745          AIR CANADA              TORONTO 506 129 1694
    ## 82   2088            AMERICAN              PHOENIX 313 990 8823
    ## 83    670      AIR FRANCE/KLM      PARIS-DE GAULLE 656-941-5355
    ## 84   2252              UNITED          LOS ANGELES 955 324 5981
    ## 85   2677      AIR FRANCE/KLM      PARIS-DE GAULLE 175 808 2189
    ## 86    938          AIR CANADA            VANCOUVER 105-687-6500
    ## 87   1958      CATHAY PACIFIC            HONG KONG 757 524 2964
    ## 88   2834              UNITED       CHICAGO-O'HARE 201 374 2424
    ## 89   2229           SOUTHWEST            SANTA ANA 766 112 6143
    ## 90    327              UNITED              BURBANK 783-463-4865
    ## 91   2179              QANTAS               SYDNEY 853-803-9900
    ## 92   1507               DELTA          LOS ANGELES 992 114 6973
    ## 93    402              UNITED         HOUSTON-BUSH 316-212-7309
    ## 94    949              ALASKA         NEW YORK-JFK 301 672 1092
    ## 95      1           SOUTHWEST            SAN DIEGO 795 137 0201
    ## 96    893          AIR CANADA              TORONTO 381-883-5497
    ## 97   2180              QANTAS               SYDNEY 100-531-4642
    ## 98   1858              UNITED          LOS ANGELES 994 923 6634
    ## 99   2001      AIR FRANCE/KLM      PARIS-DE GAULLE 920 355 8404
    ## 100  2389         UNITED INTL              BEIJING 325 795 2455
    ## 101  2059            AMERICAN         PHILADELPHIA 593 829 6250
    ## 102   419              UNITED         INDIANAPOLIS 566-482-9004
    ## 103  1276      CHINA SOUTHERN                WUHAN 542 537 6770
    ## 104  1434            FRONTIER               DENVER 716 191 1741
    ## 105  2181              QANTAS               SYDNEY 491-727-7162
    ## 106  3043            AMERICAN     DALLAS-FT. WORTH 167-336-5660
    ## 107  2548             JETBLUE               BOSTON 358 831 0725
    ## 108  2703              UNITED       CHICAGO-O'HARE 432 979 7292
    ## 109  2101            AMERICAN              PHOENIX 728 662 3934
    ## 110  2138            FRONTIER               DENVER 380-918-8572
    ## 111  3047            AMERICAN          LOS ANGELES 151 434 6989
    ## 112  2845              UNITED              SEATTLE 755 544 2629
    ## 113  1957      CATHAY PACIFIC            HONG KONG 633 181 4494
    ## 114  1370         UNITED INTL      PARIS-DE GAULLE 346 706 5964
    ## 115  1427              UNITED          KANSAS CITY 688 690 2184
    ## 116  1975      CATHAY PACIFIC            HONG KONG 618 717 1697
    ## 117   469              ALASKA             PORTLAND 185-321-6877
    ## 118  1830    TURKISH AIRLINES             ISTANBUL 726 943 7486
    ## 119  2802           SOUTHWEST              PHOENIX 670-248-0186
    ## 120   291              UNITED            SAN DIEGO 364 834 3150
    ## 121  2340              UNITED               AUSTIN 176 508 2778
    ## 122  1274         UNITED INTL      LONDON HEATHROW 120 941 0833
    ## 123  2356             JETBLUE       FT. LAUDERDALE 670 902 3199
    ## 124  2835              UNITED       CHICAGO-O'HARE 297 484 3285
    ## 125  2239           SOUTHWEST              PHOENIX 743-103-7645
    ## 126  1329         UNITED INTL             SHANGHAI 432-281-3682
    ## 127  2683      AIR FRANCE/KLM      PARIS-DE GAULLE 648 685 6188
    ## 128  2844              UNITED              SEATTLE 548 191 4898
    ## 129  2240           SOUTHWEST              PHOENIX 288 110 9483
    ## 130  1981      AIR FRANCE/KLM            AMSTERDAM 946-558-5801
    ## 131  1007          AIR CANADA            VANCOUVER 388 744 9637
    ## 132  1373         UNITED INTL             SHANGHAI 506 463 9129
    ## 133   849              ALASKA             PORTLAND 306 394 8640
    ## 134  1896              UNITED               BOSTON 170-641-3537
    ## 135  1489        HAWAIIAN AIR             HONOLULU 860 723 5066
    ## 136  2183              QANTAS               SYDNEY 814-895-6610
    ## 137  2903    TURKISH AIRLINES             ISTANBUL 598 735 8557
    ## 138   845            AMERICAN     DALLAS-FT. WORTH 593 895 6761
    ## 139   516      AIR FRANCE/KLM      PARIS-DE GAULLE 508 484 9738
    ## 140  1914      AIR FRANCE/KLM      PARIS-DE GAULLE 719 489 4724
    ## 141  2002      AIR FRANCE/KLM      PARIS-DE GAULLE 503-671-4901
    ## 142  3102                COPA          PANAMA CITY 275 649 8183
    ## 143  2843              UNITED              SEATTLE 968 130 7012
    ## 144  1372         UNITED INTL      PARIS-DE GAULLE 290 367 6676
    ## 145   252          AIR CANADA              TORONTO 499 766 9941
    ## 146  2379         UNITED INTL              BEIJING 538-393-2243
    ## 147  2550              UNITED       CHICAGO-O'HARE 845 544 4748
    ## 148  1523             JETBLUE         NEW YORK-JFK 784-458-8425
    ## 149  1546           AIR CHINA              BEIJING 708 500 2758
    ## 150   837            AMERICAN     DALLAS-FT. WORTH 982 555 9504
    ## 151   103              UNITED WASHINGTON DC-DULLES 477-307-3338
    ## 152  2254              UNITED          LOS ANGELES 744-301-1148
    ## 153  1764            AMERICAN       CHICAGO-O'HARE 389 484 8888
    ## 154  2158       CHINA EASTERN              QINGDAO 739 303 6128
    ## 155  2557              UNITED       CHICAGO-O'HARE 175 905 9962
    ## 156   842            AMERICAN     DALLAS-FT. WORTH 878-636-2294
    ## 157   401              UNITED         HOUSTON-BUSH 994 421 8642
    ## 158  2569            INTERJET          GUADALAJARA 304-225-5895
    ## 159  2803           SOUTHWEST              PHOENIX 931-522-5498
    ## 160   293              UNITED            SAN DIEGO 838 898 2275
    ## 161  1509               DELTA          LOS ANGELES 718 187 7125
    ## 162  1192           SOUTHWEST            LAS VEGAS 943 561 8955
    ## 163   191            AMERICAN          LOS ANGELES 341 261 6456
    ## 164  1447            FRONTIER               DENVER 826 266 0205
    ## 165   886              ALASKA       RALEIGH-DURHAM 589-975-0198
    ## 166   330              ALASKA            SAN DIEGO 657 680 8781
    ## 167  1462            FRONTIER               DENVER 501-668-7869
    ## 168  2746               DELTA              ATLANTA 224 365 8299
    ## 169  3206              UNITED               EUGENE 747 588 1968
    ## 170     3           SOUTHWEST          LOS ANGELES 748 446 1257
    ## 171   271              UNITED          LOS ANGELES 525-552-4162
    ## 172  1452              UNITED            LAS VEGAS 635-714-8302
    ## 173  3041            AMERICAN     DALLAS-FT. WORTH 114 668 2834
    ## 174  2543              UNITED       CHICAGO-O'HARE 404 788 2855
    ## 175   576             JETBLUE               BOSTON 673 292 2444
    ## 176  1584               DELTA       SALT LAKE CITY 136 788 1426
    ## 177  1133           SOUTHWEST          LOS ANGELES 564 677 3934
    ## 178  2191          AER LINGUS               DUBLIN 213 981 7762
    ## 179  2112            AMERICAN              PHOENIX 453-556-0852
    ## 180   866              ALASKA              SEATTLE 423 593 4483
    ## 181  3122               DELTA       SALT LAKE CITY 820 609 7454
    ## 182  2456              UNITED               DENVER 131-641-1331
    ## 183   847            AMERICAN     DALLAS-FT. WORTH 824 540 9579
    ## 184  3208            AMERICAN          LOS ANGELES 739 737 9041
    ## 185  2846              UNITED              SEATTLE 609-332-7370
    ## 186   963           LUFTHANSA               MUNICH 426-182-1365
    ## 187   615              ALASKA WASHINGTON DC-DULLES 898-210-6218
    ## 188  1173           SOUTHWEST            LAS VEGAS 938 394 0411
    ## 189  2652           SOUTHWEST               DENVER 904 844 1759
    ## 190  2195           SOUTHWEST            LAS VEGAS 658-861-4306
    ## 191   323              UNITED              BURBANK 380-105-1757
    ## 192   840            AMERICAN     DALLAS-FT. WORTH 249 452 4370
    ## 193  2451              ALASKA       SALT LAKE CITY 443 384 8253
    ## 194  1429              UNITED          KANSAS CITY 711 289 2247
    ## 195   241              UNITED            NASHVILLE 343-973-0193
    ## 196  2458              ALASKA       SALT LAKE CITY 367 650 3720
    ## 197  2404      AIR FRANCE/KLM      PARIS-DE GAULLE 148 630 8560
    ## 198  1044          AIR CANADA              CALGARY 413-727-2672
    ## 199  2192           SOUTHWEST            LAS VEGAS 148 501 5084
    ## 200  3091              ALASKA               NEWARK 564 780 8272
    ## 201  1266      CATHAY PACIFIC            HONG KONG 524 190 0899
    ## 202  3207            AMERICAN          LOS ANGELES 463 792 2782
    ## 203  2134            FRONTIER               DENVER 522-286-5318
    ## 204  1569               DELTA              DETROIT 310 719 4550
    ## 205  2676           SOUTHWEST               DENVER 314-360-4288
    ## 206  1525              ALASKA          LOS ANGELES 822 271 5719
    ## 207  1529              ALASKA          LOS ANGELES 465-550-6610
    ## 208  1443            FRONTIER               DENVER 388 100 1482
    ## 209  1368         UNITED INTL      PARIS-DE GAULLE 982 842 4913
    ## 210  1539           AIR CHINA              BEIJING 671 913 4563
    ## 211  3163            AMERICAN     DALLAS-FT. WORTH 144 468 5864
    ## 212  2421              UNITED             HONOLULU 194 344 4039
    ## 213  2411     AIR NEW ZEALAND             AUCKLAND 331 747 5714
    ## 214  2381         UNITED INTL              BEIJING 221-190-1449
    ## 215  2590            INTERJET          GUADALAJARA 676-614-9095
    ## 216   277              UNITED            BALTIMORE 956 257 9319
    ## 217  1239         UNITED INTL         TOKYO-NARITA 362 145 8268
    ## 218   375              ALASKA          LOS ANGELES 570 727 3998
    ## 219  2290                 WOW            REYKJAVIK 225 964 9193
    ## 220  2288              UNITED          SAN ANTONIO 227 419 9482
    ## 221   618              ALASKA WASHINGTON DC-DULLES 949 360 7605
    ## 222  2145          AIR CANADA            VANCOUVER 794 939 9735
    ## 223  1795              ALASKA       RALEIGH-DURHAM 413 754 3034
    ## 224   387              ALASKA         INDIANAPOLIS 806 730 0459
    ## 225  2789               DELTA       SALT LAKE CITY 791 195 8909
    ## 226  1095           SOUTHWEST            SAN DIEGO 691-318-3535
    ## 227  2111         UNITED INTL          MEXICO CITY 852-386-6029
    ## 228  2777               DELTA              ATLANTA 380 682 7795
    ## 229  1176           SOUTHWEST            LAS VEGAS 247 586 4579
    ## 230  1470             AVIANCA         SAN SALVADOR 235 257 1041
    ## 231  2573            INTERJET          GUADALAJARA 106 756 2785
    ## 232     6           SOUTHWEST          LOS ANGELES 729-102-7511
    ## 233  1825         UNITED INTL      LONDON HEATHROW 795 583 0958
    ## 234  2926           SOUTHWEST            SAN DIEGO 700-431-3918
    ## 235   384              ALASKA         INDIANAPOLIS 929 632 1068
    ## 236  1942              UNITED               BOSTON 763 906 2495
    ## 237  2943           SOUTHWEST          LOS ANGELES 469 976 6796
    ## 238  2149          AIR CANADA            VANCOUVER 887-657-4143
    ## 239   954              ALASKA         NEW YORK-JFK 574-438-5329
    ## 240  1103           SOUTHWEST            SAN DIEGO 319 127 9518
    ## 241   400              UNITED              PHOENIX 429 960 9710
    ## 242  2454              UNITED       CHICAGO-O'HARE 419 646 0299
    ## 243  2236           SOUTHWEST              PHOENIX 192 343 8515
    ## 244   296              UNITED            SAN DIEGO 521 336 8581
    ## 245  1564               DELTA              DETROIT 776 367 6109
    ## 246  3070              UNITED              SPOKANE 470 367 1392
    ## 247  2836              UNITED       CHICAGO-O'HARE 998-931-4783
    ## 248   270              UNITED          LOS ANGELES 362-178-6307
    ## 249  1431            FRONTIER               DENVER 212 286 7936
    ## 250  3264              UNITED            SAN DIEGO 936 193 9690
    ## 251   100              UNITED WASHINGTON DC-DULLES 797-870-7818
    ## 252  2891              UNITED               AUSTIN 693 907 5353
    ## 253   303           SOUTHWEST          LOS ANGELES 332 973 4943
    ## 254  1379         UNITED INTL             SHANGHAI 929 622 9077
    ## 255  1810         UNITED INTL      LONDON HEATHROW 649-379-5361
    ## 256  2640             JETBLUE         NEW YORK-JFK 395-892-5646
    ## 257  3262              UNITED            SAN DIEGO 221-628-9561
    ## 258  2700              UNITED              ORLANDO 549-649-1864
    ## 259  2035     BRITISH AIRWAYS      LONDON HEATHROW 342 941 0439
    ## 260  3066                COPA          PANAMA CITY 701 390 9814
    ## 261  2014     BRITISH AIRWAYS      LONDON HEATHROW 919-342-0230
    ## 262  2036     BRITISH AIRWAYS      LONDON HEATHROW 364-759-2705
    ## 263  2244                 WOW            REYKJAVIK 949 543 7906
    ## 264  1695              ALASKA            SANTA ANA 942 732 6403
    ## 265  2920               DELTA              SEATTLE 308-607-9855
    ## 266   958          AIR CANADA            VANCOUVER 764 645 5740
    ## 267  2284                 WOW            REYKJAVIK 472-337-8838
    ## 268  1181           SOUTHWEST            LAS VEGAS 791 847 7278
    ## 269   830              ALASKA            NASHVILLE 128 805 3828
    ## 270  2732      AIR FRANCE/KLM      PARIS-DE GAULLE 365-832-0674
    ## 271  1428              UNITED            LAS VEGAS 123-282-3494
    ## 272  3039              UNITED             PORTLAND 285 424 4318
    ## 273  2251                 WOW            REYKJAVIK 452 352 1387
    ## 274   725      AIR FRANCE/KLM      PARIS-DE GAULLE 129-377-8159
    ## 275  2425     AIR NEW ZEALAND             AUCKLAND 222 143 3131
    ## 276  2909               DELTA              SEATTLE 162-451-0594
    ## 277  2171            FRONTIER               DENVER 436-422-6171
    ## 278   506              ALASKA             PORTLAND 605 284 4260
    ## 279  2592          AEROMEXICO          MEXICO CITY 929-102-5905
    ## 280  3258              UNITED            SAN DIEGO 847 507 8268
    ## 281  2046     BRITISH AIRWAYS      LONDON HEATHROW 452-811-8088
    ## 282  2736      AIR FRANCE/KLM      PARIS-DE GAULLE 799 143 1677
    ## 283   820              ALASKA             PORTLAND 196 756 4555
    ## 284   649         UNITED INTL               MUNICH 119-444-0817
    ## 285  2319              QANTAS               SYDNEY 885 454 0883
    ## 286  1864              UNITED          LOS ANGELES 945-998-0444
    ## 287  1028          AIR CANADA            VANCOUVER 163 241 9321
    ## 288  1441              UNITED            LAS VEGAS 594-176-5811
    ## 289  2030     BRITISH AIRWAYS      LONDON HEATHROW 332 963 4103
    ## 290  2214           SOUTHWEST              PHOENIX 389 318 3975
    ## 291  2281              QANTAS               SYDNEY 894-593-7953
    ## 292  1658            EMIRATES                DUBAI 561-266-7842
    ## 293   370              ALASKA    SAN JOSE DEL CABO 354-958-8052
    ## 294  2117             JETBLUE           LONG BEACH 151 921 2775
    ## 295   498              ALASKA             PORTLAND 901 140 3759
    ## 296  2122      CATHAY PACIFIC            HONG KONG 333-520-4811
    ## 297  2215          AER LINGUS               DUBLIN 850 914 9348
    ## 298   901          AIR CANADA              TORONTO 246 272 9019
    ## 299  2900                 WOW            REYKJAVIK 504 419 9191
    ## 300   987           LUFTHANSA               MUNICH 231 863 7554
    ## 301  3019              UNITED             PORTLAND 939 253 9048
    ## 302   478              UNITED               DENVER 879-154-4494
    ## 303  2366              UNITED               AUSTIN 577 786 2546
    ## 304  1353         UNITED INTL             SHANGHAI 841-717-4447
    ## 305   192            AMERICAN          LOS ANGELES 397-353-6309
    ## 306  1967      CATHAY PACIFIC            HONG KONG 558 191 4548
    ## 307  1984      AIR FRANCE/KLM            AMSTERDAM 905 768 2297
    ## 308  2740      AIR FRANCE/KLM      PARIS-DE GAULLE 348 522 2051
    ## 309  2187              QANTAS               SYDNEY 307-323-6861
    ## 310   872            AMERICAN     DALLAS-FT. WORTH 663 886 2487
    ## 311   617              ALASKA               NEWARK 274 944 6097
    ## 312  1898           LUFTHANSA               MUNICH 328 307 0875
    ## 313  1706      AIR FRANCE/KLM      PARIS-DE GAULLE 673 524 3504
    ## 314  2198      CATHAY PACIFIC            HONG KONG 934 721 0615
    ## 315   667              UNITED               NEWARK 102-957-6486
    ## 316  2612             JETBLUE               BOSTON 794 925 8846
    ## 317  2698      AIR FRANCE/KLM      PARIS-DE GAULLE 637 281 4111
    ## 318  2156            FRONTIER               DENVER 335 802 2651
    ## 319   286              UNITED            NASHVILLE 560 699 9908
    ## 320   601             JETBLUE               BOSTON 451 163 0102
    ## 321  1696              ALASKA            SANTA ANA 613 800 0835
    ## 322  2905               DELTA              SEATTLE 370 453 5800
    ## 323  2894               DELTA       SALT LAKE CITY 192 507 5411
    ## 324   346           LUFTHANSA            FRANKFURT 182-227-4838
    ## 325  1108               DELTA MINNEAPOLIS-ST. PAUL 647 126 2332
    ## 326  2811               DELTA          LOS ANGELES 606 125 6957
    ## 327   681      AIR FRANCE/KLM      PARIS-DE GAULLE 728-404-5558
    ## 328  2615          AEROMEXICO          MEXICO CITY 506 812 6052
    ## 329  2434     AIR NEW ZEALAND             AUCKLAND 427-665-3475
    ## 330  1982      AIR FRANCE/KLM            AMSTERDAM 808 739 7162
    ## 331  1478              ALASKA         NEW YORK-JFK 375-978-3305
    ## 332  2490              UNITED       CHICAGO-O'HARE 181-708-2089
    ## 333  3165               DELTA          LOS ANGELES 242-540-4234
    ## 334  1880              UNITED               BOSTON 105 116 9695
    ## 335  2764               DELTA              ATLANTA 945 144 7892
    ## 336  2914               DELTA              SEATTLE 531 895 6695
    ## 337  3269              UNITED            LAS VEGAS 894 810 2674
    ## 338   502              ALASKA             PORTLAND 943 812 6349
    ## 339   683      AIR FRANCE/KLM      PARIS-DE GAULLE 476 168 4235
    ## 340   635              ALASKA WASHINGTON DC-DULLES 931 385 6757
    ## 341  2869              UNITED WASHINGTON DC-DULLES 100 378 8095
    ## 342  1866              ALASKA              KAHULUI 397-362-5469
    ## 343  1686      CATHAY PACIFIC            HONG KONG 938 982 5585
    ## 344  2500              ALASKA       SALT LAKE CITY 769-472-2992
    ## 345  2305              QANTAS               SYDNEY 190 204 1154
    ## 346  1063          AIR CANADA              CALGARY 321 616 1013
    ## 347   924          AIR CANADA              TORONTO 178-232-0815
    ## 348  1206           SOUTHWEST            LAS VEGAS 828 549 6666
    ## 349   360              ALASKA               NEWARK 280 544 4554
    ## 350   479              ALASKA             PORTLAND 183 208 5054
    ## 351  1414              UNITED          KANSAS CITY 203 448 1522
    ## 352   295           SOUTHWEST          LOS ANGELES 900-871-9056
    ## 353  2039     BRITISH AIRWAYS      LONDON HEATHROW 406-167-1379
    ## 354  2041     BRITISH AIRWAYS      LONDON HEATHROW 641-635-8466
    ## 355  1718      AIR FRANCE/KLM      PARIS-DE GAULLE 807-671-6158
    ## 356  2253                 WOW            REYKJAVIK 768 529 8051
    ## 357   754              ALASKA         NEW YORK-JFK 220 660 0306
    ## 358  3235            AMERICAN              PHOENIX 928-179-7556
    ## 359  3204      CATHAY PACIFIC            HONG KONG 153 756 0278
    ## 360   551            EMIRATES                DUBAI 273 829 9197
    ## 361  1770            AMERICAN       CHICAGO-O'HARE 269 463 0911
    ## 362  2874             EVA AIR               TAIPEI 696 984 8826
    ## 363  2020     BRITISH AIRWAYS      LONDON HEATHROW 905-903-5258
    ## 364  2021     BRITISH AIRWAYS      LONDON HEATHROW 267 332 4709
    ## 365  2023     BRITISH AIRWAYS      LONDON HEATHROW 534 216 6666
    ## 366  3106              ALASKA               NEWARK 971 175 2968
    ## 367  2377              UNITED               AUSTIN 274-863-3205
    ## 368   773         UNITED INTL    SAN JOSE DEL CABO 928 445 5474
    ## 369  1545               DELTA         NEW YORK-JFK 858 990 5153
    ## 370  2892                 WOW            REYKJAVIK 731-813-2043
    ## 371  2721              UNITED              KAHULUI 563-732-6802
    ## 372   819              ALASKA             PORTLAND 145 725 4021
    ## 373  3272              UNITED            LAS VEGAS 931 311 5801
    ## 374   253          AIR CANADA              TORONTO 172 990 3485
    ## 375   266          AIR CANADA              TORONTO 872 325 4341
    ## 376  1196           SOUTHWEST            LAS VEGAS 152 790 8238
    ## 377  2363              UNITED               AUSTIN 330 561 9257
    ## 378  3181               DELTA          LOS ANGELES 437 420 7546
    ## 379  2918               DELTA              SEATTLE 495 632 4027
    ## 380   904          AIR CANADA              TORONTO 311-305-4367
    ## 381  2937           SOUTHWEST            SAN DIEGO 817-400-0481
    ## 382  3143               DELTA       SALT LAKE CITY 430 723 1079
    ## 383  1758           LUFTHANSA               MUNICH 226 490 8696
    ## 384  1740          AIR CANADA              TORONTO 123 570 8640
    ## 385   265          AIR CANADA              TORONTO 665 803 2453
    ## 386  3158               DELTA       SALT LAKE CITY 639 132 6386
    ## 387  1562               DELTA              ATLANTA 437 886 0753
    ## 388  2375              UNITED               AUSTIN 626 756 5089
    ## 389  3032              UNITED             PORTLAND 714 950 3364
    ## 390  1183           SOUTHWEST            LAS VEGAS 653-786-5985
    ## 391  2354              UNITED               AUSTIN 518 286 5956
    ## 392  2751      AIR FRANCE/KLM      PARIS-DE GAULLE 194 960 2145
    ## 393  3232            AMERICAN              PHOENIX 362-136-1153
    ## 394  1287             JETBLUE         NEW YORK-JFK 376-456-0697
    ## 395   852              ALASKA             PORTLAND 657 832 1189
    ## 396  3180            AMERICAN     DALLAS-FT. WORTH 962-918-6117
    ## 397  2501              ALASKA       SALT LAKE CITY 692 929 3592
    ## 398  3277           SOUTHWEST               DENVER 805-877-3887
    ## 399   810              ALASKA            BALTIMORE 819 732 4132
    ## 400   481              ALASKA             PORTLAND 367 221 9710
    ## 401   350              ALASKA               NEWARK 361 154 1789
    ## 402  1789              UNITED       RALEIGH-DURHAM 680 488 1182
    ## 403  1256         UNITED INTL         TOKYO-NARITA 928 638 1186
    ## 404  2763      CATHAY PACIFIC            HONG KONG 588-693-9875
    ## 405  1442              UNITED             PORTLAND 681-308-7915
    ## 406  1229      ANA ALL NIPPON         TOKYO-NARITA 783 647 8490
    ## 407   969           LUFTHANSA               MUNICH 897 847 0632
    ## 408  1025          AIR CANADA            VANCOUVER 150 952 4453
    ## 409  1926      AIR FRANCE/KLM      PARIS-DE GAULLE 322-884-3020
    ## 410  2093            AMERICAN         PHILADELPHIA 176-313-5403
    ## 411  2812               DELTA          LOS ANGELES 487 109 4196
    ## 412   343              ALASKA            SAN DIEGO 544 382 8289
    ## 413   196            AMERICAN          LOS ANGELES 781 543 7456
    ## 414   337           SOUTHWEST              PHOENIX 911 829 6476
    ## 415  1799              ALASKA       RALEIGH-DURHAM 517 986 3426
    ## 416  1951      AIR FRANCE/KLM      PARIS-DE GAULLE 838 220 5397
    ## 417  1813         UNITED INTL      LONDON HEATHROW 179 163 0902
    ## 418  1929      AIR FRANCE/KLM      PARIS-DE GAULLE 539 137 8983
    ## 419  1736          AIR CANADA              TORONTO 733-154-0094
    ## 420  2282                 WOW            REYKJAVIK 639 881 3693
    ## 421  2788                 WOW            REYKJAVIK 291-830-3017
    ## 422  1653     VIRGIN ATLANTIC      LONDON HEATHROW 750 520 0167
    ## 423  2186              QANTAS               SYDNEY 676 485 8963
    ## 424  1719      AIR FRANCE/KLM      PARIS-DE GAULLE 135 566 5090
    ## 425  1440              UNITED             PORTLAND 337 260 4996
    ## 426  1343         UNITED INTL               KANSAI 371 185 2377
    ## 427   874            AMERICAN     DALLAS-FT. WORTH 280 461 1386
    ## 428  2530             JETBLUE               BOSTON 496-429-1314
    ## 429  2699           SOUTHWEST               DENVER 459 671 4698
    ## 430   545            EMIRATES                DUBAI 486-268-3312
    ## 431  1785              UNITED       RALEIGH-DURHAM 497-518-4050
    ## 432  2323              QANTAS               SYDNEY 859 495 4050
    ## 433   694      AIR FRANCE/KLM      PARIS-DE GAULLE 724-134-3870
    ## 434   822              ALASKA            NASHVILLE 125-578-4253
    ## 435  1234           SOUTHWEST            LAS VEGAS 614 800 2861
    ## 436   982           LUFTHANSA               MUNICH 487-232-4449
    ## 437  2297              QANTAS               SYDNEY 253-374-7102
    ## 438  1459              UNITED             PORTLAND 802 102 8345
    ## 439  1318         UNITED INTL      LONDON HEATHROW 417 393 0050
    ## 440  3178            AMERICAN     DALLAS-FT. WORTH 787-624-8443
    ## 441   605              ALASKA WASHINGTON DC-DULLES 341 824 5322
    ## 442  1590         UNITED INTL            HONG KONG 415 551 1608
    ## 443  1635          KOREAN AIR                SEOUL 473-238-3324
    ## 444  2068            AMERICAN         PHILADELPHIA 876-834-0624
    ## 445  2387         UNITED INTL              BEIJING 919 611 6170
    ## 446  1032          AIR CANADA            VANCOUVER 261 434 7760
    ## 447  2255              UNITED          LOS ANGELES 617 310 2684
    ## 448   555            EMIRATES                DUBAI 182-535-3412
    ## 449   718      CATHAY PACIFIC            HONG KONG 506 129 1694
    ## 450  2177            FRONTIER               DENVER 313 990 8823
    ## 451  3076 PHILIPPINE AIRLINES               MANILA 656-941-5355
    ## 452  1980      CATHAY PACIFIC            HONG KONG 955 324 5981
    ## 453   432              UNITED             HONOLULU 175 808 2189
    ## 454  3256              UNITED            SAN DIEGO 105-687-6500
    ## 455  1571               DELTA              DETROIT 757 524 2964
    ## 456  3268              UNITED            LAS VEGAS 201 374 2424
    ## 457  2327               DELTA MINNEAPOLIS-ST. PAUL 766 112 6143
    ## 458  1164           SOUTHWEST          LOS ANGELES 783-463-4865
    ## 459  2950           SOUTHWEST            SAN DIEGO 853-803-9900
    ## 460   700      AIR FRANCE/KLM      PARIS-DE GAULLE 992 114 6973
    ## 461  1369         UNITED INTL               KANSAI 316-212-7309
    ## 462  2264              UNITED          LOS ANGELES 301 672 1092
    ## 463  2205      CATHAY PACIFIC            HONG KONG 795 137 0201
    ## 464  1383         UNITED INTL             SHANGHAI 381-883-5497
    ## 465  2762               DELTA              ATLANTA 100-531-4642
    ## 466  1034          AIR CANADA            VANCOUVER 994 923 6634
    ## 467  1156           SOUTHWEST            SAN DIEGO 920 355 8404
    ## 468  2522              ALASKA       SALT LAKE CITY 325 795 2455
    ## 469  1814              ALASKA       RALEIGH-DURHAM 593 829 6250
    ## 470  3263              UNITED            SAN DIEGO 566-482-9004
    ## 471  1288         UNITED INTL      LONDON HEATHROW 542 537 6770
    ## 472  3214              UNITED        SANTA BARBARA 716 191 1741
    ## 473  1614     VIRGIN ATLANTIC      LONDON HEATHROW 491-727-7162
    ## 474  2471              ALASKA       SALT LAKE CITY 167-336-5660
    ## 475  2308              QANTAS               SYDNEY 358 831 0725
    ## 476  2544             JETBLUE           LONG BEACH 432 979 7292
    ## 477  3086                COPA          PANAMA CITY 728 662 3934
    ## 478  2987            AMERICAN            CHARLOTTE 380-918-8572
    ## 479  1263         UNITED INTL         TOKYO-NARITA 151 434 6989
    ## 480  2928    TURKISH AIRLINES             ISTANBUL 755 544 2629
    ## 481  3129              ALASKA               NEWARK 633 181 4494
    ## 482  1773            AMERICAN       CHICAGO-O'HARE 346 706 5964
    ## 483   682              UNITED               NEWARK 688 690 2184
    ## 484   491              UNITED               DENVER 618 717 1697
    ## 485   857              ALASKA             PORTLAND 185-321-6877
    ## 486   520              ALASKA      PUERTO VALLARTA 726 943 7486
    ## 487  1916      AIR FRANCE/KLM      PARIS-DE GAULLE 670-248-0186
    ## 488  1612     VIRGIN ATLANTIC      LONDON HEATHROW 364 834 3150
    ## 489  1220           SOUTHWEST            LAS VEGAS 176 508 2778
    ## 490  2472              UNITED       CHICAGO-O'HARE 120 941 0833
    ## 491   284              UNITED            BALTIMORE 670 902 3199
    ## 492  1897           LUFTHANSA               MUNICH 297 484 3285
    ## 493  2213      CATHAY PACIFIC            HONG KONG 743-103-7645
    ## 494  1551               DELTA         NEW YORK-JFK 432-281-3682
    ## 495  1139           SOUTHWEST            SAN DIEGO 648 685 6188
    ## 496  2936           SOUTHWEST            SAN DIEGO 548 191 4898
    ## 497   275              UNITED          LOS ANGELES 288 110 9483
    ## 498  1134           SOUTHWEST            SAN DIEGO 946-558-5801
    ## 499  3212            AMERICAN          LOS ANGELES 388 744 9637
    ## 500  1657            EMIRATES                DUBAI 506 463 9129
    ## 501  1690      CATHAY PACIFIC            HONG KONG 306 394 8640
    ## 502  1756           LUFTHANSA               MUNICH 170-641-3537
    ## 503  2133          AIR CANADA            VANCOUVER 860 723 5066
    ## 504   429              UNITED             HONOLULU 814-895-6610
    ## 505   960          AIR CANADA            VANCOUVER 598 735 8557
    ## 506  2797                 WOW            REYKJAVIK 593 895 6761
    ## 507   865            AMERICAN     DALLAS-FT. WORTH 508 484 9738
    ## 508   362              ALASKA               NEWARK 719 489 4724
    ## 509   577              ALASKA              SEATTLE 503-671-4901
    ## 510  2144          AIR CANADA            VANCOUVER 275 649 8183
    ## 511  3044              UNITED             PORTLAND 968 130 7012
    ## 512  2358             JETBLUE       FT. LAUDERDALE 290 367 6676
    ## 513  1617     VIRGIN ATLANTIC      LONDON HEATHROW 499 766 9941
    ## 514   848            AMERICAN     DALLAS-FT. WORTH 538-393-2243
    ## 515  2196          AER LINGUS               DUBLIN 845 544 4748
    ## 516  2203      CATHAY PACIFIC            HONG KONG 784-458-8425
    ## 517  1941      AIR FRANCE/KLM      PARIS-DE GAULLE 708 500 2758
    ## 518   743              ALASKA         NEW YORK-JFK 982 555 9504
    ## 519  1646     VIRGIN ATLANTIC      LONDON HEATHROW 477-307-3338
    ## 520  1648     VIRGIN ATLANTIC      LONDON HEATHROW 744-301-1148
    ## 521   917          AIR CANADA              TORONTO 389 484 8888
    ## 522  1495            FRONTIER               DENVER 739 303 6128
    ## 523  1728          AIR CANADA              TORONTO 175 905 9962
    ## 524   722      AIR FRANCE/KLM      PARIS-DE GAULLE 878-636-2294
    ## 525   892              ALASKA       RALEIGH-DURHAM 994 421 8642
    ## 526  1865              ALASKA              KAHULUI 304-225-5895
    ## 527  2499              UNITED               DENVER 931-522-5498
    ## 528  3035              UNITED             PORTLAND 838 898 2275
    ## 529  1868              ALASKA              KAHULUI 718 187 7125
    ## 530  2347               DELTA MINNEAPOLIS-ST. PAUL 943 561 8955
    ## 531  1497            FRONTIER               DENVER 341 261 6456
    ## 532   709      CATHAY PACIFIC            HONG KONG 826 266 0205
    ## 533  2819               DELTA          LOS ANGELES 589-975-0198
    ## 534   557            EMIRATES                DUBAI 657 680 8781
    ## 535  1021          AIR CANADA            VANCOUVER 501-668-7869
    ## 536  2085             JETBLUE           LONG BEACH 224 365 8299
    ## 537  1246         UNITED INTL         TOKYO-NARITA 747 588 1968
    ## 538  1267         UNITED INTL         TOKYO-NARITA 748 446 1257
    ## 539  2882             EVA AIR               TAIPEI 525-552-4162
    ## 540  1961      CATHAY PACIFIC            HONG KONG 635-714-8302
    ## 541  2653             JETBLUE               BOSTON 114 668 2834
    ## 542  2217      CATHAY PACIFIC            HONG KONG 404 788 2855
    ## 543  3028              UNITED             PORTLAND 673 292 2444
    ## 544   761              ALASKA         NEW YORK-JFK 136 788 1426
    ## 545   338              ALASKA            SAN DIEGO 564 677 3934
    ## 546   685      AIR FRANCE/KLM      PARIS-DE GAULLE 213 981 7762
    ## 547  1909              UNITED               BOSTON 453-556-0852
    ## 548   894              ALASKA       RALEIGH-DURHAM 423 593 4483
    ## 549  2208      CATHAY PACIFIC            HONG KONG 820 609 7454
    ## 550  1779              UNITED       RALEIGH-DURHAM 131-641-1331
    ## 551   711      AIR FRANCE/KLM      PARIS-DE GAULLE 824 540 9579
    ## 552  1062          AIR CANADA              CALGARY 739 737 9041
    ## 553   817              ALASKA             PORTLAND 609-332-7370
    ## 554  1987      AIR FRANCE/KLM            AMSTERDAM 426-182-1365
    ## 555  3161              UNITED            SAN DIEGO 898-210-6218
    ## 556  2194      CATHAY PACIFIC            HONG KONG 938 394 0411
    ## 557  3169               DELTA          LOS ANGELES 904 844 1759
    ## 558  1633          KOREAN AIR                SEOUL 658-861-4306
    ## 559  2392         UNITED INTL              BEIJING 380-105-1757
    ## 560  3112              UNITED              SPOKANE 249 452 4370
    ## 561   642              ALASKA WASHINGTON DC-DULLES 443 384 8253
    ## 562  1342         UNITED INTL               KANSAI 711 289 2247
    ## 563  1341         UNITED INTL             SHANGHAI 343-973-0193
    ## 564  3092                COPA          PANAMA CITY 367 650 3720
    ## 565   691      AIR FRANCE/KLM      PARIS-DE GAULLE 148 630 8560
    ## 566   525              ALASKA      PUERTO VALLARTA 413-727-2672
    ## 567  1707      AIR FRANCE/KLM      PARIS-DE GAULLE 148 501 5084
    ## 568  1624          KOREAN AIR                SEOUL 564 780 8272
    ## 569  1943      AIR FRANCE/KLM      PARIS-DE GAULLE 524 190 0899
    ## 570  1120               DELTA MINNEAPOLIS-ST. PAUL 463 792 2782
    ## 571  3290           SOUTHWEST               DENVER 522-286-5318
    ## 572   927          AIR CANADA              TORONTO 310 719 4550
    ## 573  2572             JETBLUE           LONG BEACH 314-360-4288
    ## 574  1068          AIR CANADA              CALGARY 822 271 5719
    ## 575  2702              UNITED              ORLANDO 465-550-6610
    ## 576  3057 PHILIPPINE AIRLINES               MANILA 388 100 1482
    ## 577  2352               DELTA MINNEAPOLIS-ST. PAUL 982 842 4913
    ## 578  1570         UNITED INTL            HONG KONG 671 913 4563
    ## 579  3118              ALASKA               NEWARK 144 468 5864
    ## 580  2766               DELTA              ATLANTA 194 344 4039
    ## 581   706      CATHAY PACIFIC            HONG KONG 331 747 5714
    ## 582  1911      AIR FRANCE/KLM      PARIS-DE GAULLE 221-190-1449
    ## 583  2567             JETBLUE           LONG BEACH 676-614-9095
    ## 584  2663             JETBLUE               BOSTON 956 257 9319
    ## 585  2080            AMERICAN         PHILADELPHIA 362 145 8268
    ## 586   229            AMERICAN          LOS ANGELES 570 727 3998
    ## 587  1787              UNITED       RALEIGH-DURHAM 225 964 9193
    ## 588  2942           SOUTHWEST            SAN DIEGO 227 419 9482
    ## 589  2749      AIR FRANCE/KLM      PARIS-DE GAULLE 949 360 7605
    ## 590   287              UNITED            BALTIMORE 794 939 9735
    ## 591   909          AIR CANADA              TORONTO 413 754 3034
    ## 592  1225           SOUTHWEST            LAS VEGAS 806 730 0459
    ## 593  1077          AIR CANADA              CALGARY 791 195 8909
    ## 594   427              UNITED             HONOLULU 691-318-3535
    ## 595   832              ALASKA              SEATTLE 852-386-6029
    ## 596   707      AIR FRANCE/KLM      PARIS-DE GAULLE 380 682 7795
    ## 597  2422     AIR NEW ZEALAND             AUCKLAND 247 586 4579
    ## 598  1639     VIRGIN ATLANTIC      LONDON HEATHROW 235 257 1041
    ## 599  1998              UNITED            BALTIMORE 106 756 2785
    ## 600  1906           LUFTHANSA               MUNICH 729-102-7511
    ## 601  1995              UNITED            BALTIMORE 795 583 0958
    ## 602  2961           SOUTHWEST          LOS ANGELES 700-431-3918
    ## 603   422              UNITED         INDIANAPOLIS 929 632 1068
    ## 604  2135          AIR CANADA            VANCOUVER 763 906 2495
    ## 605  2858              UNITED              SEATTLE 469 976 6796
    ## 606  1217           SOUTHWEST            LAS VEGAS 887-657-4143
    ## 607  1086          AIR CANADA              CALGARY 574-438-5329
    ## 608   996           LUFTHANSA               MUNICH 319 127 9518
    ## 609  1214           SOUTHWEST            LAS VEGAS 429 960 9710
    ## 610  1204           SOUTHWEST            LAS VEGAS 419 646 0299
    ## 611  1186           SOUTHWEST            LAS VEGAS 192 343 8515
    ## 612  2872              UNITED WASHINGTON DC-DULLES 521 336 8581
    ## 613   974           LUFTHANSA               MUNICH 776 367 6109
    ## 614  2675              UNITED       CHICAGO-O'HARE 470 367 1392
    ## 615   633              ALASKA WASHINGTON DC-DULLES 998-931-4783
    ## 616   613              ALASKA WASHINGTON DC-DULLES 362-178-6307
    ## 617  3291           SOUTHWEST               DENVER 212 286 7936
    ## 618   805            AMERICAN          LOS ANGELES 936 193 9690
    ## 619  2025     BRITISH AIRWAYS      LONDON HEATHROW 797-870-7818
    ## 620  1691      CATHAY PACIFIC            HONG KONG 693 907 5353
    ## 621  1860              UNITED          LOS ANGELES 332 973 4943
    ## 622  2809               DELTA          LOS ANGELES 929 622 9077
    ## 623   423              UNITED             HONOLULU 649-379-5361
    ## 624  2747      AIR FRANCE/KLM      PARIS-DE GAULLE 395-892-5646
    ## 625  2155      CATHAY PACIFIC            HONG KONG 221-628-9561
    ## 626   561              ALASKA              SEATTLE 549-649-1864
    ## 627  2938    TURKISH AIRLINES             ISTANBUL 342 941 0439
    ## 628  3151               DELTA       SALT LAKE CITY 701 390 9814
    ## 629   567              ALASKA              SEATTLE 919-342-0230
    ## 630   607             JETBLUE               BOSTON 364-759-2705
    ## 631  1533              ALASKA          LOS ANGELES 949 543 7906
    ## 632  2932           SOUTHWEST            SAN DIEGO 942 732 6403
    ## 633  2818               DELTA          LOS ANGELES 308-607-9855
    ## 634  2931           SOUTHWEST            SAN DIEGO 764 645 5740
    ## 635  1974              UNITED            BALTIMORE 472-337-8838
    ## 636  2013              UNITED               NEWARK 791 847 7278
    ## 637  3250              UNITED            SAN DIEGO 128 805 3828
    ## 638   202            AMERICAN          LOS ANGELES 365-832-0674
    ## 639  2176            FRONTIER               DENVER 123-282-3494
    ## 640  3273              UNITED            LAS VEGAS 285 424 4318
    ## 641  3275              UNITED            LAS VEGAS 452 352 1387
    ## 642  1115               DELTA MINNEAPOLIS-ST. PAUL 129-377-8159
    ## 643  2915               DELTA              SEATTLE 222 143 3131
    ## 644  1099               DELTA MINNEAPOLIS-ST. PAUL 162-451-0594
    ## 645   263          AIR CANADA              TORONTO 436-422-6171
    ## 646  3184            AMERICAN     DALLAS-FT. WORTH 605 284 4260
    ## 647  2053              UNITED               NEWARK 929-102-5905
    ## 648  2871              UNITED WASHINGTON DC-DULLES 847 507 8268
    ## 649  2672             JETBLUE               BOSTON 452-811-8088
    ## 650  2975            AMERICAN            CHARLOTTE 799 143 1677
    ## 651  2949           SOUTHWEST          LOS ANGELES 196 756 4555
    ## 652  1727          AIR CANADA              TORONTO 119-444-0817
    ## 653  1015          AIR CANADA            VANCOUVER 885 454 0883
    ## 654  2481              UNITED       CHICAGO-O'HARE 945-998-0444
    ## 655  1535               DELTA         NEW YORK-JFK 163 241 9321
    ## 656  2953           SOUTHWEST            SAN DIEGO 594-176-5811
    ## 657  2355              UNITED               AUSTIN 332 963 4103
    ## 658  1827         UNITED INTL      LONDON HEATHROW 389 318 3975
    ## 659  2634             JETBLUE         NEW YORK-JFK 894-593-7953
    ## 660  1895           LUFTHANSA               MUNICH 561-266-7842
    ## 661  2029     BRITISH AIRWAYS      LONDON HEATHROW 354-958-8052
    ## 662  2140          AIR CANADA            VANCOUVER 151 921 2775
    ## 663  2994            AMERICAN                MIAMI 901 140 3759
    ## 664  1418              UNITED            LAS VEGAS 333-520-4811
    ## 665  2426              UNITED             HONOLULU 850 914 9348
    ## 666  2613              UNITED       CHICAGO-O'HARE 246 272 9019
    ## 667  3055 PHILIPPINE AIRLINES               MANILA 504 419 9191
    ## 668  1559               DELTA         NEW YORK-JFK 231 863 7554
    ## 669  2769               DELTA              ATLANTA 939 253 9048
    ## 670   512              UNITED              KAHULUI 879-154-4494
    ## 671  1811         UNITED INTL      LONDON HEATHROW 577 786 2546
    ## 672  2403         UNITED INTL               SYDNEY 841-717-4447
    ## 673  2388         UNITED INTL               SYDNEY 397-353-6309
    ## 674  1741          AIR CANADA              TORONTO 558 191 4548
    ## 675  1293         UNITED INTL      LONDON HEATHROW 905 768 2297
    ## 676  3287           SOUTHWEST               DENVER 348 522 2051
    ## 677  1857              ALASKA              KAHULUI 307-323-6861
    ## 678  2951           SOUTHWEST            SAN DIEGO 663 886 2487
    ## 679  2469              UNITED               DENVER 274 944 6097
    ## 680  2713               DELTA          LOS ANGELES 328 307 0875
    ## 681  1070          AIR CANADA              CALGARY 673 524 3504
    ## 682  2657             JETBLUE               BOSTON 934 721 0615
    ## 683  3085              UNITED              SPOKANE 102-957-6486
    ## 684   210            AMERICAN          LOS ANGELES 637 281 4111
    ## 685  2893                 WOW            REYKJAVIK 335 802 2651
    ## 686  2725               DELTA          LOS ANGELES 560 699 9908
    ## 687  1798              ALASKA       RALEIGH-DURHAM 451 163 0102
    ## 688  2608          AEROMEXICO          MEXICO CITY 613 800 0835
    ## 689   352           LUFTHANSA            FRANKFURT 370 453 5800
    ## 690  1171           SOUTHWEST          LOS ANGELES 192 507 5411
    ## 691  1339         UNITED INTL               KANSAI 182-227-4838
    ## 692  1791              UNITED       RALEIGH-DURHAM 647 126 2332
    ## 693  1905           LUFTHANSA               MUNICH 606 125 6957
    ## 694  2238          AER LINGUS               DUBLIN 728-404-5558
    ## 695  3193      CATHAY PACIFIC            HONG KONG 506 812 6052
    ## 696   985           LUFTHANSA               MUNICH 427-665-3475
    ## 697  1259         UNITED INTL         TOKYO-NARITA 808 739 7162
    ## 698  1893           LUFTHANSA               MUNICH 375-978-3305
    ## 699  2738               DELTA          LOS ANGELES 181-708-2089
    ## 700   448              UNITED             HONOLULU 242-540-4234
    ## 701   655         UNITED INTL               MUNICH 105 116 9695
    ## 702  1510        HAWAIIAN AIR             HONOLULU 945 144 7892
    ## 703  3231              ALASKA             PORTLAND 531 895 6695
    ## 704  1965      CATHAY PACIFIC            HONG KONG 894 810 2674
    ## 705  1817         UNITED INTL      LONDON HEATHROW 943 812 6349
    ## 706  1282      CHINA SOUTHERN                WUHAN 476 168 4235
    ## 707  1816         UNITED INTL      LONDON HEATHROW 931 385 6757
    ## 708  1609     VIRGIN ATLANTIC      LONDON HEATHROW 100 378 8095
    ## 709  1802         UNITED INTL      LONDON HEATHROW 459 572 0244
    ## 710  2595             JETBLUE               BOSTON 397-362-5469
    ## 711  2757      AIR FRANCE/KLM      PARIS-DE GAULLE 938 982 5585
    ## 712  1488            FRONTIER               DENVER 769-472-2992
    ## 713  2204          AER LINGUS               DUBLIN 190 204 1154
    ## 714  2889              UNITED               AUSTIN 321 616 1013
    ## 715  1595           SOUTHWEST          LOS ANGELES 178-232-0815
    ## 716  1445              UNITED             PORTLAND 828 549 6666
    ## 717  2230          AER LINGUS               DUBLIN 280 544 4554
    ## 718  2629             JETBLUE         NEW YORK-JFK 183 208 5054
    ## 719  1924      AIR FRANCE/KLM      PARIS-DE GAULLE 203 448 1522
    ## 720  2142          AIR CANADA            VANCOUVER 900-871-9056
    ## 721  2755      AIR FRANCE/KLM      PARIS-DE GAULLE 406-167-1379
    ## 722  3157              UNITED            SAN DIEGO 641-635-8466
    ## 723  1782              UNITED       RALEIGH-DURHAM 807-671-6158
    ## 724  1455              UNITED             PORTLAND 768 529 8051
    ## 725   488              ALASKA             PORTLAND 220 660 0306
    ## 726  2680              UNITED              ORLANDO 928-179-7556
    ## 727  3065              UNITED              SPOKANE 153 756 0278
    ## 728  3254              UNITED            SAN DIEGO 273 829 9197
    ## 729   331              ALASKA            SAN DIEGO 269 463 0911
    ## 730  3284           SOUTHWEST               DENVER 696 984 8826
    ## 731  2321           SOUTHWEST          LOS ANGELES 905-903-5258
    ## 732  2448             JETBLUE               BOSTON 267 332 4709
    ## 733  2120             JETBLUE           LONG BEACH 534 216 6666
    ## 734  1254       CHINA EASTERN             SHANGHAI 971 175 2968
    ## 735  2778              UNITED               AUSTIN 274-863-3205
    ## 736  1714      AIR FRANCE/KLM      PARIS-DE GAULLE 928 445 5474
    ## 737  2619            INTERJET          GUADALAJARA 858 990 5153
    ## 738  1486            FRONTIER               DENVER 731-813-2043
    ## 739  1803              ALASKA       RALEIGH-DURHAM 563-732-6802
    ## 740  2494              UNITED               DENVER 145 725 4021
    ## 741  2489              UNITED               DENVER 931 311 5801
    ## 742  1460              UNITED             PORTLAND 172 990 3485
    ## 743  3090                COPA          PANAMA CITY 872 325 4341
    ## 744   693      CATHAY PACIFIC            HONG KONG 152 790 8238
    ## 745     9           SOUTHWEST          LOS ANGELES 330 561 9257
    ## 746  3073 PHILIPPINE AIRLINES               MANILA 437 420 7546
    ## 747  2258                 WOW            REYKJAVIK 495 632 4027
    ## 748   477              UNITED               DENVER 311-305-4367
    ## 749   677      AIR FRANCE/KLM      PARIS-DE GAULLE 817-400-0481
    ## 750  2674             JETBLUE               BOSTON 430 723 1079
    ## 751  2091            AMERICAN         PHILADELPHIA 226 490 8696
    ## 752  1540               DELTA         NEW YORK-JFK 123 570 8640
    ## 753  1744      CATHAY PACIFIC            HONG KONG 665 803 2453
    ## 754  2870              UNITED WASHINGTON DC-DULLES 639 132 6386
    ## 755  1809              ALASKA       RALEIGH-DURHAM 437 886 0753
    ## 756  2104             JETBLUE           LONG BEACH 626 756 5089
    ## 757  3189               DELTA          LOS ANGELES 714 950 3364
    ## 758  2190      CATHAY PACIFIC            HONG KONG 653-786-5985
    ## 759  1969      CATHAY PACIFIC            HONG KONG 518 286 5956
    ## 760  3255              UNITED            SAN DIEGO 194 960 2145
    ## 761   692      CATHAY PACIFIC            HONG KONG 362-136-1153
    ## 762  3116              ALASKA               NEWARK 376-456-0697
    ## 763  2704           SOUTHWEST               DENVER 657 832 1189
    ## 764  1097               DELTA MINNEAPOLIS-ST. PAUL 962-918-6117
    ## 765   733      AIR FRANCE/KLM      PARIS-DE GAULLE 692 929 3592
    ## 766  2368              UNITED               AUSTIN 805-877-3887
    ## 767  2733               DELTA          LOS ANGELES 819 732 4132
    ## 768  1170               DELTA         NEW YORK-JFK 367 221 9710
    ## 769  1786              UNITED       RALEIGH-DURHAM 361 154 1789
    ## 770   623              ALASKA WASHINGTON DC-DULLES 680 488 1182
    ## 771  3170            AMERICAN     DALLAS-FT. WORTH 928 638 1186
    ## 772  1309             JETBLUE         NEW YORK-JFK 588-693-9875
    ## 773  3283           SOUTHWEST               DENVER 681-308-7915
    ## 774  3234              ALASKA             PORTLAND 783 647 8490
    ## 775  3195      CATHAY PACIFIC            HONG KONG 897 847 0632
    ## 776  2731               DELTA          LOS ANGELES 150 952 4453
    ## 777  2946           SOUTHWEST            SAN DIEGO 322-884-3020
    ## 778   428              UNITED         INDIANAPOLIS 176-313-5403
    ## 779  2432     AIR NEW ZEALAND             AUCKLAND 487 109 4196
    ## 780  2109         UNITED INTL          MEXICO CITY 544 382 8289
    ## 781  1708      AIR FRANCE/KLM      PARIS-DE GAULLE 781 543 7456
    ## 782  2924    TURKISH AIRLINES             ISTANBUL 911 829 6476
    ## 783  3155              UNITED            SAN DIEGO 517 986 3426
    ## 784   492              ALASKA             PORTLAND 838 220 5397
    ## 785  2773                 WOW            REYKJAVIK 179 163 0902
    ## 786   283              UNITED            BALTIMORE 539 137 8983
    ## 787  2768                 WOW            REYKJAVIK 733-154-0094
    ## 788  1862              ALASKA              KAHULUI 639 881 3693
    ## 789  2218          AER LINGUS               DUBLIN 291-830-3017
    ## 790  1231      ANA ALL NIPPON         TOKYO-NARITA 750 520 0167
    ## 791   806              ALASKA            BALTIMORE 676 485 8963
    ## 792  1056          AIR CANADA              CALGARY 135 566 5090
    ## 793  2861              UNITED              SEATTLE 337 260 4996
    ## 794  2016              UNITED               NEWARK 371 185 2377
    ## 795  1209           SOUTHWEST            LAS VEGAS 280 461 1386
    ## 796   912          AIR CANADA              TORONTO 496-429-1314
    ## 797   562            EMIRATES                DUBAI 459 671 4698
    ## 798  1871              ALASKA              KAHULUI 486-268-3312
    ## 799   888              ALASKA       RALEIGH-DURHAM 497-518-4050
    ## 800  1944      AIR FRANCE/KLM      PARIS-DE GAULLE 859 495 4050
    ## 801   267          AIR CANADA              TORONTO 724-134-3870
    ## 802   627              ALASKA               NEWARK 125-578-4253
    ## 803  3148              UNITED            SAN DIEGO 614 800 2861
    ## 804  2758               DELTA              ATLANTA 487-232-4449
    ## 805  1628          KOREAN AIR                SEOUL 253-374-7102
    ## 806  1925      AIR FRANCE/KLM      PARIS-DE GAULLE 802 102 8345
    ## 807  2852              UNITED              SEATTLE 417 393 0050
    ## 808  1783              UNITED       RALEIGH-DURHAM 787-624-8443
    ## 809   921          AIR CANADA              TORONTO 341 824 5322
    ## 810  3267              UNITED            LAS VEGAS 415 551 1608
    ## 811   489              ALASKA             PORTLAND 473-238-3324
    ## 812  2137          AIR CANADA            VANCOUVER 876-834-0624
    ## 813   262          AIR CANADA              TORONTO 919 611 6170
    ## 814  2992            AMERICAN                MIAMI 261 434 7760
    ## 815  3062 PHILIPPINE AIRLINES               MANILA 617 310 2684
    ## 816  3078 PHILIPPINE AIRLINES               MANILA 182-535-3412
    ## 817  2009      AIR FRANCE/KLM      PARIS-DE GAULLE 506 129 1694
    ## 818  1315             JETBLUE         NEW YORK-JFK 313 990 8823
    ## 819  1321             JETBLUE         NEW YORK-JFK 656-941-5355
    ## 820  1902           LUFTHANSA               MUNICH 955 324 5981
    ## 821  2688           SOUTHWEST               DENVER 175 808 2189
    ## 822   371           LUFTHANSA            FRANKFURT 105-687-6500
    ## 823  1735          AIR CANADA              TORONTO 757 524 2964
    ## 824  2042     BRITISH AIRWAYS      LONDON HEATHROW 201 374 2424
    ## 825  2033     BRITISH AIRWAYS      LONDON HEATHROW 766 112 6143
    ## 826  1739          AIR CANADA              TORONTO 783-463-4865
    ## 827   734      CATHAY PACIFIC            HONG KONG 853-803-9900
    ## 828   376              ALASKA    SAN JOSE DEL CABO 992 114 6973
    ## 829   522              ALASKA      PUERTO VALLARTA 316-212-7309
    ## 830  3095              UNITED              SPOKANE 301 672 1092
    ## 831  1074          AIR CANADA              CALGARY 795 137 0201
    ## 832   855              ALASKA             PORTLAND 381-883-5497
    ## 833  2106             JETBLUE           LONG BEACH 100-531-4642
    ## 834  3150              UNITED            SAN DIEGO 994 923 6634
    ## 835  3154               DELTA       SALT LAKE CITY 920 355 8404
    ## 836  1836              ALASKA       RALEIGH-DURHAM 325 795 2455
    ## 837  1076          AIR CANADA              CALGARY 593 829 6250
    ## 838   774            AMERICAN          LOS ANGELES 566-482-9004
    ## 839   218            AMERICAN          LOS ANGELES 542 537 6770
    ## 840   305           SOUTHWEST          LOS ANGELES 716 191 1741
    ## 841  2616            INTERJET          GUADALAJARA 491-727-7162
    ## 842   934          AIR CANADA              TORONTO 167-336-5660
    ## 843  2268              UNITED          LOS ANGELES 358 831 0725
    ## 844   727      CATHAY PACIFIC            HONG KONG 432 979 7292
    ## 845  1082          AIR CANADA              CALGARY 728 662 3934
    ## 846  2632             JETBLUE         NEW YORK-JFK 380-918-8572
    ## 847  2781                 WOW            REYKJAVIK 151 434 6989
    ## 848  1114           SOUTHWEST            SAN DIEGO 755 544 2629
    ## 849  3173            AMERICAN     DALLAS-FT. WORTH 633 181 4494
    ## 850   259          AIR CANADA              TORONTO 346 706 5964
    ## 851   258          AIR CANADA              TORONTO 688 690 2184
    ## 852  3288           SOUTHWEST               DENVER 618 717 1697
    ## 853  2808               DELTA          LOS ANGELES 185-321-6877
    ## 854   976           LUFTHANSA               MUNICH 726 943 7486
    ## 855  1153           SOUTHWEST            SAN DIEGO 670-248-0186
    ## 856  2810               DELTA          LOS ANGELES 364 834 3150
    ## 857  2911               DELTA              SEATTLE 176 508 2778
    ## 858   223            AMERICAN          LOS ANGELES 120 941 0833
    ## 859  2372              UNITED               AUSTIN 670 902 3199
    ## 860  2831              UNITED              BURBANK 297 484 3285
    ## 861  1530             JETBLUE         NEW YORK-JFK 743-103-7645
    ## 862  2848              UNITED              SEATTLE 432-281-3682
    ## 863  2635             JETBLUE         NEW YORK-JFK 648 685 6188
    ## 864  1205           SOUTHWEST            LAS VEGAS 548 191 4898
    ## 865   406              UNITED         HOUSTON-BUSH 288 110 9483
    ## 866  2792               DELTA       SALT LAKE CITY 946-558-5801
    ## 867  2693           SOUTHWEST               DENVER 388 744 9637
    ## 868   193            AMERICAN          LOS ANGELES 506 463 9129
    ## 869  2622             JETBLUE               BOSTON 306 394 8640
    ## 870  1010          AIR CANADA            VANCOUVER 170-641-3537
    ## 871  2679              UNITED       CHICAGO-O'HARE 860 723 5066
    ## 872   254          AIR CANADA              TORONTO 814-895-6610
    ## 873   968           LUFTHANSA               MUNICH 598 735 8557
    ## 874   812              ALASKA            BALTIMORE 593 895 6761
    ## 875  2084            AMERICAN         PHILADELPHIA 508 484 9738
    ## 876  3213            AMERICAN          LOS ANGELES 719 489 4724
    ## 877   247              UNITED            NASHVILLE 503-671-4901
    ## 878   771              ALASKA         NEW YORK-JFK 275 649 8183
    ## 879  1952              UNITED               BOSTON 968 130 7012
    ## 880  2761              UNITED               AUSTIN 290 367 6676
    ## 881   388              ALASKA          LOS ANGELES 499 766 9941
    ## 882  2493              ALASKA       SALT LAKE CITY 538-393-2243
    ## 883  2945           SOUTHWEST          LOS ANGELES 845 544 4748
    ## 884   382              ALASKA          LOS ANGELES 784-458-8425
    ## 885   915          AIR CANADA              TORONTO 708 500 2758
    ## 886  2102         UNITED INTL          MEXICO CITY 982 555 9504
    ## 887  3185               DELTA          LOS ANGELES 477-307-3338
    ## 888   584             JETBLUE               BOSTON 744-301-1148
    ## 889  2064              UNITED               NEWARK 389 484 8888
    ## 890   285              UNITED            BALTIMORE 739 303 6128
    ## 891  1123               DELTA MINNEAPOLIS-ST. PAUL 175 905 9962
    ## 892  2827              UNITED              BURBANK 878-636-2294
    ## 893  2374              UNITED               AUSTIN 994 421 8642
    ## 894   850            AMERICAN     DALLAS-FT. WORTH 304-225-5895
    ## 895   194            AMERICAN          LOS ANGELES 931-522-5498
    ## 896   306              UNITED            SAN DIEGO 838 898 2275
    ## 897   458              UNITED     DALLAS-FT. WORTH 718 187 7125
    ## 898   701      CATHAY PACIFIC            HONG KONG 943 561 8955
    ## 899  2006      AIR FRANCE/KLM      PARIS-DE GAULLE 341 261 6456
    ## 900  2617          AEROMEXICO          MEXICO CITY 826 266 0205
    ## 901  1643     VIRGIN ATLANTIC      LONDON HEATHROW 589-975-0198
    ## 902  2668              UNITED       RALEIGH-DURHAM 657 680 8781
    ## 903  2772              UNITED               AUSTIN 501-668-7869
    ## 904  2357              UNITED               AUSTIN 224 365 8299
    ## 905  2479              UNITED       CHICAGO-O'HARE 747 588 1968
    ## 906  2780               DELTA              ATLANTA 748 446 1257
    ## 907  2333            AMERICAN       CHICAGO-O'HARE 525-552-4162
    ## 908  2342              UNITED               AUSTIN 635-714-8302
    ## 909  2359              UNITED               AUSTIN 114 668 2834
    ## 910  3099              ALASKA               NEWARK 404 788 2855
    ## 911  2310            AMERICAN       CHICAGO-O'HARE 673 292 2444
    ## 912   200            AMERICAN          LOS ANGELES 136 788 1426
    ## 913  2977            AMERICAN            CHARLOTTE 564 677 3934
    ## 914  1300         UNITED INTL      LONDON HEATHROW 213 981 7762
    ## 915  2526             JETBLUE           LONG BEACH 453-556-0852
    ## 916   238            AMERICAN          LOS ANGELES 423 593 4483
    ## 917  1122           SOUTHWEST            SAN DIEGO 820 609 7454
    ## 918  1532             JETBLUE         NEW YORK-JFK 131-641-1331
    ## 919  1877              UNITED               BOSTON 824 540 9579
    ## 920  2390         UNITED INTL               SYDNEY 739 737 9041
    ## 921  2817               DELTA          LOS ANGELES 609-332-7370
    ## 922  3182            AMERICAN     DALLAS-FT. WORTH 426-182-1365
    ## 923   225            AMERICAN          LOS ANGELES 898-210-6218
    ## 924  1175               DELTA         NEW YORK-JFK 938 394 0411
    ## 925  1457              UNITED             PORTLAND 904 844 1759
    ## 926  1757           LUFTHANSA               MUNICH 658-861-4306
    ## 927   274              UNITED          LOS ANGELES 380-105-1757
    ## 928  2873             EVA AIR               TAIPEI 249 452 4370
    ## 929  1436              UNITED            LAS VEGAS 443 384 8253
    ## 930   412              UNITED         HOUSTON-BUSH 711 289 2247
    ## 931  1565         UNITED INTL            HONG KONG 343-973-0193
    ## 932  2718      AIR FRANCE/KLM      PARIS-DE GAULLE 367 650 3720
    ## 933  1930      AIR FRANCE/KLM      PARIS-DE GAULLE 148 630 8560
    ## 934   867            AMERICAN     DALLAS-FT. WORTH 413-727-2672
    ## 935  2868              UNITED WASHINGTON DC-DULLES 148 501 5084
    ## 936  1165               DELTA         NEW YORK-JFK 564 780 8272
    ## 937   622              ALASKA               NEWARK 524 190 0899
    ## 938   658         UNITED INTL               MUNICH 463 792 2782
    ## 939   564            EMIRATES                DUBAI 522-286-5318
    ## 940  3139              ALASKA               NEWARK 310 719 4550
    ## 941  3220            AMERICAN          LOS ANGELES 314-360-4288
    ## 942  1560               DELTA              ATLANTA 822 271 5719
    ## 943  2553             JETBLUE               BOSTON 465-550-6610
    ## 944  2771               DELTA              ATLANTA 388 100 1482
    ## 945   739      CATHAY PACIFIC            HONG KONG 982 842 4913
    ## 946  2796                 WOW            REYKJAVIK 671 913 4563
    ## 947  1316         UNITED INTL      LONDON HEATHROW 144 468 5864
    ## 948  2429     AIR NEW ZEALAND             AUCKLAND 194 344 4039
    ## 949  1932      AIR FRANCE/KLM      PARIS-DE GAULLE 331 747 5714
    ## 950  2086            AMERICAN         PHILADELPHIA 221-190-1449
    ## 951   390              ALASKA          LOS ANGELES 676-614-9095
    ## 952  1626          KOREAN AIR                SEOUL 956 257 9319
    ## 953  3241            AMERICAN              PHOENIX 362 145 8268
    ## 954  3230              ALASKA             PORTLAND 570 727 3998
    ## 955   514              UNITED              KAHULUI 225 964 9193
    ## 956   365              ALASKA    SAN JOSE DEL CABO 227 419 9482
    ## 957  2280                 WOW            REYKJAVIK 949 360 7605
    ## 958   425              UNITED             HONOLULU 794 939 9735
    ## 959  2981            AMERICAN            CHARLOTTE 413 754 3034
    ## 960  2022              UNITED               NEWARK 806 730 0459
    ## 961  1674            EMIRATES                DUBAI 791 195 8909
    ## 962   374              ALASKA    SAN JOSE DEL CABO 691-318-3535
    ## 963   592             JETBLUE               BOSTON 852-386-6029
    ## 964  1732          AIR CANADA              TORONTO 380 682 7795
    ## 965  2801                 WOW            REYKJAVIK 247 586 4579
    ## 966  2805           SOUTHWEST              PHOENIX 235 257 1041
    ## 967   579             JETBLUE               BOSTON 106 756 2785
    ## 968  1907           LUFTHANSA               MUNICH 729-102-7511
    ## 969  2536             JETBLUE               BOSTON 795 583 0958
    ## 970  2735               DELTA          LOS ANGELES 700-431-3918
    ## 971  1939      AIR FRANCE/KLM      PARIS-DE GAULLE 929 632 1068
    ## 972  1567         UNITED INTL            HONG KONG 763 906 2495
    ## 973  2542             JETBLUE           LONG BEACH 469 976 6796
    ## 974   501              UNITED              KAHULUI 887-657-4143
    ## 975  1207           SOUTHWEST            LAS VEGAS 574-438-5329
    ## 976  1281         UNITED INTL      LONDON HEATHROW 319 127 9518
    ## 977  2058              UNITED               NEWARK 429 960 9710
    ## 978   674      AIR FRANCE/KLM      PARIS-DE GAULLE 419 646 0299
    ## 979   339           SOUTHWEST              PHOENIX 192 343 8515
    ## 980  2546             JETBLUE               BOSTON 521 336 8581
    ## 981  1597           SOUTHWEST          LOS ANGELES 776 367 6109
    ## 982  1213           SOUTHWEST            LAS VEGAS 470 367 1392
    ## 983  1016          AIR CANADA            VANCOUVER 998-931-4783
    ## 984  1347         UNITED INTL             SHANGHAI 362-178-6307
    ## 985  2286              QANTAS               SYDNEY 212 286 7936
    ## 986  3238              ALASKA             PORTLAND 936 193 9690
    ## 987  2927    TURKISH AIRLINES             ISTANBUL 797-870-7818
    ## 988  2962            AMERICAN         NEW YORK-JFK 693 907 5353
    ## 989  1143           SOUTHWEST          LOS ANGELES 332 973 4943
    ## 990  2341               DELTA MINNEAPOLIS-ST. PAUL 929 622 9077
    ## 991  2291              QANTAS               SYDNEY 649-379-5361
    ## 992   444              UNITED             HONOLULU 395-892-5646
    ## 993  1223           SOUTHWEST            LAS VEGAS 221-628-9561
    ## 994  2734      AIR FRANCE/KLM      PARIS-DE GAULLE 549-649-1864
    ## 995  2437     AIR NEW ZEALAND             AUCKLAND 342 941 0439
    ## 996   703      CATHAY PACIFIC            HONG KONG 701 390 9814
    ## 997  1812              ALASKA       RALEIGH-DURHAM 919-342-0230
    ## 998  2840              UNITED       CHICAGO-O'HARE 364-759-2705
    ## 999   785         UNITED INTL    SAN JOSE DEL CABO 949 543 7906
    ## 1000  409              UNITED         HOUSTON-BUSH 942 732 6403
    ## 1001 2386         UNITED INTL               SYDNEY 308-607-9855
    ## 1002 3069 PHILIPPINE AIRLINES               MANILA 764 645 5740
    ## 1003 3183               DELTA          LOS ANGELES 472-337-8838
    ## 1004 1511        HAWAIIAN AIR             HONOLULU 791 847 7278
    ## 1005 1763           LUFTHANSA               MUNICH 128 805 3828
    ## 1006 1884  SINGAPORE AIRLINES            SINGAPORE 365-832-0674
    ## 1007 2813               DELTA          LOS ANGELES 123-282-3494
    ## 1008  843              ALASKA              SEATTLE 285 424 4318
    ## 1009  356           LUFTHANSA            FRANKFURT 452 352 1387
    ## 1010  195            AMERICAN          LOS ANGELES 129-377-8159
    ## 1011  288              UNITED            BALTIMORE 222 143 3131
    ## 1012 1596  SINGAPORE AIRLINES            SINGAPORE 162-451-0594
    ## 1013 2207          AER LINGUS               DUBLIN 436-422-6171
    ## 1014  792            AMERICAN          LOS ANGELES 605 284 4260
    ## 1015 1438              UNITED             PORTLAND 929-102-5905
    ## 1016 1576               DELTA              DETROIT 847 507 8268
    ## 1017 2538             JETBLUE               BOSTON 452-811-8088
    ## 1018 2465              UNITED               DENVER 799 143 1677
    ## 1019 3279           SOUTHWEST               DENVER 196 756 4555
    ## 1020 1423              UNITED          KANSAS CITY 119-444-0817
    ## 1021  625              ALASKA               NEWARK 885 454 0883
    ## 1022 3059 PHILIPPINE AIRLINES               MANILA 945-998-0444
    ## 1023  731      CATHAY PACIFIC            HONG KONG 163 241 9321
    ## 1024 1498        HAWAIIAN AIR             HONOLULU 594-176-5811
    ## 1025 2018     BRITISH AIRWAYS      LONDON HEATHROW 332 963 4103
    ## 1026 3064                COPA          PANAMA CITY 389 318 3975
    ## 1027  511              ALASKA             PORTLAND 894-593-7953
    ## 1028 1587               DELTA       SALT LAKE CITY 561-266-7842
    ## 1029 2587             JETBLUE               BOSTON 354-958-8052
    ## 1030  631              ALASKA WASHINGTON DC-DULLES 151 921 2775
    ## 1031 1468             AVIANCA         SAN SALVADOR 901 140 3759
    ## 1032  441              UNITED             HONOLULU 333-520-4811
    ## 1033 1524               DELTA          LOS ANGELES 850 914 9348
    ## 1034 2799                 WOW            REYKJAVIK 246 272 9019
    ## 1035 2243           SOUTHWEST              PHOENIX 504 419 9191
    ## 1036 1473             AVIANCA         SAN SALVADOR 231 863 7554
    ## 1037 1563               DELTA              ATLANTA 939 253 9048
    ## 1038 1711      AIR FRANCE/KLM      PARIS-DE GAULLE 879-154-4494
    ## 1039 2424              UNITED             HONOLULU 577 786 2546
    ## 1040 2696              UNITED              ORLANDO 841-717-4447
    ## 1041 2965            AMERICAN            CHARLOTTE 397-353-6309
    ## 1042  769          AIR CANADA              TORONTO 558 191 4548
    ## 1043  980           LUFTHANSA               MUNICH 905 768 2297
    ## 1044 1111               DELTA MINNEAPOLIS-ST. PAUL 348 522 2051
    ## 1045 2003      AIR FRANCE/KLM      PARIS-DE GAULLE 307-323-6861
    ## 1046 2095             JETBLUE           LONG BEACH 663 886 2487
    ## 1047 2447             JETBLUE               BOSTON 274 944 6097
    ## 1048 3104              UNITED              SPOKANE 328 307 0875
    ## 1049 3105                COPA          PANAMA CITY 673 524 3504
    ## 1050 2794                 WOW            REYKJAVIK 934 721 0615
    ## 1051 1933      AIR FRANCE/KLM      PARIS-DE GAULLE 102-957-6486
    ## 1052 2739              UNITED              KAHULUI 794 925 8846
    ## 1053 2032              UNITED               NEWARK 637 281 4111
    ## 1054  729      AIR FRANCE/KLM      PARIS-DE GAULLE 335 802 2651
    ## 1055  838              ALASKA              SEATTLE 560 699 9908
    ## 1056 2847              UNITED              SEATTLE 451 163 0102
    ## 1057 2464              UNITED             HONOLULU 613 800 0835
    ## 1058  245              UNITED            NASHVILLE 370 453 5800
    ## 1059 2859              UNITED              SEATTLE 192 507 5411
    ## 1060 2428     AIR NEW ZEALAND             AUCKLAND 182-227-4838
    ## 1061 3080              UNITED              SPOKANE 647 126 2332
    ## 1062  530              ALASKA      PUERTO VALLARTA 606 125 6957
    ## 1063 1572         UNITED INTL            HONG KONG 728-404-5558
    ## 1064 2285              QANTAS               SYDNEY 506 812 6052
    ## 1065 2585             JETBLUE           LONG BEACH 427-665-3475
    ## 1066 2241      CATHAY PACIFIC            HONG KONG 808 739 7162
    ## 1067 2545             JETBLUE           LONG BEACH 375-978-3305
    ## 1068 3093              UNITED              SPOKANE 181-708-2089
    ## 1069 2540             JETBLUE           LONG BEACH 242-540-4234
    ## 1070 3018              UNITED             PORTLAND 105 116 9695
    ## 1071 3289           SOUTHWEST               DENVER 945 144 7892
    ## 1072 1581               DELTA              DETROIT 531 895 6695
    ## 1073 1182               DELTA         NEW YORK-JFK 894 810 2674
    ## 1074 2968            AMERICAN            CHARLOTTE 943 812 6349
    ## 1075 1248       CHINA EASTERN             SHANGHAI 476 168 4235
    ## 1076 1526             JETBLUE         NEW YORK-JFK 931 385 6757
    ## 1077 1651     VIRGIN ATLANTIC      LONDON HEATHROW 100 378 8095
    ## 1078  341              ALASKA            SAN DIEGO 459 572 0244
    ## 1079 1472              ALASKA         NEW YORK-JFK 397-362-5469
    ## 1080 3286           SOUTHWEST               DENVER 938 982 5585
    ## 1081  332           SOUTHWEST              PHOENIX 769-472-2992
    ## 1082 1030          AIR CANADA            VANCOUVER 190 204 1154
    ## 1083 3051 PHILIPPINE AIRLINES               MANILA 321 616 1013
    ## 1084 1599  SINGAPORE AIRLINES            SINGAPORE 178-232-0815
    ## 1085  738      CATHAY PACIFIC            HONG KONG 828 549 6666
    ## 1086 1221           SOUTHWEST            LAS VEGAS 280 544 4554
    ## 1087 1790              UNITED       RALEIGH-DURHAM 183 208 5054
    ## 1088 1720          AIR CANADA              TORONTO 203 448 1522
    ## 1089 3033              UNITED             PORTLAND 900-871-9056
    ## 1090 2560             JETBLUE           LONG BEACH 406-167-1379
    ## 1091 1238       CHINA EASTERN             SHANGHAI 641-635-8466
    ## 1092 1731          AIR CANADA              TORONTO 807-671-6158
    ## 1093 2274              UNITED          LOS ANGELES 768 529 8051
    ## 1094 2431     AIR NEW ZEALAND             AUCKLAND 220 660 0306
    ## 1095 2541             JETBLUE           LONG BEACH 928-179-7556
    ## 1096 1138           SOUTHWEST          LOS ANGELES 153 756 0278
    ## 1097 2234           SOUTHWEST            SANTA ANA 273 829 9197
    ## 1098 1503        HAWAIIAN AIR             HONOLULU 269 463 0911
    ## 1099 2742      AIR FRANCE/KLM      PARIS-DE GAULLE 696 984 8826
    ## 1100 2235      CATHAY PACIFIC            HONG KONG 905-903-5258
    ## 1101 1157           SOUTHWEST          LOS ANGELES 267 332 4709
    ## 1102 1334         UNITED INTL             SHANGHAI 534 216 6666
    ## 1103 2175            FRONTIER               DENVER 971 175 2968
    ## 1104 2566             JETBLUE           LONG BEACH 274-863-3205
    ## 1105 2092             JETBLUE           LONG BEACH 928 445 5474
    ## 1106 2578             JETBLUE           LONG BEACH 858 990 5153
    ## 1107 3137              ALASKA               NEWARK 731-813-2043
    ## 1108 1476              ALASKA         NEW YORK-JFK 563-732-6802
    ## 1109 2647             JETBLUE               BOSTON 145 725 4021
    ## 1110  289           SOUTHWEST          LOS ANGELES 931 311 5801
    ## 1111  784            AMERICAN          LOS ANGELES 172 990 3485
    ## 1112 1106               DELTA MINNEAPOLIS-ST. PAUL 872 325 4341
    ## 1113 3036            AMERICAN     DALLAS-FT. WORTH 152 790 8238
    ## 1114 1502        HAWAIIAN AIR             HONOLULU 330 561 9257
    ## 1115 1854           SOUTHWEST            SAN DIEGO 437 420 7546
    ## 1116  201            AMERICAN          LOS ANGELES 495 632 4027
    ## 1117 2952           SOUTHWEST          LOS ANGELES 311-305-4367
    ## 1118 3166            AMERICAN     DALLAS-FT. WORTH 817-400-0481
    ## 1119  443              UNITED         INDIANAPOLIS 430 723 1079
    ## 1120 3190      CATHAY PACIFIC            HONG KONG 226 490 8696
    ## 1121 1904              UNITED               BOSTON 123 570 8640
    ## 1122 1997              UNITED            BALTIMORE 639 132 6386
    ## 1123 2159       CHINA EASTERN              QINGDAO 437 886 0753
    ## 1124 2063            AMERICAN         PHILADELPHIA 626 756 5089
    ## 1125 3168            AMERICAN     DALLAS-FT. WORTH 714 950 3364
    ## 1126  587              ALASKA WASHINGTON DC-DULLES 653-786-5985
    ## 1127 1396         UNITED INTL             SHANGHAI 518 286 5956
    ## 1128 1737          AIR CANADA              TORONTO 194 960 2145
    ## 1129 2439     AIR NEW ZEALAND             AUCKLAND 362-136-1153
    ## 1130  602              ALASKA WASHINGTON DC-DULLES 376-456-0697
    ## 1131 2571             JETBLUE           LONG BEACH 657 832 1189
    ## 1132 2370              UNITED               AUSTIN 962-918-6117
    ## 1133  524            EMIRATES                DUBAI 692 929 3592
    ## 1134 2219           SOUTHWEST              PHOENIX 805-877-3887
    ## 1135 1928      AIR FRANCE/KLM      PARIS-DE GAULLE 819 732 4132
    ## 1136  930          AIR CANADA              TORONTO 367 221 9710
    ## 1137 2245           SOUTHWEST              PHOENIX 361 154 1789
    ## 1138 1012          AIR CANADA            VANCOUVER 680 488 1182
    ## 1139 1717      AIR FRANCE/KLM      PARIS-DE GAULLE 928 638 1186
    ## 1140 1331         UNITED INTL               KANSAI 588-693-9875
    ## 1141 1647     VIRGIN ATLANTIC      LONDON HEATHROW 681-308-7915
    ## 1142 1870              UNITED          LOS ANGELES 783 647 8490
    ## 1143 1089               DELTA MINNEAPOLIS-ST. PAUL 897 847 0632
    ## 1144  786         UNITED INTL    SAN JOSE DEL CABO 150 952 4453
    ## 1145 2507              UNITED       CHICAGO-O'HARE 322-884-3020
    ## 1146 1796              ALASKA       RALEIGH-DURHAM 176-313-5403
    ## 1147 1963      CATHAY PACIFIC            HONG KONG 487 109 4196
    ## 1148  403              UNITED              PHOENIX 544 382 8289
    ## 1149  461              UNITED     DALLAS-FT. WORTH 781 543 7456
    ## 1150  804              ALASKA            BALTIMORE 911 829 6476
    ## 1151 2979            AMERICAN            CHARLOTTE 517 986 3426
    ## 1152  905              ALASKA       RALEIGH-DURHAM 838 220 5397
    ## 1153 1730          AIR CANADA              TORONTO 179 163 0902
    ## 1154  470              UNITED               DENVER 539 137 8983
    ## 1155 1501        HAWAIIAN AIR             HONOLULU 733-154-0094
    ## 1156 1202           SOUTHWEST            LAS VEGAS 639 881 3693
    ## 1157 1380         UNITED INTL      PARIS-DE GAULLE 291-830-3017
    ## 1158    8           SOUTHWEST          LOS ANGELES 750 520 0167
    ## 1159  626              ALASKA WASHINGTON DC-DULLES 676 485 8963
    ## 1160 1801              ALASKA       RALEIGH-DURHAM 135 566 5090
    ## 1161 2247                 WOW            REYKJAVIK 337 260 4996
    ## 1162  873              ALASKA              SEATTLE 371 185 2377
    ## 1163  554            EMIRATES                DUBAI 280 461 1386
    ## 1164 1918      AIR FRANCE/KLM      PARIS-DE GAULLE 496-429-1314
    ## 1165 2838              UNITED       CHICAGO-O'HARE 459 671 4698
    ## 1166 2879             EVA AIR               TAIPEI 486-268-3312
    ## 1167 1839              ALASKA       RALEIGH-DURHAM 497-518-4050
    ## 1168 1393         UNITED INTL             SHANGHAI 859 495 4050
    ## 1169 1852           SOUTHWEST            SAN DIEGO 724-134-3870
    ## 1170 2136      CATHAY PACIFIC            HONG KONG 125-578-4253
    ## 1171 2660             JETBLUE               BOSTON 614 800 2861
    ## 1172  612             JETBLUE               BOSTON 487-232-4449
    ## 1173 1620     VIRGIN ATLANTIC      LONDON HEATHROW 253-374-7102
    ## 1174 2916    TURKISH AIRLINES             ISTANBUL 802 102 8345
    ## 1175 3242            AMERICAN              PHOENIX 417 393 0050
    ## 1176  829              ALASKA              SEATTLE 787-624-8443
    ## 1177 3077              UNITED              SPOKANE 341 824 5322
    ## 1178 2278                 WOW            REYKJAVIK 415 551 1608
    ## 1179 2935    TURKISH AIRLINES             ISTANBUL 473-238-3324
    ## 1180 2044     BRITISH AIRWAYS      LONDON HEATHROW 876-834-0624
    ## 1181 1747           LUFTHANSA               MUNICH 919 611 6170
    ## 1182  380              ALASKA          LOS ANGELES 261 434 7760
    ## 1183 2445             JETBLUE               BOSTON 617 310 2684
    ## 1184 1408              UNITED          KANSAS CITY 182-535-3412
    ## 1185 2346              UNITED               AUSTIN 506 129 1694
    ## 1186 2488              ALASKA       SALT LAKE CITY 313 990 8823
    ## 1187 2913    TURKISH AIRLINES             ISTANBUL 656-941-5355
    ## 1188 2320              QANTAS               SYDNEY 955 324 5981
    ## 1189  355              ALASKA               NEWARK 175 808 2189
    ## 1190 2416     AIR NEW ZEALAND             AUCKLAND 105-687-6500
    ## 1191 2863              UNITED              SEATTLE 757 524 2964
    ## 1192 1881  SINGAPORE AIRLINES            SINGAPORE 201 374 2424
    ## 1193  508              UNITED              KAHULUI 766 112 6143
    ## 1194 2878             EVA AIR               TAIPEI 783-463-4865
    ## 1195 3109              UNITED              SPOKANE 853-803-9900
    ## 1196 3067 PHILIPPINE AIRLINES               MANILA 992 114 6973
    ## 1197  818              ALASKA            BALTIMORE 316-212-7309
    ## 1198 2523              UNITED               NEWARK 301 672 1092
    ## 1199 1216           SOUTHWEST            LAS VEGAS 795 137 0201
    ## 1200 1659            EMIRATES                DUBAI 381-883-5497
    ## 1201  548            EMIRATES                DUBAI 100-531-4642
    ## 1202 2206          AER LINGUS               DUBLIN 994 923 6634
    ## 1203 2026     BRITISH AIRWAYS      LONDON HEATHROW 920 355 8404
    ## 1204 1547           AIR CHINA              BEIJING 325 795 2455
    ## 1205 2552             JETBLUE           LONG BEACH 593 829 6250
    ## 1206 2476              UNITED       CHICAGO-O'HARE 566-482-9004
    ## 1207 2441              UNITED             HONOLULU 542 537 6770
    ## 1208 2512              UNITED       CHICAGO-O'HARE 716 191 1741
    ## 1209 1753           LUFTHANSA               MUNICH 491-727-7162
    ## 1210  351           LUFTHANSA            FRANKFURT 167-336-5660
    ## 1211 2923               DELTA              SEATTLE 358 831 0725
    ## 1212  880              ALASKA              SEATTLE 432 979 7292
    ## 1213  586             JETBLUE               BOSTON 728 662 3934
    ## 1214 1166           SOUTHWEST          LOS ANGELES 380-918-8572
    ## 1215  984           LUFTHANSA               MUNICH 151 434 6989
    ## 1216 2089            AMERICAN         PHILADELPHIA 755 544 2629
    ## 1217 3096              ALASKA               NEWARK 633 181 4494
    ## 1218 3031            AMERICAN     DALLAS-FT. WORTH 346 706 5964
    ## 1219 2027              UNITED               NEWARK 688 690 2184
    ## 1220  359           LUFTHANSA            FRANKFURT 618 717 1697
    ## 1221 1892  SINGAPORE AIRLINES            SINGAPORE 185-321-6877
    ## 1222 1888  SINGAPORE AIRLINES            SINGAPORE 726 943 7486
    ## 1223 2719               DELTA          LOS ANGELES 670-248-0186
    ## 1224 2983            AMERICAN            CHARLOTTE 364 834 3150
    ## 1225 1350         UNITED INTL             SHANGHAI 176 508 2778
    ## 1226  493              UNITED               DENVER 120 941 0833
    ## 1227 1887              UNITED               BOSTON 670 902 3199
    ## 1228 1750           LUFTHANSA               MUNICH 297 484 3285
    ## 1229 1142           SOUTHWEST            SAN DIEGO 743-103-7645
    ## 1230 1823              ALASKA       RALEIGH-DURHAM 432-281-3682
    ## 1231 2473              UNITED               DENVER 648 685 6188
    ## 1232 2514              UNITED       CHICAGO-O'HARE 548 191 4898
    ## 1233 3282           SOUTHWEST               DENVER 288 110 9483
    ## 1234  878              ALASKA              SEATTLE 946-558-5801
    ## 1235 2315            AMERICAN       CHICAGO-O'HARE 388 744 9637
    ## 1236 2353              UNITED               AUSTIN 506 463 9129
    ## 1237 2504              ALASKA       SALT LAKE CITY 306 394 8640
    ## 1238 2028              UNITED               NEWARK 170-641-3537
    ## 1239  366           LUFTHANSA            FRANKFURT 860 723 5066
    ## 1240 1679      CATHAY PACIFIC            HONG KONG 814-895-6610
    ## 1241  385              ALASKA          LOS ANGELES 598 735 8557
    ## 1242  569              ALASKA              SEATTLE 593 895 6761
    ## 1243 1291         UNITED INTL      LONDON HEATHROW 508 484 9738
    ## 1244 1671            EMIRATES                DUBAI 719 489 4724
    ## 1245 2782               DELTA              ATLANTA 503-671-4901
    ## 1246 2850              UNITED              SEATTLE 275 649 8183
    ## 1247 3192               DELTA          LOS ANGELES 968 130 7012
    ## 1248 3274              UNITED            LAS VEGAS 290 367 6676
    ## 1249 2697              UNITED       CHICAGO-O'HARE 499 766 9941
    ## 1250 1684      CATHAY PACIFIC            HONG KONG 538-393-2243
    ## 1251 3132              UNITED            SAN DIEGO 845 544 4748
    ## 1252 2563              UNITED       CHICAGO-O'HARE 784-458-8425
    ## 1253  314              UNITED            SAN DIEGO 708 500 2758
    ## 1254 1467              ALASKA         NEW YORK-JFK 982 555 9504
    ## 1255 2568             JETBLUE           LONG BEACH 477-307-3338
    ## 1256 2570              UNITED       CHICAGO-O'HARE 744-301-1148
    ## 1257 1437              UNITED            LAS VEGAS 389 484 8888
    ## 1258 1725          AIR CANADA              TORONTO 739 303 6128
    ## 1259 1668            EMIRATES                DUBAI 175 905 9962
    ## 1260 2079             JETBLUE           LONG BEACH 878-636-2294
    ## 1261 2539             JETBLUE           LONG BEACH 994 421 8642
    ## 1262  802            AMERICAN          LOS ANGELES 304-225-5895
    ## 1263 2851              UNITED              SEATTLE 931-522-5498
    ## 1264 1036          AIR CANADA            VANCOUVER 838 898 2275
    ## 1265 2795               DELTA       SALT LAKE CITY 718 187 7125
    ## 1266 1972              UNITED            BALTIMORE 943 561 8955
    ## 1267 2535             JETBLUE           LONG BEACH 341 261 6456
    ## 1268  604             JETBLUE               BOSTON 826 266 0205
    ## 1269 1675      AIR FRANCE/KLM      PARIS-DE GAULLE 589-975-0198
    ## 1270  404              UNITED              PHOENIX 657 680 8781
    ## 1271  357           LUFTHANSA            FRANKFURT 501-668-7869
    ## 1272  858            AMERICAN     DALLAS-FT. WORTH 224 365 8299
    ## 1273  933          AIR CANADA              TORONTO 747 588 1968
    ## 1274 3159              UNITED            SAN DIEGO 748 446 1257
    ## 1275  537            EMIRATES                DUBAI 525-552-4162
    ## 1276  762          AIR CANADA              TORONTO 635-714-8302
    ## 1277 1548               DELTA         NEW YORK-JFK 114 668 2834
    ## 1278 2163            FRONTIER               DENVER 404 788 2855
    ## 1279 2466              UNITED       CHICAGO-O'HARE 673 292 2444
    ## 1280  407              UNITED              PHOENIX 136 788 1426
    ## 1281 1389         UNITED INTL             SHANGHAI 564 677 3934
    ## 1282  456              UNITED     DALLAS-FT. WORTH 213 981 7762
    ## 1283 1364         UNITED INTL               KANSAI 453-556-0852
    ## 1284 3280           SOUTHWEST               DENVER 423 593 4483
    ## 1285  851            AMERICAN     DALLAS-FT. WORTH 820 609 7454
    ## 1286  619              ALASKA               NEWARK 131-641-1331
    ## 1287 1818         UNITED INTL      LONDON HEATHROW 824 540 9579
    ## 1288 1964      CATHAY PACIFIC            HONG KONG 739 737 9041
    ## 1289 2271              UNITED          LOS ANGELES 609-332-7370
    ## 1290  472              UNITED               DENVER 426-182-1365
    ## 1291  417              UNITED              PHOENIX 898-210-6218
    ## 1292  467              UNITED               DENVER 938 394 0411
    ## 1293 1310         UNITED INTL      LONDON HEATHROW 904 844 1759
    ## 1294 2633              UNITED       CHICAGO-O'HARE 658-861-4306
    ## 1295 2201           SOUTHWEST            LAS VEGAS 380-105-1757
    ## 1296 1491        HAWAIIAN AIR             HONOLULU 249 452 4370
    ## 1297 2228          AER LINGUS               DUBLIN 443 384 8253
    ## 1298 2470              UNITED             HONOLULU 711 289 2247
    ## 1299 2419     AIR NEW ZEALAND             AUCKLAND 343-973-0193
    ## 1300 2767              UNITED               AUSTIN 367 650 3720
    ## 1301  301           SOUTHWEST          LOS ANGELES 148 630 8560
    ## 1302 2972            AMERICAN            CHARLOTTE 413-727-2672
    ## 1303  333              ALASKA            SAN DIEGO 148 501 5084
    ## 1304 1465            FRONTIER               DENVER 564 780 8272
    ## 1305  482              UNITED               DENVER 524 190 0899
    ## 1306 1701      AIR FRANCE/KLM      PARIS-DE GAULLE 463 792 2782
    ## 1307 1697              ALASKA            SANTA ANA 522-286-5318
    ## 1308 1829              ALASKA       RALEIGH-DURHAM 310 719 4550
    ## 1309 1146           SOUTHWEST          LOS ANGELES 314-360-4288
    ## 1310 3004            AMERICAN                MIAMI 822 271 5719
    ## 1311  234            AMERICAN          LOS ANGELES 465-550-6610
    ## 1312 1190           SOUTHWEST            LAS VEGAS 388 100 1482
    ## 1313 1861              ALASKA              KAHULUI 982 842 4913
    ## 1314 2555             JETBLUE           LONG BEACH 671 913 4563
    ## 1315 1054          AIR CANADA              CALGARY 144 468 5864
    ## 1316 1057          AIR CANADA              CALGARY 194 344 4039
    ## 1317 2785                 WOW            REYKJAVIK 331 747 5714
    ## 1318  777         UNITED INTL    SAN JOSE DEL CABO 221-190-1449
    ## 1319  307           SOUTHWEST          LOS ANGELES 676-614-9095
    ## 1320 1636          KOREAN AIR                SEOUL 956 257 9319
    ## 1321 2115         UNITED INTL          MEXICO CITY 362 145 8268
    ## 1322  236            AMERICAN          LOS ANGELES 570 727 3998
    ## 1323 1399         UNITED INTL      PARIS-DE GAULLE 225 964 9193
    ## 1324 1947      AIR FRANCE/KLM      PARIS-DE GAULLE 227 419 9482
    ## 1325  424              UNITED         INDIANAPOLIS 949 360 7605
    ## 1326 3253              UNITED            SAN DIEGO 794 939 9735
    ## 1327  459              ALASKA            LAS VEGAS 413 754 3034
    ## 1328 2783                 WOW            REYKJAVIK 806 730 0459
    ## 1329  671              UNITED               NEWARK 791 195 8909
    ## 1330  779            AMERICAN          LOS ANGELES 691-318-3535
    ## 1331 1258       CHINA EASTERN             SHANGHAI 852-386-6029
    ## 1332 1804         UNITED INTL      LONDON HEATHROW 380 682 7795
    ## 1333 2071            AMERICAN         PHILADELPHIA 247 586 4579
    ## 1334  418              UNITED             HONOLULU 235 257 1041
    ## 1335 2260                 WOW            REYKJAVIK 106 756 2785
    ## 1336 2114         UNITED INTL          MEXICO CITY 729-102-7511
    ## 1337 1125           SOUTHWEST            SAN DIEGO 795 583 0958
    ## 1338 1867              UNITED          LOS ANGELES 700-431-3918
    ## 1339 1960      CATHAY PACIFIC            HONG KONG 929 632 1068
    ## 1340 2313              QANTAS               SYDNEY 763 906 2495
    ## 1341  756              ALASKA         NEW YORK-JFK 469 976 6796
    ## 1342  957              ALASKA         NEW YORK-JFK 887-657-4143
    ## 1343 2295              QANTAS               SYDNEY 574-438-5329
    ## 1344 2930    TURKISH AIRLINES             ISTANBUL 319 127 9518
    ## 1345 2990            AMERICAN                MIAMI 429 960 9710
    ## 1346 1815         UNITED INTL      LONDON HEATHROW 419 646 0299
    ## 1347 2061              UNITED               NEWARK 192 343 8515
    ## 1348 1561               DELTA         NEW YORK-JFK 521 336 8581
    ## 1349 3265              UNITED            SAN DIEGO 776 367 6109
    ## 1350 1381         UNITED INTL             SHANGHAI 470 367 1392
    ## 1351  484              ALASKA             PORTLAND 998-931-4783
    ## 1352 2110             JETBLUE           LONG BEACH 362-178-6307
    ## 1353 2393         UNITED INTL               SYDNEY 212 286 7936
    ## 1354 1910      AIR FRANCE/KLM      PARIS-DE GAULLE 936 193 9690
    ## 1355 1494        HAWAIIAN AIR             HONOLULU 797-870-7818
    ## 1356 3226              ALASKA             PORTLAND 693 907 5353
    ## 1357 1938              UNITED               BOSTON 332 973 4943
    ## 1358 1303         UNITED INTL      LONDON HEATHROW 929 622 9077
    ## 1359 3191               DELTA          LOS ANGELES 649-379-5361
    ## 1360 1663            EMIRATES                DUBAI 395-892-5646
    ## 1361 1136           SOUTHWEST          LOS ANGELES 221-628-9561
    ## 1362 1185               DELTA         NEW YORK-JFK 549-649-1864
    ## 1363 1402         UNITED INTL      PARIS-DE GAULLE 342 941 0439
    ## 1364 1649     VIRGIN ATLANTIC      LONDON HEATHROW 701 390 9814
    ## 1365 1988      AIR FRANCE/KLM            AMSTERDAM 919-342-0230
    ## 1366 2220           SOUTHWEST              PHOENIX 364-759-2705
    ## 1367 2312            AMERICAN       CHICAGO-O'HARE 949 543 7906
    ## 1368 2614            INTERJET          GUADALAJARA 942 732 6403
    ## 1369 2525              UNITED               NEWARK 308-607-9855
    ## 1370 1496        HAWAIIAN AIR             HONOLULU 764 645 5740
    ## 1371 1640     VIRGIN ATLANTIC      LONDON HEATHROW 472-337-8838
    ## 1372 2724              UNITED              KAHULUI 791 847 7278
    ## 1373 3131              ALASKA               NEWARK 128 805 3828
    ## 1374 1285      CHINA SOUTHERN                WUHAN 365-832-0674
    ## 1375 2881             EVA AIR               TAIPEI 123-282-3494
    ## 1376  807            AMERICAN          LOS ANGELES 285 424 4318
    ## 1377 3147               DELTA       SALT LAKE CITY 452 352 1387
    ## 1378 2225          AER LINGUS               DUBLIN 129-377-8159
    ## 1379 1935      AIR FRANCE/KLM      PARIS-DE GAULLE 222 143 3131
    ## 1380 1843           SOUTHWEST            SAN DIEGO 162-451-0594
    ## 1381  454              UNITED     DALLAS-FT. WORTH 436-422-6171
    ## 1382  565              ALASKA              SEATTLE 605 284 4260
    ## 1383  799              ALASKA            BALTIMORE 929-102-5905
    ## 1384 1001           LUFTHANSA               MUNICH 847 507 8268
    ## 1385 2237          AER LINGUS               DUBLIN 452-811-8088
    ## 1386 2841              UNITED       CHICAGO-O'HARE 799 143 1677
    ## 1387 2898                 WOW            REYKJAVIK 196 756 4555
    ## 1388 3110              ALASKA               NEWARK 119-444-0817
    ## 1389 3133               DELTA       SALT LAKE CITY 885 454 0883
    ## 1390  926              ALASKA       RALEIGH-DURHAM 945-998-0444
    ## 1391 1463              ALASKA         NEW YORK-JFK 163 241 9321
    ## 1392 1917      AIR FRANCE/KLM      PARIS-DE GAULLE 594-176-5811
    ## 1393 1927      AIR FRANCE/KLM      PARIS-DE GAULLE 332 963 4103
    ## 1394 1522            INTERJET          GUADALAJARA 389 318 3975
    ## 1395 3210            AMERICAN          LOS ANGELES 894-593-7953
    ## 1396 2737              UNITED              KAHULUI 561-266-7842
    ## 1397 1390         UNITED INTL      PARIS-DE GAULLE 354-958-8052
    ## 1398 1366         UNITED INTL               KANSAI 151 921 2775
    ## 1399 2256              UNITED          BAKERSFIELD 901 140 3759
    ## 1400  433              UNITED             HONOLULU 333-520-4811
    ## 1401  813              ALASKA             PORTLAND 850 914 9348
    ## 1402 1375         UNITED INTL      PARIS-DE GAULLE 246 272 9019
    ## 1403  434              UNITED         INDIANAPOLIS 504 419 9191
    ## 1404 2293              QANTAS               SYDNEY 231 863 7554
    ## 1405 1667            EMIRATES                DUBAI 939 253 9048
    ## 1406 2124             JETBLUE           LONG BEACH 879-154-4494
    ## 1407 2685              UNITED       CHICAGO-O'HARE 577 786 2546
    ## 1408  787            AMERICAN          LOS ANGELES 841-717-4447
    ## 1409 2130      CATHAY PACIFIC            HONG KONG 397-353-6309
    ## 1410 1351         UNITED INTL               KANSAI 558 191 4548
    ## 1411 1191           SOUTHWEST            LAS VEGAS 905 768 2297
    ## 1412 1332         UNITED INTL             SHANGHAI 348 522 2051
    ## 1413 2625              UNITED       CHICAGO-O'HARE 307-323-6861
    ## 1414  663         UNITED INTL               MUNICH 663 886 2487
    ## 1415 2478              UNITED               DENVER 274 944 6097
    ## 1416 1218           SOUTHWEST            LAS VEGAS 328 307 0875
    ## 1417 1885  SINGAPORE AIRLINES            SINGAPORE 673 524 3504
    ## 1418 2558             JETBLUE           LONG BEACH 934 721 0615
    ## 1419 2121         UNITED INTL          MEXICO CITY 102-957-6486
    ## 1420  767          AIR CANADA              TORONTO 794 925 8846
    ## 1421 1826              ALASKA       RALEIGH-DURHAM 637 281 4111
    ## 1422 1876  SINGAPORE AIRLINES            SINGAPORE 335 802 2651
    ## 1423 2589            INTERJET          GUADALAJARA 560 699 9908
    ## 1424 2658          AEROMEXICO          MEXICO CITY 451 163 0102
    ## 1425 2584             JETBLUE               BOSTON 613 800 0835
    ## 1426 2604            INTERJET          GUADALAJARA 370 453 5800
    ## 1427  723      CATHAY PACIFIC            HONG KONG 192 507 5411
    ## 1428 2185              QANTAS               SYDNEY 182-227-4838
    ## 1429 2516              UNITED       CHICAGO-O'HARE 647 126 2332
    ## 1430 2510              UNITED       CHICAGO-O'HARE 606 125 6957
    ## 1431  300              UNITED            SAN DIEGO 728-404-5558
    ## 1432 2627              UNITED       CHICAGO-O'HARE 506 812 6052
    ## 1433 1734          AIR CANADA              TORONTO 427-665-3475
    ## 1434 2463              UNITED               DENVER 808 739 7162
    ## 1435 2326              QANTAS               SYDNEY 375-978-3305
    ## 1436 2361             JETBLUE       FT. LAUDERDALE 181-708-2089
    ## 1437  509              ALASKA             PORTLAND 242-540-4234
    ## 1438 1417              UNITED          KANSAS CITY 105 116 9695
    ## 1439 2246           SOUTHWEST              PHOENIX 945 144 7892
    ## 1440  875              ALASKA              SEATTLE 531 895 6695
    ## 1441 2531              UNITED               NEWARK 894 810 2674
    ## 1442 2752      AIR FRANCE/KLM      PARIS-DE GAULLE 943 812 6349
    ## 1443 2166            FRONTIER               DENVER 476 168 4235
    ## 1444 2118         UNITED INTL          MEXICO CITY 931 385 6757
    ## 1445 2722              UNITED       CHICAGO-O'HARE 100 378 8095
    ## 1446 2503              UNITED               NEWARK 459 572 0244
    ## 1447 3088              UNITED              SPOKANE 397-362-5469
    ## 1448 1856              ALASKA              KAHULUI 938 982 5585
    ## 1449 1641     VIRGIN ATLANTIC      LONDON HEATHROW 769-472-2992
    ## 1450 2712           SOUTHWEST               DENVER 190 204 1154
    ## 1451  764              ALASKA         NEW YORK-JFK 321 616 1013
    ## 1452 1519               DELTA          LOS ANGELES 178-232-0815
    ## 1453 2116         UNITED INTL          MEXICO CITY 828 549 6666
    ## 1454 1252         UNITED INTL         TOKYO-NARITA 280 544 4554
    ## 1455 1662            EMIRATES                DUBAI 183 208 5054
    ## 1456 2948           SOUTHWEST            SAN DIEGO 203 448 1522
    ## 1457 2705              UNITED        SANTA BARBARA 900-871-9056
    ## 1458  716      CATHAY PACIFIC            HONG KONG 406-167-1379
    ## 1459  533              ALASKA      PUERTO VALLARTA 641-635-8466
    ## 1460 3276           SOUTHWEST               DENVER 807-671-6158
    ## 1461 2105         UNITED INTL          MEXICO CITY 768 529 8051
    ## 1462 1819              ALASKA       RALEIGH-DURHAM 220 660 0306
    ## 1463  504              UNITED              KAHULUI 928-179-7556
    ## 1464 2069            AMERICAN         PHILADELPHIA 153 756 0278
    ## 1465  255          AIR CANADA              TORONTO 273 829 9197
    ## 1466  919          AIR CANADA              TORONTO 269 463 0911
    ## 1467 1712      AIR FRANCE/KLM      PARIS-DE GAULLE 696 984 8826
    ## 1468 3167               DELTA          LOS ANGELES 905-903-5258
    ## 1469 1575         UNITED INTL            HONG KONG 267 332 4709
    ## 1470  856            AMERICAN     DALLAS-FT. WORTH 534 216 6666
    ## 1471  389              ALASKA         INDIANAPOLIS 971 175 2968
    ## 1472  261          AIR CANADA              TORONTO 274-863-3205
    ## 1473  281              UNITED            BALTIMORE 928 445 5474
    ## 1474  536              ALASKA      PUERTO VALLARTA 858 990 5153
    ## 1475 2440     AIR NEW ZEALAND             AUCKLAND 731-813-2043
    ## 1476 2839              UNITED       CHICAGO-O'HARE 563-732-6802
    ## 1477 3211              UNITED        SANTA BARBARA 145 725 4021
    ## 1478  793              ALASKA            BALTIMORE 931 311 5801
    ## 1479 2399         UNITED INTL              BEIJING 172 990 3485
    ## 1480 1245       CHINA EASTERN             SHANGHAI 872 325 4341
    ## 1481 2337               DELTA MINNEAPOLIS-ST. PAUL 152 790 8238
    ## 1482 2505              UNITED       CHICAGO-O'HARE 330 561 9257
    ## 1483 2580            INTERJET          GUADALAJARA 437 420 7546
    ## 1484  198            AMERICAN          LOS ANGELES 495 632 4027
    ## 1485 2506              ALASKA       SALT LAKE CITY 311-305-4367
    ## 1486 2349              UNITED               AUSTIN 817-400-0481
    ## 1487 2651             JETBLUE               BOSTON 430 723 1079
    ## 1488 2887                 WOW            REYKJAVIK 226 490 8696
    ## 1489 1528             JETBLUE         NEW YORK-JFK 123 570 8640
    ## 1490  781            AMERICAN          LOS ANGELES 665 803 2453
    ## 1491 1699      AIR FRANCE/KLM      PARIS-DE GAULLE 639 132 6386
    ## 1492 2970            AMERICAN            CHARLOTTE 437 886 0753
    ## 1493 3188      CATHAY PACIFIC            HONG KONG 626 756 5089
    ## 1494  629              ALASKA WASHINGTON DC-DULLES 714 950 3364
    ## 1495 1568         UNITED INTL            HONG KONG 653-786-5985
    ## 1496 2966            AMERICAN            CHARLOTTE 518 286 5956
    ## 1497 3053 PHILIPPINE AIRLINES               MANILA 194 960 2145
    ## 1498 2270                 WOW            REYKJAVIK 362-136-1153
    ## 1499 3177               DELTA          LOS ANGELES 376-456-0697
    ## 1500 1778              UNITED       RALEIGH-DURHAM 657 832 1189
    ## 1501 2157            FRONTIER               DENVER 962-918-6117
    ## 1502 2784               DELTA              ATLANTA 692 929 3592
    ## 1503 1948              UNITED               BOSTON 805-877-3887
    ## 1504 2650             JETBLUE               BOSTON 819 732 4132
    ## 1505 2170            FRONTIER               DENVER 367 221 9710
    ## 1506 1715      AIR FRANCE/KLM      PARIS-DE GAULLE 361 154 1789
    ## 1507  719      AIR FRANCE/KLM      PARIS-DE GAULLE 680 488 1182
    ## 1508 2921    TURKISH AIRLINES             ISTANBUL 928 638 1186
    ## 1509 3103              ALASKA               NEWARK 588-693-9875
    ## 1510  839              ALASKA              SEATTLE 681-308-7915
    ## 1511 3007            AMERICAN                MIAMI 783 647 8490
    ## 1512  250              UNITED            NASHVILLE 897 847 0632
    ## 1513 1774            AMERICAN       CHICAGO-O'HARE 150 952 4453
    ## 1514  408              UNITED              PHOENIX 322-884-3020
    ## 1515 1630          KOREAN AIR                SEOUL 176-313-5403
    ## 1516 2350               DELTA MINNEAPOLIS-ST. PAUL 487 109 4196
    ## 1517 1292             JETBLUE         NEW YORK-JFK 544 382 8289
    ## 1518 2378             JETBLUE       FT. LAUDERDALE 781 543 7456
    ## 1519 1253       CHINA EASTERN             SHANGHAI 911 829 6476
    ## 1520 1721          AIR CANADA              TORONTO 517 986 3426
    ## 1521 2087             JETBLUE           LONG BEACH 838 220 5397
    ## 1522  440              UNITED         INDIANAPOLIS 179 163 0902
    ## 1523 2119         UNITED INTL          MEXICO CITY 539 137 8983
    ## 1524 1716      AIR FRANCE/KLM      PARIS-DE GAULLE 733-154-0094
    ## 1525 3215              UNITED        SANTA BARBARA 639 881 3693
    ## 1526 1531              ALASKA          LOS ANGELES 291-830-3017
    ## 1527 3203              UNITED      ONTARIO (CALIF) 750 520 0167
    ## 1528  487              UNITED               DENVER 676 485 8963
    ## 1529 1788              UNITED       RALEIGH-DURHAM 135 566 5090
    ## 1530 1687      CATHAY PACIFIC            HONG KONG 337 260 4996
    ## 1531 1129           SOUTHWEST            SAN DIEGO 371 185 2377
    ## 1532 2618              UNITED       CHICAGO-O'HARE 280 461 1386
    ## 1533  782         UNITED INTL    SAN JOSE DEL CABO 496-429-1314
    ## 1534 3261              UNITED            SAN DIEGO 459 671 4698
    ## 1535 1425              UNITED            LAS VEGAS 486-268-3312
    ## 1536 1828         UNITED INTL      LONDON HEATHROW 497-518-4050
    ## 1537 2665              UNITED       RALEIGH-DURHAM 859 495 4050
    ## 1538 1409              UNITED          KANSAS CITY 724-134-3870
    ## 1539  883              ALASKA              SEATTLE 125-578-4253
    ## 1540 2060            AMERICAN         PHILADELPHIA 614 800 2861
    ## 1541 2090             JETBLUE           LONG BEACH 487-232-4449
    ## 1542 1154           SOUTHWEST          LOS ANGELES 253-374-7102
    ## 1543 3179               DELTA          LOS ANGELES 802 102 8345
    ## 1544 1542               DELTA         NEW YORK-JFK 417 393 0050
    ## 1545  853            AMERICAN     DALLAS-FT. WORTH 787-624-8443
    ## 1546  877              ALASKA              SEATTLE 341 824 5322
    ## 1547 2856              UNITED              SEATTLE 415 551 1608
    ## 1548 2081             JETBLUE           LONG BEACH 473-238-3324
    ## 1549  590              ALASKA WASHINGTON DC-DULLES 876-834-0624
    ## 1550 3056            AMERICAN                MIAMI 919 611 6170
    ## 1551 3153              UNITED            SAN DIEGO 261 434 7760
    ## 1552  396              ALASKA         INDIANAPOLIS 617 310 2684
    ## 1553 2656              UNITED       RALEIGH-DURHAM 182-535-3412
    ## 1554  460              UNITED     DALLAS-FT. WORTH 506 129 1694
    ## 1555 3223            AMERICAN          LOS ANGELES 313 990 8823
    ## 1556 3130               DELTA       SALT LAKE CITY 656-941-5355
    ## 1557  637              ALASKA          NEW ORLEANS 955 324 5981
    ## 1558 2826              UNITED              BURBANK 175 808 2189
    ## 1559 2947           SOUTHWEST          LOS ANGELES 105-687-6500
    ## 1560 3160               DELTA          LOS ANGELES 757 524 2964
    ## 1561 2865              UNITED WASHINGTON DC-DULLES 201 374 2424
    ## 1562 2815               DELTA          LOS ANGELES 766 112 6143
    ## 1563 2955           SOUTHWEST          LOS ANGELES 783-463-4865
    ## 1564 1593           SOUTHWEST          LOS ANGELES 853-803-9900
    ## 1565 2373             JETBLUE       FT. LAUDERDALE 992 114 6973
    ## 1566 2823              UNITED              BURBANK 316-212-7309
    ## 1567 3034            AMERICAN     DALLAS-FT. WORTH 301 672 1092
    ## 1568 1989              UNITED            BALTIMORE 795 137 0201
    ## 1569 3271              UNITED            LAS VEGAS 381-883-5497
    ## 1570 2770              UNITED               AUSTIN 100-531-4642
    ## 1571 1344         UNITED INTL             SHANGHAI 994 923 6634
    ## 1572  309              UNITED            SAN DIEGO 920 355 8404
    ## 1573  312              UNITED            SAN DIEGO 325 795 2455
    ## 1574 2167            FRONTIER               DENVER 593 829 6250
    ## 1575  447              UNITED         INDIANAPOLIS 566-482-9004
    ## 1576 2854              UNITED              SEATTLE 542 537 6770
    ## 1577  676              UNITED               NEWARK 716 191 1741
    ## 1578 2901               DELTA       SALT LAKE CITY 491-727-7162
    ## 1579 1450              UNITED            LAS VEGAS 167-336-5660
    ## 1580 2335            AMERICAN       CHICAGO-O'HARE 358 831 0725
    ## 1581 2475              UNITED               DENVER 432 979 7292
    ## 1582 2822               DELTA          LOS ANGELES 728 662 3934
    ## 1583 1894           LUFTHANSA               MUNICH 380-918-8572
    ## 1584 1781              UNITED       RALEIGH-DURHAM 151 434 6989
    ## 1585 2750               DELTA              ATLANTA 755 544 2629
    ## 1586 2833              UNITED              BURBANK 633 181 4494
    ## 1587  273              UNITED          LOS ANGELES 346 706 5964
    ## 1588 2828              UNITED              BURBANK 688 690 2184
    ## 1589  329           SOUTHWEST              PHOENIX 618 717 1697
    ## 1590 1061          AIR CANADA              CALGARY 185-321-6877
    ## 1591 2261              UNITED          LOS ANGELES 726 943 7486
    ## 1592 1151           SOUTHWEST          LOS ANGELES 670-248-0186
    ## 1593  543            EMIRATES                DUBAI 364 834 3150
    ## 1594  405              UNITED         HOUSTON-BUSH 176 508 2778
    ## 1595  634              ALASKA               NEWARK 120 941 0833
    ## 1596 1174               DELTA         NEW YORK-JFK 670 902 3199
    ## 1597 2709              UNITED        SANTA BARBARA 297 484 3285
    ## 1598 1544               DELTA         NEW YORK-JFK 743-103-7645
    ## 1599 2056              UNITED               NEWARK 432-281-3682
    ## 1600 2459              UNITED               DENVER 648 685 6188
    ## 1601 2467              UNITED               DENVER 548 191 4898
    ## 1602 3026              UNITED             PORTLAND 288 110 9483
    ## 1603 3247            AMERICAN              PHOENIX 946-558-5801
    ## 1604 3249            AMERICAN              PHOENIX 388 744 9637
    ## 1605  364           LUFTHANSA            FRANKFURT 506 463 9129
    ## 1606 1385         UNITED INTL             SHANGHAI 306 394 8640
    ## 1607  759          AIR CANADA              TORONTO 170-641-3537
    ## 1608 1879              UNITED               BOSTON 860 723 5066
    ## 1609  597              ALASKA WASHINGTON DC-DULLES 814-895-6610
    ## 1610  583              ALASKA              SEATTLE 598 735 8557
    ## 1611  994           LUFTHANSA               MUNICH 593 895 6761
    ## 1612 2989            AMERICAN                MIAMI 508 484 9738
    ## 1613 1474              ALASKA         NEW YORK-JFK 719 489 4724
    ## 1614 2917               DELTA              SEATTLE 503-671-4901
    ## 1615 1970      CATHAY PACIFIC            HONG KONG 275 649 8183
    ## 1616  553              ALASKA              SEATTLE 968 130 7012
    ## 1617 1627          KOREAN AIR                SEOUL 290 367 6676
    ## 1618 1650     VIRGIN ATLANTIC      LONDON HEATHROW 499 766 9941
    ## 1619 3006            AMERICAN                MIAMI 538-393-2243
    ## 1620  989           LUFTHANSA               MUNICH 845 544 4748
    ## 1621 1357         UNITED INTL             SHANGHAI 784-458-8425
    ## 1622  755          AIR CANADA              TORONTO 708 500 2758
    ## 1623  349              ALASKA            SAN DIEGO 982 555 9504
    ## 1624 2174            FRONTIER               DENVER 477-307-3338
    ## 1625 3050            AMERICAN          LOS ANGELES 744-301-1148
    ## 1626 2577            INTERJET          GUADALAJARA 389 484 8888
    ## 1627 1745          AIR CANADA              TORONTO 739 303 6128
    ## 1628 2103             JETBLUE           LONG BEACH 175 905 9962
    ## 1629  379              ALASKA    SAN JOSE DEL CABO 878-636-2294
    ## 1630 2279              UNITED          LOS ANGELES 994 421 8642
    ## 1631 2637              UNITED       CHICAGO-O'HARE 304-225-5895
    ## 1632 2779                 WOW            REYKJAVIK 931-522-5498
    ## 1633 3134              ALASKA               NEWARK 838 898 2275
    ## 1634 2223          AER LINGUS               DUBLIN 718 187 7125
    ## 1635 2382         UNITED INTL              BEIJING 943 561 8955
    ## 1636 1059          AIR CANADA              CALGARY 341 261 6456
    ## 1637 1066          AIR CANADA              CALGARY 826 266 0205
    ## 1638 2273                 WOW            REYKJAVIK 589-975-0198
    ## 1639 2886              UNITED               AUSTIN 657 680 8781
    ## 1640 3117              UNITED              SPOKANE 501-668-7869
    ## 1641 2414     AIR NEW ZEALAND             AUCKLAND 224 365 8299
    ## 1642  534            EMIRATES                DUBAI 747 588 1968
    ## 1643  948          AIR CANADA            VANCOUVER 748 446 1257
    ## 1644 2484              UNITED       CHICAGO-O'HARE 525-552-4162
    ## 1645 1446              UNITED            LAS VEGAS 635-714-8302
    ## 1646 2008              UNITED               NEWARK 114 668 2834
    ## 1647 2729      AIR FRANCE/KLM      PARIS-DE GAULLE 404 788 2855
    ## 1648  833              ALASKA            NASHVILLE 673 292 2444
    ## 1649 2072             JETBLUE           LONG BEACH 136 788 1426
    ## 1650 2396         UNITED INTL               SYDNEY 564 677 3934
    ## 1651 1397         UNITED INTL      PARIS-DE GAULLE 213 981 7762
    ## 1652 1991              UNITED            BALTIMORE 453-556-0852
    ## 1653 3205      CATHAY PACIFIC            HONG KONG 423 593 4483
    ## 1654 2078            AMERICAN         PHILADELPHIA 820 609 7454
    ## 1655 1324         UNITED INTL      LONDON HEATHROW 131-641-1331
    ## 1656 2933    TURKISH AIRLINES             ISTANBUL 824 540 9579
    ## 1657 3227            AMERICAN              PHOENIX 739 737 9041
    ## 1658  640              ALASKA          NEW ORLEANS 609-332-7370
    ## 1659 2197          AER LINGUS               DUBLIN 426-182-1365
    ## 1660  864            AMERICAN     DALLAS-FT. WORTH 898-210-6218
    ## 1661  324           SOUTHWEST          LOS ANGELES 938 394 0411
    ## 1662  730      AIR FRANCE/KLM      PARIS-DE GAULLE 904 844 1759
    ## 1663 1863              ALASKA              KAHULUI 658-861-4306
    ## 1664  896              ALASKA       RALEIGH-DURHAM 380-105-1757
    ## 1665 2094            AMERICAN         PHILADELPHIA 249 452 4370
    ## 1666 2910    TURKISH AIRLINES             ISTANBUL 443 384 8253
    ## 1667 1882  SINGAPORE AIRLINES            SINGAPORE 711 289 2247
    ## 1668 1023          AIR CANADA            VANCOUVER 343-973-0193
    ## 1669  336              ALASKA            SAN DIEGO 367 650 3720
    ## 1670 2164            FRONTIER               DENVER 148 630 8560
    ## 1671 3186               DELTA          LOS ANGELES 413-727-2672
    ## 1672  311              UNITED            SAN DIEGO 148 501 5084
    ## 1673 1119           SOUTHWEST            SAN DIEGO 564 780 8272
    ## 1674 1834              ALASKA       RALEIGH-DURHAM 524 190 0899
    ## 1675  897              ALASKA       RALEIGH-DURHAM 463 792 2782
    ## 1676 1449              UNITED            LAS VEGAS 522-286-5318
    ## 1677 1694              ALASKA            SANTA ANA 310 719 4550
    ## 1678 2710      AIR FRANCE/KLM      PARIS-DE GAULLE 314-360-4288
    ## 1679 1955      AIR FRANCE/KLM      PARIS-DE GAULLE 822 271 5719
    ## 1680 1953      AIR FRANCE/KLM      PARIS-DE GAULLE 465-550-6610
    ## 1681 1820         UNITED INTL      LONDON HEATHROW 388 100 1482
    ## 1682 2527             JETBLUE               BOSTON 982 842 4913
    ## 1683 2681           SOUTHWEST               DENVER 671 913 4563
    ## 1684 1621     VIRGIN ATLANTIC      LONDON HEATHROW 144 468 5864
    ## 1685 1749           LUFTHANSA               MUNICH 194 344 4039
    ## 1686 1755           LUFTHANSA               MUNICH 331 747 5714
    ## 1687 2648             JETBLUE               BOSTON 221-190-1449
    ## 1688 2706      AIR FRANCE/KLM      PARIS-DE GAULLE 676-614-9095
    ## 1689  269          AIR CANADA              TORONTO 956 257 9319
    ## 1690  321           SOUTHWEST          LOS ANGELES 362 145 8268
    ## 1691 2890                 WOW            REYKJAVIK 570 727 3998
    ## 1692 1513               DELTA          LOS ANGELES 225 964 9193
    ## 1693 1682      CATHAY PACIFIC            HONG KONG 227 419 9482
    ## 1694 2609             JETBLUE               BOSTON 949 360 7605
    ## 1695 1484            FRONTIER               DENVER 794 939 9735
    ## 1696 1598  SINGAPORE AIRLINES            SINGAPORE 413 754 3034
    ## 1697 1188           SOUTHWEST            LAS VEGAS 806 730 0459
    ## 1698 1971              UNITED            BALTIMORE 791 195 8909
    ## 1699 1236           SOUTHWEST            LAS VEGAS 691-318-3535
    ## 1700 1615     VIRGIN ATLANTIC      LONDON HEATHROW 852-386-6029
    ## 1701 2371             JETBLUE       FT. LAUDERDALE 380 682 7795
    ## 1702  595              ALASKA WASHINGTON DC-DULLES 247 586 4579
    ## 1703  572              ALASKA              SEATTLE 235 257 1041
    ## 1704 1591         UNITED INTL            HONG KONG 106 756 2785
    ## 1705 1931      AIR FRANCE/KLM      PARIS-DE GAULLE 729-102-7511
    ## 1706  496              ALASKA             PORTLAND 795 583 0958
    ## 1707  951          AIR CANADA            VANCOUVER 700-431-3918
    ## 1708  720      CATHAY PACIFIC            HONG KONG 929 632 1068
    ## 1709 1945              UNITED               BOSTON 763 906 2495
    ## 1710  410              UNITED         HOUSTON-BUSH 469 976 6796
    ## 1711  638              ALASKA WASHINGTON DC-DULLES 887-657-4143
    ## 1712  197            AMERICAN          LOS ANGELES 574-438-5329
    ## 1713 2606             JETBLUE               BOSTON 319 127 9518
    ## 1714 2727              UNITED              KAHULUI 429 960 9710
    ## 1715 2866              UNITED WASHINGTON DC-DULLES 419 646 0299
    ## 1716  574              ALASKA              SEATTLE 521 336 8581
    ## 1717  630              ALASKA               NEWARK 776 367 6109
    ## 1718 1150           SOUTHWEST            SAN DIEGO 470 367 1392
    ## 1719 1232           SOUTHWEST            LAS VEGAS 998-931-4783
    ## 1720 1260       CHINA EASTERN             SHANGHAI 362-178-6307
    ## 1721 1444              UNITED            LAS VEGAS 212 286 7936
    ## 1722 1780              UNITED       RALEIGH-DURHAM 936 193 9690
    ## 1723 2054              UNITED               NEWARK 797-870-7818
    ## 1724 2125         UNITED INTL          MEXICO CITY 693 907 5353
    ## 1725 2583             JETBLUE           LONG BEACH 332 973 4943
    ## 1726 2601             JETBLUE               BOSTON 929 622 9077
    ## 1727 2642             JETBLUE         NEW YORK-JFK 649-379-5361
    ## 1728 2960            AMERICAN         NEW YORK-JFK 395-892-5646
    ## 1729 2997            AMERICAN                MIAMI 221-628-9561
    ## 1730 3049            AMERICAN          LOS ANGELES 549-649-1864
    ## 1731 1724          AIR CANADA              TORONTO 342 941 0439
    ## 1732  831              ALASKA            NASHVILLE 701 390 9814
    ## 1733 2855              UNITED              SEATTLE 919-342-0230
    ## 1734 1211           SOUTHWEST            LAS VEGAS 364-759-2705
    ## 1735 3054            AMERICAN                MIAMI 949 543 7906
    ## 1736 1227           SOUTHWEST            LAS VEGAS 942 732 6403
    ## 1737 2443             JETBLUE               BOSTON 308-607-9855
    ## 1738 1754           LUFTHANSA               MUNICH 764 645 5740
    ## 1739  345              ALASKA            SAN DIEGO 472-337-8838
    ## 1740  552              ALASKA              SEATTLE 791 847 7278
    ## 1741 1681      CATHAY PACIFIC            HONG KONG 128 805 3828
    ## 1742 3029            AMERICAN     DALLAS-FT. WORTH 365-832-0674
    ## 1743 1435              UNITED             PORTLAND 123-282-3494
    ## 1744  746              ALASKA         NEW YORK-JFK 285 424 4318
    ## 1745 1824         UNITED INTL      LONDON HEATHROW 452 352 1387
    ## 1746 2477              UNITED             HONOLULU 129-377-8159
    ## 1747 2807               DELTA          LOS ANGELES 222 143 3131
    ## 1748 1616     VIRGIN ATLANTIC      LONDON HEATHROW 162-451-0594
    ## 1749  563              ALASKA              SEATTLE 436-422-6171
    ## 1750 2575              UNITED       CHICAGO-O'HARE 605 284 4260
    ## 1751  457              ALASKA            LAS VEGAS 929-102-5905
    ## 1752 2741              UNITED              KAHULUI 847 507 8268
    ## 1753  972           LUFTHANSA               MUNICH 452-811-8088
    ## 1754 2446              UNITED             HONOLULU 799 143 1677
    ## 1755  747              ALASKA         NEW YORK-JFK 196 756 4555
    ## 1756  354           LUFTHANSA            FRANKFURT 119-444-0817
    ## 1757  678      AIR FRANCE/KLM      PARIS-DE GAULLE 885 454 0883
    ## 1758 1301             JETBLUE         NEW YORK-JFK 945-998-0444
    ## 1759 1622     VIRGIN ATLANTIC      LONDON HEATHROW 163 241 9321
    ## 1760 1878  SINGAPORE AIRLINES            SINGAPORE 594-176-5811
    ## 1761 3176               DELTA          LOS ANGELES 332 963 4103
    ## 1762  386              ALASKA          LOS ANGELES 389 318 3975
    ## 1763  398              ALASKA         INDIANAPOLIS 894-593-7953
    ## 1764  675              UNITED               NEWARK 561-266-7842
    ## 1765  944          AIR CANADA            VANCOUVER 354-958-8052
    ## 1766 1284         UNITED INTL      LONDON HEATHROW 151 921 2775
    ## 1767 1307         UNITED INTL      LONDON HEATHROW 901 140 3759
    ## 1768 1466              ALASKA         NEW YORK-JFK 333-520-4811
    ## 1769 1654            EMIRATES                DUBAI 850 914 9348
    ## 1770 1683      CATHAY PACIFIC            HONG KONG 246 272 9019
    ## 1771 1806              ALASKA       RALEIGH-DURHAM 504 419 9191
    ## 1772 1954      AIR FRANCE/KLM      PARIS-DE GAULLE 231 863 7554
    ## 1773 2275                 WOW            REYKJAVIK 939 253 9048
    ## 1774 2397         UNITED INTL              BEIJING 879-154-4494
    ## 1775 2498              UNITED               NEWARK 577 786 2546
    ## 1776 2607            INTERJET          GUADALAJARA 841-717-4447
    ## 1777 2793               DELTA       SALT LAKE CITY 397-353-6309
    ## 1778 2814               DELTA          LOS ANGELES 558 191 4548
    ## 1779 2897               DELTA       SALT LAKE CITY 905 768 2297
    ## 1780 2996            AMERICAN                MIAMI 348 522 2051
    ## 1781 3111                COPA          PANAMA CITY 307-323-6861
    ## 1782 3145               DELTA       SALT LAKE CITY 663 886 2487
    ## 1783 3228              ALASKA             PORTLAND 274 944 6097
    ## 1784 2610            INTERJET          GUADALAJARA 328 307 0875
    ## 1785 2126         UNITED INTL          MEXICO CITY 673 524 3504
    ## 1786  540              ALASKA      PUERTO VALLARTA 934 721 0615
    ## 1787 1901           LUFTHANSA               MUNICH 102-957-6486
    ## 1788  828              ALASKA            NASHVILLE 794 925 8846
    ## 1789 2417     AIR NEW ZEALAND             AUCKLAND 637 281 4111
    ## 1790 3135              UNITED            SAN DIEGO 335 802 2651
    ## 1791  421              UNITED             HONOLULU 560 699 9908
    ## 1792  772         UNITED INTL    SAN JOSE DEL CABO 451 163 0102
    ## 1793  442              UNITED             HONOLULU 613 800 0835
    ## 1794  760              ALASKA         NEW YORK-JFK 370 453 5800
    ## 1795 1354         UNITED INTL               KANSAI 192 507 5411
    ## 1796 2435              UNITED             HONOLULU 182-227-4838
    ## 1797 2259              UNITED          LOS ANGELES 647 126 2332
    ## 1798  393              ALASKA         INDIANAPOLIS 606 125 6957
    ## 1799 1419              UNITED          KANSAS CITY 728-404-5558
    ## 1800 2398         UNITED INTL               SYDNEY 506 812 6052
    ## 1801 3233              ALASKA             PORTLAND 427-665-3475
    ## 1802 2043              UNITED               NEWARK 808 739 7162
    ## 1803 2438              UNITED             HONOLULU 375-978-3305
    ## 1804 1338         UNITED INTL               KANSAI 181-708-2089
    ## 1805 2107         UNITED INTL          MEXICO CITY 242-540-4234
    ## 1806 2753               DELTA              ATLANTA 105 116 9695
    ## 1807 2730              UNITED              KAHULUI 945 144 7892
    ## 1808 2744              UNITED              KAHULUI 531 895 6695
    ## 1809  495              ALASKA             PORTLAND 894 810 2674
    ## 1810 2433              UNITED             HONOLULU 943 812 6349
    ## 1811 2678              UNITED              ORLANDO 476 168 4235
    ## 1812 2714              UNITED              KAHULUI 931 385 6757
    ## 1813  449              UNITED         INDIANAPOLIS 100 378 8095
    ## 1814  596             JETBLUE               BOSTON 459 572 0244
    ## 1815 1117               DELTA MINNEAPOLIS-ST. PAUL 397-362-5469
    ## 1816 2384         UNITED INTL              BEIJING 938 982 5585
    ## 1817 1832              ALASKA       RALEIGH-DURHAM 769-472-2992
    ## 1818 1299         UNITED INTL      LONDON HEATHROW 190 204 1154
    ## 1819 3014              UNITED             PORTLAND 321 616 1013
    ## 1820  436              UNITED         INDIANAPOLIS 178-232-0815
    ## 1821 3292           SOUTHWEST               DENVER 828 549 6666
    ## 1822 2549             JETBLUE           LONG BEACH 280 544 4554
    ## 1823 1349         UNITED INTL               KANSAI 183 208 5054
    ## 1824 2759      AIR FRANCE/KLM      PARIS-DE GAULLE 203 448 1522
    ## 1825 3123              ALASKA               NEWARK 900-871-9056
    ## 1826 2547             JETBLUE           LONG BEACH 406-167-1379
    ## 1827 1177               DELTA         NEW YORK-JFK 641-635-8466
    ## 1828 1805         UNITED INTL      LONDON HEATHROW 807-671-6158
    ## 1829 1215           SOUTHWEST            LAS VEGAS 768 529 8051
    ## 1830 1092               DELTA MINNEAPOLIS-ST. PAUL 220 660 0306
    ## 1831  335           SOUTHWEST              PHOENIX 928-179-7556
    ## 1832 1469              ALASKA         NEW YORK-JFK 153 756 0278
    ## 1833 2745      AIR FRANCE/KLM      PARIS-DE GAULLE 273 829 9197
    ## 1834 3224              ALASKA             PORTLAND 269 463 0911
    ## 1835 2004              UNITED               NEWARK 696 984 8826
    ## 1836  943          AIR CANADA            VANCOUVER 905-903-5258
    ## 1837 1573               DELTA              DETROIT 267 332 4709
    ## 1838 1305         UNITED INTL      LONDON HEATHROW 534 216 6666
    ## 1839 2973            AMERICAN            CHARLOTTE 971 175 2968
    ## 1840 1160           SOUTHWEST          LOS ANGELES 274-863-3205
    ## 1841 3063 PHILIPPINE AIRLINES               MANILA 928 445 5474
    ## 1842 3243            AMERICAN              PHOENIX 858 990 5153
    ## 1843   10           SOUTHWEST          LOS ANGELES 731-813-2043
    ## 1844  513              ALASKA             PORTLAND 563-732-6802
    ## 1845  610             JETBLUE               BOSTON 145 725 4021
    ## 1846 1017          AIR CANADA            VANCOUVER 931 311 5801
    ## 1847 2233          AER LINGUS               DUBLIN 172 990 3485
    ## 1848  391              ALASKA         INDIANAPOLIS 872 325 4341
    ## 1849 2394         UNITED INTL              BEIJING 152 790 8238
    ## 1850  560            EMIRATES                DUBAI 330 561 9257
    ## 1851 1977      CATHAY PACIFIC            HONG KONG 437 420 7546
    ## 1852 1200           SOUTHWEST            LAS VEGAS 495 632 4027
    ## 1853 2316              UNITED          SAN ANTONIO 311-305-4367
    ## 1854 2692      AIR FRANCE/KLM      PARIS-DE GAULLE 817-400-0481
    ## 1855 3048 PHILIPPINE AIRLINES               MANILA 430 723 1079
    ## 1856  420              UNITED         INDIANAPOLIS 226 490 8696
    ## 1857  652              ALASKA WASHINGTON DC-DULLES 123 570 8640
    ## 1858 1490            FRONTIER               DENVER 665 803 2453
    ## 1859 3136              ALASKA               NEWARK 639 132 6386
    ## 1860  256          AIR CANADA              TORONTO 437 886 0753
    ## 1861 1479             AVIANCA         SAN SALVADOR 626 756 5089
    ## 1862 1448              UNITED             PORTLAND 714 950 3364
    ## 1863 1784              UNITED       RALEIGH-DURHAM 653-786-5985
    ## 1864 1481             AVIANCA         SAN SALVADOR 518 286 5956
    ## 1865  795              ALASKA            BALTIMORE 194 960 2145
    ## 1866 1148           SOUTHWEST          LOS ANGELES 362-136-1153
    ## 1867 3037            AMERICAN     DALLAS-FT. WORTH 376-456-0697
    ## 1868 1477             AVIANCA         SAN SALVADOR 657 832 1189
    ## 1869 3278           SOUTHWEST               DENVER 962-918-6117
    ## 1870  721      CATHAY PACIFIC            HONG KONG 692 929 3592
    ## 1871 2147      CATHAY PACIFIC            HONG KONG 805-877-3887
    ## 1872 3175            AMERICAN     DALLAS-FT. WORTH 819 732 4132
    ## 1873 1792              UNITED       RALEIGH-DURHAM 367 221 9710
    ## 1874 2339               DELTA MINNEAPOLIS-ST. PAUL 361 154 1789
    ## 1875  803              ALASKA            BALTIMORE 680 488 1182
    ## 1876  876            AMERICAN     DALLAS-FT. WORTH 928 638 1186
    ## 1877 1110           SOUTHWEST            SAN DIEGO 588-693-9875
    ## 1878 1113               DELTA MINNEAPOLIS-ST. PAUL 681-308-7915
    ## 1879 1180           SOUTHWEST            LAS VEGAS 783 647 8490
    ## 1880 1499            FRONTIER               DENVER 897 847 0632
    ## 1881 2154          AIR CANADA            VANCOUVER 150 952 4453
    ## 1882 2362             JETBLUE       FT. LAUDERDALE 322-884-3020
    ## 1883 2509              ALASKA       SALT LAKE CITY 176-313-5403
    ## 1884 2623            INTERJET          GUADALAJARA 487 109 4196
    ## 1885 2151      CATHAY PACIFIC            HONG KONG 544 382 8289
    ## 1886 2188              QANTAS               SYDNEY 781 543 7456
    ## 1887 3174               DELTA          LOS ANGELES 911 829 6476
    ## 1888 1883              UNITED               BOSTON 517 986 3426
    ## 1889 3107              UNITED              SPOKANE 838 220 5397
    ## 1890 1859              ALASKA              KAHULUI 179 163 0902
    ## 1891 2345               DELTA MINNEAPOLIS-ST. PAUL 539 137 8983
    ## 1892 1669            EMIRATES                DUBAI 733-154-0094
    ## 1893 3246            AMERICAN              PHOENIX 639 881 3693
    ## 1894 1874            AMERICAN       CHICAGO-O'HARE 291-830-3017
    ## 1895  600              ALASKA WASHINGTON DC-DULLES 750 520 0167
    ## 1896 2311              QANTAS               SYDNEY 676 485 8963
    ## 1897 3119                COPA          PANAMA CITY 135 566 5090
    ## 1898 3079                COPA          PANAMA CITY 337 260 4996
    ## 1899  814              ALASKA            BALTIMORE 371 185 2377
    ## 1900    7           SOUTHWEST          LOS ANGELES 280 461 1386
    ## 1901  377              ALASKA          LOS ANGELES 496-429-1314
    ## 1902  485              UNITED               DENVER 459 671 4698
    ## 1903  765          AIR CANADA              TORONTO 486-268-3312
    ## 1904 1761           LUFTHANSA               MUNICH 497-518-4050
    ## 1905 1966              UNITED            BALTIMORE 859 495 4050
    ## 1906 2554             JETBLUE           LONG BEACH 724-134-3870
    ## 1907 2976            AMERICAN            CHARLOTTE 125-578-4253
    ## 1908 3244            AMERICAN              PHOENIX 614 800 2861
    ## 1909 2367             JETBLUE       FT. LAUDERDALE 487-232-4449
    ## 1910 1549           AIR CHINA              BEIJING 253-374-7102
    ## 1911 2620             JETBLUE               BOSTON 802 102 8345
    ## 1912 2343              UNITED               AUSTIN 417 393 0050
    ## 1913 2645              UNITED       CHICAGO-O'HARE 787-624-8443
    ## 1914 1249         UNITED INTL         TOKYO-NARITA 341 824 5322
    ## 1915  260          AIR CANADA              TORONTO 415 551 1608
    ## 1916 3260              UNITED            SAN DIEGO 473-238-3324
    ## 1917 3115              UNITED              SPOKANE 876-834-0624
    ## 1918 2907               DELTA              SEATTLE 919 611 6170
    ## 1919 1769            AMERICAN       CHICAGO-O'HARE 261 434 7760
    ## 1920  796            AMERICAN          LOS ANGELES 617 310 2684
    ## 1921  249              UNITED            NASHVILLE 182-535-3412
    ## 1922  282              UNITED            BALTIMORE 506 129 1694
    ## 1923 2728               DELTA          LOS ANGELES 313 990 8823
    ## 1924 1766            AMERICAN       CHICAGO-O'HARE 656-941-5355
    ## 1925  712      CATHAY PACIFIC            HONG KONG 955 324 5981
    ## 1926 2666             JETBLUE               BOSTON 175 808 2189
    ## 1927 1169           SOUTHWEST          LOS ANGELES 105-687-6500
    ## 1928 1680      CATHAY PACIFIC            HONG KONG 757 524 2964
    ## 1929 2701              UNITED       CHICAGO-O'HARE 201 374 2424
    ## 1930  394              ALASKA          LOS ANGELES 766 112 6143
    ## 1931  628              ALASKA WASHINGTON DC-DULLES 783-463-4865
    ## 1932 1685      CATHAY PACIFIC            HONG KONG 853-803-9900
    ## 1933 2148            FRONTIER               DENVER 992 114 6973
    ## 1934  900              ALASKA       RALEIGH-DURHAM 316-212-7309
    ## 1935  903              ALASKA       RALEIGH-DURHAM 301 672 1092
    ## 1936  588             JETBLUE               BOSTON 795 137 0201
    ## 1937  369              ALASKA    SAN JOSE DEL CABO 381-883-5497
    ## 1938 2565            INTERJET          GUADALAJARA 100-531-4642
    ## 1939 1983              UNITED            BALTIMORE 994 923 6634
    ## 1940 3025              UNITED             PORTLAND 920 355 8404
    ## 1941  689      CATHAY PACIFIC            HONG KONG 325 795 2455
    ## 1942 2837              UNITED       CHICAGO-O'HARE 593 829 6250
    ## 1943 2857              UNITED              SEATTLE 566-482-9004
    ## 1944  208            AMERICAN          LOS ANGELES 542 537 6770
    ## 1945 1601  SINGAPORE AIRLINES            SINGAPORE 716 191 1741
    ## 1946 2582              UNITED       CHICAGO-O'HARE 491-727-7162
    ## 1947 2720      AIR FRANCE/KLM      PARIS-DE GAULLE 167-336-5660
    ## 1948  399              ALASKA          LOS ANGELES 358 831 0725
    ## 1949 2452              UNITED               DENVER 432 979 7292
    ## 1950 1692              ALASKA            SANTA ANA 728 662 3934
    ## 1951  571            EMIRATES                DUBAI 380-918-8572
    ## 1952 2963            AMERICAN            CHARLOTTE 151 434 6989
    ## 1953 2912               DELTA              SEATTLE 755 544 2629
    ## 1954 1655            EMIRATES                DUBAI 633 181 4494
    ## 1955 1700      AIR FRANCE/KLM      PARIS-DE GAULLE 346 706 5964
    ## 1956  870              ALASKA              SEATTLE 688 690 2184
    ## 1957  945          AIR CANADA              TORONTO 618 717 1697
    ## 1958 2586              UNITED       CHICAGO-O'HARE 185-321-6877
    ## 1959  299           SOUTHWEST          LOS ANGELES 726 943 7486
    ## 1960 1613     VIRGIN ATLANTIC      LONDON HEATHROW 670-248-0186
    ## 1961  313              UNITED WASHINGTON DC-DULLES 364 834 3150
    ## 1962  821              ALASKA              SEATTLE 176 508 2778
    ## 1963  860              ALASKA             PORTLAND 120 941 0833
    ## 1964 1251       CHINA EASTERN             SHANGHAI 670 902 3199
    ## 1965 1656            EMIRATES                DUBAI 297 484 3285
    ## 1966 2129         UNITED INTL          MEXICO CITY 743-103-7645
    ## 1967  686      CATHAY PACIFIC            HONG KONG 432-281-3682
    ## 1968 3217              ALASKA             PORTLAND 648 685 6188
    ## 1969  653         UNITED INTL               MUNICH 548 191 4898
    ## 1970 2626              UNITED       CHICAGO-O'HARE 288 110 9483
    ## 1971 1962              UNITED               BOSTON 946-558-5801
    ## 1972 2723      AIR FRANCE/KLM      PARIS-DE GAULLE 388 744 9637
    ## 1973 2754      AIR FRANCE/KLM      PARIS-DE GAULLE 506 463 9129
    ## 1974 1693              ALASKA            SANTA ANA 306 394 8640
    ## 1975  368           LUFTHANSA            FRANKFURT 170-641-3537
    ## 1976 2401         UNITED INTL               SYDNEY 860 723 5066
    ## 1977  672      AIR FRANCE/KLM      PARIS-DE GAULLE 814-895-6610
    ## 1978 3259              UNITED            SAN DIEGO 598 735 8557
    ## 1979 2048              UNITED               NEWARK 593 895 6761
    ## 1980  532            EMIRATES                DUBAI 508 484 9738
    ## 1981  862              ALASKA             PORTLAND 719 489 4724
    ## 1982 2519              UNITED       CHICAGO-O'HARE 503-671-4901
    ## 1983 2630              UNITED       CHICAGO-O'HARE 275 649 8183
    ## 1984 2436     AIR NEW ZEALAND             AUCKLAND 968 130 7012
    ## 1985 1632          KOREAN AIR                SEOUL 290 367 6676
    ## 1986  276              UNITED          LOS ANGELES 499 766 9941
    ## 1987 1158           SOUTHWEST            SAN DIEGO 538-393-2243
    ## 1988 2222           SOUTHWEST              PHOENIX 845 544 4748
    ## 1989  450              UNITED         INDIANAPOLIS 784-458-8425
    ## 1990  451              ALASKA            LAS VEGAS 708 500 2758
    ## 1991 1071          AIR CANADA              CALGARY 982 555 9504
    ## 1992 2015     BRITISH AIRWAYS      LONDON HEATHROW 477-307-3338
    ## 1993 2520              ALASKA       SALT LAKE CITY 744-301-1148
    ## 1994 2184              QANTAS               SYDNEY 389 484 8888
    ## 1995 2165            FRONTIER               DENVER 739 303 6128
    ## 1996  823              ALASKA              SEATTLE 175 905 9962
    ## 1997 3187               DELTA          LOS ANGELES 878-636-2294
    ## 1998 2717              UNITED              KAHULUI 994 421 8642
    ## 1999 1518            INTERJET          GUADALAJARA 304-225-5895
    ## 2000 3030              UNITED             PORTLAND 931-522-5498
    ## 2001 1065          AIR CANADA              CALGARY 838 898 2275
    ## 2002  757          AIR CANADA              TORONTO 718 187 7125
    ## 2003 1644     VIRGIN ATLANTIC      LONDON HEATHROW 943 561 8955
    ## 2004 2959            AMERICAN         NEW YORK-JFK 341 261 6456
    ## 2005  936          AIR CANADA              TORONTO 826 266 0205
    ## 2006 2922               DELTA              SEATTLE 589-975-0198
    ## 2007 1244       CHINA EASTERN             SHANGHAI 657 680 8781
    ## 2008 2596          AEROMEXICO          MEXICO CITY 501-668-7869
    ## 2009 1052          AIR CANADA              CALGARY 224 365 8299
    ## 2010  931              ALASKA       RALEIGH-DURHAM 747 588 1968
    ## 2011  726      AIR FRANCE/KLM      PARIS-DE GAULLE 748 446 1257
    ## 2012  435              UNITED             HONOLULU 525-552-4162
    ## 2013 1605     VIRGIN ATLANTIC      LONDON HEATHROW 635-714-8302
    ## 2014 2263                 WOW            REYKJAVIK 114 668 2834
    ## 2015  825              ALASKA              SEATTLE 404 788 2855
    ## 2016 1752           LUFTHANSA               MUNICH 673 292 2444
    ## 2017 2964            AMERICAN            CHARLOTTE 136 788 1426
    ## 2018 1534              ALASKA          LOS ANGELES 564 677 3934
    ## 2019 2480              UNITED               DENVER 213 981 7762
    ## 2020 1405              UNITED          KANSAS CITY 453-556-0852
    ## 2021 2669          AEROMEXICO          MEXICO CITY 423 593 4483
    ## 2022 1606     VIRGIN ATLANTIC      LONDON HEATHROW 820 609 7454
    ## 2023  539            EMIRATES                DUBAI 131-641-1331
    ## 2024  570              ALASKA              SEATTLE 824 540 9579
    ## 2025 1875            AMERICAN       CHICAGO-O'HARE 739 737 9041
    ## 2026 2919    TURKISH AIRLINES             ISTANBUL 609-332-7370
    ## 2027 1333         UNITED INTL               KANSAI 426-182-1365
    ## 2028 2045              UNITED               NEWARK 898-210-6218
    ## 2029 3257              UNITED            SAN DIEGO 938 394 0411
    ## 2030  609              ALASKA WASHINGTON DC-DULLES 904 844 1759
    ## 2031 1912      AIR FRANCE/KLM      PARIS-DE GAULLE 658-861-4306
    ## 2032 1978              UNITED            BALTIMORE 380-105-1757
    ## 2033 1550           AIR CHINA              BEIJING 249 452 4370
    ## 2034 2307            AMERICAN       CHICAGO-O'HARE 443 384 8253
    ## 2035 1492            FRONTIER               DENVER 711 289 2247
    ## 2036 1521               DELTA          LOS ANGELES 343-973-0193
    ## 2037 1722          AIR CANADA              TORONTO 367 650 3720
    ## 2038 2487              UNITED       CHICAGO-O'HARE 148 630 8560
    ## 2039 2902               DELTA              SEATTLE 413-727-2672
    ## 2040 1746           LUFTHANSA               MUNICH 148 501 5084
    ## 2041 3101              ALASKA               NEWARK 564 780 8272
    ## 2042 1199           SOUTHWEST            LAS VEGAS 524 190 0899
    ## 2043 2884             EVA AIR               TAIPEI 463 792 2782
    ## 2044 3005              UNITED            NASHVILLE 522-286-5318
    ## 2045 2974              UNITED          LOS ANGELES 310 719 4550
    ## 2046 3017              ALASKA      PUERTO VALLARTA 314-360-4288
    ## 2047 2986              UNITED             PORTLAND 822 271 5719
    ## 2048 3008               DELTA         NEW YORK-JFK 465-550-6610
    ## 2049 2980              UNITED            NASHVILLE 388 100 1482
    ## 2050 2995              ALASKA         NEW YORK-JFK 982 842 4913
    ## 2051 3013              UNITED               AUSTIN 671 913 4563
    ## 2052 2985              UNITED          LOS ANGELES 144 468 5864
    ## 2053 2978           SOUTHWEST              PHOENIX 194 344 4039
    ## 2054 2967              UNITED             HONOLULU 331 747 5714
    ## 2055 3015              UNITED WASHINGTON DC-DULLES 221-190-1449
    ## 2056 2991              UNITED              KAHULUI 676-614-9095
    ## 2057 2988            FRONTIER               DENVER 956 257 9319
    ## 2058 3021              ALASKA              SEATTLE 362 145 8268
    ## 2059 3002               DELTA MINNEAPOLIS-ST. PAUL 570 727 3998
    ## 2060 2982           SOUTHWEST            SANTA ANA 225 964 9193
    ## 2061 3024              ALASKA              SEATTLE 227 419 9482
    ## 2062 9001              UNITED               AUSTIN 949 360 7605
    ## 2063 9002              UNITED               NEWARK 794 939 9735
    ## 2064 9004          AIR CANADA            VANCOUVER 413 754 3034
    ## 2065 9003              ALASKA             PORTLAND 806 730 0459

### `The End`
