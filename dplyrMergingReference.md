Merging Datasets with dplyr
================

This notebook is based on the DataCamp course on [Joining Data in R with dplyr](https://www.datacamp.com/courses/joining-data-in-r-with-dplyr). I was having a difficult time retaining all of the information and I knew I would need it in the future, so I recreated the lessons in a truncated fashion to use as a reference for myself in the future. I hope it is useful for you, too, but I highly recommend you still go through the DataCamp course (and all their other courses!) if you're not familiar with the dplyr package or tidyverse.

Note: The dplyr join functions can do all of the functions of the `merge` function in base R, but it will also always preserve row order, it has more intuitive syntax, can work with tibbles and tbl references, and can be applied to databases, spark, and more. So use dplyr! See the very end of this document for the comparisons between dplyr mutating joins and `merge`.

*A quick note about the datasets*
---------------------------------

The datasets used in this resource are from a [Kaggle dataset](https://www.kaggle.com/open-powerlifting/powerlifting-database#openpowerlifting.csv). I have cleaned them up slightly in Excel prior to importing here. Notably, I am only analyzing data from the top 3 competitions (those with the most competitors) because of the large file size. This results in data in the competitors dataframe that is from the 2015-2017 USAPL Raw National Championships. I've filtered/selected out sub-dataframes for use in the examples below.

Why Powerlifting, you might ask? I love powerlifting! I've actually competed once and you can find my data in the full dataset on Kaggle, which is from the [OpenPowerlifting database](https://www.openpowerlifting.org/). I'm not that good, but I enjoy it a lot. I had an injury a while back and never really got fully back into powerlifting, but I felt this would be a fun dataset to use as an example for this resource.

``` r
library(tidyverse) #We are mainly using the dplyr package, but it's part of the tidyverse and we try out purrr later

#Import the datasets

meets_full <- read_csv("meets.csv")
meets <- read_csv("meets_top3.csv")
competitors <- read_csv("openpowerlifting_top3.csv", col_types = "icccnccnnnncn")

#Create sub-dataframes
usapl15 <- competitors %>%
  filter(MeetID == 7015) #Only competitors in the 2015 USAPL Raw National Championships

usapl16 <- competitors %>%
  filter(MeetID == 7021) #Only competitors in the 2016 USAPL Raw National Championships

usapl17 <- competitors %>%
  filter(MeetID == 7028) #Only competitors in the 2017 USAPL Raw National Championships

usapl15_names <- usapl15 %>%
  select(Name, Sex) %>%
  distinct() #Only competitors in the 2015 USAPL Raw National Championships; just names/sex

usapl17_names <- usapl17 %>%
  select(Name, Sex) %>%
  distinct() #Only competitors in the 2017 USAPL Raw National Championships; just names/sex

females <- competitors %>%
  filter(Sex == "Female") #All female competitors

males <- competitors %>%
  filter(Sex == "Male") #All male competitors

peopledata <- competitors %>% #Just the first seven columns
  select(MeetID, Name, Sex, Division, BodyweightKg, WeightClassKg, WeightClass)

liftingdata <- competitors %>% #Just the last six columns
  select(BestSquatKg, BestBenchKg, BestDeadliftKg, TotalKg, Place, Wilks)

competitorsdistinct <- competitors %>%
  distinct(MeetID,Name, .keep_all = TRUE) #Just getting rows for unique people since there are duplicates

squat <- competitorsdistinct %>%
  select(MeetID, Name, BestSquatKg) #Just squat data for all competitors

bench <- competitorsdistinct %>%
  select(MeetID, Name, BestBenchKg) #Just bench data for all competitors

deadlift <- competitorsdistinct %>%
  select(MeetID, Name, BestDeadliftKg) #Just deadlift data for all competitors
```

**Keys**
========

------------------------------------------------------------------------

Keys are the columns that are used to match multiple datasets. They are explicitly defined by the argument (by = "varname") within your join function. However, they do not need to be explicitly defined. If you do not explicitly define it, dplyr will join the datasets across all possible keys (i.e., all columns that have the same name across both datasets) and will tell you in the output what variables it joined the datasets by.

For example, we have two datasets: meets and competitors. If we use the `glimpse` function on both datasets, we can see that the key would be "MeetID" because that is the only variable that matches both datasets.

``` r
glimpse(meets)
```

    ## Observations: 3
    ## Variables: 8
    ## $ MeetID      <int> 7015, 7021, 7028
    ## $ MeetPath    <chr> "usapl/NS-2015-06", "usapl/NS-2016-04", "usapl/NS-...
    ## $ Federation  <chr> "USAPL", "USAPL", "USAPL"
    ## $ Date        <chr> "10/15/2015", "10/13/2016", "10/10/2017"
    ## $ MeetCountry <chr> "USA", "USA", "USA"
    ## $ MeetState   <chr> NA, "GA", "FL"
    ## $ MeetTown    <chr> NA, "Atlanta", "Orlando"
    ## $ MeetName    <chr> "2015 Raw Nationals", "Raw Nationals 2016", "2017 ...

``` r
glimpse(competitors)
```

    ## Observations: 3,516
    ## Variables: 13
    ## $ MeetID         <int> 7015, 7015, 7015, 7015, 7015, 7015, 7015, 7015,...
    ## $ Name           <chr> "Nathalie Sanchez", "Sarah Cruz-Ortiz", "Jennif...
    ## $ Sex            <chr> "Female", "Female", "Female", "Female", "Female...
    ## $ Division       <chr> "R-JR", "R-JR", "R-JR", "R-O", "R-O", "R-O", "R...
    ## $ BodyweightKg   <dbl> 47.0, 46.1, 46.1, 46.5, 44.5, 46.1, 45.8, 46.4,...
    ## $ WeightClassKg  <chr> "47", "47", "47", "47", "47", "47", "47", "47",...
    ## $ WeightClass    <chr> "1: 47kg", "1: 47kg", "1: 47kg", "1: 47kg", "1:...
    ## $ BestSquatKg    <dbl> 92.5, 80.0, 92.5, 122.5, 117.5, 115.0, 115.0, 1...
    ## $ BestBenchKg    <dbl> 50.0, 50.0, 52.5, 65.0, 60.0, 60.0, 57.5, 55.0,...
    ## $ BestDeadliftKg <dbl> 122.5, 127.5, 110.0, 147.5, 150.0, 130.0, 117.5...
    ## $ TotalKg        <dbl> 265.0, 257.5, 255.0, 335.0, 327.5, 305.0, 290.0...
    ## $ Place          <chr> "1", "2", "3", "1", "2", "3", "4", "5", "6", "7...
    ## $ Wilks          <dbl> 356.40, 351.13, 347.72, 454.01, 457.65, 415.90,...

You'll learn about the different join functions below, but see what happens when join the two datasets with and without explicitly defining the key.

``` r
left_join(meets, competitors, by = "MeetID")
```

    ## # A tibble: 3,516 x 20
    ##    MeetID MeetPath      Federation Date     MeetCountry MeetState MeetTown
    ##     <int> <chr>         <chr>      <chr>    <chr>       <chr>     <chr>   
    ##  1   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  2   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  3   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  4   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  5   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  6   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  7   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  8   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  9   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ## 10   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ## # ... with 3,506 more rows, and 13 more variables: MeetName <chr>,
    ## #   Name <chr>, Sex <chr>, Division <chr>, BodyweightKg <dbl>,
    ## #   WeightClassKg <chr>, WeightClass <chr>, BestSquatKg <dbl>,
    ## #   BestBenchKg <dbl>, BestDeadliftKg <dbl>, TotalKg <dbl>, Place <chr>,
    ## #   Wilks <dbl>

``` r
left_join(meets, competitors)
```

    ## Joining, by = "MeetID"

    ## # A tibble: 3,516 x 20
    ##    MeetID MeetPath      Federation Date     MeetCountry MeetState MeetTown
    ##     <int> <chr>         <chr>      <chr>    <chr>       <chr>     <chr>   
    ##  1   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  2   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  3   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  4   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  5   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  6   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  7   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  8   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  9   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ## 10   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ## # ... with 3,506 more rows, and 13 more variables: MeetName <chr>,
    ## #   Name <chr>, Sex <chr>, Division <chr>, BodyweightKg <dbl>,
    ## #   WeightClassKg <chr>, WeightClass <chr>, BestSquatKg <dbl>,
    ## #   BestBenchKg <dbl>, BestDeadliftKg <dbl>, TotalKg <dbl>, Place <chr>,
    ## #   Wilks <dbl>

For the second join, dplyr was able to figure out that "MeetID" was the key variable!

Mismatched key names
--------------------

If keys are named differently across datasets, you can explicitly say they are the same by using the following code:

``` r
competitors2 <- competitors
competitors2$MeetNum <- competitors2$MeetID

left_join(meets, competitors2, by = c("MeetID" = "MeetNum"))
```

    ## # A tibble: 3,516 x 21
    ##    MeetID MeetPath      Federation Date     MeetCountry MeetState MeetTown
    ##     <int> <chr>         <chr>      <chr>    <chr>       <chr>     <chr>   
    ##  1   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  2   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  3   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  4   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  5   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  6   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  7   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  8   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ##  9   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ## 10   7015 usapl/NS-201~ USAPL      10/15/2~ USA         <NA>      <NA>    
    ## # ... with 3,506 more rows, and 14 more variables: MeetName <chr>,
    ## #   MeetID.y <int>, Name <chr>, Sex <chr>, Division <chr>,
    ## #   BodyweightKg <dbl>, WeightClassKg <chr>, WeightClass <chr>,
    ## #   BestSquatKg <dbl>, BestBenchKg <dbl>, BestDeadliftKg <dbl>,
    ## #   TotalKg <dbl>, Place <chr>, Wilks <dbl>

In this case, it would retain "MeetID" as the name of the key column in the resulting dataset because that was from the primary dataset.

Conflicting column names
------------------------

If you have two columns named the same thing across datasets that do *not* provide the same information, you need to explicitly define the key variables to ensure those duplicate columns are not included as a key. dplyr will then perform the join correctly and add a suffix to the same-name column names in your resulting dataset (e.g., "colname.x", "colname.y"). Also, if you want a custom suffix, you can explicitly call it with the suffix argument:

``` r
meets2 <- meets
meets2$Name <- meets2$MeetName

left_join(meets2, competitors, by = "MeetID") %>%
  select(Name.x, Name.y) #Just showing these columns for illustrative purposes
```

    ## # A tibble: 3,516 x 2
    ##    Name.x             Name.y          
    ##    <chr>              <chr>           
    ##  1 2015 Raw Nationals Nathalie Sanchez
    ##  2 2015 Raw Nationals Sarah Cruz-Ortiz
    ##  3 2015 Raw Nationals Jennifer Lotz   
    ##  4 2015 Raw Nationals Lisa Rothman    
    ##  5 2015 Raw Nationals Heather Connor  
    ##  6 2015 Raw Nationals Lisa Randazzo   
    ##  7 2015 Raw Nationals Emma Ife        
    ##  8 2015 Raw Nationals Pamela Sampson  
    ##  9 2015 Raw Nationals Kelley Sherwin  
    ## 10 2015 Raw Nationals Leah DeCesare   
    ## # ... with 3,506 more rows

We can see that it did *not* think Name was a key variable so it added the suffix ".x" and ".y" to each Name variable to differentiate them. If you want a custom suffix, you can explicitly call it with the suffix argument:

``` r
left_join(meets2, competitors, by = "MeetID", suffix = c("Meet", "Competitor"))  %>%
  select(NameMeet, NameCompetitor)
```

    ## # A tibble: 3,516 x 2
    ##    NameMeet           NameCompetitor  
    ##    <chr>              <chr>           
    ##  1 2015 Raw Nationals Nathalie Sanchez
    ##  2 2015 Raw Nationals Sarah Cruz-Ortiz
    ##  3 2015 Raw Nationals Jennifer Lotz   
    ##  4 2015 Raw Nationals Lisa Rothman    
    ##  5 2015 Raw Nationals Heather Connor  
    ##  6 2015 Raw Nationals Lisa Randazzo   
    ##  7 2015 Raw Nationals Emma Ife        
    ##  8 2015 Raw Nationals Pamela Sampson  
    ##  9 2015 Raw Nationals Kelley Sherwin  
    ## 10 2015 Raw Nationals Leah DeCesare   
    ## # ... with 3,506 more rows

Alternatively, you can always just rename them afterwards:

``` r
meets2 %>%
  left_join(competitors, by = "MeetID") %>%
  rename(NameMeet = Name.x, Name = Name.y) %>%
  select(NameMeet, Name)
```

    ## # A tibble: 3,516 x 2
    ##    NameMeet           Name            
    ##    <chr>              <chr>           
    ##  1 2015 Raw Nationals Nathalie Sanchez
    ##  2 2015 Raw Nationals Sarah Cruz-Ortiz
    ##  3 2015 Raw Nationals Jennifer Lotz   
    ##  4 2015 Raw Nationals Lisa Rothman    
    ##  5 2015 Raw Nationals Heather Connor  
    ##  6 2015 Raw Nationals Lisa Randazzo   
    ##  7 2015 Raw Nationals Emma Ife        
    ##  8 2015 Raw Nationals Pamela Sampson  
    ##  9 2015 Raw Nationals Kelley Sherwin  
    ## 10 2015 Raw Nationals Leah DeCesare   
    ## # ... with 3,506 more rows

**Types of Joins**
==================

------------------------------------------------------------------------

*Mutating joins*
----------------

Mutating joins are considered mutating joins because they add column(s).

### left\_join and right\_join

`left_join` returns all rows from the first dataset and all the columns from both datasets. `right_join` returns all rows from the second dataset and all the columns from both datasets.

In the following example, we test the meets\_full dataset (which has information for all Open Powerlifting meets) with the competitors dataset (which only has information for competitors from the 2015-2017 USAPL Raw National Championships).

``` r
left_join(meets_full, competitors, by = "MeetID")
```

    ## # A tibble: 11,995 x 20
    ##    MeetID MeetPath   Federation Date       MeetCountry MeetState MeetTown 
    ##     <int> <chr>      <chr>      <date>     <chr>       <chr>     <chr>    
    ##  1      0 365strong~ 365Strong  2016-10-29 USA         NC        Charlotte
    ##  2      1 365strong~ 365Strong  2016-11-19 USA         MO        Ozark    
    ##  3      2 365strong~ 365Strong  2016-07-09 USA         NC        Charlotte
    ##  4      3 365strong~ 365Strong  2016-06-11 USA         SC        Rock Hill
    ##  5      4 365strong~ 365Strong  2016-04-10 USA         SC        Rock Hill
    ##  6      5 365strong~ 365Strong  2017-04-22 USA         NC        Charlotte
    ##  7      6 365strong~ 365Strong  2017-01-21 USA         GA        Martinez 
    ##  8      7 365strong~ 365Strong  2017-06-10 USA         MO        Lake Oza~
    ##  9      8 365strong~ 365Strong  2017-07-15 USA         SC        Rock Hill
    ## 10      9 365strong~ 365Strong  2017-08-12 USA         NC        Fayettev~
    ## # ... with 11,985 more rows, and 13 more variables: MeetName <chr>,
    ## #   Name <chr>, Sex <chr>, Division <chr>, BodyweightKg <dbl>,
    ## #   WeightClassKg <chr>, WeightClass <chr>, BestSquatKg <dbl>,
    ## #   BestBenchKg <dbl>, BestDeadliftKg <dbl>, TotalKg <dbl>, Place <chr>,
    ## #   Wilks <dbl>

``` r
right_join(meets_full, competitors, by = "MeetID")
```

    ## # A tibble: 3,516 x 20
    ##    MeetID MeetPath    Federation Date       MeetCountry MeetState MeetTown
    ##     <int> <chr>       <chr>      <date>     <chr>       <chr>     <chr>   
    ##  1   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  2   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  3   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  4   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  5   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  6   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  7   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  8   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  9   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ## 10   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ## # ... with 3,506 more rows, and 13 more variables: MeetName <chr>,
    ## #   Name <chr>, Sex <chr>, Division <chr>, BodyweightKg <dbl>,
    ## #   WeightClassKg <chr>, WeightClass <chr>, BestSquatKg <dbl>,
    ## #   BestBenchKg <dbl>, BestDeadliftKg <dbl>, TotalKg <dbl>, Place <chr>,
    ## #   Wilks <dbl>

Notice how using `left_join` results in 11,995 rows while using `right_join` results in 3,516 rows. This is because the left\_join uses all rows for the meets\_full dataset (8482 meets) and then adds in columns when it can match on the "MeetID" key, which adds information of the 3516 competitors in the 3 largest USAPL meets, duplicating each meet row for each competitor row for the 3 USAPL raw championships.

The `right_join` results in only the data for the 3,516 competitors because it starts with all rows from the second (right) dataset and then adds columns from the first (left) dataset.

Furthermore, `left_join` and `right_join` essentially do the same thing if you flip the dataset order. We can test this using the `setequal` function.

``` r
setequal(left_join(meets_full, competitors, by = "MeetID"),
         right_join(competitors, meets_full, by = "MeetID"))
```

    ## TRUE

Note that we could also use the `identical` function instead of `setequal`, but `setequal` has the added benefit of not requiring row order to be identical.

### inner\_join

`inner_join` is an *exclusive join* and only joins data that match both datasets.

``` r
inner_join(meets_full, competitors, by = "MeetID")
```

    ## # A tibble: 3,516 x 20
    ##    MeetID MeetPath    Federation Date       MeetCountry MeetState MeetTown
    ##     <int> <chr>       <chr>      <date>     <chr>       <chr>     <chr>   
    ##  1   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  2   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  3   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  4   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  5   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  6   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  7   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  8   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ##  9   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ## 10   7015 usapl/NS-2~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ## # ... with 3,506 more rows, and 13 more variables: MeetName <chr>,
    ## #   Name <chr>, Sex <chr>, Division <chr>, BodyweightKg <dbl>,
    ## #   WeightClassKg <chr>, WeightClass <chr>, BestSquatKg <dbl>,
    ## #   BestBenchKg <dbl>, BestDeadliftKg <dbl>, TotalKg <dbl>, Place <chr>,
    ## #   Wilks <dbl>

``` r
inner_join(competitors, meets_full, by = "MeetID")
```

    ## # A tibble: 3,516 x 20
    ##    MeetID Name       Sex   Division BodyweightKg WeightClassKg WeightClass
    ##     <int> <chr>      <chr> <chr>           <dbl> <chr>         <chr>      
    ##  1   7015 Nathalie ~ Fema~ R-JR             47   47            1: 47kg    
    ##  2   7015 Sarah Cru~ Fema~ R-JR             46.1 47            1: 47kg    
    ##  3   7015 Jennifer ~ Fema~ R-JR             46.1 47            1: 47kg    
    ##  4   7015 Lisa Roth~ Fema~ R-O              46.5 47            1: 47kg    
    ##  5   7015 Heather C~ Fema~ R-O              44.5 47            1: 47kg    
    ##  6   7015 Lisa Rand~ Fema~ R-O              46.1 47            1: 47kg    
    ##  7   7015 Emma Ife   Fema~ R-O              45.8 47            1: 47kg    
    ##  8   7015 Pamela Sa~ Fema~ R-O              46.4 47            1: 47kg    
    ##  9   7015 Kelley Sh~ Fema~ R-O              45.7 47            1: 47kg    
    ## 10   7015 Leah DeCe~ Fema~ R-O              45.5 47            1: 47kg    
    ## # ... with 3,506 more rows, and 13 more variables: BestSquatKg <dbl>,
    ## #   BestBenchKg <dbl>, BestDeadliftKg <dbl>, TotalKg <dbl>, Place <chr>,
    ## #   Wilks <dbl>, MeetPath <chr>, Federation <chr>, Date <date>,
    ## #   MeetCountry <chr>, MeetState <chr>, MeetTown <chr>, MeetName <chr>

The `inner_join` function returns the same data regardless of the order of the dataframes, but the order of the columns change depending on which dataframe is the primary dataframe.

### full\_join

`full_join` is an *inclusive join* and combines all rows regardless if there's matching data in the other dataset.

``` r
full_join(meets_full, competitors, by = "MeetID")
```

    ## # A tibble: 11,995 x 20
    ##    MeetID MeetPath   Federation Date       MeetCountry MeetState MeetTown 
    ##     <int> <chr>      <chr>      <date>     <chr>       <chr>     <chr>    
    ##  1      0 365strong~ 365Strong  2016-10-29 USA         NC        Charlotte
    ##  2      1 365strong~ 365Strong  2016-11-19 USA         MO        Ozark    
    ##  3      2 365strong~ 365Strong  2016-07-09 USA         NC        Charlotte
    ##  4      3 365strong~ 365Strong  2016-06-11 USA         SC        Rock Hill
    ##  5      4 365strong~ 365Strong  2016-04-10 USA         SC        Rock Hill
    ##  6      5 365strong~ 365Strong  2017-04-22 USA         NC        Charlotte
    ##  7      6 365strong~ 365Strong  2017-01-21 USA         GA        Martinez 
    ##  8      7 365strong~ 365Strong  2017-06-10 USA         MO        Lake Oza~
    ##  9      8 365strong~ 365Strong  2017-07-15 USA         SC        Rock Hill
    ## 10      9 365strong~ 365Strong  2017-08-12 USA         NC        Fayettev~
    ## # ... with 11,985 more rows, and 13 more variables: MeetName <chr>,
    ## #   Name <chr>, Sex <chr>, Division <chr>, BodyweightKg <dbl>,
    ## #   WeightClassKg <chr>, WeightClass <chr>, BestSquatKg <dbl>,
    ## #   BestBenchKg <dbl>, BestDeadliftKg <dbl>, TotalKg <dbl>, Place <chr>,
    ## #   Wilks <dbl>

``` r
full_join(competitors, meets_full, by = "MeetID")
```

    ## # A tibble: 11,995 x 20
    ##    MeetID Name       Sex   Division BodyweightKg WeightClassKg WeightClass
    ##     <int> <chr>      <chr> <chr>           <dbl> <chr>         <chr>      
    ##  1   7015 Nathalie ~ Fema~ R-JR             47   47            1: 47kg    
    ##  2   7015 Sarah Cru~ Fema~ R-JR             46.1 47            1: 47kg    
    ##  3   7015 Jennifer ~ Fema~ R-JR             46.1 47            1: 47kg    
    ##  4   7015 Lisa Roth~ Fema~ R-O              46.5 47            1: 47kg    
    ##  5   7015 Heather C~ Fema~ R-O              44.5 47            1: 47kg    
    ##  6   7015 Lisa Rand~ Fema~ R-O              46.1 47            1: 47kg    
    ##  7   7015 Emma Ife   Fema~ R-O              45.8 47            1: 47kg    
    ##  8   7015 Pamela Sa~ Fema~ R-O              46.4 47            1: 47kg    
    ##  9   7015 Kelley Sh~ Fema~ R-O              45.7 47            1: 47kg    
    ## 10   7015 Leah DeCe~ Fema~ R-O              45.5 47            1: 47kg    
    ## # ... with 11,985 more rows, and 13 more variables: BestSquatKg <dbl>,
    ## #   BestBenchKg <dbl>, BestDeadliftKg <dbl>, TotalKg <dbl>, Place <chr>,
    ## #   Wilks <dbl>, MeetPath <chr>, Federation <chr>, Date <date>,
    ## #   MeetCountry <chr>, MeetState <chr>, MeetTown <chr>, MeetName <chr>

We can see these datasets are identical.

``` r
setequal(full_join(meets_full, competitors, by = "MeetID"),
         full_join(competitors, meets_full, by = "MeetID"))
```

    ## TRUE

*Filtering joins*
-----------------

These joins are called filtering joins because they provide data from the primary dataframe that has been filtered based on the secondary dataframe. These are useful to see which data does match (`semi_join`) or does not match (`anti_join`) across dataframes.

### semi\_join

`semi_join` only shows the rows from the first dataframe that has data in the second data frame.

``` r
semi_join(meets_full, competitors, by = "MeetID")
```

    ## # A tibble: 3 x 8
    ##   MeetID MeetPath     Federation Date       MeetCountry MeetState MeetTown
    ##    <int> <chr>        <chr>      <date>     <chr>       <chr>     <chr>   
    ## 1   7015 usapl/NS-20~ USAPL      2015-10-15 USA         <NA>      <NA>    
    ## 2   7021 usapl/NS-20~ USAPL      2016-10-13 USA         GA        Atlanta 
    ## 3   7028 usapl/NS-20~ USAPL      2017-10-10 USA         FL        Orlando 
    ## # ... with 1 more variable: MeetName <chr>

``` r
semi_join(competitors, meets_full, by = "MeetID")
```

    ## # A tibble: 3,516 x 13
    ##    MeetID Name       Sex   Division BodyweightKg WeightClassKg WeightClass
    ##     <int> <chr>      <chr> <chr>           <dbl> <chr>         <chr>      
    ##  1   7015 Nathalie ~ Fema~ R-JR             47   47            1: 47kg    
    ##  2   7015 Sarah Cru~ Fema~ R-JR             46.1 47            1: 47kg    
    ##  3   7015 Jennifer ~ Fema~ R-JR             46.1 47            1: 47kg    
    ##  4   7015 Lisa Roth~ Fema~ R-O              46.5 47            1: 47kg    
    ##  5   7015 Heather C~ Fema~ R-O              44.5 47            1: 47kg    
    ##  6   7015 Lisa Rand~ Fema~ R-O              46.1 47            1: 47kg    
    ##  7   7015 Emma Ife   Fema~ R-O              45.8 47            1: 47kg    
    ##  8   7015 Pamela Sa~ Fema~ R-O              46.4 47            1: 47kg    
    ##  9   7015 Kelley Sh~ Fema~ R-O              45.7 47            1: 47kg    
    ## 10   7015 Leah DeCe~ Fema~ R-O              45.5 47            1: 47kg    
    ## # ... with 3,506 more rows, and 6 more variables: BestSquatKg <dbl>,
    ## #   BestBenchKg <dbl>, BestDeadliftKg <dbl>, TotalKg <dbl>, Place <chr>,
    ## #   Wilks <dbl>

The first `semi_join` function shows that there are only 3 rows in the meets\_full dataframe that has data in the competitors dataframe.

The second `semi_join` function shows that all 3,516 rows in the competitors dataframe has data in the meets\_full dataframe.

### anti\_join

`anti_join` shows only the rows from the first dataframe that does NOT have data in the second dataframe.

In the following example, we see that there are 8,479 meets that do not have competitors data in the competitors dataframe I created. If there are 8,482 meets total and 8,479 meets in the resulting dataframe, then we have only have competitors data on 3 meets total.

``` r
anti_join(meets_full, competitors, by = "MeetID")
```

    ## # A tibble: 8,479 x 8
    ##    MeetID MeetPath   Federation Date       MeetCountry MeetState MeetTown 
    ##     <int> <chr>      <chr>      <date>     <chr>       <chr>     <chr>    
    ##  1      0 365strong~ 365Strong  2016-10-29 USA         NC        Charlotte
    ##  2      1 365strong~ 365Strong  2016-11-19 USA         MO        Ozark    
    ##  3      2 365strong~ 365Strong  2016-07-09 USA         NC        Charlotte
    ##  4      3 365strong~ 365Strong  2016-06-11 USA         SC        Rock Hill
    ##  5      4 365strong~ 365Strong  2016-04-10 USA         SC        Rock Hill
    ##  6      5 365strong~ 365Strong  2017-04-22 USA         NC        Charlotte
    ##  7      6 365strong~ 365Strong  2017-01-21 USA         GA        Martinez 
    ##  8      7 365strong~ 365Strong  2017-06-10 USA         MO        Lake Oza~
    ##  9      8 365strong~ 365Strong  2017-07-15 USA         SC        Rock Hill
    ## 10      9 365strong~ 365Strong  2017-08-12 USA         NC        Fayettev~
    ## # ... with 8,469 more rows, and 1 more variable: MeetName <chr>

### Counting rows

Both of the filtering joins are useful to check for matching/mismatching data. While we can see that information in the first row of the resulting datasets, perhaps we don't care about seeing the dataset but only want to know the resulting number. We can do that with the following code:

``` r
meets_full %>%
  semi_join(competitors, by = "MeetID") %>%
  nrow()
```

    ## [1] 3

*Set operations*
----------------

Used when two datasets have the exact same variables (colnames) and you want to add rows, not columns.

### union

Returns every row that appears in either dataset. However, if the same row appears multiple times, it will only provide that row once (removes duplicates).

The following code takes the males and females datasets and uses `union` to combine males *and* females.

``` r
union(males, females)
```

    ## # A tibble: 3,516 x 13
    ##    MeetID Name       Sex   Division BodyweightKg WeightClassKg WeightClass
    ##     <int> <chr>      <chr> <chr>           <dbl> <chr>         <chr>      
    ##  1   7015 Amanda Ri~ Fema~ R-O             103.  84+           7: 84+kg   
    ##  2   7028 Anthony C~ Male  R-O              92.3 93            5: 93kg    
    ##  3   7028 Susan Hin~ Fema~ R-M2             84.8 84+           7: 84+kg   
    ##  4   7015 Eric LaPo~ Male  R-JR             72.9 74            3: 74kg    
    ##  5   7015 Pete Groh~ Male  R-M1b            95.4 105           6: 105kg   
    ##  6   7021 Danny Sul~ Male  R-JR             93   93            5: 93kg    
    ##  7   7021 Stephen C~ Male  R-O              71.5 74            3: 74kg    
    ##  8   7015 Pomrening~ Fema~ R-O              81.1 84            6: 84kg    
    ##  9   7021 Mitchell ~ Male  R-JR            102.  105           6: 105kg   
    ## 10   7015 Thomas Co~ Male  R-O              92.2 93            5: 93kg    
    ## # ... with 3,506 more rows, and 6 more variables: BestSquatKg <dbl>,
    ## #   BestBenchKg <dbl>, BestDeadliftKg <dbl>, TotalKg <dbl>, Place <chr>,
    ## #   Wilks <dbl>

### intersect

Returns every row that appears in both datasets.

In the following code, I have created datasets with just the names and sexes of competitors to see which competitors competed in both the USAPL 2015 and 2017 Raw National Championships. We can see that 199 people competed in both 2015 and 2017.

``` r
intersect(usapl15_names, usapl17_names)
```

    ## # A tibble: 199 x 2
    ##    Name                 Sex   
    ##    <chr>                <chr> 
    ##  1 Lisa Rothman         Female
    ##  2 Shyami Murphy        Female
    ##  3 Suzanne Hartwig-Gary Female
    ##  4 Mune van Luen        Female
    ##  5 Winnie Abramson      Female
    ##  6 Melissa Forbis       Female
    ##  7 Marisa Inda          Female
    ##  8 Heather Faas         Female
    ##  9 Emma Ife             Female
    ## 10 Kate Cohen           Female
    ## # ... with 189 more rows

### setdiff

Returns every row that appears in the first dataset but not the second.

Let's use the same datasets we did for `intersect` but now see who competed in 2015 and did not return for 2017. It should be the difference of the total competitors in 2015 minus the 199 who also competed in 2017.

``` r
setdiff(usapl15_names, usapl17_names)
```

    ## # A tibble: 782 x 2
    ##    Name              Sex   
    ##    <chr>             <chr> 
    ##  1 Nathalie Sanchez  Female
    ##  2 Sarah Cruz-Ortiz  Female
    ##  3 Jennifer Lotz     Female
    ##  4 Heather Connor    Female
    ##  5 Lisa Randazzo     Female
    ##  6 Pamela Sampson    Female
    ##  7 Kelley Sherwin    Female
    ##  8 Leah DeCesare     Female
    ##  9 Samantha Reynolds Female
    ## 10 Susan Elwyn       Female
    ## # ... with 772 more rows

### Putting it all together

If we put all of the information together, we should see that `union` should equal both `setdiff` plus `intersect`.

``` r
sumall <- nrow(intersect(usapl15_names, usapl17_names)) +
          nrow(setdiff(usapl15_names, usapl17_names)) +
          nrow(setdiff(usapl17_names, usapl15_names))

nrow(union(usapl15_names, usapl17_names)) == sumall
```

    ## [1] TRUE

Similarly, `setdiff` and `intersect` should be able to equal the total number of people in each dataframe.

``` r
sum15 <- nrow(intersect(usapl15_names, usapl17_names)) +
         nrow(setdiff(usapl15_names, usapl17_names))

nrow(usapl15_names) == sum15
```

    ## [1] TRUE

``` r
sum17 <- nrow(intersect(usapl17_names, usapl15_names)) +
         nrow(setdiff(usapl17_names, usapl15_names))

nrow(usapl17_names) == sum17
```

    ## [1] TRUE

*Binds*
-------

Binds are used when datasets have the exact same columns or the exact same rows in the same order. cbind() and rbind() can do this work, but dplyr's functions are faster, returns tibbles instead of dataframes, are more flexible, can handle lists of dataframes, and has the added functionality of the .id argument which can add which dataframe the data came from.

### bind\_cols

Similar to `cbind`, `bind_cols` adds columns from the second dataset to the right of the columns from the second dataset. This is a lazy join and is very risky because it requires rows in both datasets to be in the same order! It is much better to use a mutating join with a key variable.

``` r
bind_cols(peopledata, liftingdata)
```

    ## # A tibble: 3,516 x 13
    ##    MeetID Name       Sex   Division BodyweightKg WeightClassKg WeightClass
    ##     <int> <chr>      <chr> <chr>           <dbl> <chr>         <chr>      
    ##  1   7015 Nathalie ~ Fema~ R-JR             47   47            1: 47kg    
    ##  2   7015 Sarah Cru~ Fema~ R-JR             46.1 47            1: 47kg    
    ##  3   7015 Jennifer ~ Fema~ R-JR             46.1 47            1: 47kg    
    ##  4   7015 Lisa Roth~ Fema~ R-O              46.5 47            1: 47kg    
    ##  5   7015 Heather C~ Fema~ R-O              44.5 47            1: 47kg    
    ##  6   7015 Lisa Rand~ Fema~ R-O              46.1 47            1: 47kg    
    ##  7   7015 Emma Ife   Fema~ R-O              45.8 47            1: 47kg    
    ##  8   7015 Pamela Sa~ Fema~ R-O              46.4 47            1: 47kg    
    ##  9   7015 Kelley Sh~ Fema~ R-O              45.7 47            1: 47kg    
    ## 10   7015 Leah DeCe~ Fema~ R-O              45.5 47            1: 47kg    
    ## # ... with 3,506 more rows, and 6 more variables: BestSquatKg <dbl>,
    ## #   BestBenchKg <dbl>, BestDeadliftKg <dbl>, TotalKg <dbl>, Place <chr>,
    ## #   Wilks <dbl>

### bind\_rows

Similar to `rbind`, `bind_rows` adds rows from the second dataset below the rows from the first dataset.

``` r
bind_rows(males, females)
```

    ## # A tibble: 3,516 x 13
    ##    MeetID Name       Sex   Division BodyweightKg WeightClassKg WeightClass
    ##     <int> <chr>      <chr> <chr>           <dbl> <chr>         <chr>      
    ##  1   7015 Steven Ga~ Male  R-T1             55.3 59            1: 59kg    
    ##  2   7015 Michael B~ Male  R-T2             56.9 59            1: 59kg    
    ##  3   7015 Henry Die~ Male  R-T2             58.6 59            1: 59kg    
    ##  4   7015 Aaron Hall Male  R-T3             58   59            1: 59kg    
    ##  5   7015 Taskin Am~ Male  R-JR             56.4 59            1: 59kg    
    ##  6   7015 Matthew F~ Male  R-JR             58.9 59            1: 59kg    
    ##  7   7015 Khemarint~ Male  R-JR             58.6 59            1: 59kg    
    ##  8   7015 Walter Ca~ Male  R-JR             58.7 59            1: 59kg    
    ##  9   7015 Slade Woo~ Male  R-JR             56.8 59            1: 59kg    
    ## 10   7015 Shawn Fra~ Male  R-O              57.6 59            1: 59kg    
    ## # ... with 3,506 more rows, and 6 more variables: BestSquatKg <dbl>,
    ## #   BestBenchKg <dbl>, BestDeadliftKg <dbl>, TotalKg <dbl>, Place <chr>,
    ## #   Wilks <dbl>

\#\#\#.id argument Perhaps you would like to retain the information of which dataframe your data came from when using `bind_rows`. You can add the .id argument to achieve this.

``` r
bind_rows("USAPL 2015" = usapl15, "USAPL 2017" = usapl17, .id = "USAPL")
```

    ## # A tibble: 2,324 x 14
    ##    USAPL     MeetID Name         Sex   Division BodyweightKg WeightClassKg
    ##    <chr>      <int> <chr>        <chr> <chr>           <dbl> <chr>        
    ##  1 USAPL 20~   7015 Nathalie Sa~ Fema~ R-JR             47   47           
    ##  2 USAPL 20~   7015 Sarah Cruz-~ Fema~ R-JR             46.1 47           
    ##  3 USAPL 20~   7015 Jennifer Lo~ Fema~ R-JR             46.1 47           
    ##  4 USAPL 20~   7015 Lisa Rothman Fema~ R-O              46.5 47           
    ##  5 USAPL 20~   7015 Heather Con~ Fema~ R-O              44.5 47           
    ##  6 USAPL 20~   7015 Lisa Randaz~ Fema~ R-O              46.1 47           
    ##  7 USAPL 20~   7015 Emma Ife     Fema~ R-O              45.8 47           
    ##  8 USAPL 20~   7015 Pamela Samp~ Fema~ R-O              46.4 47           
    ##  9 USAPL 20~   7015 Kelley Sher~ Fema~ R-O              45.7 47           
    ## 10 USAPL 20~   7015 Leah DeCesa~ Fema~ R-O              45.5 47           
    ## # ... with 2,314 more rows, and 7 more variables: WeightClass <chr>,
    ## #   BestSquatKg <dbl>, BestBenchKg <dbl>, BestDeadliftKg <dbl>,
    ## #   TotalKg <dbl>, Place <chr>, Wilks <dbl>

Note that now we have a column at the beginning that ID's which dataframe it came from.

Missing/Duplicate values/columns
================================

------------------------------------------------------------------------

Missing key values
------------------

Imagine you have an NA in the key column of your primary dataset.There is nothing you can do to match it to the second dataset. You can filter out those missing rows before joining:

df1 %&gt;% filter(!is.na(keyvar)) %&gt;% left\_join(df2, by = "keyvar")

Missing key columns
-------------------

If you do not have a matching key column in the matching dataset, you cannot match the two datasets. But if you have a matching key column in the matching dataset that is simply named something else, you can define that in the join.

Othertimes the rownames are stored in the row.names attribute. This is not a tidy way to store data. You can pull that into a column like so:

df1 %&gt;% rownames\_to\_column(var = "keyvar")

Duplicate key values
--------------------

Sometimes there are duplicate key values for a reason. dplyr will return every combination of key value matches across datasets in this case.

Joining Multiple Datasets
=========================

There are multiple methods for combining multiple datasets. We could do this with multiple pipes, and in fact we may be required to do this if keys are all over the place, but we also have other tools in our toolbox!

*Reduce*
--------

The purrr package has a useful function `reduce` that can combine lists of dataframes with the same join function.

``` r
list(squat, bench, deadlift) %>%
  reduce(left_join, by = c("MeetID", "Name"))
```

    ## # A tibble: 3,095 x 5
    ##    MeetID Name             BestSquatKg BestBenchKg BestDeadliftKg
    ##     <int> <chr>                  <dbl>       <dbl>          <dbl>
    ##  1   7015 Nathalie Sanchez        92.5        50             122.
    ##  2   7015 Sarah Cruz-Ortiz        80          50             128.
    ##  3   7015 Jennifer Lotz           92.5        52.5           110 
    ##  4   7015 Lisa Rothman           122.         65             148.
    ##  5   7015 Heather Connor         118.         60             150 
    ##  6   7015 Lisa Randazzo          115          60             130 
    ##  7   7015 Emma Ife               115          57.5           118.
    ##  8   7015 Pamela Sampson         102.         55             120 
    ##  9   7015 Kelley Sherwin          90          55             130 
    ## 10   7015 Leah DeCesare          100          57.5           100 
    ## # ... with 3,085 more rows

*Binding*
---------

We have three dataframes, one for each year of the USAPL Raw National Championships. We can put this into a list to apply a join function to all of them simultaneously!

``` r
usapl <- list(usapl15, usapl16, usapl17)

usapl %>%
  bind_rows(.id = "usapl")
```

    ## # A tibble: 3,516 x 14
    ##    usapl MeetID Name             Sex   Division BodyweightKg WeightClassKg
    ##    <chr>  <int> <chr>            <chr> <chr>           <dbl> <chr>        
    ##  1 1       7015 Nathalie Sanchez Fema~ R-JR             47   47           
    ##  2 1       7015 Sarah Cruz-Ortiz Fema~ R-JR             46.1 47           
    ##  3 1       7015 Jennifer Lotz    Fema~ R-JR             46.1 47           
    ##  4 1       7015 Lisa Rothman     Fema~ R-O              46.5 47           
    ##  5 1       7015 Heather Connor   Fema~ R-O              44.5 47           
    ##  6 1       7015 Lisa Randazzo    Fema~ R-O              46.1 47           
    ##  7 1       7015 Emma Ife         Fema~ R-O              45.8 47           
    ##  8 1       7015 Pamela Sampson   Fema~ R-O              46.4 47           
    ##  9 1       7015 Kelley Sherwin   Fema~ R-O              45.7 47           
    ## 10 1       7015 Leah DeCesare    Fema~ R-O              45.5 47           
    ## # ... with 3,506 more rows, and 7 more variables: WeightClass <chr>,
    ## #   BestSquatKg <dbl>, BestBenchKg <dbl>, BestDeadliftKg <dbl>,
    ## #   TotalKg <dbl>, Place <chr>, Wilks <dbl>

merge
=====

Base R has the `merge` function that has the same functions as the mutating joins above. This is shown for illustrative purposes, but I recommend using the dplyr functions and not `merge`.

``` r
setequal(left_join(meets, competitors, by = "MeetID"), 
         merge(meets, competitors, by = "MeetID", all.x = TRUE, all.y = FALSE))
```

    ## TRUE

``` r
setequal(right_join(meets, competitors, by = "MeetID"), 
         merge(meets, competitors, by = "MeetID", all.x = FALSE, all.y = TRUE))
```

    ## TRUE

``` r
setequal(inner_join(meets, competitors, by = "MeetID"),
         merge(meets, competitors, by = "MeetID", all = FALSE))
```

    ## TRUE

``` r
setequal(full_join(meets, competitors, by = "MeetID"),
         merge(meets, competitors, by = "MeetID", all = TRUE))
```

    ## TRUE
