Age differences in Hollywood movies
================
2/27/23

``` r
library(tidyverse)
```

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ dplyr     1.1.0     ✔ readr     2.1.4
    ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ✔ ggplot2   3.4.1     ✔ tibble    3.1.8
    ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ✔ purrr     1.0.1     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the ]8;;http://conflicted.r-lib.org/conflicted package]8;; to force all conflicts to become errors

``` r
library(dplyr) 
library(ggplot2) 
library(stringr)

theme_set(theme_light())

tuesdata <- tidytuesdayR::tt_load(2023, week = 7)
```

    --- Compiling #TidyTuesday Information for 2023-02-14 ----
    --- There is 1 file available ---
    --- Starting Download ---


        Downloading file 1 of 1: `age_gaps.csv`

    --- Download complete ---

``` r
age_gaps <- tuesdata$age_gaps

#age_gaps
```

# PART I. Age differences between characters in movies directed by Woody Allen.

``` r
age_gaps |> 
    filter(director == "Woody Allen") |> 
    select(movie_name, release_year, age_difference, couple_number) |>
    arrange(desc(release_year))
```

| movie_name             | release_year | age_difference | couple_number |
|:-----------------------|-------------:|---------------:|--------------:|
| Café Society           |         2016 |             28 |             1 |
| Café Society           |         2016 |              7 |             2 |
| Café Society           |         2016 |              4 |             3 |
| Irrational Man         |         2015 |             14 |             1 |
| Magic in the Moonlight |         2014 |             28 |             1 |
| Whatever Works         |         2009 |             40 |             1 |
| Whatever Works         |         2009 |             18 |             2 |
| Whatever Works         |         2009 |              4 |             3 |
| Scoop                  |         2006 |             16 |             1 |
| Hollywood Ending       |         2002 |             33 |             1 |
| Hollywood Ending       |         2002 |             15 |             2 |
| Mighty Aphrodite       |         1995 |             32 |             1 |
| Mighty Aphrodite       |         1995 |             31 |             2 |
| Mighty Aphrodite       |         1995 |             19 |             3 |
| Husbands and Wives     |         1992 |             38 |             1 |
| Husbands and Wives     |         1992 |             21 |             2 |
| Husbands and Wives     |         1992 |             10 |             3 |
| Manhattan              |         1979 |             26 |             1 |
| Manhattan              |         1979 |              8 |             2 |
| Annie Hall             |         1977 |             11 |             1 |

**1. What is the range of age differences between characters in Woody
Allen’s movies?**

``` r
age_gaps |> 
    filter(director == "Woody Allen") |>
    select(age_difference) |> 
    range()
```

    [1]  4 40

The range of age differences is 4-40 years.

2.  **What is the mean age difference between characters in Woody
    Allen’s movies across the years?**

``` r
age_gaps |> 
    filter(director == "Woody Allen") |> 
    select(movie_name, release_year, age_difference, couple_number) |>
    count(movie_name, sort = TRUE)
```

| movie_name             |   n |
|:-----------------------|----:|
| Café Society           |   3 |
| Husbands and Wives     |   3 |
| Mighty Aphrodite       |   3 |
| Whatever Works         |   3 |
| Hollywood Ending       |   2 |
| Manhattan              |   2 |
| Annie Hall             |   1 |
| Irrational Man         |   1 |
| Magic in the Moonlight |   1 |
| Scoop                  |   1 |

Out of 20 movies directed by Woody Allen in this data set, 6 movies have
entries detailing the age difference between more than one couple.
Therefore, it would be helpful to know the mean age difference of all
couples in each movie to make data visualisation easier.

``` r
age_difference_WA_movies <-
age_gaps |> 
    filter(director == "Woody Allen") |> 
    select(movie_name, release_year, age_difference, couple_number) |>
    arrange(desc(release_year))

age_difference_WA_movies
```

| movie_name             | release_year | age_difference | couple_number |
|:-----------------------|-------------:|---------------:|--------------:|
| Café Society           |         2016 |             28 |             1 |
| Café Society           |         2016 |              7 |             2 |
| Café Society           |         2016 |              4 |             3 |
| Irrational Man         |         2015 |             14 |             1 |
| Magic in the Moonlight |         2014 |             28 |             1 |
| Whatever Works         |         2009 |             40 |             1 |
| Whatever Works         |         2009 |             18 |             2 |
| Whatever Works         |         2009 |              4 |             3 |
| Scoop                  |         2006 |             16 |             1 |
| Hollywood Ending       |         2002 |             33 |             1 |
| Hollywood Ending       |         2002 |             15 |             2 |
| Mighty Aphrodite       |         1995 |             32 |             1 |
| Mighty Aphrodite       |         1995 |             31 |             2 |
| Mighty Aphrodite       |         1995 |             19 |             3 |
| Husbands and Wives     |         1992 |             38 |             1 |
| Husbands and Wives     |         1992 |             21 |             2 |
| Husbands and Wives     |         1992 |             10 |             3 |
| Manhattan              |         1979 |             26 |             1 |
| Manhattan              |         1979 |              8 |             2 |
| Annie Hall             |         1977 |             11 |             1 |

``` r
character_mean_age_difference_WA_movies <-
    age_difference_WA_movies |> 
    group_by(movie_name) |> 
    summarize(mean_age_difference = 
                            round(mean(age_difference, digits = 0))) |>
    arrange(desc(mean_age_difference))

character_mean_age_difference_WA_movies
```

| movie_name             | mean_age_difference |
|:-----------------------|--------------------:|
| Magic in the Moonlight |                  28 |
| Mighty Aphrodite       |                  27 |
| Hollywood Ending       |                  24 |
| Husbands and Wives     |                  23 |
| Whatever Works         |                  21 |
| Manhattan              |                  17 |
| Scoop                  |                  16 |
| Irrational Man         |                  14 |
| Café Society           |                  13 |
| Annie Hall             |                  11 |

<!-- Checking if these calculations are correct by manually calculating two examples. -->
<!-- mighty_aphrodite_mean_age_difference <-  -->
<!-- sum((32 + 31 + 19)/3) |>  -->
<!-- round(digits = 0) |> -->
<!-- print() -->
<!-- character_mean_age_difference_WA_movies |>  -->
<!-- filter(movie_name =="Mighty Aphrodite") -->
<!-- The mean age difference between the three couples in the "Mighty Aphrodite" movie is 27 years according to the manual calculation. -->

3.  **Visualising how age difference between characters in Woody Allen’s
    movies have fluctuated over the years.**

<!-- character_mean_age_difference_WA_movies  -->
<!-- The data frame showing all mean age difference scores for all WA movies -->

``` r
age_difference_WA_movies_clean <-
    age_difference_WA_movies |>
    select(-age_difference, -couple_number) |>
    distinct(movie_name, release_year)

age_difference_WA_movies_clean
```

| movie_name             | release_year |
|:-----------------------|-------------:|
| Café Society           |         2016 |
| Irrational Man         |         2015 |
| Magic in the Moonlight |         2014 |
| Whatever Works         |         2009 |
| Scoop                  |         2006 |
| Hollywood Ending       |         2002 |
| Mighty Aphrodite       |         1995 |
| Husbands and Wives     |         1992 |
| Manhattan              |         1979 |
| Annie Hall             |         1977 |

This table shows all Woody Allen’s movie names and their release years.
It would be helpful to merge it with one of the earlier tables showing
mean age difference scores for all of these movies.

<!-- Now joining these two data frames together. -->

``` r
WA_character_age_difference <- 
    merge(x = character_mean_age_difference_WA_movies,
                y = age_difference_WA_movies_clean,
                by = "movie_name") |>
    arrange(desc(release_year))

WA_character_age_difference
```

| movie_name             | mean_age_difference | release_year |
|:-----------------------|--------------------:|-------------:|
| Café Society           |                  13 |         2016 |
| Irrational Man         |                  14 |         2015 |
| Magic in the Moonlight |                  28 |         2014 |
| Whatever Works         |                  21 |         2009 |
| Scoop                  |                  16 |         2006 |
| Hollywood Ending       |                  24 |         2002 |
| Mighty Aphrodite       |                  27 |         1995 |
| Husbands and Wives     |                  23 |         1992 |
| Manhattan              |                  17 |         1979 |
| Annie Hall             |                  11 |         1977 |

<!-- This data frame now includes all movie titles, their release year, and mean age difference between included couple characters. -->

Now it’s possible to visualise the mean age difference between
characters in Woody Allen’s movies across the years.

``` r
WA_mean_age_plot <- 
    WA_character_age_difference |>
    ggplot(aes(release_year, mean_age_difference, colour = movie_name)) +
    geom_line() + 
    geom_point(size = 3, show.legend = TRUE) + 
    expand_limits(x = c(1970, 2020),
                                y = c(0, 30)) + 
    labs(x = "Movie release year",
             y = "Mean age difference between love characters",
             title = "Age difference between love characters in Woody Allen's films (1977-2016) \nover the years",
             colour = "Movie name")

WA_mean_age_plot
```

    `geom_line()`: Each group consists of only one observation.
    ℹ Do you need to adjust the group aesthetic?

![](Age_differences_in_hollywood_movies_1_files/figure-commonmark/unnamed-chunk-9-1.png)

\#PART II. Age differences in movies across the data set.

1.  Which movies has the highest and the lowest age differences between
    love characters?

``` r
highest_age_difference <- age_gaps |>
    select(movie_name, release_year, age_difference) |>
    head(10) |>
    arrange(desc(age_difference))

highest_age_difference
```

| movie_name         | release_year | age_difference |
|:-------------------|-------------:|---------------:|
| Harold and Maude   |         1971 |             52 |
| Venus              |         2006 |             50 |
| The Quiet American |         2002 |             49 |
| The Big Lebowski   |         1998 |             45 |
| Beginners          |         2010 |             43 |
| Poison Ivy         |         1992 |             42 |
| Whatever Works     |         2009 |             40 |
| Entrapment         |         1999 |             39 |
| Husbands and Wives |         1992 |             38 |
| Magnolia           |         1999 |             38 |

``` r
highest_age_difference |>
    ggplot(aes(age_difference, movie_name, color = movie_name)) +
    geom_point(show.legend = TRUE) +
    expand_limits(x = c(35,55)) +
    labs(x = "Age difference between characters",
             y = "Movie title",
             color = "Movie title",
             title = "10 movies with highest age difference\n between love characters") +
    theme_classic() +
    theme(plot.title = element_text(hjust= 0.5, color = "red", face = "bold"))
```

![](Age_differences_in_hollywood_movies_1_files/figure-commonmark/unnamed-chunk-11-1.png)

``` r
lowest_age_difference <- 
    age_gaps |>
    select(movie_name, release_year, age_difference) |>
    tail(10)

lowest_age_difference
```

| movie_name                      | release_year | age_difference |
|:--------------------------------|-------------:|---------------:|
| Spanglish                       |         2004 |              0 |
| Speed                           |         1994 |              0 |
| Spider-Man: Far From Home       |         2019 |              0 |
| The Crush                       |         1993 |              0 |
| The Holiday                     |         2006 |              0 |
| The Hunger Games: Catching Fire |         2013 |              0 |
| The Lake House                  |         2006 |              0 |
| Tolkien                         |         2019 |              0 |
| Transformers                    |         2007 |              0 |
| X-Men: First Class              |         2015 |              0 |
