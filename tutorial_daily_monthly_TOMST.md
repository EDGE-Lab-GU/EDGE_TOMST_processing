

# Calculate daily means from TOMST data

Written by: Katrín Björnsdóttir (katrin.bjornsdottir@bioenv.gu.se), 20.
March 2025

This tutorial demonstrates how to calculate daily and monthly means from
raw TOMST logger readings.

## Prepare the data

1.  Load packages *(make sure you have all packages installed already)*

``` r
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(data.table)
```

    Warning: package 'data.table' was built under R version 4.3.3


    Attaching package: 'data.table'

    The following objects are masked from 'package:dplyr':

        between, first, last

``` r
library(lubridate)
```

    Warning: package 'lubridate' was built under R version 4.3.3


    Attaching package: 'lubridate'

    The following objects are masked from 'package:data.table':

        hour, isoweek, mday, minute, month, quarter, second, wday, week,
        yday, year

    The following objects are masked from 'package:base':

        date, intersect, setdiff, union

``` r
library(purrr)
```


    Attaching package: 'purrr'

    The following object is masked from 'package:data.table':

        transpose

``` r
library(tidyverse)
```

    Warning: package 'tidyverse' was built under R version 4.3.3

    Warning: package 'ggplot2' was built under R version 4.3.3

    Warning: package 'readr' was built under R version 4.3.3

    Warning: package 'forcats' was built under R version 4.3.3

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ forcats 1.0.0     ✔ stringr 1.5.1
    ✔ ggplot2 3.5.1     ✔ tibble  3.2.1
    ✔ readr   2.1.5     ✔ tidyr   1.3.0

    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ data.table::between() masks dplyr::between()
    ✖ dplyr::filter()       masks stats::filter()
    ✖ data.table::first()   masks dplyr::first()
    ✖ lubridate::hour()     masks data.table::hour()
    ✖ lubridate::isoweek()  masks data.table::isoweek()
    ✖ dplyr::lag()          masks stats::lag()
    ✖ data.table::last()    masks dplyr::last()
    ✖ lubridate::mday()     masks data.table::mday()
    ✖ lubridate::minute()   masks data.table::minute()
    ✖ lubridate::month()    masks data.table::month()
    ✖ lubridate::quarter()  masks data.table::quarter()
    ✖ lubridate::second()   masks data.table::second()
    ✖ purrr::transpose()    masks data.table::transpose()
    ✖ lubridate::wday()     masks data.table::wday()
    ✖ lubridate::week()     masks data.table::week()
    ✖ lubridate::yday()     masks data.table::yday()
    ✖ lubridate::year()     masks data.table::year()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

1.  Load in the cleaned raw TOMST readings.

``` r
tomst_data <- read_csv("Outputs/Data/output_tomst_test.csv") %>%
  mutate(datetime = with_tz(datetime, tzone = "Europe/Stockholm"))
```

## Calculate daily means (and other useful variables)

``` r
daily_means <- tomst_data %>%
  mutate(date = as_date(datetime)) %>%
  group_by(plot_id, date) %>%
  mutate(
    roll_diff_T1 = T1 - lead(T1), # calculates the difference in soil temp between days
    roll_diff_T2 = T2 - lead(T2), # calculates the difference in surf temp between days
    roll_diff_T3 = T3 - lead(T3)  # calculates the difference in air temp between days
    ) %>%
  summarise(
    soil_mean = mean(T1), # daily mean temperature
    soil_median = median(T1), # daily median temperature
    soil_sd = sd(T1), # daily standard deviation
    soil_min = min(T1), # minimum temperature per day
    soil_max = max(T1), # maximum temperature per day
    soil_quantile_95 = quantile(T1, probs = 0.95), # 95% upper quantile temperature
    soil_quantile_5 = quantile(T1, probs = 0.05),  # 5% lower quantile temperature
    surf_mean = mean(T2),
    surf_median = median(T2),
    surf_sd = sd(T2),
    surf_min = min(T2),
    surf_max = max(T2),
    surf_quantile_95 = quantile(T2, probs = 0.95),
    surf_quantile_5 = quantile(T2, probs = 0.05),
    air_mean = mean(T3),
    air_median = median(T3),
    air_sd = sd(T3),
    air_min = min(T3),
    air_max = max(T3),
    air_quantile_95 = quantile(T3, probs = 0.95),
    air_quantile_5 = quantile(T3, probs = 0.05),
    corr_soil_air = cor(T1, T3, use = "pairwise.complete.obs"), # daily correlation between soil and air temperature
    max_roll_diff_soil = max(roll_diff_T1, na.rm = TRUE), # maximum daily difference
    max_roll_diff_surf = max(roll_diff_T2, na.rm = TRUE), # maximum daily difference
    max_roll_diff_air = max(roll_diff_T3, na.rm = TRUE),  # maximum daily difference
    moist_mean = mean(moist),
    moist_median = median(moist),
    moist_sd = sd(moist),
    moist_min = min(moist),
    moist_max = max(moist),
    moist_quantile_95 = quantile(moist, probs = 0.95),
    moist_quantile_5 = quantile(moist, probs = 0.05)
    ) %>%
  mutate(
    soil_air_diff = air_mean - soil_mean,
    soil_air_max_diff = air_max - soil_max
    )
```

## Calculate monthly means (and other useful variables)

``` r
monthly_means <- tomst_data %>%
  mutate(date = as_date(datetime),
         # Extract year and month
         year = year(date),
         month = month.name[month(date)]
  ) %>%
  group_by(plot_id, year, month) %>%
  summarise(
    # summarise how many days are included in the monthly mean summary to make sure the data across months are comparable
    nr_days = n_distinct(date),
    soil_mean = mean(T1), # monthly mean temperature
    soil_median = median(T1), # monthly median temperature
    soil_sd = sd(T1), # monthly standard deviation
    soil_min = min(T1), # monthly minimum temperature
    soil_max = max(T1), # monthly maximum temperature
    soil_quantile_95 = quantile(T1, probs = 0.95), # monthly 95% upper quantile
    soil_quantile_5 = quantile(T1, probs = 0.05), # monthly 5% lower quantile
    surf_mean = mean(T2),
    surf_median = median(T2),
    surf_sd = sd(T2),
    surf_min = min(T2),
    surf_max = max(T2),
    surf_quantile_95 = quantile(T2, probs = 0.95),
    surf_quantile_5 = quantile(T2, probs = 0.05),
    air_mean = mean(T3),
    air_median = median(T3),
    air_sd = sd(T3),
    air_min = min(T3),
    air_max = max(T3),
    air_quantile_95 = quantile(T3, probs = 0.95),
    air_quantile_5 = quantile(T3, probs = 0.05),
    moist_mean = mean(moist),
    moist_median = median(moist),
    moist_sd = sd(moist),
    moist_min = min(moist),
    moist_max = max(moist),
    moist_quantile_95 = quantile(moist, probs = 0.95),
    moist_quantile_5 = quantile(moist, probs = 0.05)
  )
```

    `summarise()` has grouped output by 'plot_id', 'year'. You can override using
    the `.groups` argument.

## Save daily and monthly means outputs

``` r
write_csv(daily_means, "Outputs/Data/output_daily_means.csv")

write_csv(monthly_means, "Outputs/Data/output_monthly_means.csv")
```
