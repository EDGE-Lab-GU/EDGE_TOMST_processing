

# Import TOMST logger data with R

Written by: Katrín Björnsdóttir (katrin.bjornsdottir@bioenv.gu.se), 19.
march 2025

This tutorial goes through how you can import TOMST logger data and
process the raw readings.

## Prepare data:

Prepare your raw data according to these instructions.

1.  Packages:

    *Make sure you install all packages if you don’t have them already*

    ``` r
    library(tidyverse)
    library(lubridate)
    library(data.table)
    library(purrr)
    ```

2.  Upload the raw data that you downloaded directly from the loggers to
    this folder
    [“Inputs/Raw_data/”](https://github.com/EDGE-Lab-GU/EDGE_TOMST_processing/tree/main/Inputs/Raw_data).
    For best results, each TOMST logger should have a separate folder
    and the folder should be named after the plot name (see how the test
    data is organized).

For Windows:

``` r
# List all TOMST files
f <- list.files("Inputs/Raw_data/", pattern = "data_", full.names = TRUE, recursive = TRUE)

# Prepare information for importing
fi <- tibble(file = f) %>%
  mutate(
    plot_id = str_split(file, "/", simplify = TRUE)[, 3],  # Extract 3rd folder from the path which should be the plot_id
    tomst_id = str_extract(file, "(?<=data_)[0-9]+") %>% as.numeric() # Extracts the TOMST_id from the data file name path
    ) %>%
  arrange(plot_id)
```

For Mac:

``` r
    # List all TOMST files
    f <- list.files("Inputs/Raw_data", pattern = "data_", full.names = TRUE, recursive = TRUE)

    # Prepare information for importing
    fi <- tibble(file = f) %>%
      mutate(
        plot_id = str_split(file, "/", simplify = TRUE)[, 3],  # Extract 3rd folder from the path which should be the plot_id
        tomst_id = str_extract(file, "(?<=data_)[0-9]+") %>% as.numeric() # Extracts the TOMST_id from the data file name path
        ) %>%
      arrange(plot_id)
```

1.  Before importing the data from your TOMST loggers you will need a
    datatable with the following information:

    *plot_id:* Add the name or id of the specific plot the TOMST logger
    belongs to.

    *installation_date:* Add the date when the logger was installed in
    the field.

    *tomst_id:* Add the TOMST id number from the logger.

    Since the TOMST data loggers usually start recording data before
    being installed in the field, we need installation date information
    which we will use to filter out values that were recorded before the
    logger was installed.

    You can use the template which you can access on the repository
    here:
    [imput_tomst_setup](https://github.com/EDGE-Lab-GU/EDGE_TOMST_processing/blob/main/Inputs/Raw_data/input_tomst_setup.csv).
    Note: It should be placed in the “Inputs/Raw_data/” folder for the
    code to work.

    Now load this datatable into R:

``` r
tomst_setup <- read_csv("Inputs/Raw_data/input_tomst_setup.csv") %>% 
  mutate(installation_date = as.Date(installation_date)) %>%
  select(plot_id, installation_date) %>%
  # Add one day because we don't want to include only half a day in daily means calculations
  mutate(installation_date_new = installation_date + 1) %>%
  filter(!is.na(installation_date))
```

## Import TOMST data

Use this function to load in all TOMST data into one datafile.

``` r
# Read and clean data
readdata <- function(i) {
  files_to_read <- fi %>% 
    filter(grepl(i, fi$file))
  
  map_dfr(files_to_read$file, ~ {
    d <- fread(.x) %>%
      select(V2, V3, V4, V5, V6, V7) %>%
      filter(!duplicated(V2, fromLast = TRUE)) %>%
      mutate(across(V4:V6, ~ as.numeric(gsub(",", ".", .))),
             V2 = ymd_hm(V2, tz = "UTC"),
             V2 = with_tz(V2, tzone = "Europe/Stockholm"))

    d$plot_id <- fi$plot_id[which(fi$file == .x)][1]
    d$tomst_id <- fi$tomst_id[which(fi$file == .x)][1]

    d
  })
}

# Apply the function using map
# Side note: map is a function used to loop over a list of vectors from the 'purrr' package
mylist <- map(fi$file, readdata)

tomst_data <- bind_rows(mylist) %>% 
  # Clean up column names
  rename(datetime = V2, zone = V3, T1 = V4, T2 = V5, T3 = V6, moist = V7) %>%
  arrange(plot_id, datetime) %>% 
  # Match installation times and filter out data before installation
  left_join(tomst_setup, by = "plot_id") %>% 
  filter(datetime > installation_date_new) %>%
  select(-installation_date_new)
```

## Save as outputs

For further use, you can save the data as a csv data table output. You
can for example use this location - “Outputs/Data/”.

``` r
write_csv(tomst_data, "Outputs/Data/output_tomst_test.csv") # change the output name to fit your data
```
