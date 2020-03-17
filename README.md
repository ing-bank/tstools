
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="man/figures/tstools_logo.png" width="150px" />

[![lifecycle](man/figures/lifecycle-stable-brightgreen.png)](https://www.tidyverse.org/lifecycle/#stable)

  - [Overview](#overview)
  - [Features](#features)
  - [Installation](#installation)
  - [How to use](#how_to_use)
  - [Contribute](#contribute)

## Overview<a name="overview"></a>

tstools contains a set of R functions that can be used to do several
simple tasks to support time series data cleaning and forecasting, in
the following categories:

  - DATA manipulation:
      - Grouping, getting features and transforming
  - DATE manipulation:
      - Parsing, ranging and creating periods
  - HIERARCHICAL functions:
      - Creating lists and matrixes, adding lineage and identifying
        hierarchical leaves
  - NETWORK functions:
      - Fast read and write from/to network and browser detection
  - NUMERIC manipulation:
      - Summarizing large values and transforming into characters
  - MISC functions:
      - Dev and plotting functions

The original developers of this R package are [Gertjan van den
Bos](mailto:gertjan.bos@ing.com), [Mehmet
Kutluay](mailto:yasar.kutluay@ing.com) & [Berke
Aslan](mailto:berke.aslan@edu.devinci.fr).

## Features<a name="features"></a>

The following functions are available within this R package.

  - DATA manipulation:
      - `add_grouping_column()` is a function to combine one or more
        grouping columns into a single grouping column.
      - `get_column_features()` is a function to show the type, unique
        values and missing values of the column(s) in a dataset
      - `get_feature_with_date_columns()` is an extension of the
        `get_column_features()` function. The extension is that a more
        thorough feature extraction is done to detect potential Date
        columns
      - `indicate_empty_character_cols()` is a function to replace all
        missing values in the column(s) of a dataset with the specified
        indicator.
      - `initialize_ts_forecast_data()` is a function to prepare any
        dataset for use within the time series forecasting framework of
        the tsforecast package. The function renames a selection of
        indicated columns to a global format that is used within the
        time series forecasting framework. Currently only datasets with
        a monthly time frequency are supported.
      - `split_grouping_column()` is a function to split a single
        grouping column back into one or more grouping columns.
      - `transform_data_to_ts_object()` is a function to transform a set
        of forecast data to a time series object.
      - `unlist_if_required()` is a function to take an item, usually a
        data frame, out of a list of length one.
  - DATE manipulation:
      - `date_to_period()` is a function to transform a date object into
        a period (numeric - yyyymm format).
      - `get_date_range()` is a function to summarize the range of dates
        in a given column.
      - `months_between_periods()` is a function to count the number of
        months between two periods.
      - `parse_date_columns()` is a function that parses columns that
        are detected to be of type Date and parses them into YYYY-MM-DD
        POISXct format. The detection is done via the
        `get_feature_with_date_columns()` function.
      - `parse_date_from_vector()` is a function to transform an input
        vector into a vector of date objects of the format YYYY-MM-DD.
      - `period_delta()` is a function to add or subtract a number of
        months to or from a given period.
      - `period_to_first_day()` is a function to transform a period into
        the date object for the first day of that month.
      - `period_to_last_day()` is a function to transform a period into
        the date object for the last day of that month.
      - `period_to_quarter()` is a function to transform a period into
        the numeric value for the corresponding quarter (e.g. 1 for
        January, February and March, 2 for April, May, June, etc.).
  - HIERARCHICAL functions:
      - `add_hierarchical_lineage()` is a function that identifies both
        the direct parents of every node, as well as the leaf groups in
        a hierarchy.
      - `create_hierarchy_list()` is a function that creates the named
        list “hierarchy”, which includes both the hierarchical matrix as
        well as the data used to create that matrix.
      - `create_hierarchy_matrix()` is a function that creates the
        hierarchy matrix. This is a matrix that stores the relevant
        information about the relationship (i.e. connections) between
        the different groups in your hierarchy.
      - `get_ultimate_leaves()` is a function that creates a vector of
        the leaves corresponding to one of the columns within a specific
        grouping, based on data containing the hierarchy information.
      - `plot_hierarchy()` is a function to visualize the hierarchical
        tree for one of the hierarchical columns available within the
        hierarchy data. The hierarchy is visualizes as a tree with a
        layer of nodes for each separate level and edges between the
        inidividual nodes to indicate (hierarchical) relationships
        between them.
  - NETWORK functions:
      - `fast_read_RData_from_network()` is a function to more quickly
        read data from network locations.
      - `fast_write_RData_to_network()` is a function to more quickly
        write data to network locations.
      - `get_browser_to_use_for_shiny()` is a function to determine
        which browsers are available to host Shiny and return the one
        that is most suitable (in that Chrome \> Firefox \> IE in terms
        of support of Shiny features).
  - NUMERIC manipulation:
      - `format_big_n()` is a function to format big numbers in a pretty
        way, to be used when plotting.
      - `numeric_cols_to_character()` is a function to transform the
        numeric column(s) into character column(s).
      - `summarize_numeric_value()` is a function to convert a large
        numeric value to thousands, millions, billions or trillions.
  - MISC functions:
      - `check_data_format()` is a function to check whether the
        specified data has the same format as is expected by the
        function in which this check is performed.
      - `get_plot_colors()` is a function to generate a vector with
        color codes, which can be used for plotting. Currently, 23
        distinct colors are implemented.

-----

## Installation<a name="installation"></a>

tstools is a package developed within ING and is not available on CRAN.
The fastest way to install the package is to use a terminal or command
prompt. Clone this repository and stay in the parent directory.
Afterwards, simply type:

``` bash
R CMD INSTALL tstools
```

Some prerequisites for installing the package:

  - R version 3.6.0 or later
  - Rtools installed ([How
    to?](https://thecoatlessprofessor.com/programming/installing-rtools-for-compiled-code-via-rcpp))

Any required packages that are missing from your R library will be
indicated during installation or loading of the package, please [install
these missing
packages](https://stat.ethz.ch/R-manual/R-devel/library/utils/html/install.packages.html)
and retry installing/loading tstools.

-----

## How to use<a name="how_to_use"></a>

This section contains a limited number of examples of functions within
the different categories, but the full list of functions (including
function documentation and an example) is available when loading the
package after installation.

The package contains example data (a modified version of
expsmooth::gasprice) that can be used to try out the package:

``` r
library(tstools)
#> Loading required package: magrittr
#> Registered S3 method overwritten by 'quantmod':
#>   method            from
#>   as.zoo.data.frame zoo
head(dummy_gasprice)
#> # A tibble: 6 x 6
#>   year_month state    oil_company gasprice spotprice gemprice
#>   <date>     <chr>    <chr>          <dbl>     <dbl>    <dbl>
#> 1 1991-01-31 New York CompanyA        1.22      29.4     44.5
#> 2 1991-02-28 New York CompanyA        1.22      18.5     41.8
#> 3 1991-03-31 New York CompanyA        1.16      16.5     38.8
#> 4 1991-04-30 New York CompanyA        1.26      25.0     59.0
#> 5 1991-05-31 New York CompanyA        1.33      24.6     44.5
#> # ... with 1 more row
str(dummy_gasprice)
#> Classes 'tbl_df', 'tbl' and 'data.frame':    764 obs. of  6 variables:
#>  $ year_month : Date, format: "1991-01-31" "1991-02-28" ...
#>  $ state      : chr  "New York" "New York" "New York" "New York" ...
#>  $ oil_company: chr  "CompanyA" "CompanyA" "CompanyA" "CompanyA" ...
#>  $ gasprice   : num  1.22 1.22 1.16 1.26 1.33 ...
#>  $ spotprice  : num  29.4 18.5 16.5 25 24.6 ...
#>  $ gemprice   : num  44.5 41.8 38.8 59 44.5 ...
```

Once you loaded your data, you can pre-process it. One way to start this
stage can be to get different features of your columns (using
`get_column_features()`) such as the type, unique values and missing
values.

``` r
get_column_features(dummy_gasprice)
#> # A tibble: 6 x 4
#>   name        type      unique_values na_values
#>   <chr>       <chr>             <int>     <int>
#> 1 year_month  Date                191         0
#> 2 state       character             2         0
#> 3 oil_company character             2         0
#> 4 gasprice    numeric             764         0
#> 5 spotprice   numeric             764         0
#> # ... with 1 more row
```

Using the data function `add_grouping_column()`, which combines one or
more grouping columns into a single grouping column, you can summarize
the values in each of the specified group for every row :

``` r
add_grouping_column(
  data = dummy_gasprice,
  group_cols = c("state", "oil_company")
)
#> # A tibble: 764 x 7
#>   year_month state    oil_company gasprice spotprice gemprice
#>   <date>     <chr>    <chr>          <dbl>     <dbl>    <dbl>
#> 1 1991-01-31 New York CompanyA        1.22      29.4     44.5
#> 2 1991-02-28 New York CompanyA        1.22      18.5     41.8
#> 3 1991-03-31 New York CompanyA        1.16      16.5     38.8
#> 4 1991-04-30 New York CompanyA        1.26      25.0     59.0
#> 5 1991-05-31 New York CompanyA        1.33      24.6     44.5
#>   grouping                                     
#>   <chr>                                        
#> 1 state = New York   &   oil_company = CompanyA
#> 2 state = New York   &   oil_company = CompanyA
#> 3 state = New York   &   oil_company = CompanyA
#> 4 state = New York   &   oil_company = CompanyA
#> 5 state = New York   &   oil_company = CompanyA
#> # ... with 759 more rows
```

After looking into the features of your data, you can start working with
the date column(s) if your dataset includes a time dimension. First of
all, we can check the date range of our data using the
`get_date_range()` function :

``` r
get_date_range(
  data = dummy_gasprice,
  date_col = "year_month"
)
#> $start_ym
#> [1] 199101
#> 
#> $start_year
#> [1] 1991
#> 
#> $start_month
#> [1] 1
#> 
#> $end_ym
#> [1] 200611
#> 
#> $end_year
#> [1] 2006
#> 
#> $end_month
#> [1] 11
```

Initially, the date column can be in any format. The date functions in
this package use a single format (which is YYYYMM) to manipulate the
date column vector. In order to use the period functions, it is the best
to transform the date column into the package’s standard format by using
the `date_to_period()` function:

``` r
date_to_period(
  date = as.Date("20 Jan 2019", "%d %b %Y")
)
#> [1] 201901
```

If you are dealing with hierarchical data, you can use the
`create_hierarchy_list()` function to get a hierarchical list of your
data :

``` r
dummy_hierarchical_gasprice <- add_grouping_column(
  data = dummy_hierarchical_gasprice,
  group_cols = c("oil_company", "location", "currency")
)

create_hierarchy_list(
  data = head(dummy_hierarchical_gasprice)
)
#> $matrix
#>                                             location = USA   &   oil_company = CompanyA
#> location = USA   &   oil_company = CompanyC                                           1
#> location = USA   &   oil_company = CompanyA                                           1
#> location = USA   &   oil_company = CompanyB                                           0
#>                                             location = USA   &   oil_company = CompanyB
#> location = USA   &   oil_company = CompanyC                                           1
#> location = USA   &   oil_company = CompanyA                                           0
#> location = USA   &   oil_company = CompanyB                                           1
#> 
#> $data
#> # A tibble: 3 x 9
#>   location oil_company level_location level_oil_company parent_location
#>   <chr>    <chr>                <dbl>             <dbl> <chr>          
#> 1 USA      CompanyC                 1                 1 <NA>           
#> 2 USA      CompanyA                 1                 2 <NA>           
#> 3 USA      CompanyB                 1                 2 <NA>           
#>   leaf_location parent_oil_company leaf_oil_company
#>           <dbl> <chr>                         <dbl>
#> 1             1 <NA>                              0
#> 2             1 CompanyC                          1
#> 3             1 CompanyC                          1
#>   grouping                                   
#>   <chr>                                      
#> 1 location = USA   &   oil_company = CompanyC
#> 2 location = USA   &   oil_company = CompanyA
#> 3 location = USA   &   oil_company = CompanyB
```

Finally, reading large numerical values are easier with the
`summarize_numeric_value()` function:

``` r
summarize_numeric_value(123012301032)
#> [1] "123.01 B"
```

-----

## Contribute<a name="contribute"></a>

  - Idea? Please open an issue
  - Bug? Please open an issue
  - Want to contribute? Awesome\! Please open an issue :)

## License<a name ="license"></a>

This package is free and open source software, licensed under GPL-3.
More information can be found
[here](https://www.gnu.org/licenses/gpl-3.0.en.html).
