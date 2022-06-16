
<!-- README.md is generated from README.Rmd. Please edit that file -->

# zippeR

[![R build
status](https://github.com/chris-prener/zippeR/workflows/R-CMD-check/badge.svg)](https://github.com/chris-prener/zippeR/actions)
[![Coverage
status](https://codecov.io/gh/chris-prener/zippeR/branch/master/graph/badge.svg)](https://codecov.io/github/chris-prener/zippeR?branch=main)

## Motivation

`zippeR` provides a set of functions for working with ZCTAs and building
spatial and demographic data for three-digit ZCTAs. These three-digit
ZCTAs have limitations (they are large regions), but they are also used
within American health care to protect patient confidentiality. `zippeR`
therefore offers researchers who must use three-digit ZCTAs the
capability to download geometric data and also aggregate demographic
data from five-digit ZCTAs to three-digit ZCTAs. In addition, `zippeR`
includes functions for validating and formatting vectors of ZIP Codes or
ZCTAs as well as tools for crosswalking ZIP codes with ZCTAs.

## Installation

### Installing zippeR

The development version of `zippeR` can be accessed from GitHub with
`remotes`:

``` r
# install.packages("remotes")
remotes::install_github("chris-prener/zippeR")
```

## Usage

### ZIP Code and ZCTA Formatting

One of the core features of `zippeR` validate inputs of ZIP codes or
ZCTA codes. For example, here are a set of ZCTAs that lie on the
Missouri/Iowa border:

``` r
zcta5 <- c("51640", "52542", "52573", "5262x")
```

Notice how the last element contains a non-numeric character. When
`zcta5` is passed to `zi_validate()`, it will catch the formatting
issue. There are two options, one of which returns a single logical
value (`TRUE` or `FALSE`):

``` r
> library(zippeR)
> zi_validate(zcta5)
[1] FALSE
```

The other option, with `verbose = TRUE`, provides additional data about
where formatting issues may exist:

``` r
> zi_validate(zcta5, verbose = TRUE)
# A tibble: 4 × 2
  condition                                   result
  <chr>                                       <lgl> 
1 Input is a character vector?                TRUE  
2 All input values have 5 characters?         TRUE  
3 No input values are over 5 characters long? TRUE  
4 All input values are numeric?               FALSE 
```

For the third and fourth tests, users are strongly encourage to attempt
to manually correct problems. However, `zi_repair()` can be used to
address the first and second tests, and will return `NA` values for ZIPs
or ZCTAs that do not pass the third and fourth tests:

``` r
> zi_repair(zcta5)
[1] "51640" "52542" "52573" NA     
Warning message:
In zi_repair(zcta5) : NAs introduced by coercion
```

When malformed ZIPs or ZCTAs are replaced with `NA` values,
`zi_repair()` will return a warning. Note that `zi_validate()` also
works with three-digit ZCTAs as well:

``` r
> zcta3 <- c("516", "525", "526")
> zi_validate(zcta3, style = "zcta3")
[1] TRUE
```

Note that, at this time, the validation process does not ensure that
inputs correspond to valid ZCTAs. That functionality is planned for
addition prior to release.

### ZCTA Geometry

`zippeR` providers support for downloading geometric data as `sf`
objects. For five-digit ZCTAs, `tigris` is used to access the TIGER/Line
database. For three-digit ZCTAs, these are downloaded from [a repository
on GitHub](https://github.com/chris-prener/zcta3) maintained by the
author. These are generalized versions of the ZCTA geometry designed to
provide a smaller file size suitable for storage on GitHub.
Nevertheless, each file is still approximately 60MB in size. Users
should carefully evaluate the geometric data to ensure they are fit for
purpose, and note that these are provided without warrenty.

One major difference between `tigris::zctas()` and
`zipper::get_geometry()` is that `zippeR` provides support for
automatically returning ZCTAs that intersect or whose centroids lie
within states and/or counties. This can be used with both the five- and
three-digit ZCTAs. In general, the intersect method will yield more
ZCTAs than are necessary while the centroid method will yield too few.
For example, for Missouri, we need to use the `includes` argument to add
several additional ZCTAs in whose centroids do not fall within Missouri
itself (they all lie in Iowa):

``` r
geo12 <- zi_get_geometry(year = 2012, state = "MO", method = "centroid", 
    includes = c("51640", "52542", "52573", "52626"))
```

The same process can be used for three-digit ZCTAs, though note that
only three-digit areas are passed to the `includes` argument:

``` r
geo12_3 <- zi_get_geometry(year = 2012, style = "zcta3", state = "MO", method = "centroid",
                           includes = c("516", "525"))
```

If you use the intersect method, the corresponding `excludes` argument
can be used to drop ZCTAs that are not needed. Note that all ZCTAs
passed to `starts_with` (which accepts two-digit values), `includes`,
and `excludes` are validated to ensure they follow formatting
requirements. As noted above, the validation process does not ensure
that inputs correspond to valid ZCTAs at this time.

One additional option to note is that, when downloading nation-wide ZCTA
data, you can use the `shift_geo` argument to place Alaska, Hawaii, and
Puerto Rico in the lower left-hand corner of your map:

``` r
geo10 <- zi_get_geometry(year = 2010, shift_geo = TRUE)
```

### ZCTA Demographics

Downloading the geometric data can be used for mapping, or simply to
produce a vector of all five- or three-digit ZCTAs in a given area.
These, in-turn, can be used to define areas where demographic data are
desired. The `zi_get_demographics()` wraps `tidycensus` functions to get
data using a single, uniform API. For instance, we can download the
total population for all ZCTAs based on the 2010 Decennial Census:

``` r
pop10 <- zi_get_demographics(year = 2010, variables = "P001001", survey = "sf1")
```

*Note: As of now, the Census Bureau has not released ZCTA data for the
2020 Decennial Census.*

If you are working with output from `zi_get_geometry()`, you can specify
the `GEOID` vector in your call to limit the number of ZCTAs returned:

``` r
gini12 <- zi_get_demographics(year = 2012, table = "B19083", survey = "acs5", zcta = geo12$GEOID)
```

*This should only be used for five-digit ZCTAs.* If you are working with
three-digit ZCTAs, you must aggregate your data first to ensure that the
numbers generated reflect the entirety of the three-digit ZCTA.
Aggregation is accomplished with `zi_aggregate()`. First, we’ll download
some data *without* specifying any input for the `zcta` argument:

``` r
pop12 <- zi_get_demographics(year = 2012, variables = "B01003_001", survey = "acs5")
```

Once these are obtained, we can pass the object to `zi_aggregate()` and
can specify an input for `zcta` at this stage:

``` r
pop12_mo3 <- zi_aggregate(pop12, year = 2012, extensive = "B01003_001", survey = "acs5", zcta = geo12_3$ZCTA3)
```

The `zi_aggregate()` function requires that you specify two sets of
variable lists - those that are `extensive` (i.e. count data) and those
that are `intensive` (i.e. ratio or median data). For `extensive` data,
`zi_aggregate()` sums the estimates and applies a formula to the margins
of error (the square root of the sum of squared margins of error for
each five-digit ZCTA within a three-digit region). For `intensive`
variables, a weighted mean or median is used for both the estimate and
the margin of error. Note that you can pipe this workflow and can
specify multiple variables at once for aggregation:

``` r
zi_get_demographics(year = 2020, variables = c("B01003_001", "B19083_001"), survey = "acs5") %>%
  zi_aggregate(year = 2020, extensive = "B01003_001", intensive = "B19083_001", survey = "acs5") -> demo20
```

The `variables`, `table` (which can be used in place of `variables` for
`zi_get_demographics()`), `extensive`, and `intensive` arguments are not
validated before being passed via `tidycensus` to the Census Bureau, so
mis-formatted variable or table names will generate potentially cryptic
errors.

### ZIP Code to ZCTA Crosswalks

Finally, `zippeR` provides an interface for accessing the [UDS Mapper
ZIP to ZCTA crosswalk
files](https://udsmapper.org/zip-code-to-zcta-crosswalk/). Crosswalk
files are critical because not all ZIP codes are in the exact same ZCTA.
For example, the first two entries below have very different ZCTAs. The
UDS files are available from 2010 through 2021 in a standardized format:

``` r
> zi_load_crosswalk(year = 2020)
# A tibble: 41,096 × 6                                                                                                                                                                                 
   ZIP   PO_NAME    STATE ZIP_TYPE                             ZCTA  zip_join_type       
   <chr> <chr>      <chr> <chr>                                <chr> <chr>               
 1 00501 Holtsville NY    Post Office or large volume customer 11742 Spatial join to ZCTA
 2 00544 Holtsville NY    Post Office or large volume customer 11742 Spatial join to ZCTA
 3 00601 Adjuntas   PR    ZIP Code Area                        00601 ZIP Matches ZCTA    
 4 00602 Aguada     PR    ZIP Code Area                        00602 ZIP Matches ZCTA    
 5 00603 Aguadilla  PR    ZIP Code Area                        00603 ZIP Matches ZCTA    
 6 00604 Aguadilla  PR    Post Office or large volume customer 00603 Spatial join to ZCTA
 7 00605 Aguadilla  PR    Post Office or large volume customer 00603 Spatial join to ZCTA
 8 00606 Maricao    PR    ZIP Code Area                        00606 ZIP Matches ZCTA    
 9 00610 Anasco     PR    ZIP Code Area                        00610 ZIP Matches ZCTA    
10 00611 Angeles    PR    Post Office or large volume customer 00641 Spatial join to ZCTA
# … with 41,086 more rows
```

As with the three-digit ZCTA geometry, users should evaluate these data
carefully before using them to ensure they are fit for purpose. In
particular, they should note that ZIPs that do not have corresponding
ZCTAs (such as Armed Forces mailing ZIPs and those in some overseas
territories) are not included. Users should also remember that
individuals may live in a different ZCTA from their mailing address when
that address is a Post Office or some other large volume customer.

They can be used with `zi_crosswalk()` to convert given ZIP codes to
ZCTAs:

``` r
> zips <- data.frame(id = c(1:3), ZIP = c("63139", "63108", "00501"))
> zi_crosswalk(zips, input_zip = ZIP, dict = "UDS 2021") 
# A tibble: 3 × 3                                                                                                                                                                                      
     id ZIP   ZCTA 
  <int> <chr> <chr>
1     1 63139 63139
2     2 63108 63108
3     3 00501 11742
```

If `"UDS 2021"` (or any other year between 2010 and 2021) is given for
`dict`, `zi_crosswalk()` will automatically download the UDS crosswalk
file. A custom crosswalk can also be supplied for `dict` in lieu of
using the UDS data. In that case, `dict_zip` and `dict_zcta` should be
updated to correctly match input variable names. `style` can also be
used if the custom dictionary contains three digit ZCTAs instead. If no
custom dictionary is supplied, `zi_crosswalk()` will try to conver the
dictionary’s five-digit ZCTAs to three-digits:

``` r
> zi_crosswalk(zips, input_zip = ZIP, dict = "UDS 2021", style = "zcta3") 
Dictionary five-digit ZCTAs converted to three-digit ZCTAs.                                                                                                                                            
# A tibble: 3 × 3
     id ZIP   ZCTA3
  <int> <chr> <chr>
1     1 63139 631  
2     2 63108 631  
3     3 00501 117  
```

## Contributor Code of Conduct

Please note that this project is released with a Contributor [Code of
Conduct](/.github/CODE_OF_CONDUCT.md). By participating in this project
you agree to abide by its terms.
