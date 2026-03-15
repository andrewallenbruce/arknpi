# Format a 9-digit zip code

Format a 9-digit zip code

## Usage

``` r
make_zip(x)
```

## Arguments

- x:

  A character vector

## Value

A character vector

## Examples

``` r
make_zip(c("31605", "316051234", NA))
#> [1] "31605"      "31605-1234" NA          
```
