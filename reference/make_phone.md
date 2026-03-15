# Format a 10-digit phone number

Format a 10-digit phone number

## Usage

``` r
make_phone(x)
```

## Arguments

- x:

  A character vector

## Value

A character vector

## Examples

``` r
make_phone("9122474701")
#> [1] "(912) 247-4701"

make_phone("9122474701, 9122474701")
#> [1] "(912) 247-4701, (912)-247-4701"
```
