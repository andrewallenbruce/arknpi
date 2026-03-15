# Make an Address Purpose Column

Make an Address Purpose Column

## Usage

``` r
make_purpose(x, y)
```

## Arguments

- x:

  Mailing address column

- y:

  Practice address column

## Value

character string:

- If x is not `NA` and y is, **`M`**

- If x is `NA` and y is not, **`P`**

- Otherwise, **`MP`**

## Examples

``` r
make_purpose(c("1", NA, NA), c(NA, "2", NA))
#> [1] "M"  "P"  "MP"
```
