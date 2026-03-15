# Make a case statement

Make a case statement

## Usage

``` r
make_case(x, y)
```

## Arguments

- x:

  A character vector

- y:

  A character vector

## Value

character string:

- If x & y are not `NA` and equal, **`y`**

- If x & y are not `NA` and not equal, **`x, y`**

- If x is not `NA` and y is, **`x`**

- If x is `NA` and y is not, **`y`**

- Otherwise, **`NA`**

## Examples

``` r
make_case(
   c("1", "1", "3", NA),
   c("1", "2", NA, "4"))
#> [1] "1"    "1, 2" "3"    "4"   
```
