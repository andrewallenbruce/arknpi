# NPPES Other Identifier Type

NPPES Other Identifier Type

## Usage

``` r
other_id_type(x)
```

## Arguments

- x:

  `<chr>` vector of `otid` types

## Value

`<chr>` vector of `otid` descriptions

## See also

Other NPPES Decoders:
[`other_name_type()`](https://andrewallenbruce.github.io/arknpi/reference/other_name_type.md)

## Examples

``` r
data.table::data.table(
  otid_type = as.character(1:8),
  otid_desc = other_id_type(as.character(1:8)))
#>    otid_type                    otid_desc
#>       <char>                       <char>
#> 1:         1                        Other
#> 2:         2                Medicare UPIN
#> 3:         3                         <NA>
#> 4:         4 Medicare ID-Type Unspecified
#> 5:         5                     Medicaid
#> 6:         6 Medicare Oscar-Certification
#> 7:         7                 Medicare NSC
#> 8:         8                 Medicare PIN
```
