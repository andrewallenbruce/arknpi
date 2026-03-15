# NPPES Other Name Type

NPPES Other Name Type

## Usage

``` r
other_name_type(x)
```

## Arguments

- x:

  `<chr>` vector; `other_last_type` or `other_org_type`

## Value

`<chr>` vector of `other_*_type` descriptions

## See also

Other NPPES Decoders:
[`other_id_type()`](https://andrewallenbruce.github.io/arknpi/reference/other_id_type.md)

## Examples

``` r
data.table::data.table(
  oname_type = as.character(1:5),
  oname_desc = other_name_type(as.character(1:5)))
#>    oname_type                 oname_desc
#>        <char>                     <char>
#> 1:          1                Former Name
#> 2:          2          Professional Name
#> 3:          3          Doing Business As
#> 4:          4 Former Legal Business Name
#> 5:          5                 Other Name
```
