# Create an `EprimeFrame` object

This constructor function converts a character vector into an
`EprimeFrame` object, which is just a list with some special metadata
values. Strings with the format `"key: value"` are parsed into
`key = value` list items (via
[`listify()`](https://www.tjmahr.com/rprime/reference/listify.md)).

## Usage

``` r
EprimeFrame(keys_values)
```

## Arguments

- keys_values:

  a character vector containing some `"key: value"` strings.

## Value

a list with the class `EprimeFrame` and with special `Eprime.` metadata,
`Running` and `Procedure` values, all set to `NA` by default.

## Examples

``` r
# Default metadata values
lines <- c(
  "key: value",
  "question: answer",
  "garbage text"
)

EprimeFrame(lines)
#> List of 8
#>  $ Eprime.Level      : num 1
#>  $ Eprime.LevelName  : logi NA
#>  $ Eprime.Basename   : logi NA
#>  $ Eprime.FrameNumber: logi NA
#>  $ Procedure         : logi NA
#>  $ Running           : logi NA
#>  $ key               : chr "value"
#>  $ question          : chr "answer"
#>  - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
#> List of 8
#> $ Eprime.Level      : num 1
#> $ Eprime.LevelName  : logi NA
#> $ Eprime.Basename   : logi NA
#> $ Eprime.FrameNumber: logi NA
#> $ Procedure         : logi NA
#> $ Running           : logi NA
#> $ key               : chr "value"
#> $ question          : chr "answer"

# Normalize [Running] related lines
keys_values <- c(
  "Running: Demo",
  "Demo: ExampleCode",
  "Demo.Cycle: 1",
  "Demo.Sample: 1",
  "Key: Value"
)

EprimeFrame(keys_values)
#> List of 9
#>  $ Eprime.Level      : num 1
#>  $ Eprime.LevelName  : chr "Demo_ExampleCode"
#>  $ Eprime.Basename   : logi NA
#>  $ Eprime.FrameNumber: logi NA
#>  $ Procedure         : logi NA
#>  $ Running           : chr "Demo"
#>  $ Cycle             : chr "1"
#>  $ Sample            : chr "1"
#>  $ Key               : chr "Value"
#>  - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
#> List of 9
#> $ Eprime.Level      : num 1
#> $ Eprime.LevelName  : chr "Demo_ExampleCode"
#> $ Eprime.Basename   : logi NA
#> $ Eprime.FrameNumber: logi NA
#> $ Procedure         : logi NA
#> $ Running           : chr "Demo"
#> $ Cycle             : chr "1"
#> $ Sample            : chr "1"
#> $ Key               : chr "Value"
```
