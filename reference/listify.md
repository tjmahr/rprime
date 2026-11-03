# Convert a vector of colon-separated text lines into a list of named elements

Convert a vector of colon-separated text lines into a list of named
elements

## Usage

``` r
listify(colon_sep_xs)
```

## Arguments

- colon_sep_xs:

  a character vector with lines of the form `"key: value"`

## Value

a named list of the values in the colon-separated lines. `"key: value"`
yields `list(key = "value")`

## Details

Some minor cleaning of the input is performed:

- Lines without a colon-space separator `": "` are filtered out.

- Once the strings are split at the separator, white-space on the left
  and right sides of each half-string is omitted.
