# Convert `EprimeFrame` objects into dataframes

Convert `EprimeFrame` objects into dataframes

## Usage

``` r
to_data_frame(x)
```

## Arguments

- x:

  an
  [`EprimeFrame()`](https://www.tjmahr.com/rprime/reference/EprimeFrame.md)
  object, or a
  [`FrameList()`](https://www.tjmahr.com/rprime/reference/FrameList.md)
  object (a list of
  [`EprimeFrame()`](https://www.tjmahr.com/rprime/reference/EprimeFrame.md)
  objects)

## Value

all of the
[`EprimeFrame()`](https://www.tjmahr.com/rprime/reference/EprimeFrame.md)
objects combined into a single dataframe.

## Details

Individual
[`EprimeFrame()`](https://www.tjmahr.com/rprime/reference/EprimeFrame.md)
objects are converted to a dataframe using
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html). (Strings
are not converted to factors.)

The individual dataframes are then
[`rbind()`](https://rdrr.io/r/base/cbind.html)-ed together, with missing
columns being filled with `NA`.

## See also

[`plyr::rbind.fill()`](https://rdrr.io/pkg/plyr/man/rbind.fill.html)
