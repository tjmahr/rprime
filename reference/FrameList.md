# Convert lines from an Eprime file into `EprimeFrame` objects

Convert character vectors of implicit key-value pairs (e.g.,
`c("key1: value1", "key2: value2")`) into lists of explicit key-value
pairs, `list(key1 = "value1", key2 = "value2")`.

## Usage

``` r
FrameList(x)
```

## Arguments

- x:

  a character vector with lines of the form `"key: value"`, or a list of
  vectors of colon-separated text

## Value

When passed a list of character vectors of `"key: value"` lines, a
`FrameList()` object (a list of
[`EprimeFrame()`](https://www.tjmahr.com/rprime/reference/EprimeFrame.md)
objects) is returned. When passed a single vector of `"key: value"`
lines, a single
[`EprimeFrame()`](https://www.tjmahr.com/rprime/reference/EprimeFrame.md)
object is returned inside of a `FrameList()` object.

## Details

During the conversion, if `Running: x`, then the `x.Sample` and
`x.Cycle` lines are simplified into `Sample` and `Cycle` lines. The
`x: value` line is recoded as `Eprime.LevelName: x_value`. The purpose
of this tidying is to force the same set of key names (eventually,
column names) onto frames with different values for `Running`.

## Examples

``` r
lines <- c(
  "\t*** LogFrame Start ***",
  "\tProcedure: FamTask",
  "\titem1: bear",
  "\titem2: chair",
  "\tCorrectResponse: bear",
  "\tImageSide: Left",
  "\tDuration: 885",
  "\tFamiliarization: 1",
  "\tFamInforcer: 1",
  "\tReinforcerImage: Bicycle1",
  "\tFamiliarization.Cycle: 1",
  "\tFamiliarization.Sample: 1",
  "\tRunning: Familiarization",
  "\tFamTarget.RESP: ",
  "\tCorrect: True",
  "\t*** LogFrame End ***"
)
#> List of 1
#> $ :List of 17
#> ..$ Eprime.Level      : num 2
#> ..$ Eprime.LevelName  : chr "Familiarization_1"
#> ..$ Eprime.Basename   : chr "NA"
#> ..$ Eprime.FrameNumber: chr "1"
#> ..$ Procedure         : chr "FamTask"
#> ..$ Running           : chr "Familiarization"
#> ..$ item1             : chr "bear"
#> ..$ item2             : chr "chair"
#> ..$ CorrectResponse   : chr "bear"
#> ..$ ImageSide         : chr "Left"
#> ..$ Duration          : chr "885"
#> ..$ FamInforcer       : chr "1"
#> ..$ ReinforcerImage   : chr "Bicycle1"
#> ..$ Cycle             : chr "1"
#> ..$ Sample            : chr "1"
#> ..$ FamTarget.RESP    : chr ""
#> ..$ Correct           : chr "True"
#> ..- attr(*, "class")= chr [1:2] "EprimeFrame" "list"
#> - attr(*, "class")= chr [1:2] "list" "FrameList"
```
