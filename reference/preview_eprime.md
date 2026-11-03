# Preview the levels in a parsed Eprime file

Preview the levels in a parsed Eprime file

## Usage

``` r
preview_eprime(frame_list)

preview_levels(frame_list)

preview_frames(frame_list)
```

## Arguments

- frame_list:

  a
  [`FrameList()`](https://www.tjmahr.com/rprime/reference/FrameList.md)
  object (a list of
  [`EprimeFrame()`](https://www.tjmahr.com/rprime/reference/EprimeFrame.md)
  objects)

## Value

The input
[`FrameList()`](https://www.tjmahr.com/rprime/reference/FrameList.md)
object, invisibly. Preview text is printed to the console.

## Details

`preview_levels()` prints out the unique combinations of `Eprime.Level`
number, `Procedure`, and `Running` in the frame list. `preview_frames()`
prints out an example frame from each of the unique levels.
`preview_eprime()` does both.

## Examples

``` r
data <- read_eprime(example_eprime_file("MINP")) |>
  FrameList() |>
  preview_eprime()
#> Level Counts: 
#>  Eprime.Level         Running Procedure freq
#>             1          Header    Header    1
#>             2 Familiarization   FamTask    2
#>             2 Familiarization  DiscTask    4
#>             2            Test  DiscTask   30
#>             1            <NA>      <NA>    1
#> 
#>  Eprime.Level Running Procedure
#>             1  Header    Header
#> List of 21
#>  $ Eprime.Level       : num 1
#>  $ Eprime.LevelName   : chr "Header_"
#>  $ Eprime.Basename    : chr "MINP_001L00XS1"
#>  $ Eprime.FrameNumber : chr "1"
#>  $ Procedure          : chr "Header"
#>  $ Running            : chr "Header"
#>  $ VersionPersist     : chr "1"
#>  $ LevelName          : chr "LogLevel10"
#>  $ Experiment         : chr "SAE_MinimalPairDiscrim"
#>  $ SessionDate        : chr "07-10-2013"
#>  $ SessionTime        : chr "10:46:22"
#>  $ SessionTimeUtc     : chr "3:46:22 PM"
#>  $ Dialect            : chr "Yes"
#>  $ Subject            : chr "001"
#>  $ ExpTyp             : chr "L"
#>  $ Age                : chr "00"
#>  $ Gender             : chr "X"
#>  $ Session            : chr "1"
#>  $ RandomSeed         : chr "-261296366"
#>  $ Group              : chr "1"
#>  $ Display.RefreshRate: chr "60.003"
#>  - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
#> 
#>  Eprime.Level         Running Procedure
#>             2 Familiarization   FamTask
#> List of 17
#>  $ Eprime.Level      : num 2
#>  $ Eprime.LevelName  : chr "Familiarization_1"
#>  $ Eprime.Basename   : chr "MINP_001L00XS1"
#>  $ Eprime.FrameNumber: chr "2"
#>  $ Procedure         : chr "FamTask"
#>  $ Running           : chr "Familiarization"
#>  $ item1             : chr "bear"
#>  $ item2             : chr "chair"
#>  $ CorrectResponse   : chr "bear"
#>  $ ImageSide         : chr "Left"
#>  $ Duration          : chr "885"
#>  $ FamInforcer       : chr "1"
#>  $ ReinforcerImage   : chr "Bicycle1"
#>  $ Cycle             : chr "1"
#>  $ Sample            : chr "1"
#>  $ FamTarget.RESP    : chr ""
#>  $ Correct           : chr "True"
#>  - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
#> 
#>  Eprime.Level         Running Procedure
#>             2 Familiarization  DiscTask
#> List of 17
#>  $ Eprime.Level      : num 2
#>  $ Eprime.LevelName  : chr "Familiarization_3"
#>  $ Eprime.Basename   : chr "MINP_001L00XS1"
#>  $ Eprime.FrameNumber: chr "4"
#>  $ Procedure         : chr "DiscTask"
#>  $ Running           : chr "Familiarization"
#>  $ item1             : chr "man"
#>  $ item2             : chr "moon"
#>  $ CorrectResponse   : chr "moon"
#>  $ ImageSide         : chr "Right"
#>  $ Duration          : chr "1045"
#>  $ FamInforcer       : chr "1"
#>  $ ReinforcerImage   : chr "Bicycle1"
#>  $ Cycle             : chr "1"
#>  $ Sample            : chr "3"
#>  $ Correct           : chr "True"
#>  $ Target.RESP       : chr ""
#>  - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
#> 
#>  Eprime.Level Running Procedure
#>             2    Test  DiscTask
#> List of 17
#>  $ Eprime.Level      : num 2
#>  $ Eprime.LevelName  : chr "Test_5"
#>  $ Eprime.Basename   : chr "MINP_001L00XS1"
#>  $ Eprime.FrameNumber: chr "8"
#>  $ Procedure         : chr "DiscTask"
#>  $ Running           : chr "Test"
#>  $ item1             : chr "keys"
#>  $ item2             : chr "cheese"
#>  $ CorrectResponse   : chr "keys"
#>  $ ImageSide         : chr "Left"
#>  $ Duration          : chr "1062"
#>  $ ReinforcerImage   : chr "Bicycle1"
#>  $ Reinforcer        : chr "1"
#>  $ Correct           : chr "False"
#>  $ Target.RESP       : chr ""
#>  $ Cycle             : chr "1"
#>  $ Sample            : chr "1"
#>  - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
#> 
#>  Eprime.Level Running Procedure
#>             1    <NA>      <NA>
#> List of 19
#>  $ Eprime.Level       : num 1
#>  $ Eprime.LevelName   : logi NA
#>  $ Eprime.Basename    : chr "MINP_001L00XS1"
#>  $ Eprime.FrameNumber : chr "38"
#>  $ Procedure          : logi NA
#>  $ Running            : logi NA
#>  $ Experiment         : chr "SAE_MinimalPairDiscrim"
#>  $ SessionDate        : chr "07-10-2013"
#>  $ SessionTime        : chr "10:46:22"
#>  $ SessionTimeUtc     : chr "3:46:22 PM"
#>  $ Dialect            : chr "Yes"
#>  $ Subject            : chr "001"
#>  $ ExpTyp             : chr "L"
#>  $ Age                : chr "00"
#>  $ Gender             : chr "X"
#>  $ Session            : chr "1"
#>  $ RandomSeed         : chr "-261296366"
#>  $ Group              : chr "1"
#>  $ Display.RefreshRate: chr "60.003"
#>  - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
```
