<!-- README.md is generated from README.Rmd. Please edit that file -->

# rprime

<!-- badges: start -->

[![R-CMD-check](https://github.com/tjmahr/rprime/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tjmahr/rprime/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rprime)](https://CRAN.R-project.org/package=rprime)
<!-- badges: end -->

**rprime** is an R package for parsing `.txt` generated by E-Prime, a
program for running psychological experiments.

## Overview

The main workflow for rprime involves:

1.  `read_eprime`: reliably read in data from an Eprime log (`.txt`)
    file.
2.  `FrameList`: extract the text in each `"LogFrame"` in the file,
    storing each log-frame as an R list.
3.  `preview_levels`, `preview_eprime`: explore the structure of the
    parsed data.
4.  `keep_levels`, `drop_levels`, `filter_in`, `filter_out`: select and
    filter particular levels from the txt-file.
5.  `to_data_frame`: make a data-frame from the parsed data.

## Installation

To get the current, released version from CRAN:

``` r
install.packages("rprime")
```

## Examples

### Getting data into R

Load the file with `read_eprime` and parse its contents with
`FrameList`.

``` r
library("rprime")
# Read in an Eprime text file
experiment_lines <- read_eprime("data/SAILS/SAILS_001X00XS1.txt")

# Extract and parse the log-frames from the file
experiment_data <- FrameList(experiment_lines)
```

### Exploring

In the text file, frames were distinguished by the procedure they are
running as well as the their level of nesting. Get an overview of the
different types of frames with `preview_levels`.

``` r
# There are six different kinds of frames in this file
preview_levels(experiment_data)
#> Level Counts: 
#>  Eprime.Level       Running         Procedure freq
#>             1        Header            Header    1
#>             3 PracticeBlock PracticeTrialProc   10
#>             2  PracticeList      PracticeProc    1
#>             3    TrialLists         TrialProc   70
#>             2     BlockList         BlockProc    7
#>             1          <NA>              <NA>    1
```

Get a preview of the data in each kind of frame with `preview_frames`.

``` r
preview_frames(experiment_data)
#> 
#>  Eprime.Level Running Procedure
#>             1  Header    Header
#> List of 22
#>  $ Eprime.Level       : num 1
#>  $ Eprime.LevelName   : chr "Header_"
#>  $ Eprime.Basename    : chr "SAILS_001X00XS1"
#>  $ Eprime.FrameNumber : chr "1"
#>  $ Procedure          : chr "Header"
#>  $ Running            : chr "Header"
#>  $ VersionPersist     : chr "1"
#>  $ LevelName          : chr "LogLevel10"
#>  $ Experiment         : chr "SAILS"
#>  $ SessionDate        : chr "12-01-2013"
#>  $ SessionTime        : chr "11:00:00"
#>  $ SessionTimeUtc     : chr "5:00:00 PM"
#>  $ Dilaect            : chr "Yes"
#>  $ Subject            : chr "001"
#>  $ ExpTyp             : chr "X"
#>  $ Age                : chr "00"
#>  $ Gender             : chr "X"
#>  $ Session            : chr "1"
#>  $ ChildNativeDialect : chr "S"
#>  $ RandomSeed         : chr "-676084859"
#>  $ Group              : chr "1"
#>  $ Display.RefreshRate: chr "60.004"
#>  - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
#> 
#>  Eprime.Level       Running         Procedure
#>             3 PracticeBlock PracticeTrialProc
#> List of 25
#>  $ Eprime.Level            : num 3
#>  $ Eprime.LevelName        : chr "PracticeBlock_1"
#>  $ Eprime.Basename         : chr "SAILS_001X00XS1"
#>  $ Eprime.FrameNumber      : chr "2"
#>  $ Procedure               : chr "PracticeTrialProc"
#>  $ Running                 : chr "PracticeBlock"
#>  $ Practice                : chr "1"
#>  $ Sound                   : chr "LAKE1.WAV"
#>  $ Correct                 : chr "Word"
#>  $ Module                  : chr "LAKE"
#>  $ Color                   : chr "red"
#>  $ WordImage               : chr "lake"
#>  $ NonWordImage            : chr "notlake"
#>  $ PuzzleImage             : chr "up"
#>  $ Cycle                   : chr "1"
#>  $ Sample                  : chr "1"
#>  $ TrialSlide.OnsetDelay   : chr "197"
#>  $ TrialSlide.OnsetTime    : chr "899413"
#>  $ TrialSlide.DurationError: chr "-999999"
#>  $ TrialSlide.RTTime       : chr "0"
#>  $ TrialSlide.ACC          : chr "0"
#>  $ TrialSlide.RT           : chr "0"
#>  $ TrialSlide.RESP         : chr ""
#>  $ TrialSlide.CRESP        : chr ""
#>  $ Response                : chr "Word"
#>  - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
#> 
#>  Eprime.Level      Running    Procedure
#>             2 PracticeList PracticeProc
#> List of 11
#>  $ Eprime.Level      : num 2
#>  $ Eprime.LevelName  : chr "PracticeList_1"
#>  $ Eprime.Basename   : chr "SAILS_001X00XS1"
#>  $ Eprime.FrameNumber: chr "12"
#>  $ Procedure         : chr "PracticeProc"
#>  $ Running           : chr "PracticeList"
#>  $ Cycle             : chr "1"
#>  $ Sample            : chr "1"
#>  $ PuzzleMovie       : chr "up"
#>  $ TrainingNonWord   : chr "notlake"
#>  $ TrainingWord      : chr "lake"
#>  - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
#> 
#>  Eprime.Level    Running Procedure
#>             3 TrialLists TrialProc
#> List of 25
#>  $ Eprime.Level            : num 3
#>  $ Eprime.LevelName        : chr "TrialLists_1"
#>  $ Eprime.Basename         : chr "SAILS_001X00XS1"
#>  $ Eprime.FrameNumber      : chr "13"
#>  $ Procedure               : chr "TrialProc"
#>  $ Running                 : chr "TrialLists"
#>  $ Sound                   : chr "LAKE23B.WAV"
#>  $ Correct                 : chr "Word"
#>  $ LakeLevel1              : chr "8"
#>  $ Module                  : chr "LAKE"
#>  $ Color                   : chr "blue"
#>  $ WordImage               : chr "lake"
#>  $ NonWordImage            : chr "notlake"
#>  $ PuzzleImage             : chr "butterfly"
#>  $ TrialSlide.OnsetDelay   : chr "119"
#>  $ TrialSlide.OnsetTime    : chr "1023939"
#>  $ TrialSlide.DurationError: chr "-999999"
#>  $ TrialSlide.RTTime       : chr "0"
#>  $ TrialSlide.ACC          : chr "0"
#>  $ TrialSlide.RT           : chr "0"
#>  $ TrialSlide.RESP         : chr ""
#>  $ TrialSlide.CRESP        : chr ""
#>  $ Response                : chr "Word"
#>  $ Cycle                   : chr "1"
#>  $ Sample                  : chr "1"
#>  - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
#> 
#>  Eprime.Level   Running Procedure
#>             2 BlockList BlockProc
#> List of 11
#>  $ Eprime.Level      : num 2
#>  $ Eprime.LevelName  : chr "BlockList_1"
#>  $ Eprime.Basename   : chr "SAILS_001X00XS1"
#>  $ Eprime.FrameNumber: chr "23"
#>  $ Procedure         : chr "BlockProc"
#>  $ Running           : chr "BlockList"
#>  $ PuzzleMovie       : chr "butterfly"
#>  $ TrainingNonWord   : chr "notlake"
#>  $ TrainingWord      : chr "lake"
#>  $ Cycle             : chr "1"
#>  $ Sample            : chr "1"
#>  - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
#> 
#>  Eprime.Level Running Procedure
#>             1    <NA>      <NA>
#> List of 20
#>  $ Eprime.Level       : num 1
#>  $ Eprime.LevelName   : logi NA
#>  $ Eprime.Basename    : chr "SAILS_001X00XS1"
#>  $ Eprime.FrameNumber : chr "90"
#>  $ Procedure          : logi NA
#>  $ Running            : logi NA
#>  $ Experiment         : chr "SAILS"
#>  $ SessionDate        : chr "12-01-2013"
#>  $ SessionTime        : chr "11:00:00"
#>  $ SessionTimeUtc     : chr "5:00:00 PM"
#>  $ Dilaect            : chr "Yes"
#>  $ Subject            : chr "001"
#>  $ ExpTyp             : chr "X"
#>  $ Age                : chr "00"
#>  $ Gender             : chr "X"
#>  $ Session            : chr "1"
#>  $ ChildNativeDialect : chr "S"
#>  $ RandomSeed         : chr "-676084859"
#>  $ Group              : chr "1"
#>  $ Display.RefreshRate: chr "60.004"
#>  - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
```

`preview_eprime` (not demonstrated here) does both kinds of previews
(levels and frames).

### Filtering

Use `keep_levels` and `drop_levels` to filter frames according to
nesting level.

``` r
# Filter (out) by depth of nesting
not_level_1 <- drop_levels(experiment_data, 1)
preview_levels(not_level_1)
#> Level Counts: 
#>  Eprime.Level       Running         Procedure freq
#>             3 PracticeBlock PracticeTrialProc   10
#>             2  PracticeList      PracticeProc    1
#>             3    TrialLists         TrialProc   70
#>             2     BlockList         BlockProc    7

# Filter (in) by depth of nesting
just_level_3 <- keep_levels(experiment_data, 3)
preview_levels(just_level_3)
#> Level Counts: 
#>  Eprime.Level       Running         Procedure freq
#>             3 PracticeBlock PracticeTrialProc   10
#>             3    TrialLists         TrialProc   70
```

Use `filter_in` and `filter_out` to filter frames using attribute
values. Use repeated filtering statements to drill down into the list of
frames.

``` r
# Filter (out) by attribute values
no_header <- filter_out(experiment_data, "Running", values = "Header")
preview_levels(no_header)
#> Level Counts: 
#>  Eprime.Level       Running         Procedure freq
#>             3 PracticeBlock PracticeTrialProc   10
#>             2  PracticeList      PracticeProc    1
#>             3    TrialLists         TrialProc   70
#>             2     BlockList         BlockProc    7
#>             1          <NA>              <NA>    1

# Filter (in) by attribute values
not_practice <- filter_in(experiment_data, "Running", "TrialLists")
preview_levels(not_practice)
#> Level Counts: 
#>  Eprime.Level    Running Procedure freq
#>             3 TrialLists TrialProc   70

# Drill down further into the trials by filtering again
sue_trials <- filter_in(not_practice, "Module", "SUE")
preview_eprime(sue_trials)
#> Level Counts: 
#>  Eprime.Level    Running Procedure freq
#>             3 TrialLists TrialProc   30
#> 
#>  Eprime.Level    Running Procedure
#>             3 TrialLists TrialProc
#> List of 25
#>  $ Eprime.Level            : num 3
#>  $ Eprime.LevelName        : chr "TrialLists_5"
#>  $ Eprime.Basename         : chr "SAILS_001X00XS1"
#>  $ Eprime.FrameNumber      : chr "57"
#>  $ Procedure               : chr "TrialProc"
#>  $ Running                 : chr "TrialLists"
#>  $ Sound                   : chr "SUE4.WAV"
#>  $ Correct                 : chr "Word"
#>  $ SueLevel1               : chr "10"
#>  $ Module                  : chr "SUE"
#>  $ Color                   : chr "blue"
#>  $ WordImage               : chr "sue"
#>  $ NonWordImage            : chr "notsue"
#>  $ PuzzleImage             : chr "madagascar"
#>  $ TrialSlide.OnsetDelay   : chr "127"
#>  $ TrialSlide.OnsetTime    : chr "1360767"
#>  $ TrialSlide.DurationError: chr "-999999"
#>  $ TrialSlide.RTTime       : chr "0"
#>  $ TrialSlide.ACC          : chr "0"
#>  $ TrialSlide.RT           : chr "0"
#>  $ TrialSlide.RESP         : chr ""
#>  $ TrialSlide.CRESP        : chr ""
#>  $ Response                : chr "Word"
#>  $ Cycle                   : chr "1"
#>  $ Sample                  : chr "41"
#>  - attr(*, "class")= chr [1:2] "EprimeFrame" "list"
```

### Exporting

Convert to a dataframe with `to_dataframe`. Attribute names in the
log-frames become column names in the dataframe.

``` r
# Export to dataframe
sue_trials_df <- to_data_frame(sue_trials)
str(sue_trials_df)
#> 'data.frame':    30 obs. of  27 variables:
#>  $ Eprime.Level            : num  3 3 3 3 3 ...
#>  $ Eprime.LevelName        : chr  "TrialLists_5" "TrialLists_5" ...
#>  $ Eprime.Basename         : chr  "SAILS_001X00XS1" "SAILS_001X00XS1" ...
#>  $ Eprime.FrameNumber      : chr  "57" "58" ...
#>  $ Procedure               : chr  "TrialProc" "TrialProc" ...
#>  $ Running                 : chr  "TrialLists" "TrialLists" ...
#>  $ Sound                   : chr  "SUE4.WAV" "SUE15.WAV" ...
#>  $ Correct                 : chr  "Word" "NotWord" ...
#>  $ SueLevel1               : chr  "10" "2" ...
#>  $ Module                  : chr  "SUE" "SUE" ...
#>  $ Color                   : chr  "blue" "blue" ...
#>  $ WordImage               : chr  "sue" "sue" ...
#>  $ NonWordImage            : chr  "notsue" "notsue" ...
#>  $ PuzzleImage             : chr  "madagascar" "madagascar" ...
#>  $ TrialSlide.OnsetDelay   : chr  "127" "98" ...
#>  $ TrialSlide.OnsetTime    : chr  "1360767" "1364450" ...
#>  $ TrialSlide.DurationError: chr  "-999999" "-999999" ...
#>  $ TrialSlide.RTTime       : chr  "0" "0" ...
#>  $ TrialSlide.ACC          : chr  "0" "0" ...
#>  $ TrialSlide.RT           : chr  "0" "0" ...
#>  $ TrialSlide.RESP         : chr  "" "" ...
#>  $ TrialSlide.CRESP        : chr  "" "" ...
#>  $ Response                : chr  "Word" "NotWord" ...
#>  $ Cycle                   : chr  "1" "1" ...
#>  $ Sample                  : chr  "41" "42" ...
#>  $ SueLevel2               : chr  NA NA ...
#>  $ SueLevel3               : chr  NA NA ...

# Don't need every column
columns_to_keep <- c("Eprime.Basename", "Module", "Sample", 
                     "Correct", "Response")
sue_trials_df <- sue_trials_df[columns_to_keep]
head(sue_trials_df)
#>   Eprime.Basename Module Sample Correct Response
#> 1 SAILS_001X00XS1    SUE     41    Word     Word
#> 2 SAILS_001X00XS1    SUE     42 NotWord  NotWord
#> 3 SAILS_001X00XS1    SUE     43 NotWord     Word
#> 4 SAILS_001X00XS1    SUE     44    Word     Word
#> 5 SAILS_001X00XS1    SUE     45 NotWord  NotWord
#> 6 SAILS_001X00XS1    SUE     46    Word     Word
```

**Note**: rprime thinks that all the values in the final dataframe are
character values. You can use `type_convert` in the readr package to
correct the column types:

``` r
# Right now the sample numbers are stored as character values
str(sue_trials_df)
#> 'data.frame':    30 obs. of  5 variables:
#>  $ Eprime.Basename: chr  "SAILS_001X00XS1" "SAILS_001X00XS1" ...
#>  $ Module         : chr  "SUE" "SUE" ...
#>  $ Sample         : chr  "41" "42" ...
#>  $ Correct        : chr  "Word" "NotWord" ...
#>  $ Response       : chr  "Word" "NotWord" ...

library("readr")
sue_trials_df <- type_convert(sue_trials_df)
#> 
#> ── Column specification ────────────────────────────────────────────────────────
#> cols(
#>   Eprime.Basename = col_character(),
#>   Module = col_character(),
#>   Sample = col_double(),
#>   Correct = col_character(),
#>   Response = col_character()
#> )
# Now, they are stored as integers...
str(sue_trials_df)
#> 'data.frame':    30 obs. of  5 variables:
#>  $ Eprime.Basename: chr  "SAILS_001X00XS1" "SAILS_001X00XS1" ...
#>  $ Module         : chr  "SUE" "SUE" ...
#>  $ Sample         : num  41 42 43 44 45 ...
#>  $ Correct        : chr  "Word" "NotWord" ...
#>  $ Response       : chr  "Word" "NotWord" ...
```
