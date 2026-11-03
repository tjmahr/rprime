# Parsing summary

Eprime, or more officially E-Prime® 2.0, is a set of programs by
[Psychology Software Tools, Inc.](https://www.pstnet.com/) used for
running psychological experiments. Data from experiment sessions are
saved into two files: 1) a proprietary binary `.edat2` file, and 2) a
plain-text `.txt` file. (File formats used by Eprime are documented on
the site at this URL:
`https://support.pstnet.com/hc/en-us/articles/229354727-INFO-E-Prime-file-extensions-18091`.)
The rprime package provides a set of functions for parsing these `.txt`
files inside R.

This vignette documents how rprime parses an Eprime text file and the
changes it makes during that process. **See the quick-start vignette for
examples of how to use this package.**

**Disclaimer**: I write this documentation as someone who has spent
little time creating and designing experiments in Eprime but has spent a
lot of time parsing Eprime text files in R. Therefore, this vignette is
not documentation for Eprime software or its data format. Instead, this
vignette just outlines this package’s approach to extracting data from
Eprime text files.

## Background

An Eprime `txt` file is a series of **frames**. Each frame is a list of
key-value pairs. Frames are bracketed by special start and stop lines.

Each file begins with a special **Header** frame. The header is set off
by the brackets `*** Header Start ***` and `*** Header End ***`. The
fields in the header describe the basic settings of the experiment. A
header frame looks like something like this (the `Clock.Information`
line, several hundred characters long, has been omitted):

    *** Header Start ***
    VersionPersist: 1
    LevelName: Session
    LevelName: Block
    LevelName: Trial
    LevelName: SubTrial
    LevelName: LogLevel5
    LevelName: LogLevel6
    LevelName: LogLevel7
    LevelName: LogLevel8
    LevelName: LogLevel9
    LevelName: LogLevel10
    Experiment: SAE_MinimalPairDiscrim
    SessionDate: 07-10-2013
    SessionTime: 10:46:22
    SessionTimeUtc: 3:46:22 PM
    Dialect: Yes
    Subject: 001
    ExpTyp: L
    Age: 00
    Gender: X
    Session: 1
    RandomSeed: -261296366
    Group: 1
    Display.RefreshRate: 60.003
    *** Header End ***

The other frames in the text file are **LogFrames**. These record data
about the individual procedures or trials that are executed during the
experiment. The following lines immediately follow the header from the
previous example:

        Level: 2
        *** LogFrame Start ***
        Procedure: FamTask
        item1: bear
        item2: chair
        CorrectResponse: bear
        ImageSide: Left
        Duration: 885
        Familiarization: 1
        FamInforcer: 1
        ReinforcerImage: Bicycle1
        Familiarization.Cycle: 1
        Familiarization.Sample: 1
        Running: Familiarization
        FamTarget.RESP:
        Correct: True
        *** LogFrame End ***

Trials and procedures can be **nested** inside of higher-order sections
of the experiment. For example, practice trials may be nested inside of
a practice block of an experiment. The level of nesting in the above
frame is indicated by the number of tabs before each line as well as the
`Level` line. The only Level 1 frames in an experiment appear to be the
header frame and the final frame of an experiment (which largely
duplicates the header frame).

Some **special fields** appear in every log-frame. These fields are
`Procedure`, `Running`, `[Running].Sample`, `[Running].Cycle` and
`[Running]` where `[Running]` is the value of the running field. In the
previous example, these were:

        Procedure: FamTask
        Familiarization.Cycle: 1
        Familiarization.Sample: 1
        Running: Familiarization
        Familiarization: 1

When trials are presented in a random order, the `[Running].Sample`
field records the sequential trial number.

## Strategy

The basic strategy for parsing the Eprime file:

1.  Read in the Eprime file.
2.  Create `EprimeFrame` objects.
    1.  Extract the frames.
    2.  Create key-value pairs inside the frame by splitting text lines
        at `:` characters.
    3.  Add some helpful metadata inside each frame.
    4.  Bundle up the key-value pairs into an R `list` object with an
        added `EprimeFrame` class.
3.  Bundle the frames from an experiment together inside of a
    `FrameList` object.

Each of these steps is handled by some lower-level functions. **In
practice, only two high-level functions are necessary to transform from
a text file to a list of `EprimeFrame` objects:
[`read_eprime()`](https://www.tjmahr.com/rprime/reference/read_eprime.md)
and
[`FrameList()`](https://www.tjmahr.com/rprime/reference/FrameList.md).**
Here’s how these two functions can be used to get us the header frame
from a text file.

``` r

library(rprime)
eprime_lists <- FrameList(read_eprime(example_eprime_file("MINP")))
eprime_lists[[1]]
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
```

### File reading

Lines from the text file are read into R and stored into a character
vector. Here are the first few lines from the example file:

``` r

exp_lines <- read_eprime(example_eprime_file("MINP"))
head(exp_lines)
#> [1] "*** Header Start ***" "VersionPersist: 1"    "LevelName: Session"  
#> [4] "LevelName: Block"     "LevelName: Trial"     "LevelName: SubTrial"
```

Some things to note about file reading. By default, the enormous
`Clock.Information` lines are omitted. Also, if the file is not an
Eprime `.txt` file, a **dummy header** is created so that the file may
be treated like any other Eprime file. The user is warned as this
happens.

``` r

bad_lines <- read_eprime(example_eprime_file("not_an_eprime_file"))
#> Warning in read_eprime(example_eprime_file("not_an_eprime_file")):
#> /home/runner/work/_temp/Library/rprime/example-data/not_an_eprime_file.txt is
#> not an Eprime txt file. Dummy text will be used instead.
head(bad_lines)
#> [1] "*** Header Start ***" "*** Header End ***"
```

I chose to use a dummy header instead of raising an error so that code
which loads multiple files at once will not fail outright when it
encounters a bad file.

### Chunking

The next step in parsing the file is to extract all the frames. This is
accomplished by pulling out lines of text that fall between a pair of
bracketing lines. When there is no closing bracket, as when an
experiment is aborted, a warning is raised and the partial frame is
skipped.

``` r

# Experiment aborted on trial 3
aborted <- FrameList(read_eprime(example_eprime_file("MP")))
#> Warning in make_ranges(starts, ends, eprime_log): Incomplete Log Frame found on line 72
#>      *** LogFrame Start ***
#>      TrialList: 3
#>      Procedure: TrialProcedure
#>      ImageL: marmoset1
#>      ImageR: girl1
#>      Carrier: fin
#>      Target: ImageR
#>      AudioStim: AAE_Fin_girl_312_10
#>      Attention: AAE_check2_10
#>      CarrierDur: 1858
#>      AttentionDur: 914
```

The low-level function for chunking the lines of text is
[`extract_chunks()`](https://www.tjmahr.com/rprime/reference/extract_chunks.md).
During the chunking, some metadata is inserted into the frames as
additional `Key: Value` lines. These fields are:

1.  `Eprime.FrameNumber`, the number of the log frame in the file
2.  `Eprime.Basename`, the
    [`basename()`](https://rdrr.io/r/base/basename.html) of the source
    file, without its extension
3.  The header frame also gets the lines `Procedure: Header` and
    `Running: Header`.

The result of chunking is a list of character vectors. Here’s how the
second frame looks after chunking.

``` r

chunks <- extract_chunks(exp_lines)
chunks[[2]]
#>  [1] "\t*** LogFrame Start ***"        "\tProcedure: FamTask"           
#>  [3] "\titem1: bear"                   "\titem2: chair"                 
#>  [5] "\tCorrectResponse: bear"         "\tImageSide: Left"              
#>  [7] "\tDuration: 885"                 "\tFamiliarization: 1"           
#>  [9] "\tFamInforcer: 1"                "\tReinforcerImage: Bicycle1"    
#> [11] "\tFamiliarization.Cycle: 1"      "\tFamiliarization.Sample: 1"    
#> [13] "\tRunning: Familiarization"      "\tFamTarget.RESP: "             
#> [15] "\tCorrect: True"                 "Eprime.FrameNumber: 2"          
#> [17] "Eprime.Basename: MINP_001L00XS1" "\t*** LogFrame End ***"         
#> attr(,"class")
#> [1] "EprimeChunk" "character"
```

### Building lists from “Key: Value” strings

The data from each log-frame have been stored as a character vector in a
list. Next, we convert each vector of `"Key: Value"` strings into a list
of named elements.
[`EprimeFrame()`](https://www.tjmahr.com/rprime/reference/EprimeFrame.md)
carries out this task.

During this stage, the special fields (noted above) are parsed. The
original form of the special fields is:

       Running: [Key]
       [Key]: [Value]
       [Key].Cycle: [Cycle]
       [Key].Sample: [Sample]

These `[Key]` values make it harder to merge together dataframes later
on, since each unique `[Key]` gets its own column name. Therefore, we
normalize these fields like so:

1.  The `[Key]: [Value]` line is deleted and stored in the
    `Eprime.LevelName` field instead as `"[Key]_[Value]"`.
2.  `[Key].Sample` and `[Key].Cycle` are renamed to just `Sample` and
    `Cycle`, respectively.

One additional field is added, `Eprime.Level`, to record the depth of
nesting (equal to the number of tabs plus one).

Here is how the second frame appears after this stage of parsing:

``` r

EprimeFrame(chunks[[2]])
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
```
