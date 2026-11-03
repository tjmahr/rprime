# Working with multiple files

Suppose we want to load all the Eprime files in a directory and combine
the results in a dataframe.

My strategy in this scenario is to figure out what I need to do for a
single file and then wrap those steps in a function that takes a
filepath to a text file and returns a dataframe. After some exploration
and interactive programming, I come up with the following function.

``` r

library(plyr)
reduce_sails <- function(sails_path) {
  sails_lines <- read_eprime(sails_path)
  sails_frames <- FrameList(sails_lines)

  # Trials occur at level 3
  sails_frames <- keep_levels(sails_frames, 3)
  sails <- to_data_frame(sails_frames)

  # Tidy up
  to_pick <- c(
    "Eprime.Basename", "Running", "Module", "Sound",
    "Sample", "Correct", "Response"
  )
  sails <- sails[to_pick]
  running_map <- c(TrialLists = "Trial", PracticeBlock = "Practice")
  sails$Running <- running_map[sails$Running]

  # Renumber trials in the practice and experimental blocks separately.
  # Numerically code correct response.
  sails <- ddply(sails, .(Running), mutate,
    TrialNumber = seq(from = 1, to = length(Running)),
    CorrectResponse = ifelse(Correct == Response, 1, 0)
  )
  sails$Sample <- NULL

  # Optionally, one might save the processed file via:
  # csv <- paste0(file_path_sans_ext(sails_path), ".csv")
  # write.csv(sails, csv, row.names = FALSE)
  sails
}
```

Here’s a preview of what the function returns when given a filepath.

``` r

head(reduce_sails(example_eprime_file("SAILS1")))
#>   Eprime.Basename  Running Module     Sound Correct Response TrialNumber
#> 1 SAILS_001X00XS1 Practice   LAKE LAKE1.WAV    Word     Word           1
#> 2 SAILS_001X00XS1 Practice   LAKE  MAKE.WAV NotWord  NotWord           2
#> 3 SAILS_001X00XS1 Practice   LAKE LAKE1.WAV    Word     Word           3
#> 4 SAILS_001X00XS1 Practice   LAKE LAKE1.WAV    Word     Word           4
#> 5 SAILS_001X00XS1 Practice   LAKE  MAKE.WAV NotWord  NotWord           5
#> 6 SAILS_001X00XS1 Practice   LAKE LAKE1.WAV    Word     Word           6
#>   CorrectResponse
#> 1               1
#> 2               1
#> 3               1
#> 4               1
#> 5               1
#> 6               1
```

Now that the function works on one file, I can use
[`ldply()`](https://rdrr.io/pkg/plyr/man/ldply.html) to apply the
function to several files, returning results in a single dataframe. (For
`dplyr`, I would use [`lapply()`](https://rdrr.io/r/base/lapply.html) to
apply the function to each path to get a list of dataframes, then use
`bind_rows()` to combine into a single dataframe.)

``` r

sails_paths <- c(example_eprime_file("SAILS1"), example_eprime_file("SAILS2"))
sails_paths
#> [1] "/home/runner/work/_temp/Library/rprime/example-data/SAILS/SAILS_001X00XS1.txt"
#> [2] "/home/runner/work/_temp/Library/rprime/example-data/SAILS/SAILS_002X00XS1.txt"
ensemble <- ldply(sails_paths, reduce_sails)
```

Finally, with all of the subjects’ data contained in a single dataframe,
I can use [`ddply()`](https://rdrr.io/pkg/plyr/man/ddply.html) with
[`summarise()`](https://rdrr.io/pkg/plyr/man/summarise.html) to compute
summary scores at different levels of aggregation within each subject.

``` r

# Score trials within subjects
overall <- ddply(ensemble, .(Eprime.Basename, Running), summarise,
  Score = sum(CorrectResponse),
  PropCorrect = Score / length(CorrectResponse)
)
overall
#>   Eprime.Basename  Running Score PropCorrect
#> 1 SAILS_001X00XS1 Practice    10   1.0000000
#> 2 SAILS_001X00XS1    Trial    61   0.8714286
#> 3 SAILS_002X00XS1 Practice     9   0.9000000
#> 4 SAILS_002X00XS1    Trial    57   0.8142857

# Score modules within subjects
modules <- ddply(ensemble, .(Eprime.Basename, Running, Module), summarise,
  Score = sum(CorrectResponse),
  PropCorrect = mean(CorrectResponse)
)
modules
#>    Eprime.Basename  Running Module Score PropCorrect
#> 1  SAILS_001X00XS1 Practice   LAKE    10   1.0000000
#> 2  SAILS_001X00XS1    Trial    CAT     9   0.9000000
#> 3  SAILS_001X00XS1    Trial   LAKE    10   1.0000000
#> 4  SAILS_001X00XS1    Trial    RAT    16   0.8000000
#> 5  SAILS_001X00XS1    Trial    SUE    26   0.8666667
#> 6  SAILS_002X00XS1 Practice   LAKE     9   0.9000000
#> 7  SAILS_002X00XS1    Trial    CAT     8   0.8000000
#> 8  SAILS_002X00XS1    Trial   LAKE    10   1.0000000
#> 9  SAILS_002X00XS1    Trial    RAT    11   0.5500000
#> 10 SAILS_002X00XS1    Trial    SUE    28   0.9333333
```
