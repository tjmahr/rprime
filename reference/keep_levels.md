# Filter levels in or out of a `FrameList` based on `Eprime.Level` values

These functions are shortcuts for calls to
[`filter_in()`](https://www.tjmahr.com/rprime/reference/filter_in.md) or
[`filter_out()`](https://www.tjmahr.com/rprime/reference/filter_in.md).

## Usage

``` r
keep_levels(frame_list, level_numbers)

drop_levels(frame_list, level_numbers)
```

## Arguments

- frame_list:

  a list of
  [`EprimeFrame()`](https://www.tjmahr.com/rprime/reference/EprimeFrame.md)
  objects

- level_numbers:

  the included or excluded values for `Eprime.Level`

## Value

for `keep_levels()`, only log-frames where the level matches one of the
`level_numbers` are kept. For `drop_levels()`, log-frames where the
level matches one of the `level_numbers` are omitted.

## Details

Note that the meaning of the `Eprime.Level` value in a log-frame
ultimately is equal to one plus the number of tabs before each line in
the log-frame.
