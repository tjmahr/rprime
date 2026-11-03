# Changelog

## rprime 0.1.4

- [`example_eprime_file()`](https://www.tjmahr.com/rprime/reference/example_eprime_file.md)
  provides filepaths to bundled example files by short name or full
  filename. Call it without arguments to list available files.
- [`filter_in()`](https://www.tjmahr.com/rprime/reference/filter_in.md),
  [`filter_out()`](https://www.tjmahr.com/rprime/reference/filter_in.md),
  [`keep_levels()`](https://www.tjmahr.com/rprime/reference/keep_levels.md),
  and
  [`drop_levels()`](https://www.tjmahr.com/rprime/reference/keep_levels.md)
  now handle empty inputs, including results from earlier filters with
  no matches.
- Fix a failing CRAN check following changes to
  [`str()`](https://rdrr.io/r/utils/str.html).
  ([\#16](https://github.com/tjmahr/rprime/issues/16))
- [`preview_eprime()`](https://www.tjmahr.com/rprime/reference/preview_eprime.md),
  [`preview_frames()`](https://www.tjmahr.com/rprime/reference/preview_eprime.md),
  and
  [`preview_levels()`](https://www.tjmahr.com/rprime/reference/preview_eprime.md)
  now return their input invisibly, allowing previews within a
  data-processing pipeline.
- [`to_data_frame()`](https://www.tjmahr.com/rprime/reference/to_data_frame.md)
  now has tests covering its methods, metadata, missing columns, column
  types, and empty inputs
  ([\#3](https://github.com/tjmahr/rprime/issues/3)).
- Package upkeep. ([\#17](https://github.com/tjmahr/rprime/issues/17))

## rprime 0.1.3

CRAN release: 2025-04-25

- Fix link in a documentation.

## rprime 0.1.2

CRAN release: 2020-09-24

- Fixed package for compatibility with stringi 1.5.3.
- Added a `NEWS.md` file to track changes to the package.
