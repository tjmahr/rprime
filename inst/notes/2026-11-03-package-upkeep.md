---
status: complete
---

# Package upkeep — 2026-11-03

Maintenance work for [#17](https://github.com/tjmahr/rprime/issues/17), prompted
by the [CRAN failure in #16](https://github.com/tjmahr/rprime/issues/16).
Upkeep is complete; the original checklist was optional, not a requirement to
modernize everything. Keep future changes small for this maintenance-mode package.

## Completed

- Fixed the CRAN failure caused by R-devel changes to `str()` formatting by
  removing the brittle spacing assertion. Verified by GitHub Actions, as
  confirmed by the user.
- Switched pkgdown site building/deployment to GitHub Actions, updated the
  site configuration and Bootstrap theme, and removed the old Travis setup.
- Converted documentation to roxygen2 Markdown, refreshed package documentation,
  fixed typos and links, and added examples. Proofread all three vignettes.
- Restyled the package, adopted the native pipe (`|>`), and raised the minimum
  R version to 4.1. Converted vignette chunk options to in-body YAML syntax.
- Adopted testthat edition 3 and its standard test runner. Aligned test filenames
  with `R/` files, clarified test descriptions, and used `testthat::test_path()`
  for test fixtures.
- Added coverage for every `to_data_frame()` method, including metadata,
  column types/order, missing columns, empty strings, and empty inputs (#3).
- Made `preview_levels()`, `preview_frames()`, and `preview_eprime()` return
  their input invisibly; updated tests, help, vignette, and NEWS.
- Moved bundled vignette files to `inst/example-data` and added exported
  `example_eprime_file()`. Accepts short names or full filenames; omitting
  `which` prints the available index. Updated vignettes, README, and pkgdown.
- Fixed repeated filtering after zero matches with a type-stable logical index;
  regression tests cover all four filtering functions on empty inputs.
- Refreshed README source/output, package URLs, development metadata, and NEWS.
- Corrected NEWS issue references (#16 for the CRAN fix, #17 for upkeep) and
  excluded `inst/notes` from package builds via `.Rbuildignore`.

## Verification

- GitHub Actions verified the CRAN fix (user confirmed).
- Local `R CMD check` on Windows/R 4.6.1: **0 errors, 0 warnings, 0 notes**
  (`--as-cran --no-manual`, with remote incoming checks disabled). This preceded
  the final empty-filter fix; the full suite afterward passed **86 expectations**.
- All three vignettes execute; Rd and pkgdown checks pass. Bundled example-file
  lookup was also checked from an installed package. Whitespace checks pass.
- Local check subprocesses needed `LC_ALL=C`, `LANG=C`, and `LANGUAGE=en`;
  pkgdown used RStudio's bundled Pandoc.

## Remaining release housekeeping / deferred

- Before submission: set the release version and update `cran-comments.md`.
- Previewing an empty `FrameList` still errors; left for a separate small fix.
  `to_data_frame()` on empty inputs continues to return `NULL` intentionally
  for this upkeep scope.

## Style preferences

Use `dataframe` and `filepath`; write function references with `()` and use
roxygen2 links where useful. Prefer `EprimeFrame() objects` to awkward plurals,
and use unquoted package names in `library()` calls.
