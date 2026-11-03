# rprime 0.1.4

* `example_eprime_file()` provides filepaths to bundled example files by short
  name or full filename. Call it without arguments to list available files.

* [ ] Fix a failing test because of how `str()` works. (#17)

* `preview_eprime()`, `preview_frames()`, and `preview_levels()` now return
  their input invisibly, allowing previews within a data-processing pipeline.
* `to_data_frame()` now has tests covering its methods, metadata, missing
  columns, column types, and empty inputs (#3).
* Package upkeep. (#16)

# rprime 0.1.3

* Fix link in a documentation.

# rprime 0.1.2

* Fixed package for compatibility with stringi 1.5.3.
* Added a `NEWS.md` file to track changes to the package.
