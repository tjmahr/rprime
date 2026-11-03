#' Locate a bundled example Eprime file
#'
#' @param which A short name or full filename identifying an example file.
#'   Omit `which` to print the available names and filenames.
#' @return A filepath to the selected file. If `which` is missing, the named
#'   vector of available filenames is printed and returned invisibly.
#' @details This function uses [system.file()] to locate files bundled with
#'   rprime. Available short names are `Blending`, `MINP`, `MP`, `SAILS1`,
#'   `SAILS2`, and `not_an_eprime_file`.
#'
#'   `MP` contains an interrupted experiment. `not_an_eprime_file` is a
#'   non-Eprime file used to demonstrate how [read_eprime()] handles such input.
#' @export
#' @examples
#' example_eprime_file()
#' example_eprime_file("MINP")
#' example_eprime_file("MINP_001L00XS1.txt")
#' read_eprime(example_eprime_file("SAILS1"))
example_eprime_file <- function(which) {
  files <- c(
    Blending = "Blending_001L00XS4.txt",
    MINP = "MINP_001L00XS1.txt",
    MP = "MP_Block1_001P00XA1.txt",
    SAILS1 = "SAILS_001X00XS1.txt",
    SAILS2 = "SAILS_002X00XS1.txt",
    not_an_eprime_file = "not_an_eprime_file.txt"
  )

  if (missing(which)) {
    print(files)
    return(invisible(files))
  }

  if (!is.character(which) || length(which) != 1L || is.na(which)) {
    stop("`which` must be a single filename or short name.", call. = FALSE)
  }

  if (which %in% names(files)) {
    filename <- unname(files[[which]])
  } else if (which %in% files) {
    filename <- which
  } else {
    stop(
      "Unknown example file. Call example_eprime_file() to list files.",
      call. = FALSE
    )
  }

  if (filename %in% files[c("SAILS1", "SAILS2")]) {
    system.file("example-data", "SAILS", filename, package = "rprime", mustWork = TRUE)
  } else {
    system.file("example-data", filename, package = "rprime", mustWork = TRUE)
  }
}
