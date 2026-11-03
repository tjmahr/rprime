# Helpers
first <- function(...) head(..., n = 1)
last <- function(...) tail(..., n = 1)
first_line_header <- function(x) first(x) == "*** Header Start ***"
count_blanks <- function(xs) sum(stringr::str_detect(xs, "^$"))

# Test files
good_file <- testthat::test_path("data", "MINP_001L00XS1.txt")
bad_encoding <- testthat::test_path("data", "Blending_001L00XS4.txt")
crashed_experiment <- testthat::test_path("data", "MP_Block1_001P00XA1.txt")
no_footer <- testthat::test_path("data", "Coartic_Block1_001P00XS1.txt")
not_an_eprime_file <- testthat::test_path("data", "not_an_eprime_file.txt")


test_that("read_eprime() preserves frame boundaries and optionally removes clock lines", {
  eprime_log <- read_eprime(good_file, remove_clock = FALSE)
  expect_equal(first(eprime_log), "*** Header Start ***")
  expect_equal(eprime_log[29], "\t*** LogFrame Start ***")
  expect_equal(last(eprime_log), "*** LogFrame End ***")

  # Removing the clock removes 2 lines, one of which is in the header
  no_clock <- read_eprime(good_file, remove_clock = TRUE)
  expect_equal(no_clock[28], "\t*** LogFrame Start ***")
  expect_equal(length(no_clock) + 2, length(eprime_log))
})

test_that("read_eprime() reads unexpected encoding without warnings or blank lines", {
  # no warnings
  warnings <- evaluate_promise(read_eprime(bad_encoding))$warnings
  expect_equal(length(warnings), 0)

  # first line header and no blank lines
  bad_encoding_lines <- read_eprime(bad_encoding)
  expect_true(first_line_header(bad_encoding_lines))
  expect_lt(count_blanks(bad_encoding_lines), 1)
})

test_that("read_eprime() reads a crashed experiment without warnings or blank lines", {
  # no warnings
  warnings <- evaluate_promise(read_eprime(crashed_experiment))$warnings
  expect_equal(length(warnings), 0)

  # first line header and no blank lines
  crashed_experiment_lines <- read_eprime(crashed_experiment)
  expect_true(first_line_header(crashed_experiment_lines))
  expect_lt(count_blanks(crashed_experiment_lines), 1)
})

test_that("read_eprime() warns when the file is not an Eprime text file", {
  expect_warning(read_eprime(not_an_eprime_file), "not an Eprime txt file")
})

test_that("read_eprime() supplies a header when the file is not an Eprime text file", {
  # first line header
  not_an_eprime_file_lines <- suppressWarnings(read_eprime(not_an_eprime_file))
  expect_true(first_line_header(not_an_eprime_file_lines))
})
