require("stringr")
require("stringi")

# Test files
bad_encoding <- "data/Blending_001L00XS4.txt"
crashed_experiment <- "data/MP_Block1_001P00XA1.txt"
no_footer <- "data/Coartic_Block1_001P00XS1.txt"
not_an_eprime_file <- "data/not_an_eprime_file.txt"

is_header_line <- function(x) x == "*** Header Start ***"
first_line <- function(xs) xs[1]
count_blanks <- function(xs) sum(str_detect(xs, "^$"))




context("load file with unexpected encoding")

test_that("naive file-loading fails", {
  # embedded nul warnings
  expect_warning(readLines(bad_encoding, encoding = "UCS-2LE"),
                 regexp = "embedded nul")
  # lots of blank lines
  expect_more_than(count_blanks(readLines(bad_encoding, skipNul = TRUE)), 1)
})

test_that("read_eprime works", {
  # no warnings
  warnings <- evaluate_promise(read_eprime(bad_encoding))$warnings
  expect_equal(length(warnings), 0)

  bad_encoding_lines <- read_eprime(bad_encoding)

  # first line header and no blank lines
  expect_true(is_header_line(first_line(bad_encoding_lines)))
  expect_less_than(count_blanks(bad_encoding_lines), 1)
})




context("load file from a crashed experiment")

test_that("naive file-loading fails", {
  # incomplete final line warning
  expect_warning(readLines(crashed_experiment, skipNul = TRUE),
                 regexp = "incomplete final line")
})

test_that("read_eprime works", {
  # no warnings
  warnings <- evaluate_promise(read_eprime(crashed_experiment))$warnings
  expect_equal(length(warnings), 0)

  crashed_experiment_lines <- read_eprime(crashed_experiment)

  # first line header and no blank lines
  expect_true(is_header_line(first_line(crashed_experiment_lines)))
  expect_less_than(count_blanks(crashed_experiment_lines), 1)
})




context("load non-eprime file")

test_that("raises warning", {
  expect_warning(read_eprime(not_an_eprime_file), "not an Eprime txt file")
})

test_that("dummy text", {
  # first line header
  not_an_eprime_file_lines <- suppressWarnings(read_eprime(not_an_eprime_file))
  expect_true(is_header_line(first_line(not_an_eprime_file_lines)))
})
