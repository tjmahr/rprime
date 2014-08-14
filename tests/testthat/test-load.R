
# Test files
bad_encoding <- "data/Blending_001L00XS4.txt"
crashed_experiment <- "data/MP_Block1_001P00XA1.txt"
no_footer <- "data/Coartic_Block1_001P00XS1.txt"
not_an_eprime_file <- "data/not_an_eprime_file.txt"

is_header_line <- function(x) x == "*** Header Start ***"
first_line <- function(xs) xs[1]
count_blanks <- function(xs) sum(str_detect(xs, "^$"))

context("loading non-standard files")

test_that("load file with unexpected encoding", {
  # no warnings
  warnings <- evaluate_promise(read_eprime(bad_encoding))$warnings
  expect_equal(length(warnings), 0)

  # first line header and no blank lines
  bad_encoding_lines <- read_eprime(bad_encoding)
  expect_true(is_header_line(first_line(bad_encoding_lines)))
  expect_less_than(count_blanks(bad_encoding_lines), 1)
})

test_that("load file from a crashed experiment", {
  # no warnings
  warnings <- evaluate_promise(read_eprime(crashed_experiment))$warnings
  expect_equal(length(warnings), 0)

  # first line header and no blank lines
  crashed_experiment_lines <- read_eprime(crashed_experiment)
  expect_true(is_header_line(first_line(crashed_experiment_lines)))
  expect_less_than(count_blanks(crashed_experiment_lines), 1)
})

test_that("non-eprime file raises warning", {
  expect_warning(read_eprime(not_an_eprime_file), "not an Eprime txt file")
})

test_that("non-eprime file uses dummy text", {
  # first line header
  not_an_eprime_file_lines <- suppressWarnings(read_eprime(not_an_eprime_file))
  expect_true(is_header_line(first_line(not_an_eprime_file_lines)))
})
